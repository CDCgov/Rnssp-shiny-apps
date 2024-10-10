#-------------------------------------------------
# Rnssp-shiny-apps: alerts_of_alerts App
#
# Authors:
#   Joshua Kimrey
#   Michael Sheppard
#   Raghav Ramachandran
#   Howard Burkom
#   Roseric Azondekon
#-------------------------------------------------

# Help Popup
helpPopup <- function(
    id,
    word = "methods",
    title,
    content,
    placement = c("right", "top", "left", "bottom"),
    trigger = c("click", "hover", "focus", "manual"),
    icon_name = "question-circle",
    icon_style = "color:red; cursor:pointer;") {
  tagList(
    singleton(
      tags$head(
        tags$script(
          HTML("
            $(function() {
              $('[data-toggle=\"popover\"]').popover({ html : true, sanitize: false });
            });")
        )
      )
    ),
    HTML(id),
    tags$span(
      shiny::icon(name = icon_name, class = "shinyhelper-icon", style = icon_style),
      word,
      style = paste("margin-left:10px;", icon_style),
      `data-toggle` = "popover",
      title = title,
      `data-content` = content,
      `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok = TRUE)[1]
    )
  )
}


#-------------------------------------------------------------------------------
# -------------------Rnssp replacement code for alert_switch()------------------
# Plan to pull this out when Rnssp and ESSENCE versions are synced

# Alert Switch (identical to Rnssp 0.3.0 except for "alert_ewma_(...)")
alert_switch_ <- function(df, t = date, y = count, B = 28,
                          g = 2, w1 = 0.4, w2 = 0.9) {
  
  # Check baseline length argument
  if (B < 7) {
    cli::cli_abort("Error in {.fn alert_switch}: baseline length argument {.var B} must be greater than or equal to 7")
  }
  
  if (B %% 7 != 0) {
    cli::cli_abort("Error in {.fn alert_switch}: baseline length argument {.var B} must be a multiple of 7")
  }
  
  # Check guardband length argument
  if (g < 0) {
    cli::cli_abort("Error in {.fn alert_switch}: guardband length argument {.var g} cannot be negative")
  }
  
  # Check for sufficient baseline data
  if (nrow(df) < B + g + 1) {
    cli::cli_abort("Error in {.fn alert_switch}: not enough historical data")
  }
  
  # Check for grouping variables
  grouped_df <- is.grouped_df(df)
  
  t <- enquo(t)
  y <- enquo(y)
  
  base_tbl <- df %>%
    mutate({{ t }} := as.Date(!!t))
  
  alert_tbl_reg <- base_tbl %>%
    alert_regression(t = !!t, y = !!y, B = B, g = g) %>%
    select(-sigma) %>%
    mutate(detector = "Adaptive Multiple Regression")
  
  alert_tbl_ewma <- base_tbl %>%
    alert_ewma(t = !!t, y = !!y, B = B, g = g, w1 = w1, w2 = w2) %>%
    mutate(detector = "EWMA")
  
  join_cols <- setdiff(names(alert_tbl_reg), c("baseline_expected", "test_statistic", "p.value", "adjusted_r_squared", "alert", "detector"))
  
  replace_dates <- alert_tbl_reg %>%
    filter(is.na(adjusted_r_squared) | adjusted_r_squared < 0.60) %>%
    select(-c(baseline_expected, test_statistic, p.value, adjusted_r_squared, alert, detector)) %>%
    inner_join(alert_tbl_ewma, by = join_cols)
  
  if (grouped_df) {
    groups <- group_vars(base_tbl)
    
    combined_out <- alert_tbl_reg %>%
      filter(adjusted_r_squared >= 0.60) %>%
      select(-adjusted_r_squared) %>%
      bind_rows(replace_dates) %>%
      arrange(!!!syms(groups), !!enquo(t)) %>%
      mutate(detector = ifelse(is.na(test_statistic), NA, detector))
  } else {
    combined_out <- alert_tbl_reg %>%
      filter(adjusted_r_squared >= 0.60) %>%
      select(-adjusted_r_squared) %>%
      bind_rows(replace_dates) %>%
      arrange(!!enquo(t)) %>%
      mutate(detector = ifelse(is.na(test_statistic), NA, detector))
  }
  return(combined_out)
}

# adaptive_regression() from Rnssp
adaptive_regression <- function(df, t, y, B, g) {
  t <- enquo(t)
  y <- enquo(y)
  
  N <- nrow(df)
  
  # Populate algorithm parameters
  min_df <- 3
  min_baseline <- 11
  max_baseline <- B
  df_range <- 1:(B - min_df)
  
  ucl_alert <- round(qt(1 - 0.01, df = df_range), 5)
  ucl_warning <- round(qt(1 - 0.05, df = df_range), 5)
  
  # Bound standard error of regression
  min_sigma <- 1.01 / ucl_warning
  
  # Initialize result vectors
  test_stat <- rep(NA, N)
  p_val <- rep(NA, N)
  expected <- rep(NA, N)
  sigma <- rep(NA, N)
  r_sqrd_adj <- rep(NA, N)
  
  # Vector of dates
  dates <- df %>%
    pull(!!t)
  
  # Vector of observations
  y_obs <- df %>%
    pull(!!y)
  
  # Initialize baseline indices
  ndx_baseline <- 1:(min_baseline - 1)
  
  # Adaptive multiple regression loop
  for (i in (min_baseline + g + 1):N) {
    
    # Pad baseline until full baseline is obtained
    if (last(ndx_baseline) < max_baseline) {
      ndx_baseline <- c(0, ndx_baseline)
    }
    
    # Advance baseline for current iteration
    ndx_baseline <- ndx_baseline + 1
    
    # Indices for baseline and test date
    if (last(ndx_baseline) < max_baseline) {
      ndx_time <- 1:last(ndx_baseline)
      ndx_test <- last(ndx_baseline) + g + 1
    } else {
      ndx_time <- 1:B
      ndx_test <- (B + g + 1)
    }
    
    # Set number of degrees of freedom
    n_df <- length(ndx_baseline) - 8
    
    # Baseline and current data
    baseline_data <- df[ndx_baseline, ]
    
    B_length <- length(ndx_baseline)
    
    # Baseline observed values
    baseline_obs <- baseline_data %>%
      pull(!!y)
    
    # Form regression matrix
    X <- as.matrix(
      cbind(ndx_time,
            baseline_data[, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat")])
    )
    
    # Fit regression model with lm.fit() for efficiency
    lm_fit <- lm.fit(x = cbind(1, X), y = baseline_obs)
    
    # Extract model components
    beta <- lm_fit$coefficients
    res <- lm_fit$residuals
    mse <- (1 / (n_df)) * sum(res^2)
    
    # Compute adjusted R-squared value
    fit_vals <- lm_fit$fitted.values
    mss <- sum((fit_vals - mean(fit_vals))^2)
    rss <- sum(res^2)
    r2 <- (mss / (rss + mss))
    r2_adj <- 1 - (1 - r2) * ((nrow(X) - 1) / lm_fit$df.residual)
    r_sqrd_adj[i] <- if_else(is.nan(r2_adj), 0, r2_adj)
    
    # Calculate bounded standard error of regression with derived formula for efficiency
    sigma[i] <- max(
      sqrt(mse) * sqrt(((B_length + 7) * (B_length - 4)) /
                         (B_length * (B_length - 7))), min_sigma[n_df]
    )
    
    # Day of week for test date
    dow_test <- as.numeric(format(dates[i], "%u"))
    
    # Calculate forecast on test date
    expected[i] <- if (dow_test < 7) {
      max(0, beta[[1]] + ndx_test * beta[[2]] + beta[[dow_test + 2]])
    } else {
      max(0, beta[[1]] + ndx_test * beta[[2]])
    }
    
    # Calculate test statistic
    test_stat[i] <- (y_obs[i] - expected[i]) / sigma[i]
    
    # Calculate p-value
    p_val[i] <- 1 - pt(test_stat[i], df = n_df)
  }
  
  tibble::tibble(
    baseline_expected = expected,
    test_statistic = test_stat,
    p.value = p_val,
    sigma = sigma,
    adjusted_r_squared = r_sqrd_adj
  )
}

# alert_regression() from Rnssp
alert_regression <- function(df, t = date, y = count, B = 28, g = 2) {
  
  # Check baseline length argument
  if (B < 7) {
    cli::cli_abort("Error in {.fn alert_regression}: baseline length argument
                   {.var B} must be greater than or equal to 7")
  }
  
  if (B %% 7 != 0) {
    cli::cli_abort("Error in {.fn alert_regression}: baseline length argument
                   {.var B} must be a multiple of 7")
  }
  
  # Check guardband length argument
  if (g < 0) {
    cli::cli_abort("Error in {.fn alert_regression}: guardband length argument
                   {.var g} cannot be negative")
  }
  
  # Check for sufficient baseline data
  if (nrow(df) < B + g + 1) {
    cli::cli_abort("Error in {.fn alert_regression}: not enough historical data")
  }
  
  # Check for grouping variables
  grouped_df <- is.grouped_df(df)
  
  t <- enquo(t)
  y <- enquo(y)
  
  base_tbl <- df %>%
    mutate(
      {{ t }} := as.Date(!!t),
      dow = weekdays(!!t, abbreviate = TRUE),
      dummy = 1
    ) %>%
    pivot_wider(names_from = dow, values_from = dummy, values_fill = 0)
  
  if (grouped_df) {
    groups <- group_vars(base_tbl)
    
    base_tbl %>%
      nest(data_split = -all_of(groups)) %>%
      mutate(anomalies = map(
        .x = data_split,
        .f = adaptive_regression, t = !!t, y = !!y, B = B, g = g)
      ) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      ) %>%
      select(-c(Mon, Tue, Wed, Thu, Fri, Sat, Sun))
  } else {
    unique_dates <- base_tbl %>%
      pull(!!t) %>%
      unique()
    
    if (length(unique_dates) != nrow(base_tbl)) {
      cli::cli_abort("Error in {.fn alert_regression}: Number of unique dates does
                     not equal the number of rows. Should your dataframe be grouped?")
    }
    
    base_tbl %>%
      nest(data_split = everything()) %>%
      mutate(anomalies = map(
        .x = data_split,
        .f = adaptive_regression, t = !!t, y = !!y, B = B, g = g)
      ) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      ) %>%
      select(-c(Mon, Tue, Wed, Thu, Fri, Sat, Sun))
  }
}

# modified ewma_loop()

ewma_loop <- function(df, t, y, B, g, w1, w2) {
  t <- enquo(t)
  y <- enquo(y)
  
  N <- nrow(df)
  
  # Populate algorithm parameters
  min_df <- 3
  min_baseline <- 11
  max_baseline <- 28
  length_baseline <- min_baseline:max_baseline
  
  # Vector of observations
  y <- df %>%
    pull(!!y)
  if(max(y) <= 1) { y <- y / median(y[y>0]) }   # 16May2023 edit for application to proportion data
  
  
  # Initialize result vectors
  expected <- rep(NA, N)
  z1 <- rep(NA, N)
  z2 <- rep(NA, N)
  sigma1 <- rep(NA, N)
  sigma2 <- rep(NA, N)
  test_stat1 <- rep(NA, N)
  test_stat2 <- rep(NA, N)
  pval1 <- rep(NA, N)
  pval2 <- rep(NA, N)
  
  z <- z1
  test_stat <- test_stat1
  p_val <- pval1
  
  # Initialize EWMA values
  z1[1] <- y[1]
  z2[1] <- y[1]
  
  for (i0 in 2:(min_baseline + g)) {
    z1[i0] <- w1 * y[i0] + (1 - w1) * z1[i0 - 1]
    z2[i0] <- w2 * y[i0] + (1 - w2) * z2[i0 - 1]
  }
  
  # Initialize baseline indices
  ndx_baseline <- 1:(min_baseline - 1)
  
  # EWMA loop
  for (i in (min_baseline + g + 1):N) {
    
    # Pad baseline until full baseline is obtained
    if (last(ndx_baseline) < max_baseline) {
      ndx_baseline <- c(0, ndx_baseline)
    }
    
    # Advance baseline for current iteration
    ndx_baseline <- ndx_baseline + 1
    
    # Set number of degrees of freedom
    n_df <- length(ndx_baseline) - 1
    
    # Baseline and current data
    y_baseline <- y[ndx_baseline]
    
    expected[i] <- mean(y_baseline)
    sigma <- sd(y_baseline)
    
    sigma_correction1 <- sqrt((w1 / (2 - w1)) + (1 / length(ndx_baseline)) - 2 * (1 - w1)^(g + 1) * ((1 - (1 - w1)^length(ndx_baseline)) / length(ndx_baseline)))
    sigma_correction2 <- sqrt((w2 / (2 - w2)) + (1 / length(ndx_baseline)) - 2 * (1 - w2)^(g + 1) * ((1 - (1 - w2)^length(ndx_baseline)) / length(ndx_baseline)))
    
    ucl_alert <- round(qt(1 - 0.01, df = n_df), 5)
    ucl_warning <- round(qt(1 - 0.05, df = n_df), 5)
    
    min_sigma1 <- (w1 / ucl_warning) * (1 + 0.5 * (1 - w1)^2)
    min_sigma2 <- (w2 / ucl_warning) * (1 + 0.5 * (1 - w2)^2)
    
    constant1 <- (0.1289 - (0.2414 - 0.1826 * (1 - w1)^4) * log(10 * 0.05)) * (w1 / ucl_warning)
    constant2 <- (0.1289 - (0.2414 - 0.1826 * (1 - w2)^4) * log(10 * 0.05)) * (w2 / ucl_warning)
    
    sigma1[i] <- max(min_sigma1, sigma * sigma_correction1 + constant1)
    sigma2[i] <- max(min_sigma2, sigma * sigma_correction2 + constant2)
    
    # EWMA values
    z1[i] <- w1 * y[i] + (1 - w1) * z1[i - 1]
    z2[i] <- w2 * y[i] + (1 - w2) * z2[i - 1]
    
    # Calculate test statistics
    test_stat1[i] <- (z1[i] - expected[i]) / sigma1[i]
    test_stat2[i] <- (z2[i] - expected[i]) / sigma2[i]
    
    if (abs(test_stat1[i]) > ucl_alert) {
      z1[i] <- expected[i] + sign(test_stat1[i]) * ucl_alert * sigma1[i]
    }
    
    if (abs(test_stat2[i]) > ucl_alert) {
      z2[i] <- expected[i] + sign(test_stat2[i]) * ucl_alert * sigma2[i]
    }
    
    # Compute p-values
    pval1[i] <- 1 - pt(test_stat1[i], df = n_df)
    pval2[i] <- 1 - pt(test_stat2[i], df = n_df)
    
    # Determine minimum p-value
    if (pval1[i] < pval2[i]) {
      p_val[i] <- pval1[i]
      test_stat[i] <- test_stat1[i]
      z[i] <- z1[i]
    } else {
      p_val[i] <- pval2[i]
      test_stat[i] <- test_stat2[i]
      z[i] <- z2[i]
    }
  }
  
  tibble::tibble(
    baseline_expected = expected,
    test_statistic = test_stat,
    p.value = p_val
  )
}

# modified alert_ewma()
alert_ewma <- function(df, t = date, y = count, B = 28, g = 2, w1 = 0.4, w2 = 0.9) {
  
  # Check baseline length argument
  if (B < 7) {
    cli::cli_abort("Error in {.fn alert_ewma}: baseline length argument {.var B} must be greater than or equal to 7")
  }
  
  # Check guardband length argument
  if (g < 0) {
    cli::cli_abort("Error in {.fn alert_ewma}: guardband length argument {.var g} cannot be negative")
  }
  
  # Check for sufficient baseline data
  if (nrow(df) < B + g + 1) {
    cli::cli_abort("Error in {.fn alert_ewma}: not enough historical data")
  }
  
  # Check for grouping variables
  grouped_df <- is.grouped_df(df)
  
  t <- enquo(t)
  y <- enquo(y)
  
  base_tbl <- df %>%
    mutate({{ t }} := as.Date(!!t))
  
  if (grouped_df) {
    groups <- group_vars(base_tbl)
    
    alert_tbl <- base_tbl %>%
      nest(data_split = -all_of(groups)) %>%
      mutate(anomalies = map(.x = data_split, .f = ewma_loop, t = !!t, y = !!y, B = B, g = g, w1 = w1, w2 = w2)) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      )
    
    return(alert_tbl)
  } else {
    unique_dates <- base_tbl %>%
      pull(!!t) %>%
      unique()
    
    if (length(unique_dates) != nrow(base_tbl)) {
      cli::cli_abort("Error in {.fn alert_regression}: Number of unique dates does not equal the number of rows. Should your dataframe be grouped?")
    }
    
    alert_tbl <- base_tbl %>%
      nest(data_split = everything()) %>%
      mutate(anomalies = map(.x = data_split, .f = ewma_loop, t = !!t, y = !!y, B = B, g = g, w1 = w1, w2 = w2)) %>%
      unnest(c(data_split, anomalies)) %>%
      mutate(
        alert = case_when(
          p.value < 0.01 ~ "red",
          p.value >= 0.01 & p.value < 0.05 ~ "yellow",
          p.value >= 0.05 ~ "blue",
          TRUE ~ "grey"
        )
      )
  }
}

## plotly annotations
get_annotations <- function(topy, middley, bottomy) {
  list(
    # annotations for 1st legendgroup
    list(
      x = 1.1, y = topy, xref = "paper", yref = "paper", showarrow = FALSE,
      text = "None", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = topy, xref = "paper", yref = "paper", showarrow = FALSE,
      text = "", bgcolor = "blue", bordercolor = "black", borderwidth = 1,
      borderpad = 1, height = 10, width = 10
    ),
    list(
      x = 1.1, y = topy - 0.05, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "Warning", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = topy - 0.05, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "", bgcolor = "yellow", bordercolor = "black",
      borderwidth = 1, borderpad = 1, height = 10, width = 10
    ),
    list(
      x = 1.1, y = topy - 0.1, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "Alert", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = topy - 0.1, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "", bgcolor = "red", bordercolor = "black", 
      borderwidth = 1, borderpad = 1, height = 10, width = 10
    ),
    # annotations for 2nd legendgroup
    list(
      x = 1.1, y = middley, xref = "paper", yref = "paper", showarrow = FALSE,
      text = "None", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = middley, xref = "paper", yref = "paper", showarrow = FALSE,
      text = "", bgcolor = "blue", bordercolor = "black", borderwidth = 1,
      borderpad = 1, height = 10, width = 10
    ),
    list(
      x = 1.1, y = middley - 0.05, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "Warning", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = middley - 0.05, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "", bgcolor = "yellow", bordercolor = "black",
      borderwidth = 1, borderpad = 1, height = 10, width = 10
    ),
    list(
      x = 1.1, y = middley - 0.1, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "Alert", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = middley - 0.1, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "", bgcolor = "red", bordercolor = "black",
      borderwidth = 1, borderpad = 1, height = 10, width = 10
    ),
    # annotations for 3rd legendgroup
    list(
      x = 1.1, y = bottomy, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "Decreasing", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = bottomy, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "", bgcolor = "blue", bordercolor = "black", 
      borderwidth = 1, borderpad = 1, height = 10, width = 10
    ),
    list(
      x = 1.1, y = bottomy - 0.05, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "Stable", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = bottomy - 0.05, xref = "paper", yref = "paper", 
      showarrow = FALSE, text = "", bgcolor = "yellow", bordercolor = "black",
      borderwidth = 1, borderpad = 1, height = 10, width = 10
    ),
    list(
      x = 1.1, y = bottomy - 0.1, xref = "paper", yref = "paper",
      showarrow = FALSE, text = "Increasing", xanchor = "left", align = "left"
    ),
    list(
      x = 1.1, y = bottomy - 0.1, xref = "paper", yref = "paper",
      showarrow = FALSE, text = "", bgcolor = "red", bordercolor = "black",
      borderwidth = 1, borderpad = 1, height = 10, width = 10
    )
  )
}
