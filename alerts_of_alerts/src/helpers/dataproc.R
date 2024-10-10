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

get_proc_data <- function(input, output, session) {
  reactive({
    ccdd_list <- "https://essence.syndromicsurveillance.org/nssp_essence/servlet/SyndromeDefinitionsServlet_CCDD?action=getCCDDTerms" %>%
      get_api_data(profile = myProfile) %>%
      pluck("categories") %>%
      select(category) %>%
      mutate(
        category_api = tolower(category),
        category_api = gsub(" ", "%20", category_api)
      ) %>%
      arrange(category)
    
    fips_data <- county_sf %>%
      rename(state_fips = STATEFP) %>%
      left_join(state_helper, by = "state_fips")
    
    # Conditional filtering
    if (input$state == 'All') {
      fips_for_url <- fips_data %>%
        pull(GEOID) %>%
        as.character() %>%
        paste0(., collapse = "&facilityfips=")
    } else {
      fips_for_url <- fips_data %>%
        filter(state_name == input$state) %>%
        pull(GEOID) %>%
        as.character() %>%
        paste0(., collapse = "&facilityfips=")
    }
    
    category_for_url <- ccdd_list %>%
      filter(category == input$ccdd) %>%
      pull(category_api)
    
    url <- paste0(
      "https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?nonZeroComposite=false",
      "&startMonth=january&graphOnly=true&datasource=va_hosp&startDate=",
      format(input$startDate %m-% days(13), "%d%b%Y"),
      "&medicalGroupingSystem=essencesyndromes&userId=2362&multiStratVal=facilityfips&endDate=",
      format(input$endDate, "%d%b%Y"),
      "&facilityfips=", fips_for_url,
      "&percentParam=ccddCategory&graphOptions=multiplesmall&aqtTarget=TimeSeries&ccddCategory=",
      category_for_url,
      "&geographySystem=hospital&detector=probrepswitch&removeZeroSeries=true&timeResolution=daily&hasBeenE=1"
    )
    
    withProgress(message="Loading and processing data:", value = 0, {
      incProgress(0.25, detail = "Loading data...")
      df <- url %>%
        get_api_data(profile = myProfile) %>% 
        {
          if (length(.$timeSeriesData) == 0) {
            NULL
          } else {
            pluck(., "timeSeriesData") %>%
              clean_names() %>%
              mutate(
                date = as.Date(date),
                alert_percent = case_when(
                  color %in% c("grey", "blue") ~ "None",
                  color == "yellow" ~ "Warning",
                  color == "red" ~ "Alert"
                ),
                alert_count = case_when(
                  color_data_count %in% c("grey", "blue") ~ "None",
                  color_data_count == "yellow" ~ "Warning",
                  color_data_count == "red" ~ "Alert"
                )
              ) %>%
              separate(line_label, c("state_abbr", "county"), sep = " - ") %>%
              select(
                state_abbr, fips = facilityfips_id, county, date, data_count,
                all_count, percent = count, alert_percent, alert_count,
                p = levels, color, color_data_count
              ) %>%
              arrange(fips, date) %>%
              mutate(
                warning_or_alert_percent_factor = ifelse(
                  alert_percent %in% c("Warning", "Alert"), 1, 0
                ),
                alert_percent_factor = ifelse(
                  alert_percent == "Alert", 1, 0
                ),
                alert_count_factor = ifelse(
                  alert_count == "Alert", 1, 0
                ),
                p = as.numeric(p)
              )
          }
        }
      
      #-----Compute each of total % CCDD alerts, alerts of alerts, and increasing CCDD percent alerts-----
      
      # Compute alerts over state-wide CCDD Category % df
      
      incProgress(0.25, detail = "Testing CCDD Category % for Alerts...")
      df_switch_percent <- df %>%
        group_by(date) %>%
        summarise(
          total_CCDD = sum(data_count),
          total_all = sum(all_count),
          .groups = "drop"
        ) %>%
        mutate(percent = (total_CCDD / total_all) * 100.0) %>%
        {
          nan_dates <- .$date[is.nan(.$percent)]
          if (length(nan_dates) > 0) {
            warning(paste("0 statewide visits reported for dates:", paste(nan_dates, collapse = ", "), ". CCDD Category % treated as 0."))
          }
          mutate(., percent = ifelse(is.nan(percent), 0, percent))
        } %>%
        select(date, percent) %>%
        alert_switch_(., t = date, y = percent) %>%
        select(
          date,
          percent,
          alert_percent = alert,
          p.value_percent = p.value
        )
      
      # Compute alerts over total alert counts df
      
      incProgress(0.25, detail = "Testing daily Alert counts % for Alerts...")
      df_switch_alert_count <- df %>%
        group_by(date) %>%
        summarise(count = sum(alert_percent == "Alert")) %>%
        alert_switch_(., t = date, y = count) %>%
        select(
          date,
          count,
          alert_alert = alert,
          p.value_alert = p.value
        )
      
      # Compute trend classification (i.e., increasing, decreasing, stable) alerts of alerts df
      
      incProgress(0.25, detail = "Estimating county trends and testing for increase (this may take a moment)...")
      
      ed_county_waves <- df %>%
        nest(data = -fips) %>%
        mutate(
          model = map(.x = data, function(.x) {
            sparsity_ratio90 <- .x %>%
              arrange(date) %>%
              slice_tail(n = 90) %>%
              mutate(
                nonzero_obs = sum(data_count > 0),
                sp_ratio = n() / nonzero_obs
              ) %>%
              pull(sp_ratio) %>%
              unique()
            
            sparsity_ratio <- .x %>%
              arrange(date) %>%
              filter(date >= max(date) %m-% years(1) + 1) %>%
              mutate(
                nonzero_obs = sum(data_count > 0),
                sp_ratio = n() / nonzero_obs
              ) %>%
              pull(sp_ratio) %>%
              unique()
            
            if (sparsity_ratio < 5 & sparsity_ratio90 < 5) {
              .y <- .x %>%
                select(
                  date,
                  data_count,
                  all_count
                ) %>%
                mutate(date = as.double(date))
              
              .gam_out <- gam(
                cbind(data_count, all_count - data_count) ~ s(as.double(date),
                                                              bs = "ad"
                ),
                family = binomial,
                data = .y,
                method = "REML",
                control = gam.control(maxit = 3, eps = 1e-2, mgcv.tol = 1e-2, trace = FALSE)
              )
              
              if (.gam_out$converged) {
                .deriv <- derivatives(.gam_out, data = .y, n = nrow(.y), level = 0.95, type = "central")
                
                data.frame(
                  fitted = .gam_out$fitted.values * 100,
                  sp_ratio90 = sparsity_ratio90,
                  sp_ratio = sparsity_ratio
                ) %>%
                  bind_cols(.deriv) %>%
                  rename(
                    deriv = .derivative,
                    lower = .lower_ci,
                    upper = .upper_ci
                  )
              } else {
                data.frame(
                  fitted = rep(NA, nrow(.x)),
                  sp_ratio90 = sparsity_ratio90,
                  sp_ratio = sparsity_ratio
                ) %>%
                  mutate(
                    lower = NA,
                    deriv = NA,
                    upper = NA
                  )
              }
            } else {
              data.frame(
                fitted = rep(NA, nrow(.x)),
                sp_ratio90 = sparsity_ratio90,
                sp_ratio = sparsity_ratio
              ) %>%
                mutate(
                  lower = NA,
                  deriv = NA,
                  upper = NA
                )
            }
          })
        ) %>%
        unnest(c(data, model)) %>%
        group_by(fips) %>%
        mutate(
          row = row_number(),
          trajectory = case_when(
            is.na(sp_ratio90) ~ "Sparse",
            sp_ratio90 >= 5 | sp_ratio >= 5 ~ "Sparse",
            deriv > 0 & lower > 0 ~ "Increasing",
            deriv < 0 & upper < 0 ~ "Decreasing",
            deriv > 0 & lower < 0 ~ "Stable",
            deriv < 0 & upper > 0 ~ "Stable",
            is.na(deriv) ~ "Sparse",
            TRUE ~ "Stable"
          )
        ) %>%
        group_by(fips, grp = with(rle(trajectory), rep(seq_along(lengths), lengths))) %>%
        mutate(
          period_total = max(seq_along(grp)),
          counter = seq_along(grp),
          start_date = min(date)
        ) %>%
        ungroup() %>%
        mutate(trajectory = factor(trajectory, levels = c("Increasing", "Stable", "Decreasing", "Sparse"))) %>%
        mutate(inc_factor = ifelse(trajectory == "Increasing", 1, 0))
      
      ed_increasing_status <- ed_county_waves %>%
        group_by(date) %>%
        summarise(
          data_count = sum(data_count),
          all_count = sum(all_count),
          n_increasing = sum(trajectory == "Increasing"),
          n_decreasing = sum(trajectory == "Decreasing"),
          n_stable = sum(trajectory == "Stable"),
          n_total = n_distinct(fips)
        ) %>%
        mutate(
          percent = (data_count / all_count) * 100,
          n_check = n_increasing + n_decreasing + n_stable
        ) %>%
        ungroup() %>%
        mutate(
          percent_increasing = (n_increasing / n_total) * 100,
          percent_decreasing = (n_decreasing / n_total) * 100,
          percent_stable = (n_stable / n_total) * 100
        )
      
      ed_anomalies_trend <- ed_increasing_status %>%
        mutate(
          sparsity_ratio90 = {
            ed_increasing_status %>%
              arrange(date) %>%
              slice_tail(n = 90) %>%
              mutate(
                nonzero_obs = sum(n_increasing > 0),
                sp_ratio = n() / nonzero_obs
              ) %>%
              pull(sp_ratio) %>%
              unique()
          },
          sparsity_ratio = {
            ed_increasing_status %>%
              arrange(date) %>%
              filter(date >= max(date) %m-% years(1) + 1) %>%
              mutate(
                nonzero_obs = sum(n_increasing > 0),
                sp_ratio = n() / nonzero_obs
              ) %>%
              pull(sp_ratio) %>%
              unique()
          }
        ) %>%
        mutate(
          model_output = if (sparsity_ratio[1] < 5 & sparsity_ratio90[1] < 5) {
            .y <- ed_increasing_status %>%
              select(
                date,
                n_increasing,
                n_total
              ) %>%
              mutate(date = as.double(date))
            
            .gam_out <- gam(
              cbind(n_increasing, n_total - n_increasing) ~ s(as.double(date),
                                                              bs = "ad"
              ),
              family = binomial,
              data = .y,
              method = "REML",
              control = gam.control(maxit = 3, eps = 1e-2, mgcv.tol = 1e-2, trace = FALSE)
            )
            
            if (.gam_out$converged) {
              .deriv <- derivatives(.gam_out, data = .y, n = nrow(.y), level = 0.95, type = "central")
              
              data.frame(
                fitted = .gam_out$fitted.values * n_total,
                sp_ratio90 = sparsity_ratio90,
                sp_ratio = sparsity_ratio
              ) %>%
                bind_cols(.deriv) %>%
                rename(
                  deriv = .derivative,
                  lower = .lower_ci,
                  upper = .upper_ci
                )
            } else {
              data.frame(
                fitted = rep(NA, nrow(ed_increasing_status)),
                sp_ratio90 = sparsity_ratio90,
                sp_ratio = sparsity_ratio
              ) %>%
                mutate(
                  lower = NA,
                  deriv = NA,
                  upper = NA
                )
            }
          } else {
            data.frame(
              fitted = rep(NA, nrow(ed_increasing_status)),
              sp_ratio90 = sparsity_ratio90,
              sp_ratio = sparsity_ratio
            ) %>%
              mutate(
                lower = NA,
                deriv = NA,
                upper = NA
              )
          }
        ) %>%
        unnest(cols = c(model_output)) %>%
        mutate(
          row = row_number(),
          trajectory = case_when(
            is.na(sp_ratio90) ~ "Sparse",
            sp_ratio90 >= 5 | sp_ratio >= 5 ~ "Sparse",
            deriv > 0 & lower > 0 ~ "Increasing",
            deriv < 0 & upper < 0 ~ "Decreasing",
            deriv > 0 & lower < 0 ~ "Stable",
            deriv < 0 & upper > 0 ~ "Stable",
            is.na(deriv) ~ "Sparse",
            TRUE ~ "Stable"
          )
        ) %>%
        group_by(grp = with(rle(trajectory), rep(seq_along(lengths), lengths))) %>%
        mutate(
          period_total = max(seq_along(grp)),
          counter = seq_along(grp),
          start_date = min(date)
        ) %>%
        ungroup() %>%
        mutate(trajectory = factor(trajectory, levels = c("Increasing", "Stable", "Decreasing", "Sparse"))) %>%
        mutate(
          alert_trend = case_when(
            trajectory == "Increasing" ~ "red",
            trajectory == "Stable" ~ "yellow",
            trajectory == "Decreasing" ~ "blue",
            trajectory == "Sparse" ~ "lightgray"
          ),
          alert_trend = factor(alert_trend, levels = c("red", "yellow", "blue", "lightgray"))
        ) %>%
        select(
          date,
          n_increasing,
          fitted,
          trajectory,
          alert_trend
        )
      
      df_all <- df_switch_alert_count %>%
        left_join(., df_switch_percent, by = "date") %>%
        left_join(., ed_anomalies_trend, by = "date") %>%
        tail(., -13)
      
      list(
        "df_all" = df_all,
        "ed_county_waves" = ed_county_waves
      )
    })
  })
}