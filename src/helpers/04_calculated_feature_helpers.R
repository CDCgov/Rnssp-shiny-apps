# These helpers are the single place where calculated feature values are
# generated from the fitted model and written into the stored data table.

# Format a quantile probability into the normalized string form used in
# calculated-feature IDs and quantile table column names.
calculated_feature_fmt_qname <- function(q, digits = 3) {
  q <- suppressWarnings(as.numeric(q))
  out <- prettyNum(round(q, digits), digits = 12, drop0trailing = TRUE)
  sub("^\\.", "0.", out)
}

# Standardize quantile-table column names returned by posterior helper
# functions so later lookups can use a consistent naming scheme.
calculated_feature_normalize_qdf_names <- function(qdf) {
  data.table::setDT(qdf)
  old <- names(qdf)
  new <- sub("^(props_|counts?_)", "", old)
  num_like <- !is.na(suppressWarnings(as.numeric(new)))
  if (any(num_like)) {
    new[num_like] <- calculated_feature_fmt_qname(new[num_like], digits = 12)
  }
  data.table::setnames(qdf, old, new, skip_absent = TRUE)
  qdf
}

# Keep only the region/date keys and requested quantile columns from a wide
# posterior quantile table.
calculated_feature_slice_qdf <- function(qdf, data_cls, cols_keep) {
  data.table::setDT(qdf)
  reg_col <- data_cls$region_column
  date_col <- data_cls$date_column
  if (reg_col %in% names(qdf)) {
    qdf[, (reg_col) := as.character(get(reg_col))]
  }
  keep <- intersect(c(reg_col, date_col, cols_keep), names(qdf))
  if (length(keep) == 0) {
    return(data.table::data.table())
  }
  qdf[, ..keep]
}

# Merge calculated feature values back onto the stored data using the app's
# canonical region/date keys while dropping overlapping value columns first.
calculated_feature_merge_by_region_date <- function(x, y, data_cls) {
  data.table::setDT(x)
  data.table::setDT(y)
  reg_col <- data_cls$region_column
  date_col <- data_cls$date_column
  x[, (reg_col) := as.character(get(reg_col))]
  y[, (reg_col) := as.character(get(reg_col))]
  overlap <- setdiff(intersect(names(x), names(y)), c(reg_col, date_col))
  if (length(overlap)) {
    x[, (overlap) := NULL]
  }
  data.table::setkeyv(x, c(reg_col, date_col))
  data.table::setkeyv(y, c(reg_col, date_col))
  y[x]
}

# Precompute and cache the posterior summary tables needed to calculate a set
# of calculated features efficiently.
build_calculated_feature_context <- function(features, model, data_cls) {
  features <- Filter(Negate(is.null), features)
  q_features <- Filter(function(f) {
    ft <- f$feature_type %||% ""
    ft %in% c("quantile", "confidence_interval")
  }, features)

  q_probs <- sort(unique(unlist(lapply(q_features, function(f) {
    ft <- f$feature_type %||% ""
    if (identical(ft, "quantile")) {
      return(as.numeric(f$params$q %||% 0.5))
    }
    if (identical(ft, "confidence_interval")) {
      ci <- as.numeric(f$params$ci %||% 0.90)
      a <- (1 - ci) / 2
      return(c(a, 1 - a))
    }
    numeric(0)
  }))))

  reg_col <- data_cls$region_column
  date_col <- data_cls$date_column

  qdf_props <- if (length(q_probs)) {
    qdf <- get_posterior_quantiles(model, data_cls, probs = q_probs, use_count_scale = FALSE)
    qdf <- calculated_feature_normalize_qdf_names(qdf)
    data.table::as.data.table(qdf)
  } else {
    NULL
  }

  qdf_counts <- if (length(q_probs)) {
    qdf <- get_posterior_quantiles(model, data_cls, probs = q_probs, use_count_scale = TRUE)
    qdf <- calculated_feature_normalize_qdf_names(qdf)
    qdf <- data.table::as.data.table(qdf)
    q_only <- setdiff(names(qdf), c(reg_col, date_col))
    if (length(q_only)) {
      data.table::setnames(qdf, q_only, paste0(q_only, "_count"))
    }
    qdf
  } else {
    NULL
  }

  list(
    model = model,
    data_cls = data_cls,
    qdf_props = qdf_props,
    qdf_counts = qdf_counts,
    mean_cache = new.env(parent = emptyenv()),
    exceed_cache = new.env(parent = emptyenv()),
    change_cache = new.env(parent = emptyenv())
  )
}

# Fetch posterior means on the requested scale and cache the keyed result for
# reuse across multiple calculated features.
get_calculated_feature_mean_dt <- function(context, use_count_scale = FALSE) {
  key <- if (use_count_scale) "counts" else "proportion"
  if (exists(key, envir = context$mean_cache, inherits = FALSE)) {
    return(get(key, envir = context$mean_cache, inherits = FALSE))
  }

  dcls <- context$data_cls
  reg_col <- dcls$region_column
  date_col <- dcls$date_column

  dt <- get_posterior_means(
    context$model,
    dcls,
    use_suffix = FALSE,
    use_count_scale = use_count_scale
  )
  data.table::setDT(dt)

  mean_col <- intersect(c("predicted_mean", "mean"), names(dt))[1]
  if (is.na(mean_col) || is.null(mean_col)) {
    return(data.table::data.table())
  }

  keep <- intersect(c(reg_col, date_col, mean_col), names(dt))
  if (!length(keep)) {
    return(data.table::data.table())
  }

  dt <- dt[, ..keep]
  if (reg_col %in% names(dt)) {
    dt[, (reg_col) := as.character(get(reg_col))]
  }

  out_col <- if (use_count_scale) "mean_count" else "mean_prop"
  data.table::setnames(dt, mean_col, out_col)
  assign(key, dt[], envir = context$mean_cache)
  get(key, envir = context$mean_cache, inherits = FALSE)
}

# Fetch and cache exceedance-probability summaries for a given threshold and
# scale combination.
get_calculated_feature_exceedance_dt <- function(context, threshold, use_count_scale = FALSE) {
  key <- paste0(threshold, "::", use_count_scale)
  if (exists(key, envir = context$exceed_cache, inherits = FALSE)) {
    return(get(key, envir = context$exceed_cache, inherits = FALSE))
  }

  dt <- get_exceedance_probs(
    inla_model = context$model,
    data_cls = context$data_cls,
    threshold = threshold,
    use_suffix = TRUE,
    use_count_scale = use_count_scale
  )
  assign(key, dt, envir = context$exceed_cache)
  get(key, envir = context$exceed_cache, inherits = FALSE)
}

# Fetch and cache probability-of-increase summaries for a given threshold,
# lookback, and scale-mode combination.
get_calculated_feature_change_dt <- function(context, threshold, dt, scale_mode, use_normal_approx = FALSE) {
  key <- paste(threshold, dt, scale_mode, use_normal_approx, sep = "::")
  if (exists(key, envir = context$change_cache, inherits = FALSE)) {
    return(get(key, envir = context$change_cache, inherits = FALSE))
  }

  dt_out <- epistemic::get_probability_of_increase(
    inla_model = context$model,
    data_cls = context$data_cls,
    threshold = threshold,
    dt = dt,
    scale_mode = scale_mode,
    use_suffix = TRUE,
    use_normal_approx = use_normal_approx
  )
  assign(key, dt_out, envir = context$change_cache)
  get(key, envir = context$change_cache, inherits = FALSE)
}

# Calculate one feature from the fitted model summaries and store its output
# columns in the shared data table.
calculate_and_store_calculated_feature <- function(out, feature, context) {
  dcls <- context$data_cls
  ft <- feature$feature_type %||% ""
  sc <- feature$feature_scale %||% "other"
  out_cols <- feature$out_cols %||% character(0)

  if (!length(out_cols)) {
    return(out)
  }

  data.table::setDT(out)
  out2 <- data.table::copy(out)
  for (cc in out_cols) {
    if (!cc %in% names(out2)) {
      out2[, (cc) := NA_real_]
    }
  }

  if (ft == "mean") {
    use_count <- identical(sc, "counts")
    res <- get_calculated_feature_mean_dt(context, use_count)
    mcol <- if (use_count) "mean_count" else "mean_prop"
    out2 <- calculated_feature_merge_by_region_date(out2, res, dcls)
    out2[, (out_cols[[1]]) := as.numeric(get(mcol))]
    out2[, (mcol) := NULL]
    return(out2[])
  }

  if (ft == "quantile") {
    qn <- calculated_feature_fmt_qname(feature$params$q %||% 0.5)
    if (identical(sc, "counts")) {
      qdf <- context$qdf_counts
      src <- paste0(qn, "_count")
    } else {
      qdf <- context$qdf_props
      src <- qn
    }
    out2 <- calculated_feature_merge_by_region_date(
      out2,
      calculated_feature_slice_qdf(qdf, dcls, src),
      dcls
    )
    out2[, (out_cols[[1]]) := as.numeric(get(src))]
    out2[, (src) := NULL]
    return(out2[])
  }

  if (ft == "confidence_interval") {
    ci <- feature$params$ci %||% 0.90
    a <- (1 - ci) / 2
    qL <- calculated_feature_fmt_qname(a)
    qU <- calculated_feature_fmt_qname(1 - a)
    if (identical(sc, "counts")) {
      qdf <- context$qdf_counts
      srcL <- paste0(qL, "_count")
      srcU <- paste0(qU, "_count")
    } else {
      qdf <- context$qdf_props
      srcL <- qL
      srcU <- qU
    }
    out2 <- calculated_feature_merge_by_region_date(
      out2,
      calculated_feature_slice_qdf(qdf, dcls, c(srcL, srcU)),
      dcls
    )
    out2[, (out_cols[[1]]) := as.numeric(get(srcL))]
    out2[, (out_cols[[2]]) := as.numeric(get(srcU))]
    out2[, c(srcL, srcU) := NULL]
    return(out2[])
  }

  if (ft == "exceedance_probability") {
    thr <- as.numeric(feature$params$threshold %||% 0)
    use_count <- identical(sc, "counts")
    res <- get_calculated_feature_exceedance_dt(context, thr, use_count)
    out2 <- calculated_feature_merge_by_region_date(out2, res, dcls)
    ex_col <- grep("^exceedance_prob", names(out2), value = TRUE)[1]
    out2[, (out_cols[[1]]) := as.numeric(get(ex_col))]
    out2[, (ex_col) := NULL]
    return(out2[])
  }

  if (ft == "change_probability") {
    thr <- as.numeric(feature$params$threshold %||% 0)
    dt_days <- as.integer(feature$params$dt %||% 1L)
    scale_mode <- as.character(feature$params$scale_mode %||% "absolute_count")
    use_normal_approx <- isTRUE(feature$params$use_normal_approx)
    res <- get_calculated_feature_change_dt(context, thr, dt_days, scale_mode, use_normal_approx)
    out2 <- calculated_feature_merge_by_region_date(out2, res, dcls)
    change_col <- grep("^change_prob", names(out2), value = TRUE)[1]
    out2[, (out_cols[[1]]) := as.numeric(get(change_col))]
    out2[, (change_col) := NULL]
    return(out2[])
  }

  out2
}

# Calculate a batch of supported calculated features and append their stored
# columns to the supplied data table.
calculate_and_store_calculated_features <- function(out, features, model, data_cls) {
  features <- Filter(function(f) {
    !is.null(f) && (f$feature_type %||% "") %in% c("mean", "quantile", "confidence_interval", "exceedance_probability", "change_probability")
  }, features)
  if (!length(features)) {
    return(data.table::as.data.table(out))
  }

  context <- build_calculated_feature_context(features, model, data_cls)
  out2 <- data.table::as.data.table(out)
  for (f in features) {
    out2 <- calculate_and_store_calculated_feature(out2, f, context)
  }
  out2[]
}
