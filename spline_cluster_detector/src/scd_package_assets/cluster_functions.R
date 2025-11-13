# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

#' Get candidate clusters and locations in baseline intervals
#'
#' Given raw case counts by location, and some dates and other params return
#' candidate clusters and counts
#' @param cases frame of cases with counts, location(s) and dates
#' @param detect_date date to end examination of detection of clusters
#' @param baseline_length number of days (integer) used for baseline detection
#'   (default = 90)
#' @param max_test_window_days integer, max number of days in a detected
#'   cluster, defaults to 7
#' @param guard_band integer (default=0) number of days buffer between test
#'   interval and baseline
#' @param baseline_adjustment one of three string options: "add_one" (default),
#'   "add_test", or "none".  All methods except for "none" will ensure that the
#'   log(obs/expected) is always defined (i.e. avoids expected =0). For the
#'   default, this will add 1 to the expected for any individual calculation if
#'   expected would otherwise be zero. For "add_test_interval", each location in
#'   the baseline is increased by the number of cases in that location during
#'   the test interval. If "none", no adjustment is made.
#' @param adj_constant numeric (default=1.0); this is the constant to be added
#'   if \code{baseline_adjustment == 'add_one'} or \code{baseline_adjustment ==
#'   'add_one'}
#' @export
#' @returns an object of class `CaseGrids` contain a list of items
#' \itemize{
#'  \item `baseline_counts_by_location`: a frame of counts over the baseline
#'  interval by location
#'  \item `case_grid`: a frame of cases during the test dates, with reverse
#'  cumulative counts within location, by date
#'  \item `case_grid_totals_by_date`: reverse cumulative sum of counts over all
#'   locations, by date
#'  \item `test_cases`: case location counts only during the test dates
#'  \item `detect_date`: the detect date passed to this function
#'  \item `baseline_total`: an integer holding the total counts over all
#'  locations and dates
#' }
generate_case_grids <- function(
    cases,
    detect_date,
    baseline_length = 90,
    max_test_window_days = 7,
    guard_band = 0,
    baseline_adjustment = c("add_one", "add_one_global", "add_test", "none"),
    adj_constant = 1.0) {
  # make sure this is data.table
  if (!"data.table" %in% class(cases)) {
    cases <- data.table::copy(cases)
    data.table::setDT(cases)
  }

  # make sure that it has count, location, and date
  check_vars(cases, c("count", "location", "date"))

  # make sure that location is of type character
  if (typeof(cases[["location"]]) != "character") {
    cli::cli_abort("location column must of type character")
  }

  # make sure that date is of type date
  if (!"Date" %in% class(cases[["date"]])) {
    cli::cli_abort("date column must be of class Date or IDate")
  }

  # match the baseline adjustment argument
  baseline_adjustment <- match.arg(baseline_adjustment)

  # could set this to IDate for speed
  detect_date <- as.Date(detect_date)

  # global declarations to avoid check CMD errors
  count <- base_loc_sums <- location <- count_sum <- v1 <- NULL

  # get the dates of the test vector
  test_dates <- get_test_dates(detect_date, max_test_window_days)

  # get the unique locations
  test_locations <- cases[, unique(location)]

  # get the cases during the test dates
  test_cases <- cases[
    data.table::between(date, min(test_dates), max(test_dates))
  ]

  # make a data grid for the test dates and the test locations

  case_grid <- data.table::CJ(test_dates, test_locations)
  data.table::setnames(case_grid, c("date", "location"))

  # add the cases during the test dates into this grid.
  # will result in some dates with NA, because not all locations have a row
  # in the cases counts frame corresponding to every combination of location
  # and test_date
  case_grid <- test_cases[case_grid, on = c("date", "location")]

  # fill the missing with zeros
  case_grid[, count := data.table::nafill(count, fill = 0)]

  # Retains all the locations where there are one or more cases,
  # but truncates, them so they only have dates up to the last
  # date there was a case in the test interval

  # 1. First get the reverse cumulative sum vector for each location
  case_grid[order(-date), count_sum := cumsum(count), by = "location"]

  # 2. Second, retain only rows where that cum sum vector exceeds 0
  case_grid <- case_grid[count_sum > 0]

  # 3. Now, we get the totals by date over all the locations that have
  # one or more case..
  case_grid_totals_by_date <- case_grid[
    ,
    list(test_totals = sum(count_sum, na.rm = TRUE)),
    by = "date"
  ]

  # 4. For any locations where the test interval has data but the baseline
  # interval does not, add those locations to the baseline totals frame with
  # zeros or ones for sums:


  # First, get the dates of the baseline interval
  baseline_dates <- get_baseline_dates(
    detect_date,
    max_test_window_days,
    baseline_length,
    guard_band
  )

  # get the case totals by location, during the baseline period
  baseline_case_totals <- cases[
    data.table::between(date, min(baseline_dates), max(baseline_dates)),
    list(base_loc_sums = sum(count, na.rm = TRUE)),
    location
  ]

  # Now what about missing baseline locations. First, we find
  # all the missing locations (i.e, the locations that exist in the
  # test cases, but don't exist in the baseline totals; Note that
  # such locations can exist because when we sum baseline case totals
  # above, we only retain those locations that have any cases in the baseline
  # period; if there were no cases, these locations would not be retained)

  missing_baseline_locations <- setdiff(
    test_cases$location,
    baseline_case_totals$location
  )

  # Second, we add a row for each of these locations, and set the total
  # to zero.
  baseline_case_totals <- rbind(
    baseline_case_totals,
    data.table::CJ(missing_baseline_locations, 0),
    use.names = FALSE
  )

  # Now, we need to adjust the baseline totals. We either add the adj_constant
  # to ALL the locations, or we add the test interval totals to the locations

  # if add_one, then add one to all locations
  if (baseline_adjustment == "add_one_global") {
    baseline_case_totals[, base_loc_sums := base_loc_sums + adj_constant]
  }
  # if add_test, then add the test interval data
  if (baseline_adjustment == "add_test") {
    tc <- test_cases[, sum(count, na.rm = TRUE), location]
    data.table::setnames(tc, c("location", "v1"))

    baseline_case_totals <- tc[
      baseline_case_totals,
      on = "location",
      list(base_loc_sums = sum(base_loc_sums, v1, na.rm = TRUE)),
      .EACHI
    ]
  }

  # Note that we don't do anything for the default "add_one" or the "none"
  # methods. For the former "add_one", we do this on an as needed basis
  # only when calculating the expected

  # sum of counts of all baseline data
  baseline_total <- baseline_case_totals[, sum(base_loc_sums, na.rm = TRUE)]

  cg <- list(
    "baseline_counts_by_location" = baseline_case_totals,
    "case_grid" = case_grid,
    "case_grid_totals_by_date" = case_grid_totals_by_date,
    "test_cases" = test_cases,
    "detect_date" = detect_date,
    "baseline_total" = baseline_total
  )

  class(cg) <- append(class(cg), "CaseGrids")

  return(cg)
}


#' Return baseline and test period case grids restricting by distance
#'
#' Function takes a distance matrix between locations, a set of baseline period
#' case sums by location, and grid of test period cases by date and location,
#' and given a distance limit, returns two frames: 1. A frame that has for each
#' location, a list of nearby locations and the cumulative sum of cases from
#' those locations (over increasing distance) 2. A frame that has for each
#' location, a list of nearby locations and the observed cumulative sum of cases
#' by date (over increasing distance)
#' @param cg object of class `CaseGrids`, such as returned from the
#'   `generate_case_grids()`
#' @param distance_matrix a square distance matrix, named on both dimensions or
#'   a list of distance vectors, one for each location
#' @param distance_limit numeric value indicating the distance threshold to
#'   define "near" locations; must be input in the same units as the distances
#'   in the `distance_matrix`. Note that if passing the list version of
#'   distance_matrix, this limit has already been used in that construction and
#'   thus is ignored
#' @export
#' @returns an object of class `NearbyClusterGrids` which is list of two
#'   dataframes, including "baseline" (has the nearby information for baseline
#'   counts) and "test" (which holds the nearby information for test interval
#'   counts)
gen_nearby_case_info <- function(
    cg,
    distance_matrix,
    distance_limit) {
  if (!"CaseGrids" %in% class(cg)) {
    cli::cli_abort("cg must be an object of class 'CaseGrids'")
  }

  # global declarations to avoid check CMD errors
  distance_value <- base_clust_sums <- base_loc_sums <- target <- NULL

  baseline_loc_totals <- cg[["baseline_counts_by_location"]]
  cases_grid <- cg[["case_grid"]]

  # create the looks up distances table less than distance_limit to each target
  # this is dataframe that has, for each target location, a list of all other
  # locations within distance_limit, and the distance to that location

  if("matrix" %in% class(distance_matrix)) {
    wd <- data.table::rbindlist(
      apply(distance_matrix, 1, \(x) {
        utils::stack(x[x <= distance_limit])
      }),
      idcol = "target"
    )
  }

  if("list" %in% class(distance_matrix)) {
    wd <- data.table::rbindlist(
      lapply(distance_matrix, \(d) {
        data.table::data.table(
          values=d,
          ind=names(d)
        )
      }),
      idcol = "target"
    )
  }

  data.table::setnames(wd, c("target", "distance_value", "location"))

  # restrict to only those places that are the baseline totals by location frame
  bl <- wd[baseline_loc_totals, on = c("location")]

  # sort by distance, and get the cumulative sum of location sums, by target
  bl <- bl[
    order(distance_value),
    base_clust_sums := cumsum(base_loc_sums),
    target
  ]

  # now, similarly for the case_grid over the test days
  dt_cases_grid_clust <- wd[
    cases_grid,
    on = c("location"),
    allow.cartesian = TRUE
  ] |>
    _[order(distance_value)]

  nci <- list("baseline" = bl, "test" = dt_cases_grid_clust)
  class(nci) <- c(class(nci), "NearbyClusterGrids")

  nci
}

#' Generate the observed and expected information
#'
#' Function takes an object of class `NearbyClusterGrids`, as returned from the
#' `generate_nearby_case_information()`, and adds the observed and expected
#' information
#' @param nearby_counts an object of class `NearbyClusterGrids`
#' @param cases_grids an object of class `CaseGrids`
#' @param adjust boolean default TRUE, set to \code{FALSE} to avoid adding
#' one to the expected when it is zero. Could result in errors.
#' @param adj_constant numeric (default=1.0); this is the constant to be added
#'   if \code{baseline_adjustment == 'add_one'} or \code{baseline_adjustment ==
#'   'add_one'}
#' @export
#' @returns a dataframe of class `ObservedExpectedGrid`
generate_observed_expected <- function(
    nearby_counts,
    cases_grids,
    adjust = FALSE,
    adj_constant = 1.0) {
  if (!"CaseGrids" %in% class(cases_grids)) {
    cli::cli_abort("cases_grids must be an object of class CaseGrids")
  }

  if (!"NearbyClusterGrids" %in% class(nearby_counts)) {
    cli::cli_abort(
      "nearby_counts must be an object of class NearbyClusterGrids"
    )
  }

  # global declarations to avoid check CMD errors
  observed <- count_sum <- base_clust_sums <- test_totals <- log_obs_exp <- NULL
  expected <- target <- distance_value <- location <- NULL


  dt_cases_grid_clust <- data.table::copy(nearby_counts[["test"]])
  dt_cases_grid_clust[
    order(distance_value),
    observed := cumsum(count_sum),
    by = c("date", "target")
  ]

  dt_cases_grid_clust <- nearby_counts[["baseline"]][
    ,
    list(target, location, base_clust_sums)
  ] |>
    _[dt_cases_grid_clust, on = c("target", "location")]

  # Now, bring in the test totals on each of the dates (note that this is a
  # constant, within date, across all locations)
  dt_cases_grid_clust <- cases_grids[["case_grid_totals_by_date"]][
    dt_cases_grid_clust,
    on = c("date")
  ]

  dt_cases_grid_clust[, `:=`(
    detect_date = cases_grids[["detect_date"]],
    baseline_total = cases_grids[["baseline_total"]],
    expected = test_totals * base_clust_sums / cases_grids[["baseline_total"]]
  )]

  # add the log of observed to expected
  if (adjust == TRUE) {
    dt_cases_grid_clust[, log_obs_exp := data.table::fifelse(
      expected == 0,
      log(observed / (expected + adj_constant)),
      log(observed / expected)
    )]
  } else {
    dt_cases_grid_clust[, log_obs_exp := log(observed / expected)]
  }

  dt_cases_grid_clust[, log_obs_exp := data.table::fifelse(
    expected == 0 & adjust == TRUE,
    log(observed / (expected + adj_constant)),
    log(observed / expected)
  )]

  class(dt_cases_grid_clust) <- c(
    class(dt_cases_grid_clust),
    "ObservedExpectedGrid"
  )

  return(dt_cases_grid_clust)
}


#' Use spline lookup to restrict `ObserveExpectedGrid` to potential clusters
#'
#' Function takes a spline lookup table (or uses package default), and an object
#' of class ObservedExpectedGrid and identfies which rows in each potential
#' centroid have observed over expected values that exceed a threshold for that
#' observed value
#' @param oe_grid An object of class `ObservedExpectedGrid` generated by
#'   `generate_observed_expected()`
#' @param spline_lookup default NULL; either a spline lookup table, which is a
#'   data frame that has at least two columns: including "observed" and
#'   "spl_thresh", OR a string indicating to use one of the built in lookup
#'   tables: i.e. one of \code{"001", "005", "01", "05"}. If NULL, the default
#'   table will be 01 (i.e. \code{spline_01} dataset)
#' @export
#' @returns a frame containing rows of oe_grid that are candidate alert clusters
add_spline_threshold <- function(oe_grid, spline_lookup = NULL) {
  if (is.null(spline_lookup)) {
    # use the default
    spline_lookup <- spline_01
  } else if (inherits(spline_lookup, "character")) {
    if (!spline_lookup %in% c("001", "005", "01", "05")) {
      cli::cli_abort(
        "If calling a built-in spline lookup, use one of '001','005','01','05'"
      )
    } else {
      spline_lookup <- get(paste0("spline_", spline_lookup))
    }
  } else if (!inherits(spline_lookup, "data.frame")) {
    cli::cli_abort(
      "spline lookup must be NULL, an allowed string, or of class data.frame"
    )
  }

  # Check: is spline_lookup a frame with required columns:
  if (!"data.frame" %in% class(spline_lookup)) {
    cli::cli_abort("spline_lookup must be a data.frame")
  }

  if (!all(c("spl_thresh", "observed") %in% names(spline_lookup))) {
    cli::cli_abort(
      "spline_lookup does not have columns 'observed' and/or 'spl_thresh'"
    )
  }

  # Check: is oe_grid an object of class ObservedExpectedGrid?
  if (!"ObservedExpectedGrid" %in% class(oe_grid)) {
    cli::cli_abort("oe_grid must be an object of ObservedExpectedGrid;
         see ?generated_observed_expected")
  }

  # global declarations to avoid check CMD errors
  log_obs_exp <- spl_thresh <- alert_ratio <- alert_gap <- NULL
  min_dist <- distance_value <- NULL

  # Join the observed expected grid to the spline lookup so
  # that the spline threshold for each observed value can be obtained
  dt_cases_grid_clust <- spline_lookup[oe_grid, on = c("observed")]

  # For each candidate centroid, and associated nearby location, and countSum,
  # get the minimum distance by count_sum. Check is this doing what we actually
  # need?
  dt_cases_grid_clust[
    , min_dist := min(distance_value),
    by = c("target", "location", "count_sum")
  ]

  # For each candidate centroid, reduce to only the rows where distance is
  # minimum Note that this is not necessarily the way to do this.. Because of
  # precision issues, we might not capture the minimum. Furthermore, it can
  # return multiple rows.  Is that what we really want? If we only want one row,
  # why not order by distance_value, and take the first row in each group?

  tol <- .Machine$double.eps^0.5
  dt_cases_grid_clust <- dt_cases_grid_clust[
    ,
    .SD[data.table::between(distance_value, min_dist - tol, min_dist + tol)],
    by = c("target")
  ]

  # Add the alert ration, which is the ration of the log of observed over
  # expected divided by the spline thresholds.  That is, if this ratio is over
  # 1, then the observed/expected is higher than the spline line
  dt_cases_grid_clust[, alert_ratio := log_obs_exp / spl_thresh]

  # Add the alert gap, which is the distance between log of observed over
  # expected and the spline threshold.
  dt_cases_grid_clust[, alert_gap := log_obs_exp - spl_thresh]

  # In this case, we are using the latter of these two signals (ie. the alert
  # gap) to restrict.  That is the alert gap must be positive for us to retain
  cluster_alert_table <- dt_cases_grid_clust[alert_gap > 0]

  # add class to return objec
  class(cluster_alert_table) <- c(
    class(cluster_alert_table),
    "ClusterAlertTable"
  )

  cluster_alert_table
}

#' Compress a cluster_alert_table
#'
#' Function reduces an object of class ClusterAlertTable to the final set of
#' clusters and locations. The idea of this function is to retain only the most
#' significant, non-overlapping clusters from the cluster alert table. The
#' surrogate for significance is 'alertGap', or log(observed/expected) minus the
#' threshold that the spline assigns to the observed value`. The logic below
#' keeps two running tables, the table 'dt_keep' of clusters to be kept, in
#' descending order of 'alertGap', and 'dt_clust', the remaining rows of the
#' cluster alert table, which are reduced each time a cluster is accepted into
#' dt_keep Each row of the cluster alert table represents a candidate cluster,
#' with a column 'target', the cluster center, and a column 'location', the most
#' distant location from the center. Each time a cluster is accepted into
#' dt_keep, jClust is incremented, and remaining rows of 'dt_clust' are dropped
#' if either 'target' or 'location' is the center of the newly accepted cluster
#' in 'dt_keep'

#' @param cluster_alert_table an object of class `ClusterAlertTable`
#' @param distance_matrix a square distance matrix, named on both dimensions or
#'   a list of distance vectors, one for each location
#' @export
#' @returns a list including a data.frame of clusters and another frame of
#'   individual location counts
compress_clusters <- function(
    cluster_alert_table,
    distance_matrix) {
  if (!"ClusterAlertTable" %in% class(cluster_alert_table)) {
    cli::cli_abort(
      "cluster_alert_table must be an object of type ClusterAlertTable"
    )
  }

  if (nrow(cluster_alert_table) == 0) {
    cli::cli_abort("No clusters; cluster_alert_table is empty", call. = FALSE)
  }

  # global declarations to avoid check CMD errors
  target <- alert_gap <- distance_value <- location <- nr_locs <- NULL


  # Define dt_clust as the cluster_alert_table sorted by descending alertGap as
  # a surrogate for cluster significance

  dt_clust <- data.table::copy(cluster_alert_table) |>
    _[order(-alert_gap)]

  # nrLocs is meant to store the number of locations in each cluster, will later
  # be used as a column in dt_keep:
  nr_locs <- rep(NA, nrow(dt_clust))

  matrix_index_func <- function(dm, dt_clust, ndx) {
    dm[ndx, ] <= dt_clust[1, distance_value]
  }

  list_index_func <- function(dm, dt_clust, ndx) {
    dm[[ndx]] <= dt_clust[1, distance_value]
  }


  if("matrix" %in% class(distance_matrix)) {
    location_set <- colnames(distance_matrix)
    dm_index_f <- matrix_index_func
  } else {
    location_set <- names(distance_matrix)
    dm_index_f <- list_index_func
  }



  # Now take the first row of dt_clust, find the index of its center location
  # (target) in location_set
  ndx <- which(location_set == dt_clust[1, target])

  # The maximum distance (miles) from the center location to another location in
  # this cluster is in the column distance_value Now collect all of the
  # locations in this cluster within the maximum distance
  loc_set <- location_set[dm_index_f(distance_matrix, ndx, dt_clust)]

  # Initialize dt_keep to have the number of rows equal to the number of cluster
  # centers with at least one positive alertGap Some of these rows will probably
  # be dropped because of overlap with previous kept clusters. An alternative to
  # this awkward construction is to just initialize dt_keep as a single row and
  # just keep using rbind() to grow dt_keep if/when more nonoverlapping clusters
  # are found
  dt_keep <- dt_clust[1:data.table::uniqueN(dt_clust[, target])]

  # Now save the cluster with the greatest alertGap in the first row of dt_keep,
  # set remaining rows to NA
  dt_keep[1, ] <- dt_clust[1, ]
  dt_keep[-1, ] <- NA

  # Now reduce dt_clust by dropping all rows where 'target' or 'location' is the
  # center (current 'target') of the retained cluster
  dt_clust <- dt_clust[
    !target == dt_clust[1, target] &
      !location == dt_clust[1, target]
  ]

  # Store the number of locations in the cluster in row 1
  nr_locs[1] <- length(loc_set)
  j_clust <- 1 # jClust is the number of kept clusters
  clust_loc_list <- list()
  clust_loc_list[[1]] <- loc_set

  # Now carefully repeat the process for subsequent rows of 'dt_clust'
  while (nrow(dt_clust) > 0) {
    # Find the index of the next most significant cluster in the reduced
    # dt_clust
    ndx <- which(location_set == dt_clust[1, target])
    # Now find the set of all locations in this cluster
    loc_set_curr <- location_set[
      dm_index_f(distance_matrix, ndx, dt_clust)
      #distance_matrix[ndx, ] <= dt_clust[1, distance_value]
    ]
    # Keep the new cluster only if none of its locations is contained in a
    # previous cluster in dt_keep
    if (length(intersect(loc_set, loc_set_curr)) == 0) {
      j_clust <- j_clust + 1
      dt_keep[j_clust, ] <- dt_clust[1, ]
      nr_locs[j_clust] <- length(loc_set_curr)
      clust_loc_list[[j_clust]] <- loc_set_curr
      # update the set of 'used' locations, part of any cluster in 'dt_keep'
      # reduce dt_clust again, dropping any cluster where 'target' or 'location'
      # is the new cluster center
      loc_set <- union(loc_set, loc_set_curr)
      dt_clust <- dt_clust[
        !target == dt_clust[1, target] &
          !location == dt_clust[1, target]
      ]
    } else {
      # the cluster in this row intersected a previous cluster, so drop it
      # however, there may be a smaller cluster in 'dt_clust' with the same
      # center that does not intersect any previous cluster
      dt_clust <- dt_clust[-1, ]
    }
  }

  dt_keep <- dt_keep[1:j_clust, ] # drop unused rows
  dt_keep[, nr_locs := nr_locs[1:j_clust]]

  clusters <- list(
    cluster_alert_table = dt_keep[],
    clust_loc_list = clust_loc_list
  )
  class(clusters) <- c(class(clusters), "clusters")

  # return this list
  clusters
}

#' Fast version of compress clusters
#'
#' Function reduces an object of class ClusterAlertTable to the final set of
#' clusters and locations. The idea of this function is to retain only the most
#' significant, non-overlapping clusters from the cluster alert table. The
#' surrogate for significance is 'alertGap', or log(observed/expected) minus the
#' threshold that the spline assigns to the observed value`.
#' @param cluster_alert_table an object of class `ClusterAlertTable`
#' @param distance_matrix a square distance matrix, named on both dimensions or
#'   a list of distance vectors, one for each location
#' @export
#' @returns a list including a data.frame of clusters and another frame of
#'   individual location counts
compress_clusters_fast <- function(
    cluster_alert_table,
    distance_matrix) {
  if (!"ClusterAlertTable" %in% class(cluster_alert_table)) {
    cli::cli_abort(
      "cluster_alert_table must be an object of type ClusterAlertTable"
    )
  }
  if (nrow(cluster_alert_table) == 0) {
    cli::cli_abort("No clusters; cluster_alert_table is empty", call. = FALSE)
  }

  id <- distance_value <- target <- v1 <- nr_locs <- NULL

  # generate helper function, used internally.
  get_locs_matrix <- function(d, m, t) which(m[t, ] <= d) |> names()
  get_locs_list <- function(d, m, t) which(m[[t]] <= d) |> names()
  if("matrix" %in% class(distance_matrix)) {
    get_locs_f <- get_locs_matrix
  } else {
    get_locs_f <- get_locs_list
  }

  # add an id to the list of rows
  cluster_alert_table[, id := .I]

  # make sure it is sorted by alert_gap
  data.table::setorderv(cluster_alert_table, "alert_gap", order = -1)

  # create a long table that has the id, the target, the location, and all the
  # locations that are within distance_value from this target
  y <- cluster_alert_table[
    ,
    list(v1 = get_locs_f(distance_value, distance_matrix, target)),
    by = c("id", "target", "location")
  ]

  # make a list to hold all the clusters
  clusters <- list()

  # Now, we can reduce y to zero row using a while condition
  while (dim(y)[1] > 0) {
    # get the current cluster
    cl <- y[1, id]
    # get the locations for this cluster
    locs <- y[id == cl, v1]
    # add this cluster to the list
    clusters[[as.character(cl)]] <- list(
      row = cluster_alert_table[id == cl],
      locs = locs
    )

    # reduce y to only those rows that don't have this cluster in V1
    y <- y[!data.table::CJ(id = y[v1 %in% locs, unique(id)]), on = "id"]
  }

  # get all the rows by rowbinding the first element of this list
  cluster_alert_table <- data.table::rbindlist(lapply(clusters, `[[`, 1))
  # get all the locations for each cluster
  clust_loc_list <- lapply(clusters, `[[`, 2)

  # add the number of locations to each cluster
  cluster_alert_table[, nr_locs := sapply(clust_loc_list, length)]

  clusters <- list(
    cluster_alert_table = cluster_alert_table,
    clust_loc_list = clust_loc_list
  )
  class(clusters) <- c(class(clusters), "clusters")

  # return this list
  clusters
}


#' Add location counts to cluster location list
#'
#' Add counts of individual cluster locations Operates on the output list of the
#' 'compress_clusters()' component Calculates indiv. location counts for each
#' cluster, appends to the cluster location list
#' @param cluster_list output list from 'compress_clusters', cluster summary
#'   rows and locations in each cluster
#' @param cases original data in 3-column format of location, count, date
#' @export
#' @returns the cluster list from compress_clusters with individual location
#'   counts appended
add_location_counts <- function(cluster_list, cases) {
  # check this is an object of class clusters
  if (!"clusters" %in% class(cluster_list)) {
    cli::cli_abort("Must pass an object of class 'clusters'")
  }

  location <- count <- target <- NULL

  # extract the 2 items in cluster_list
  clust_loc_list <- cluster_list$clust_loc_list
  cluster_alert_table <- cluster_list$cluster_alert_table

  # make sure cases is a data.table
  if (!"data.table" %in% class(cases)) {
    cases <- data.table::copy(cases)
    data.table::setDT(cases)
  }


  # initialize the table of individual location counts
  cluster_location_counts <- data.table::data.table()

  for (j in seq_len(nrow(cluster_alert_table))) {
    # get all locations with counts in current cluster
    clust_locs <- clust_loc_list[[j]]
    # get input case table filtered to cluster dates and locations
    location_counts <- cases[
      date >= cluster_alert_table$date[j] &
        date <= cluster_alert_table$detect_date[j] &
        location %in% clust_locs &
        count>0
    ] |>
      _[, list(count = sum(count),max_date = max(date)), location][order(location), max_date:=max(max_date)]

    zero_cl <- setdiff(clust_locs, unique(location_counts$location))
    if (length(zero_cl) > 0) {
      location_counts <- rbind(
        location_counts,
        data.table::data.table(
          location = zero_cl,
          count = 0,
          max_date = location_counts[["max_date"]][[1]]
        )
      )
    }

    # add the current cluster center location to this filtered table
    location_counts[, target := cluster_alert_table$target[j]]
    data.table::setnames(location_counts, c("location", "count", "max_date", "target"))

    # concatenate these rows to table for all cluster
    cluster_location_counts <- rbind(cluster_location_counts, location_counts)
  }

  # add the max date of each cluster to the main table
  cluster_alert_table <- merge(
    cluster_alert_table,
    unique(cluster_location_counts[, .(target, max_date)]),
    by = c("target"),
  )
  cluster_location_counts[, max_date:=NULL]

  nr_locs <- NULL

  # check that nr_locations match
  s <- cluster_alert_table[, list(target, nr_locs)][order(target)]
  t <- cluster_location_counts[, list(nr_locs = .N), target][order(target)]
  setkey(s, NULL) # this drops any key from s to make sure it is identical to t
  if (!identical(s, t)) {
    cli::cli_abort(
      paste0(
        "When adding locations, the number of locations in cluster ",
        "differs from number of locations in location counts frame"
      )
    )
  }

  clusters <- list(
    cluster_alert_table = cluster_alert_table,
    cluster_location_counts = cluster_location_counts
  )



  class(clusters) <- c(class(clusters), "clusters")

  clusters
}
#' Find clusters
#'
#' Function will return clusters, given a frame of case counts by location and
#' date, a distance matrix, a spline lookup table, and other parameters
#' @param cases a frame of case counts by location and date
#' @param distance_matrix a square distance matrix, named on both dimensions or
#'   a list of distance vectors, one for each location
#' @param detect_date a date that indicates the end of the test window in which
#'   we are looking for clusters
#' @param spline_lookup default NULL; either a spline lookup table, which is a
#'   data frame that has at least two columns: including "observed" and
#'   "spl_thresh", OR a string indicating to use one of the built in lookup
#'   tables: i.e. one of \code{"001", "005", "01", "05"}. If NULL, the default
#'   table will be 01 (i.e. \code{spline_01} dataset)
#' @param baseline_length integer (default = 90) number of days in the baseline
#'   interval
#' @param max_test_window_days integer (default = 7) number of days for the test
#'   window
#' @param guard_band integer (default = 0) buffer days between baseline and test
#'   interval
#' @param distance_limit numeric (default=15) maximum distance to consider
#'   cluster size. Note that the units of the value default (miles) should be
#'   the same unit as the values in the distance matrix
#' @param baseline_adjustment one of four string options: "add_one" (default),
#'   "add_one_global", "add_test", or "none".  All methods except for "none"
#'   will ensure that the log(obs/expected) is always defined (i.e. avoids
#'   expected =0). For the default, this will add 1 to the expected for any
#'   individual calculation if expected would otherwise be zero.
#'   "add_one_global", will add one to all baseline location case counts. For
#'   "add_test_interval", each location in the baseline is increased by the
#'   number of cases in that location during the test interval. If "none", no
#'   adjustment is made.
#' @param adj_constant numeric (default=1.0); this is the constant to be added
#'   if \code{baseline_adjustment == 'add_one'} or \code{baseline_adjustment ==
#'   'add_one'}
#' @param min_clust_cases (default = 0); minimum number of cluster cases to
#'   retain before compression
#' @param max_clust_cases (default = Inf); maximum number of cluster cases to
#'   retain before compression
#' @param post_cluster_min_count (default=0); a second (or alternative) way to
#'   limit cluster. This parameter can be set to a non-negative integer to
#'   require that any final clusters (post compression from candidate rows) have
#'   at least \code{post_cluster_min_count} cases, when aggregated over all
#'   locations within the identified cluster
#' @param use_fast boolean (default = TRUE) - set to TRUE to use the fast verson
#'   of the compress clusters function
#' @param return_interim boolean (default = FALSE) - set to TRUE to return all
#'   interim objects of the find_clusters() function
#' @export
#' @returns returns a list of two of two dataframes.
find_clusters <- function(
    cases,
    distance_matrix,
    detect_date,
    spline_lookup = NULL,
    baseline_length = 90,
    max_test_window_days = 7,
    guard_band = 0,
    distance_limit = 15,
    baseline_adjustment = c("add_one", "add_one_global", "add_test", "none"),
    adj_constant = 1.0,
    min_clust_cases = 0,
    max_clust_cases = Inf,
    post_cluster_min_count = 0,
    use_fast = TRUE,
    return_interim = FALSE) {
  baseline_adjustment <- match.arg(baseline_adjustment)

  if (max_test_window_days <= 0) {
    cli::cli_abort("Test interval/length must be positive")
  }

  interim_results <- list()

  # Internal function to try expression, and add an error attribute
  # to the interim results list; note that error returns NULL, but
  # interim_results object is updated
  handle_try <- function(expr, name) {
    tryCatch(
      {
        result <- expr
        interim_results[[name]] <<- result
        result
      },
      error = function(e) {
        attr(interim_results, "error") <<- list(
          step = name,
          message = e$message
        )
        NULL
      }
    )
  }

  report_error <- function() {
    m <- paste0(
      "Error at step ", attr(interim_results, "error")$step, ": ",
      attr(interim_results, "error")$message
    )
    if (return_interim) {
      cli::cli_alert_danger(text = m)
      interim_results
    } else {
      cli::cli_abort(message = m)
    }
  }

  # 1. Get the baseline counts, the test interval counts, totals etc
  case_grid_info <- handle_try(
    generate_case_grids(
      cases = cases,
      detect_date = detect_date,
      baseline_length = baseline_length,
      max_test_window_days = max_test_window_days,
      guard_band = guard_band,
      baseline_adjustment = baseline_adjustment,
      adj_constant = adj_constant
    ),
    "case_grid_info"
  )

  if (is.null(case_grid_info)) {
    return(report_error())
  }

  # 2. Get the nearby care information
  nearby_case_info <- handle_try(
    gen_nearby_case_info(
      cg = case_grid_info,
      distance_matrix = distance_matrix,
      distance_limit = distance_limit
    ),
    "nearby_case_info"
  )

  if (is.null(nearby_case_info)) {
    return(report_error())
  }


  # 3. Get the observe/expected values
  obs_expected_frame <- handle_try(
    generate_observed_expected(
      nearby_counts = nearby_case_info,
      cases_grids = case_grid_info,
      adjust = baseline_adjustment != "none",
      adj_constant = adj_constant
    ),
    "observed_expected"
  )

  if (is.null(obs_expected_frame)) {
    return(report_error())
  }

  # 4. Get the spline information
  obs_expected_frame_with_spline <- handle_try(
    add_spline_threshold(
      spline_lookup = spline_lookup,
      oe_grid = obs_expected_frame
    ),
    "obs_expected_filtered_via_spline"
  )
  if (is.null(obs_expected_frame_with_spline)) {
    return(report_error())
  }

  observed <- NULL
  # limit the candidates to the min/max params
  obs_expected_frame_with_spline <-
    obs_expected_frame_with_spline[
      data.table::between(observed, min_clust_cases, max_clust_cases)
    ]

  # If nrow is zero, this is not an error, just no clusters
  if(nrow(obs_expected_frame_with_spline) == 0) {
    cli::cli_alert_info("No clusters found")
    if(return_interim == TRUE) return(interim_results)
    else return(invisible())
  }

  # 5. Compress Clusters
  if (use_fast) {
    compressed_clusters <- handle_try(
      compress_clusters_fast(
        cluster_alert_table = obs_expected_frame_with_spline,
        distance_matrix = distance_matrix
      ),
      "compress_clusters_fast"
    )
  } else {
    compressed_clusters <- handle_try(
      compress_clusters(
        cluster_alert_table = obs_expected_frame_with_spline,
        distance_matrix = distance_matrix
      ),
      "compress_clusters"
    )
  }

  if (is.null(compressed_clusters)) {
    return(report_error())
  }

  # 6. Add counts of individual locations, each cluster.
  result <- handle_try(
    add_location_counts(compressed_clusters, cases),
    "adding_location_counts"
  )

  if (is.null(result)) {
    return(report_error())
  }

  if (return_interim == TRUE) {
    return(list(
      case_grid_info = case_grid_info,
      nearby_case_info = nearby_case_info,
      obs_expected_frame = obs_expected_frame,
      obs_expected_frame_with_spline = obs_expected_frame_with_spline,
      compressed_clusters = compressed_clusters,
      result = result
    ))
  } else {
    return(result)
  }
}


#' Generate baseline dates vector
#'
#' Function to generate baseline dates given an end date and test length, plus
#' optional guard, and length of baseline
#' @param end_date End date of the test interval
#' @param test_length (integer) length of the test interval in days
#' @param baseline_length (integer) length of baseline period in days
#' @param guard (integer) default = 0; buffer between end of baseline and start
#'   of test interval
#' @returns vector of dates
#' @export
get_baseline_dates <- function(
    end_date,
    test_length,
    baseline_length,
    guard = 0) {
  end_date <- as.Date(end_date)
  dates <- as.Date((end_date - test_length + 1 - guard) - (baseline_length:1))
  return(dates)
}

#' Generate test dates vector
#'
#' Function to generate test dates given an end date and test length
#' @param end_date End date of the test interval
#' @param test_length (integer) length of the test interval in days
#' @export
#' @returns vector of dates
get_test_dates <- function(end_date, test_length) {
  end_date <- as.Date(end_date)
  dates <- seq.Date((end_date - test_length + 1), end_date, 1)
  dates
}

#' check for variables in frame
#'
#' Function checks for variables in frame
#' @param d input data frame to check for variables
#' @param required vector of column names that must be in `d`
check_vars <- function(d, required) {
  if (!all(required %in% names(d))) {
    cli::cli_abort(
      paste0(
        "One or more of the required columns (",
        paste0(required, collapse = ","),
        ") not in found in input frame"
      )
    )
  }
}

#' Filter clusters on minimum overall count
#'
#' Function takes a set of clusters identified via \code{compress_clusters()}
#' and a minimum threshold for counts, and reduces the identified clusters to
#' only those clusters where the total number of observed across the cluster
#' meets that minimum threshold.
#' @param cl a object of class \code{clusters}, as returned from
#'   \code{compress_clusters}
#' @param minimum numeric (default = 0); minimum number across all locations in
#'   a cluster in order to retain
#' @export
#' @returns an object of class \code{clusters}
reduce_clusters_to_min <- function(cl, minimum = 0) {
  # check this is an object of class clusters
  if (!"clusters" %in% class(cl)) {
    cli::cli_abort("Must pass an object of class 'clusters'")
  }

  # return if no reduction necessary
  if (minimum <= 1) {
    return(cl)
  }

  # get the class
  clcl <- class(cl)

  count <- v1 <- target <- NULL
  targets <- cl[[2]][, list(v1 = sum(count, na.rm = TRUE)), target] |>
    _[v1 >= minimum, target]

  cl[[1]] <- cl[[1]][target %in% targets]
  cl[[2]] <- cl[[2]][target %in% targets]

  class(cl) <- clcl

  return(cl)
}
