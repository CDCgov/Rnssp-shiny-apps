# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


#' Get distance matrix for zip codes within a state
#'
#' Function returns a list of zipcodes and a matrix with the distance between
#' those zip codes. leverages a built in dataset (`zipcodes`) that maps
#' zipcodes to counties.
#' @param st two-character string denoting a state
#' @param unit string, one of "miles" (default), "kilometers", or "meters".
#'   Indicating the desired unit for the distances
#' @export
#' @examples
#' # example code
#' zip_distance_matrix("MD")
zip_distance_matrix <- function(
    st,
    unit = c("miles", "kilometers", "meters")) {
  # match unit, and create a lookup
  unit <- match.arg(unit)
  unit_factors <- c(
    "miles" = 0.00062137,
    "kilometers" = 1 / 1000.0,
    "meters" = 1
  )


  # global declarations to avoid check CMD errors
  state <- zip_code <- longitude <- latitude <- NULL

  # get the subset of the built in dataset for this state, limit rows, and only
  # where lat/long available

  mapping <- zipcodes[
    state %chin% st & !is.na(latitude) & !latitude == "NULL",
    list("location" = zip_code, latitude, longitude)
  ]

  # make numeric
  for (c in c("latitude", "longitude")) {
    data.table::set(mapping, j = c, value = as.numeric(mapping[[c]]))
  }

  # get distance matrix
  distance_matrix <- geosphere::distm(x = mapping[, list(longitude, latitude)])

  # apply factor
  distance_matrix <- distance_matrix * unit_factors[unit]

  # set dimension names
  dimnames(distance_matrix) <- list(
    mapping[["location"]],
    mapping[["location"]]
  )

  # return two-element list
  return(list(
    loc_vec = mapping[["location"]],
    distance_matrix = distance_matrix
  ))
}


#' Get distance matrix for counties within a state
#'
#' Function returns a list of counties and a matrix with the distance between
#' those counties. leverages a built in dataset (`counties`).
#' @param st two-character string denoting a state, or "US". If "US", then this
#'   is equivalent to calling the us_distance_matrix() function
#' @param unit string, one of "miles" (default), "kilometers", or "meters".
#'   Indicating the desired unit for the distances
#' @param source string indicating either "rnssp" (default) or "tigris". The
#'   former will use `Rnssp::county_sf` file and leverage sf_centroid() and
#'   sf_distance() functions to produce the distance matrix. The latter will use
#'   this package's default `counties` dataset and `geosphere` package to get
#'   distance. The results are slightly different.
#' @export
#' @examples
#' # example code
#' county_distance_matrix("MD", source = "tigris")
#' county_distance_matrix("CT", "kilometers", source = "rnssp")
county_distance_matrix <- function(
    st,
    unit = c("miles", "kilometers", "meters"),
    source = c("rnssp", "tigris")) {
  # if State = "US" pass this request on to us_distance_matrix()
  if (st == "US") {
    return(us_distance_matrix(unit = unit))
  }

  # match source
  source <- match.arg(source)

  # match unit, and create a lookup
  unit <- match.arg(unit)
  unit_factors <- c(
    "miles" = 0.00062137,
    "kilometers" = 1 / 1000.0,
    "meters" = 1
  )

  # global declarations to avoid check CMD errors
  state <- fips <- longitude <- latitude <- NULL

  # get the subset of the built in dataset for this state, limit rows, and only
  # where lat/long available

  if (source == "rnssp") {
    requireNamespace("Rnssp", quietly = TRUE)
    requireNamespace("sf", quietly = TRUE)

    # look up the state fips code for this two letter code
    st <- Rnssp::state_sf[Rnssp::state_sf$STUSPS == toupper(st), ]$STATEFP |>
      as.character()

    distance_matrix <- Rnssp::county_sf[Rnssp::county_sf$STATEFP == st, ]
    loc_vec <- distance_matrix$GEOID
    distance_matrix <- suppressWarnings(
      distance_matrix |> sf::st_centroid() |> sf::st_distance()
    )
    dimnames(distance_matrix) <- list(loc_vec, loc_vec)
    units(distance_matrix) <- NULL
  } else {
    mapping <- counties[
      state %chin% toupper(st) & !is.na(latitude) & !latitude == "NULL",
      list("location" = fips, latitude, longitude)
    ]

    # make numeric
    for (c in c("latitude", "longitude")) {
      data.table::set(mapping, j = c, value = as.numeric(mapping[[c]]))
    }

    # get distance matrix
    distance_matrix <- geosphere::distm(
      x = mapping[, list(longitude, latitude)]
    )

    # add dimension names
    dimnames(distance_matrix) <- list(
      mapping[["location"]],
      mapping[["location"]]
    )

    loc_vec <- mapping[["location"]]
  }

  # apply factor
  distance_matrix <- distance_matrix * unit_factors[unit]

  return(list(
    loc_vec = loc_vec |> as.character(),
    distance_matrix = distance_matrix
  ))
}


#' Get distance matrix for all counties in the US
#'
#' Function returns a list of counties and a matrix with the distance between
#' those counties. leverages a built in dataset (`counties`). Note that the
#' generation of this matrix can take a few seconds. Note: it is better and
#' faster to use create_dist_matrix()
#' @param unit string, one of "miles" (default), "kilometers", or "meters".
#'   Indicating the desired unit for the distances
#' @export
us_distance_matrix <- function(
    unit = c("miles", "kilometers", "meters")) {
  # provide warning re the time it takes to construct this matrix
  cli::cli_alert_danger("Warning... this will take a few seconds...")

  # match unit, and create a lookup
  unit <- match.arg(unit)
  unit_factors <- c(
    "miles" = 0.00062137,
    "kilometers" = 1 / 1000.0,
    "meters" = 1
  )

  # global declarations to avoid check CMD errors
  fips <- longitude <- latitude <- NULL

  mapping <- counties[
    !is.na(latitude) & !latitude == "NULL",
    list("location" = fips, latitude, longitude)
  ]

  # make numeric
  for (c in c("latitude", "longitude")) {
    data.table::set(mapping, j = c, value = as.numeric(mapping[[c]]))
  }

  # get distance matrix
  distance_matrix <- geosphere::distm(
    x = mapping[, list(longitude, latitude)]
  )

  # add dimension names
  dimnames(distance_matrix) <- list(
    mapping[["location"]],
    mapping[["location"]]
  )

  loc_vec <- mapping[["location"]]

  # apply factor
  distance_matrix <- distance_matrix * unit_factors[unit]

  cli::cli_alert_success("... Ok, complete.")


  return(list(
    loc_vec = loc_vec |> as.character(),
    distance_matrix = distance_matrix
  ))
}

#' Generalized distance list as sparse list
#'
#' This function is an alternative to the package functions that create a square
#' distance matrix of dimension N, with all pairwise distances. In this approach
#' a list of named vectors is returned, where there is one element in the list
#' for each location, and each named vector holds the distance within
#' `threshold` of the location.
#' @param level string either "county" (default) or "zip"
#' @param threshold numeric value; include in each location-specific named
#'   vector only those locations that a within `threshold` distance units of the
#'   target. The defaults is 50 (miles) and 15 (miles) for county and zip, respectively, but
#'   these can be adjusted. Note if a different unit other than miles is used, then the
#'   user should also adjust this parameter appropriately
#' @param st string; optional to specify a state; if NULL distances are returned
#'   for all zip codes or counties in the US
#' @param unit string one of miles (default), kilometers, or meters; this is the
#'   unit relevant to the threshold
#' @export
create_dist_list <- function(
    level = c("county", "zip"),
    threshold= {if(level=="county") 50 else 15},
    st=NULL,
    unit = c("miles", "kilometers", "meters")
) {


  state <- zip_code <- latitude <- longitude <- fips <- NULL

  level = match.arg(level)
  unit = match.arg(unit)
  factor = list("miles" = 1609, "kilometers" = 1000, "meters"=1)[[unit]]

  # In case threshold is null, revert to default
  if(is.null(threshold)) {
    threshold = ifelse(level=="county", 50, 15)
  }

  # convert within to meters
  threshold = threshold*factor

  if(level == "zip")
    if(!is.null(st)) {
      locs <- zipcodes[state == st, list(location = zip_code, latitude,longitude)]
    } else {
      locs <- zipcodes[, list(location = zip_code, latitude,longitude)]
    }
  else {
    if(!is.null(st)) {
      locs <- counties[state == st, list(location = fips, latitude,longitude)]
    } else {
      locs <- counties[, list(location = fips, latitude,longitude)]
    }
  }

  # reduce to valid locations
  locs <- locs[!is.na(latitude) & !latitude=="NULL"]

  # replace any char with numeric
  for(c in c("latitude", "longitude")) data.table::set(
    locs, j=c, value=as.numeric(locs[[c]])
  )

  # convert to sf
  locs <- sf::st_as_sf(locs, coords = c("longitude", "latitude"), crs=4236)

  # get sparse predicate that holds just those locations within certain distance
  # i.e. the neighbors
  nb = sf::st_is_within_distance(locs, dist=threshold)

  # get the number of locations
  n <- nrow(locs)

  # Flatten indices (aligned with sgbp order)
  ii <- rep.int(seq_len(n), lengths(nb))
  jj <- unlist(nb, use.names = FALSE)

  # get the coordinates from locations
  coords <- sf::st_coordinates(locs)

  # Use geodist package to get the distances
  d <- geodist::geodist(
    x = coords[ii, , drop = FALSE],
    y = coords[jj, , drop = FALSE],
    paired = TRUE,
    measure = "geodesic"
  ) / factor

  # convert to list
  d <- split(d, rep.int(seq_len(n), lengths(nb)))

  # Now, lets get the locations in each of d
  loc_names <- locs$location[unlist(nb,use.names = F)]
  loc_names <- split(loc_names, rep.int(seq_len(n), lengths(nb)))

  d <- lapply(seq_along(d), \(i) sort(stats::setNames(d[[i]], loc_names[[i]])))
  names(d) <- locs$location

  return(d)
}
