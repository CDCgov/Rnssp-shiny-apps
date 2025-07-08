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
#' zip_distance_matrix("TX", "kilometers")
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
#' county_distance_matrix("CT", "kilometers", source = "tigris")
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
#' generation of this matrix can take a few seconds
#' @param unit string, one of "miles" (default), "kilometers", or "meters".
#'   Indicating the desired unit for the distances
#' @export
#' @examples
#' # example code
#' us_distance_matrix()
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
