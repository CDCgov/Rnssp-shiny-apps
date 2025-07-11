# How do we make a function that can pull the records associated with clusters
# found?

DEFAULT_LL_FIELDS <- c(
  "C_BioSense_ID",
  "HospitalName",
  "Date",
  "Time", 
  "Region",
  "State", 
  "ZipCode",
  "Sex", 
  "Age",
  "CRace_CEth_Combined_Broad",
  "ChiefComplaintUpdates",
  "DischargeDiagnosisUpdates",
  "Diagnosis_Combo",
  "TriageNotesOrig",
  "DispositionCategory"
)

# Function will replace a url's geography from state to specific locations, and
# will change geography system to either region or county (from state)
replace_state_geo_with_locations_patient <- function(url, locations, res = c("zip", "county")) {

  res <- match.arg(res)

  # make geography string:
  geo_string <- paste0("geography=", xml2::url_escape(tolower(locations)), collapse = "&")

  # replace geography state with individual region geographies
  url <- sub("geography=[A-z]*(?=&)", geo_string, url, perl = TRUE)

  # replace geographySystem state with geographySystem=region or zipcode
  url <- sub(
    "geographySystem=state",
    fifelse(res == "zip", "geographySystem=zipcode", "geographySystem=region"),
    url,
    fixed = TRUE
  )

  # return the url
  return(url)
}


replace_state_geo_with_locations_facility <- function(
    url, locations, res = c("zip", "county"), geo_string_target = "patientLoc="
    ) {

  res <- match.arg(res)

  # make geography string:
  geo_string <- paste0(geo_string_target, xml2::url_escape(tolower(locations)), collapse = "&")

  # replace geography state with individual region geographies
  url <- sub("geography=[A-z]*(?=&)", geo_string, url, perl = TRUE)

  # # replace geographySystem state with geographySystem=region or zipcode
  # url = sub(
  #   "geographySystem=state",
  #   fifelse(res=="zip", "geographySystem=zipcode", "geographySystem=region"),
  #   url,
  #   fixed=TRUE
  # )
  #
  # return the url
  return(url)
}


get_clusters_for_line_listing <- function(cluster_locations) {
  strsplit(cluster_locations, ", ") |> unlist() |> unique()
}

get_affected_locations_for_line_listing <- function(affected_locations) {
  cluster_locations <- gsub("^.*</summary>", "", affected_locations)
  cluster_locations <- gsub("</details>", "", cluster_locations)
  strsplit(cluster_locations, ", ") |> unlist() |> unique()
}


filter_ll_to_clusters <- function(ll, cluster_table, res) {
  # what do we need this function to do?
  # First, we create a lookup table for each center
  centers <- melt(
    cbind(
      cluster_table[, Center],
      cluster_table[, tstrsplit(`Cluster Locations`, ", ")]
    ),
    "V1",
    na.rm = T
  )
  centers[, variable := NULL]
  setnames(centers, new = c("Center", "location"))

  # add the start date
  centers <- centers[cluster_table[, .(Center, `Cluster Date`)], on = c("Center")]

  # now we filter using these centers and the date
  ll[, jdate := Date]
  if (res == "zip") {
    filter_ll <- ll[centers, on = .(ZipCode = location, jdate >= `Cluster Date`)]
  } else {
    filter_ll <- ll[centers, on = .(Region = location, jdate >= `Cluster Date`)]
  }

  setcolorder(filter_ll, c("Center", "jdate", DEFAULT_LL_FIELDS))
  setnames(filter_ll, old = "jdate", new = "Cluster Date")
  filter_ll

}

get_line_listing_from_clusters <- function(
    cluster_table,
    state_abbreviation,
    end_date,
    res,
    synd_drop_menu,
    synd_cat,
    data_source,
    profile,
    dedup = TRUE,
    update_geos = TRUE,
    reference_details = NULL
    ) {
  # 1. Get the clusters affected <-
  locations <- get_clusters_for_line_listing(cluster_table[["Cluster Locations"]])

  # 2. Get the start date for these clusters
  start_date <- cluster_table[["Cluster Date"]] |> min()

  # 3. Get the base url
  base_url <- generate_url(
    state_value = state_abbreviation,
    synd_drop_menu = synd_drop_menu,
    synd_cat = synd_cat,
    end_date = end_date,
    start_date = start_date,
    res = res,
    data_type = "details",
    data_source = data_source,
    fields = DEFAULT_LL_FIELDS
  )

  if (update_geos == TRUE) {
    # 4. Update this url, given the locations
    if (data_source == "patient") {
      updated_url <- replace_state_geo_with_locations_patient(
        url = base_url,
        locations = locations,
        res = res
      )
    }
    if (data_source == "facility") {
      updated_url <- replace_state_geo_with_locations_facility(
        url = base_url,
        locations = locations,
        res = res
      )
    }
  } else {
    updated_url <- base_url
  }

  # 5. Pull the data down
  ll <- get_api_data(url = updated_url, profile = profile)$dataDetails

  # set to DT, fix dates, and dedup if
  setDT(ll)
  # return(ll)
  ll[, Date := as.IDate(Date, "%m/%d/%Y")]
  if (dedup == TRUE) ll <- deduplicate_datadetails(ll)


  # 6. Filter the data to match the clusters
  ll <- filter_ll_to_clusters(
    ll = ll,
    cluster_table = cluster_table,
    res = res
  )

  # 7. Further filter to make sure that we restrict to only those
  # records that are in the filtered data details. Note that if the user has
  # applied a filter to data details generated data (like an age or sex
  # filter), we need to restrict to those also. We can do this by simply
  # running an inner join on the ll thus far with the reference details rows
  ll_cols <- names(ll)
  if (!is.null(reference_details)) {
    ll <- reference_details[, .(C_BioSense_ID)][ll, on = "C_BioSense_ID", nomatch = NULL]
  }

  # 7 Return the result
  ll[, .SD, .SDcols = ll_cols]

}

# cl <- fread("~/../Downloads/clusters_found.csv")
# ll <- get_line_listing_from_clusters(
#   cl,
#   "OH",
#   "2025-02-03",
#   "county",
#   "fever",
#   "synd",
#   "facility",
#   CREDENTIALS$profile,
#   update_geos = T
# )
