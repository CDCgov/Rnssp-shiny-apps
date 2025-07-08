# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# This look-up was derived from
# https://en.wikipedia.org/wiki/ZIP_Code#/media/File:ZIP_Code_zones.svg

zip_prefix_lookup <- list(
  "WA" = "^98|^99[0-4]",
  "OR" = "^97",
  "CA" = "^9[0-5]|^96[01]",
  "AZ" = "^8[56]",
  "NV" = "^889|^89",
  "ID" = "^83[2-9]",
  "MT" = "^59",
  "WY" = "^82|^83[01]",
  "UT" = "^84",
  "NM" = "^87|^88[0-4]",
  "CO" = "^8[01]",
  "ND" = "^58",
  "SD" = "^57",
  "NE" = "^6[89]",
  "KS" = "^6[67]",
  "OK" = "^7[34]",
  "TX" = "^7[5-9]|^885",
  "LA" = "^70|^71[0-5]",
  "AR" = "^71[6-9]|^72",
  "MO" = "^6[345]",
  "IA" = "^5[0-2]",
  "MN" = "^55|^56[0-7]",
  "WI" = "^5[34]",
  "IL" = "^6[0-2]",
  "MI" = "^4[89]",
  "IN" = "^4[67]",
  "OH" = "^4[345]",
  "KY" = "^4[012]",
  "TN" = "^37|^38[0-5]",
  "MS" = "^38[6-9]|^39[0-7]",
  "AL" = "^3[56]",
  "GA" = "^3[01]|^39[89]",
  "FL" = "^3[234]",
  "SC" = "^29",
  "NC" = "^2[78]",
  "VA" = "^201|^22|^23|^24[0-6]",
  "DC" = "^200|^569",
  "WV" = "^24[789]|^2[56]",
  "MD" = "^20[6789]|^21", 
  "PA" = "^1[5-8]|^19[0-6]",
  "DE" = "^19[789]",
  "NJ" = "^0[78]",
  "NY" = "^005|^1[0-4]",
  "CT" = "^06",
  "RI" = "^02[89]",
  "MA" = "^01|^02[0-7]",
  "VT" = "^05",
  "NH" = "^03[0-8]",
  "ME" = "^039|^04",
  "HI" = "^96[78]",
  "AK" = "^99[5-9]"
)

county_prefix_lookup <- c(
  MS = "^28", NC = "^37", OK = "^40", VA = "^51", WV = "^54", 
  LA = "^22", MI = "^26", MA = "^25", ID = "^16", FL = "^12", NE = "^31", 
  WA = "^53", NM = "^35", SD = "^46", TX = "^48", CA = "^06", AL = "^01", 
  GA = "^13", PA = "^42", MO = "^29", CO = "^08", UT = "^49", TN = "^47", 
  WY = "^56", NY = "^36", KS = "^20", NV = "^32", IL = "^17", VT = "^50", 
  MT = "^30", IA = "^19", SC = "^45", NH = "^33", AZ = "^04", DC = "^11", 
  NJ = "^34", MD = "^24", ME = "^23", DE = "^10", RI = "^44", KY = "^21", 
  OH = "^39", WI = "^55", OR = "^41", ND = "^38", AR = "^05", IN = "^18", 
  MN = "^27", CT = "^09", AK = "^02", HI = "^15")

# Function to detect states, given zip code location vector, and lookup
get_states_from_location_lookup <- function(locations, lookup) {
  locations = unique(as.character(locations))
  hits <- sapply(lookup, \(l) any(grepl(l,locations)))
  if(any(hits)) names(which(hits)) else NULL
}

get_states_from_location <- function(locations, res=c(NA, "zip", "county")) {
  res = match.arg(res)
  locations = unique(locations)
  
  if(!is.na(res) && res=="zip") {
    return(get_states_from_location_lookup(locations, zip_prefix_lookup))
  }
  cstates = get_states_from_location_lookup(locations, county_prefix_lookup)
  string_states = base::intersect(c("DC", state.abb),unique(substr(locations,1,2)))
  if(is.na(res)) {
    zstates = get_states_from_location_lookup(locations, zip_prefix_lookup)
  } else zstates = NULL
  result = unique(c(cstates, string_states, zstates))
  if(length(result)>0) return(result) else NULL
}

get_sites_by_state <- function(st) {
  site_lookup <- structure(list(
    `Site ID` = c("857", "858", "859", "860", "861", 
      "862", "950", "973", "863", "865", "1019", "996", "977", "866", 
      "867", "868", "869", "871", "872", "873", "955", "978", "880", 
      "879", "881", "882", "884", "885", "979", "886", "887", "888", 
      "889", "890", "893", "892", "894", "895", "896", "899", "901", 
      "902", "903", "904", "906", "905", "907", "908", "909", "910", 
      "911", "912", "913", "914", "916", "915", "917", "918", "919", 
      "920", "922", "923", "924", "925", "926", "929", "928", "930", 
      "931", "933", "934", "936", "937", "938"),
    `Site Name` = c("Alaska", "Alabama", "Arkansas","Arizona", "California",
                    "El Dorado County", "Long Beach", "Los Angeles County",
                    "Monterey County", "Nevada County", "Orange County",
                    "Placer County", "Plumas County", "Riverside County",
                    "Sacramento County", "San Diego County", "San Mateo County",
                    "Santa_Cruz County", "Solano County", "Stanislaus County",
                    "Yolo", "Yosemite Gateway Region", "Connecticut", "Colorado",
                    "District of Columbia", "Delaware", "Florida", "Georgia",
                    "Guam", "Hawaii", "Iowa", "Linn County", "Idaho", "Illinois",
                    "Marion County", "Indiana", "Kansas", "Kentucky", "Louisiana",
                    "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota",
                    "Missouri", "Mississippi", "Montana", "North Carolina",
                    "North Dakota", "Nebraska", "New Hampshire", "New Jersey",
                    "New Mexico", "Nevada", "New York", "New York City", "Ohio",
                    "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                    "South Carolina", "South Dakota", "Tennessee", "Texas",
                    "Texas Region 2/3", "TX_Region65", "Utah", "Virginia",
                    "Vermont", "Washington", "Wisconsin", "West Virginia",
                    "Wyoming"),
    State = c("AK", "AL", "AR", "AZ", "CA", "CA", "CA", "CA", "CA", "CA", "CA",
              "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA", "CA",
              "CN", "CO", "DC", "DE", "FL", "GE", "GU", "HI", "IA", "IA", "ID",
              "IL", "IN", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
              "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY",
              "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "TX",
              "TX", "UT", "VI", "VT", "WA", "WI", "WV", "WY")
  ), row.names = c(NA, -74L), class = "data.frame")
  
  data.table::setDT(site_lookup)
  
  site_lookup[State==st, unique(`Site ID`)]
}

# Function tries to extract locations from url
extract_locations_from_url <- function(url, res=c("zip", "county")) {
  
  res = match.arg(res)
  
  regex = list("zip" = "\\d{5}", "county" = "[A-Za-z]{2}_")
  
  stringr::str_extract_all(
    url,
    paste0("geography=", regex[[res]])
  )[[1]] |>
    stringr::str_remove("geography=") 
}


# function uses zfi ( a zcta fips intersection table)
# to find the counties touched by zips, or the zips
# touched by counties
find_intersects<- function(clt, zfi, res=c("zip", "county")) {
  res = match.arg(res)
  if(res=="zip") {
    result <- unique(zfi[clt, on=.(zipcode == location)][, .(fips, target)])
    result[, fips:=gen_display_name_from_fips(fips)]
    result[, .(intersected_locs = toString(fips)), target]
  } else {
    result <- unique(zfi[clt, on=.(fips == location)][, .(zipcode, target)])
    result[, .(intersected_locs = toString(zipcode)), target]
  }
}


