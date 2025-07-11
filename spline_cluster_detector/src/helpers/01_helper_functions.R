# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


## Function to return a copy of a dataframe with specific columns
## replaced with masked values
obscure_pii <- function(d, cols=NULL) {
  
  if(is.null(cols)) cols = names(d)
  
  df <- data.table::copy(d)
  for (col in cols) {
    set(df, j = col, value = replicate(
      nrow(df),
      paste0(sample(
        c(LETTERS, letters, 0:9), 10,
        replace = TRUE
      ), collapse = "")
    ))
  }
  df[]
}


# Function to get an interpolation function for any value of observe
# Requires a list of datatables, each one a specific spline lookup
get_interpolation_functions <- function(spline_library) {
  sl <- rbindlist(spline_library, idcol="p")[, p:=as.numeric(stringr::str_remove(p,"Spline-"))]
  sl[
    order(p, spl_thresh),
    list(lower_domain = min(spl_thresh),
         upper_domain = max(spl_thresh),
         func = list(approxfun(spl_thresh,p, yleft=max(p), yright=min(p)))
         ),
    observed
  ]
}

# This function takes a frame of interpolation functions, an observed, and
# a log(obs/exp), and returns the approximate p-value (as character). If 
# the value is outside the domain, it returns with prefix "<" or ">"
interpolate_p_value <- function(interpfuncs, o, loe) {
  ip_row <- interpfuncs[observed==o]
  if(dim(ip_row)[1] == 0) return ("NA")
  pval = ip_row[1, func[[1]](loe)] |> sprintf(fmt = "%4.3g")
  # return the p-value string, with prefix if necessary
  fcase(
    loe>ip_row[1, upper_domain], paste0("<",pval),
    loe<ip_row[1, lower_domain], paste0(">",pval),
    default = pval
  )
}

# Function to place info circle tool tip on a input label
# l must be a two element list, first element holds the label
# 2nd element holds the tool tip message
labeltt <- function(l, ...) {
  tooltip(
    trigger=list(
      l[[1]],
      bsicons::bs_icon(name = "info-circle-fill",class="text-primary")
    ),
    p(l[[2]],style='text-align:left;'),
    ...
  )
}

# Function to get data from a custom url
get_custom_url_data <- function(url, profile) {
  
  if(is.null(url) || url=="") {
    cli::cli_abort("No url provided")
  }
  
  # There are two checks that we will  make directly
  # The first is that the url is a tableBuilder URL
  if(!grepl("tableBuilder", url, ignore.case = T)) {
    cli::cli_abort("Custom URL must be a tableBuilder query.")
  }
  
  # The second is that if there is both start and end date, then
  # the latter must be >= the former
  dates = extract_dates_from_url(url)
  if(any(is.na(dates)) || dates[["start"]] > dates[["end"]]) {
    cli::cli_abort("Custom URL is missing dates, or has start date after end date")
  }
  
  # is this url an csv url?
  is_csv = grepl("/csv\\?", url)
  
  # crude validation.. If data is not a dataframe something went wrong
  data = tryCatch(
    get_api_data(url = url,fromCSV = is_csv,profile = profile),
    error=function(e) cli::cli_abort("Call to API failed"),
    warning=function(e) cli::cli_abort("Call to API failed")
  )
  
  if(!is.data.frame(data) || dim(data)[1] == 0) {
    cli::cli_abort(
      "Custom URL call failed. Check URL."
    )
  }
  
  
  if(is_csv) {
    data <- data |> tidyr::pivot_longer(
      cols=-1,
      names_to = "location",
      values_to = "count"
    )
  }
  
  # Now, we have to prepare this raw data. We will assume it is "table", 
  # but we have to guess the geography level.. What about all the other
  # characteristic we need for clustering!.. dates, baseline length, etc
  res_guess <- fifelse(grepl("zip",url,ignore.case=T), "zip", "county")
  
  data <- prepare_raw_data(data, "table", res_guess)
  
  # Note that prepare raw data now returns a list of
  # length two with names "data", and "data_details"
  
  data[["data"]] <- post_process_data_pull(data[["data"]], res=res_guess)
  
  return(data)
  
}

get_data <- function(
    source_data,
    USE_NSSP,
    profile,
    state,
    res=c("zip", "county"),
    data_type=c("table", "details"),
    data_source=c("patient", "facility"), 
    deduplicate=TRUE
) {
  
  data_source = match.arg(data_source)
  data_type = match.arg(data_type)
  res = match.arg(res)
  
  if(USE_NSSP == TRUE) {
    if(nchar(source_data)>=6000) {
      httr::set_config(config=httr::config(http_version = 2L))
    }
    raw_data <- get_api_data(source_data, profile=profile)
    httr::reset_config()
    
    raw_data <- prepare_raw_data(
      raw_data,
      data_type,
      res,
      deduplicate = deduplicate,
      state=state,
      data_source=data_source
    )
    data <- raw_data[["data"]]
    data_details <- raw_data[["data_details"]]
  
  } else {
    data <- source_data
    data_details <- NULL # no data details if this is local file
  }
  
  data <- post_process_data_pull(data, res=res)
  
  return(list(
    data = data, 
    data_details = data_details
  ))

}

post_process_data_pull <- function(data, res=c("zip", "county")) {
  
  # could be empty
  if(is.null(data) || nrow(data)== 0) return(data)
  
  # make sure data location column is character
  data[, location:=as.character(location)]
  
  # Note, if res="county", we really should replace the location 
  # <ST>_<CountyName> with the actual fips, but retain a display
  # name?
  
  if(res == "county") {
    fips_lookup = prepare_county_sf_for_url(st = NULL)
    
    st = data[1, substr(location,1,2)]
    if(is.na(suppressWarnings(as.numeric(st)))) {
      # the location is in <st>_county_name format
      data <- fips_lookup[data, on=.(url_name =  location)]
    } else {
      # the location is in fips format already
      data <- fips_lookup[data, on=.(GEOID =  location)]
    }
    data <- data[, .(location=GEOID, count, date, display_name = url_name)]
  } else {
    data <- data[, .(location=location, count, date, display_name = location)]
  }
  
}

deduplicate_datadetails <- function(
    data, 
    excl_fac_types = "urgent|primary|other|medical speciality"
) {
  
  if(!all(
    c("FacilityType", "Visit_ID", "Hospital") %in% colnames(data)
  )) {
    cli::cli_abort(
      "Cannot de-duplicate unless all of FacilityType, VisitID, and Hospital
      are in the data frame"
    )
  }
    
  setDT(data) |>
    # 1. filter out certain types
    _[!grepl(excl_fac_types, FacilityType,ignore.case = T)] |>
    # 2. deduplicate
    _[order(Date), .SD[1], .(Visit_ID, Hospital)]
  
}

# This function can process the raw data returned from get_api_data()
# call. Note, that the content of that call will differ depending on 
# whether the url passed to that call was a details or table builder call
# and how we process it should depend on the resolution. it will return 
# a list data, and data_details, which is NULL if the original source is just
# a tableBuilder query

prepare_raw_data <- function(data, data_type, res, deduplicate=FALSE, state=NULL, data_source=NULL) {
  if(data_type == "details") {
    # First, a datadetails call returns a list, with the actual
    # frame as the first element
    data <- data[["dataDetails"]]
    
    # Set this to data.table
    setDT(data)
    
    # could be empty
    if(is.null(data) || nrow(data) == 0) return(data)
    
    # if deduplication, call the deduplication function
    if(deduplicate) data <- deduplicate_datadetails(data = data)
    # make a copy of this, which is data details
    data_details <- data.table::copy(data)
    
    # Reduce data details to counts
    data <- reduce_data_details_to_counts(
      data=data, res=res, state=state, data_source = data_source
    )  
  
  } else {
    setDT(data)
    data[, timeResolution:=as.IDate(timeResolution)]
    data_details <- NULL
  }
  
  data <- check_and_standarize_data_cols(data)
  
  return(list(
    data = data, 
    data_details = data_details
  ))
}

reduce_data_details_by_filters <- function(
    data,
    filters
) {
  for (f in filters) {
    # 1. parse the string into an R expression
    expr <- parse(text = f)[[1]]
    # 2. use eval(), within df
    data <- data[eval(expr, envir=data)]
  }
  return(data)
}

reduce_data_details_to_counts <- function(
    data,
    res=c("zip", "county"),
    state,
    data_source=c("patient", "facility")
) {
  
  # Now we have four possibilities:
  # 1. zip/patient - we count by patient zip code
  # 2. zip/facility - we still count by patient zipcode, but we have
  # to assign an in-state zip code to those that have out of state home zip
  # codes. We assign them to the facility zip code
  
  # 3. county/patient - we count by region
  # 4. county/facility - we still count by patient region, but we have
  # to assign an instate region to those that have out of state home regions.
  # We assign these encounters to the facility region
  
  res = match.arg(res)
  data_source=match.arg(data_source)
  
  byvar = fifelse(res=="zip", "ZipCode", "Region")
  
  # Now before we count_by, we need to update the data if facility
  if(data_source == "facility") {
    if(res == "zip") {
      data <- data[, .(count = .N), by=.(Date, Zipcode = fifelse(State == state, ZipCode, HospitalZip))]
    }
    if(res == "county") {
      data <- data[, .(count = .N), by=.(Date, Region = fifelse(State == state, Region, HospitalRegion))]
    }
  } else {
    data <- data[, .(count = .N), by=c("Date", byvar)]
  }
  
  data[, Date:=as.IDate(Date, "%m/%d/%Y")]
  setnames(data, new=c("date", "location", "count"))
  
  data
  
}

check_and_standarize_data_cols <- function(data) { 
  
  if(ncol(data)>4) {
    cli:cli_abort("Returned data has more than 4 columns and can't be processed")
  }
  
  if(!"count" %in% names(data)) {
    cli:cli_abort("Returned data has no count column, perhaps check custom URL?")
  }
  
  ## Okay, now we see if we can correctly rename the cols
  # 1. We assume that there is a timeResolution or Date column, and its the first
  setnames(data, old = names(data)[1], new="date")
  
  # 2. We assume that the second column is a location indicator
  setnames(data, old = names(data)[2], new="location")
  
  # 3. If there are four columns, we are going to sum over that extra column
  if(ncol(data)==4) {
    data <- data[, .(count = sum(count, na.rm=T)), by = c("date", "location")]
  }
  
  setcolorder(data, c("location", "count", "date"))
  
  # Sort by date and location
  data <- data[order(date,location)]
  
  # return
  data
  
}
  

prepare_county_sf_for_url <- function(st=NULL) {
  
  csf <- as.data.table(Rnssp::county_sf)
  ssf <- as.data.table(Rnssp::state_sf)
  
  csf <- csf[ssf[, .(STATEFP,STUSPS)], on="STATEFP", .(STATEFP, STUSPS, NAME, GEOID)]
  if(!is.null(st)) csf <- csf[STUSPS == st]
  
  # built in look up structure
  name_change_lookup <- structure(
    list(
      GEOID = c("02105", "17099", "19141", "22059", "24033", "24035", "24037",
                "24510", "29510", "35013", "51530", "51600", "51620", "51760",
                "51770"),
      url_name = c("Hoonah-Angoo", "La Salle", "O Brien", "La Salle", 
                   "Prince Georges", "Queen Annes", "St. Marys", "Baltimore City",
                   "St. Louis City", "Dona Ana","Buena Vista City", "Fairfax City",
                   "Franklin City", "Richmond City", "Roanoke City"),
      NAME = c("Hoonah-Angoon", "LaSalle", "O'Brien", "LaSalle", "Prince George's",
               "Queen Anne's", "St. Mary's", "Baltimore", "St. Louis", "DoÃ±a Ana",
               "Buena Vista", "Fairfax", "Franklin", "Richmond", "Roanoke")
    ),
    row.names = c(NA, -15L),
    class = "data.frame"
  )
  setDT(name_change_lookup)
  
  # merge the look up structure on the set of names and coalesce
  name_change_lookup[csf, on="GEOID"] |> 
    _[, url_name:=dplyr::coalesce(url_name, i.NAME)] |> 
    _[, .(GEOID, url_name = paste0(STUSPS, "_", url_name))]
  
}

cluster_computation_psa <- function() {
  psa = paste0("NB: The process of defining clusters using this approach is ",
               "currently under development, and may undergo further maturation, ",
               "including possible agglomeration of contiguous clusters.")
  return(psa)
}



get_base_vals <- function(use_nssp, profile=NULL) {
  # NOTE THAT THIS FUNCTION WILL GRAB SPECIFIC VALUES
  
  ccdd_cats <- c("A", "B", "C")
  
  syndromes <- c(
    "Bot_like", "Exposure", "Fever", "GI", "Hemr_ill", "ILI",
    "Injury", "Neuro", "Rash", "RecordsOfInterest", "Resp", "Shk_coma"
  )
  
  subsyndromes <- c(
    "AbdominalPain", "AcuteBloodAbnormalities", "AcuteBronchitis", "AcuteRespDistress",
    "Agitation", "AlcoholUse", "AlteredMentalStatus", "Anthrax", "Aspiration", "Assault",
    "Asthma", "AsthmaOrRAD", "BiteOrSting", "BleedingGums", "Bloating", "BlurredVision",
    "BodyAches", "Botulism", "Bronchitis", "Brucellosis", "Bubo", "Campylobacteriosis",
    "ChestCongestion", "ChickenPox", "Chills", "Cholera", "Chronic", "Ciguatera", "Coma",
    "COPoisoning", "Cough", "CreutzfeldtJakob", "Cryptosporidiosis", "CutOrPierce",
    "Cyclosporiasis", "Death", "Delirium", "Dengue", "Diarrhea", "DifficultyBreathing",
    "DifficultyFocusing", "DifficultySpeaking", "DifficultySwallowing", "DilatedPupils",
    "Diphtheria", "DisseminatedIntravascularCoagulation", "Dizziness", "DoubleVision",
    "DrowningOrSubmersion", "Drowsiness", "DryMouth", "Ehrlichiosis", "Electrocution",
    "Encephalitis", "Eschar", "EscherichiaColi", "ExcessiveHeat", "Exposure", "Fall",
    "Fatigue", "FetalDeath", "FeverOnly", "FeverOrChills", "FeverPlus", "Firearm",
    "FireBurnExplosives", "Flushing", "FoodPoisoning", "ForeignBody", "Gastroenteritis",
    "GIBleeding", "Glanders", "Haemophilus", "Hansen", "Hantavirus", "Headache",
    "Hematemesis", "Hemoptysis", "Hepatitis", "Hypotension", "ILI", "InducedFetalDeath",
    "InfectiousHepatitis", "Influenza", "Laryngitis", "LeadPoisoning", "Legionnaires",
    "Leprosy", "Leptospirosis", "Listeriosis", "LossOfAppetite", "LossOfConsciousness",
    "LowerRespiratoryInfection", "Lyme", "Malaise", "Malaria", "Measles", "Melioidosis",
    "Meningitis", "Meningococcemia", "MercuryPoisoning", "MotorVehicle", "Mumps",
    "MuscleWeakness", "NasalCongestion", "Nausea", "NonILIFevers", "NonMotorVehicle",
    "Nosebleed", "NVD", "Occupational", "OtitisMedia", "Overexertion", "P_and_I",
    "P_and_I_Without_Exposure", "Pertussis", "PesticidePoisoning", "Petechiae", "Plague",
    "Pneumonia", "Poisoning", "Polio", "Projectile", "ProjectileVomiting", "Prostration",
    "Psittacosis", "Ptosis", "QFever", "Rabies", "Rash", "ReactiveAirwayDisease",
    "RockyMountain", "Rubella", "Salmonellosis", "Seizure", "Sepsis", "SepticShock",
    "Shigellosis", "Shock", "ShortnessOfBreath", "SidedWeakness", "Smallpox", "Sores",
    "SoreThroat", "SportsOrExerciseRelated", "StrawberryTongue", "StruckBy", "Suffocation",
    "SuicideOrSelfInflicted", "SwollenGlands", "Tachycardia", "ToolsOrMachinery",
    "Toxoplasmosis", "Trichinosis", "Tularemia", "Typhoid", "Typhus", "UnexplainedDeath",
    "UpperRespiratoryInfection", "Vibrio", "Vomiting", "Watercraft", "Wheezing", "YellowFever"
  )
  
  if(use_nssp == TRUE) {
    
    if(is.null(profile)) return(NULL)
    
    #########################################################
    comb_url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/combinedCategory"
    url <- "https://essence.syndromicsurveillance.org/nssp_essence/servlet/SyndromeDefinitionsServlet_CCDD?action=getCCDDTerms"
    ##################################################   Note CCDD name change!
    
    combinedCategories <- comb_url |>  get_api_data(profile = profile)
    ccddterms <- get_api_data(url, profile = profile)
    
    if(!is.null(combinedCategories) && !is.null(ccddterms)) {
      
      combinedCategories <- combinedCategories |>       
        pluck("values") |> 
        rename(combined_category = display) |> 
        left_join(
          ccddterms |> 
            pluck("categories") |> 
            select(combined_category = category, query_logic = definition) |> 
            mutate(combined_category = paste("CCDD", combined_category)), 
          by = "combined_category"
        ) |> 
        mutate(across(where(is.character), ~replace_na(., "")))
      
      
      setDT(combinedCategories)
      ccdd_cats <- combinedCategories[grepl("^CCDD", combined_category), combined_category]
      ccdd_cats <- gsub("CCDD ", "", ccdd_cats)
      syndromes <- combinedCategories[grepl("^SYNDROME", combined_category), combined_category]
      syndromes <- gsub("SYNDROME ", "", syndromes)
      subsyndromes <- combinedCategories[grepl("^SUBSYNDROME", combined_category), combined_category]
      subsyndromes <- gsub("SUBSYNDROME ", "", subsyndromes)
    } 
      
  }
  
  return(list(
    ccdd_cats = ccdd_cats,
    syndromes = syndromes,
    subsyndromes = subsyndromes
  ))

}


# Function to get display name from fips
gen_display_name_from_fips <- function(fips, st=NULL, default=NA) {
  l <- prepare_county_sf_for_url(st = st)
  result = l[data.table::CJ(GEOID =fips,sorted=F), on="GEOID", url_name]
  fifelse(is.na(result),default, result)
}


# Function to extract dates from ulr
extract_dates_from_url <- function(url) {
  setNames(
    lapply(c("start", "end"), \(s) {
      rx = paste0("(?<=",s, "Date=)\\d{1,2}[A-z]{3}\\d{4}")
      stringr::str_extract(url, rx) |> as.Date("%d%b%Y")
    }),
    c("start", "end")
  )
}

# Function is similar to change_dates() from Rnssp package, but allows
# dates to be injected completely if missing, and also does not warn/error
# if start date is after end date (users of such a url take responsibility
# to check this elsewhere)
inject_dates_into_url <- function(url, start_date=NULL, end_date=NULL, inject_if_missing = FALSE) {
  
  # internal  helper function
  ud <- \(u,d,stem) {
    d <- as.Date(d)
    p <- paste0("(", stem, "=\\d{1,2}[A-z]{3}\\d{4})|(", stem, "=NA)")
    
    r <- paste0(stem, "=",format(d,"%d%b%Y"))
    if(stringr::str_detect(u,p)) return(stringr::str_replace(u, p, r))
    if(inject_if_missing) stringr::str_c(u, "&", r)
    else u
  }
  
  # call the helper for both start and end
  if(!is.null(start_date)) url <- ud(url, start_date, "startDate")
  if(!is.null(end_date)) url <- ud(url, end_date, "endDate")
  
  return(url)
}

toggle_task_button_color <- function(btn_id, busy=TRUE) {
  cl = c("btn-primary", "btn-danger")
  if(!busy) cl <- rev(cl)
  shinyjs::runjs(paste0('$("#', btn_id, '").removeClass("', cl[1], '").addClass("', cl[2],'");'))
}

