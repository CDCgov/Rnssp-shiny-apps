# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# Look up of valid fields

# if we were to use FacilityType as a filter, we might want to exclude
# - primary Care
# - urgent care

valid_details_fields <- c(
  "PID", "Date", "Time", "HospitalName", "ZipCode", "Region", 
  "C_Patient_County", "ChiefComplaintOrig", "ChiefComplaintParsed", 
  "Category_flat", "SubCategory_flat", "DischargeDiagnosis", "C_BioSense_ID", 
  "C_Visit_Date_Time", "C_Visit_Date_Source", "EssenceID", "Visit_ID", 
  "Sex", "Age", "Birth_Date_Time", "C_Patient_Age", "C_Patient_Age_Units", 
  "C_Patient_Age_Source", "C_Unique_Patient_ID", "C_Unique_Patient_ID_Source", 
  "Ethnicity_flat", "Race_flat", "HospitalZip", "Patient_State", 
  "Patient_Country", "Body_Mass_Index", "Travel_History", "Admit_Date_Time", 
  "Admit_Reason_Code", "Admit_Reason_Combo", "Admit_Source", "Admission_Type", 
  "AdmissionTypeCategory", "Arrived_Date_Time", "C_Facility_ID_Source", 
  "C_Patient_Class", "Chief_Complaint_Combo", "ClinicalImpression", 
  "Diagnosis_Combo", "Discharge_Date_Time", "DischargeDisposition", 
  "Initial_Temp", "Initial_Temp_Calc", "Initial_Pulse_Oximetry", 
  "Initial_Pulse_Oximetry_Calc", "C_Patient_Class_Updates", "C_Patient_Class_MDT_Updates", 
  "CCDD", "CCDDCategory_flat", "ChiefComplaintUpdates", "ChiefComplaintMDTUpdates", 
  "DischargeDiagnosisUpdates", "DischargeDiagnosisMDTUpdates", 
  "DischargeDispositionUpdates", "DischargeDispositionMDTUpdates", 
  "Patient_Class", "PatientClassList", "PatientClassUpdates", "PatientClassMDTUpdates", 
  "TriageNotesParsed", "Trigger_Event", "Trigger_Event_Updates", 
  "Trigger_Event_MDT_Updates", "MedRecNo", "Hospital", "HalfHour", 
  "ModeOfArrival", "ProviderDiagnosis", "MessageControlID", "Onset_Date", 
  "DispositionDateTime", "PregnancyStatus", "InitEDAcuityAssessment", 
  "Insurance_Coverage", "Insurance_Company_ID", "DiagnosisDatetime", 
  "VitalSignData", "ObsSympClinicalFindings", "HighestTemp", "HighestTemp_Calc", 
  "ProcedureCode", "TriageNotesOrig", "TNCategory_flat", "TNSubCategory_flat", 
  "State", "HospitalRegion", "FacilityType", "Hospital_HRSA_County_Designation", 
  "HospitalDHHSRegion", "HospitalState", "dhhsregion", "SiteID", 
  "WeekYear", "MonthYear", "QuarterYear", "Year", "VisitNumber", 
  "AlternatePatientID", "OnsetDateTimeText", "DiagnosisText", "BloodPressure", 
  "BaseFilename", "DeathDateTime", "DeathIndicator", "DDConverted", 
  "DDProcessed", "StagingRowID", "Message_ID", "Create_Processed_Date_Time", 
  "Create_Raw_Date_Time", "Update_Processed_Date_Time", "Create_ER_Import_Date_Time", 
  "Feed_Name", "Sending_Application", "Message_Type", "Site_ID", 
  "C_Chief_Complaint_Source", "Patient_Class_Code", "Facility_Type_Code", 
  "Facility_Type_Description", "C_FacType_Patient_Class", "C_Patient_Class_List", 
  "C_MFT_Patient_Class", "C_Patient_Class_Source", "Treating_Facility_ID", 
  "Sending_Facility_ID", "C_Processed_Facility_ID", "Procedure_Date_Time", 
  "Death_Date_Time", "Recorded_Date_Time", "Diagnosis_Date_Time", 
  "Time_Zone", "Medical_Record_Number", "C_Death", "C_Death_Source", 
  "C_Processed_BioSense_ID", "Patient_Zip", "C_Patient_County_Source", 
  "Diagnosis_Type", "Procedure_Code", "Procedure_Combo", "Problem_List_Code", 
  "Problem_List_Combo", "Initial_Acuity_Code", "Initial_Acuity_Combo", 
  "Unique_Physician_Identifier", "Provider_Type_Code", "Provider_Type_Combo", 
  "Patient_City", "Hospital_Unit_Code", "Hospital_Unit_Description", 
  "Height", "Height_Units", "Weight", "Weight_Units", "Smoking_Status_Code", 
  "Systolic_Blood_Pressure", "Systolic_Blood_Pressure_Units", "Diastolic_Blood_Pressure", 
  "Diastolic_Blood_Pressure_Units", "Systolic_Diastolic_Blood_Pressure", 
  "Systolic_Diastolic_Blood_Pressure_Units", "Medication_List", 
  "Medication_Code", "Medication_Combo", "Previous_Hospital_Unit", 
  "Pregnancy_Status_Code", "Hospital_Service", "Discharge_Instructions", 
  "Orig_Sex", "HasBeenE", "HasBeenI", "HasBeenO", "HasBeenAdmitted", 
  "DDAvailable", "DDInformative", "CCAvailable", "CCInformative", 
  "Create_ER_Base_Date_Time", "Create_Cache_ER_Base_Date_Time_Detection", 
  "Create_Cache_ER_Base_Date_Time_Web", "HomeFacilityDistance", 
  "CCOrig_Length", "CCParsed_Length", "DD_Length", "MinutesFromVisitToDischarge", 
  "HoursFromVisitToDischarge", "DaysFromVisitToDischarge", "Str_Birth_Date_Time", 
  "FirstDateTimeAdded", "c_race", "c_ethnicity", "CRace_CEth_Combined_Broad", 
  "CRace_CEth_Combined_Narrow", "ICD_Chapter_Flat", "ICD_Chapter_Desc_Flat", 
  "ICD_Section_Flat", "ICD_Section_Desc_Flat", "ICD_Diagnosis_Flat", 
  "ICD_Diagnosis_Desc_Flat", "ICD_CCSR_Flat", "ICD_CCSR_Desc_Flat", 
  "C_DiagnosisCode_ICD10_Flat", "C_DiagnosisCode_ICD9_Flat", "C_DiagnosisCode_SNOMED_Flat", 
  "DDParsed", "CCDDParsed", "PatientHSA", "PatientHSANCI", "DispositionCategory", 
  "AgeGroup"
)


# Function creates a url to pass to API
# If all fields desired, pass "all"
generate_url <- function(
    state_value,
    synd_drop_menu,
    synd_cat = c("ccdd", "synd", "subsynd"),
    end_date = NULL,
    start_date = NULL,
    res = c("zip","county"), 
    inject_site=TRUE,
    data_type = c("table", "details"), 
    data_source = c("patient", "facility"),
    fields = NULL
) {
  
  state_value <- toupper(state_value)

  data_type = match.arg(data_type)
  res = match.arg(res)
  synd_cat = match.arg(synd_cat)
  data_source = match.arg(data_source)
  
  # if source is facility, then we cannot do zip and table builder
  if(data_source == "facility" && res == "zip" && data_type == "table") {
    cli::cli_abort(
      "Invalid combination: Table Builder Queries not available for zip/facility"
    )
  }
  
  if(state_value == "US" && (data_type == "details" || res == "zip")) {
    cli::cli_abort(
      "Invalid combination: National Level cannot use details and/or be at the
      zip code level"
    )
  }
  
  # Take provided fields and ensure that the base field are provided
  if(data_type=="details") {
    # if not null and keyword "all", set to all fields
    if(!is.null(fields) && tolower(fields)[1] == "all") fields <- valid_details_fields
    
    # Now, make sure whatever is passed has our key fields included
    base_fields <-  c(
      "Region","State","Date","ZipCode",
      "HospitalRegion", "FacilityType", "C_BioSense_ID", "Visit_ID", "Hospital",
      "Age", "Sex"
    )
    
    fields <- base::union(base_fields, fields)
    
    # Now check all are in the valid_details_fields (this protects against misspellings)
    if(any(!tolower(fields) %in% tolower(valid_details_fields))) {
      cli::cli_abort("Misspelled fields, cannot construct url")
    }
  } else fields <- NULL
  
  
  if(state_value == "US") {
    # No site injection, and res must be county
    if(inject_site == TRUE) {
      cli::cli_alert_warning("Site Injection Not allowed/ignored for US")
      inject_site <- FALSE
    }
    if(res!="county") {
      cli::cli_alert_warning("When US selected, resolution will auto-set to county")
      res = "county"
    }
  }
  
  # get the geographies vector, given the state and resolution
  if(data_type=="table") geos = get_geos(st = state_value, res=res)
  else geos=tolower(state_value)
  
  # get the url
  nssp_url <- gen_url(
    geographies = geos,
    syndrome = synd_drop_menu,
    syndrome_cat = synd_cat,
    res = res,
    data_type = data_type,
    data_source = data_source
  )
  
  # update the dates
  date_message = "
  Dates/Baseline Length not provided; check the Time Parameters in Run Specifications
  "
  if(is.null(start_date) || is.na(start_date) || length(start_date) == 0 ||
     is.null(end_date) || is.na(end_date) || length(end_date) == 0) {
    cli::cli_abort(date_message)
  }
  
  nssp_url <- Rnssp::change_dates(nssp_url, start_date, end_date)
  
  # inject sites if requested
  if(inject_site) {
    nssp_url <- inject_site(nssp_url, state_value)
  }
  
  # append fields if not NULL
  if(!is.null(fields) & data_type=="details") nssp_url <- paste0(nssp_url, "&", get_fields(fields))
  
  # return the url
  return(nssp_url)
  
}

# Function injects sites into url, given a state
inject_site <- function(url, st) {
  # Get the sites
  sites = get_sites_by_state(st = st)
  
  # If none found, return the url
  if(length(sites) == 0) return(url)
  
  # else make a string of site=<site>, separated by "&"
  sites = paste0("site=",sites,collapse = "&")
  
  # return the original url suffixed with this new string
  # of sites
  paste0(url,"&",sites)
}

# Function returns a vector of geographies for importing
# into a essence query. Not that if st == "US", NULL is returned
get_geos <- function(st, res) {
  
  if(st == "US") return(NULL)
  
  if(res == "zip") {
    #mapping <- splineClusterDetector::zipcodes
    mapping <- zipcodes
    geos <- mapping[state == st, zip_code]
  }
  
  if(res == "county") {
    mapping <- prepare_county_sf_for_url(st = st)
    geos <- mapping[,url_name]
  }
  
  return(geos)
  
}

# Function returns a text string of field names to append
# to the url if data is to be pulled using data details, an
# option unavailable if the geography is US
get_fields <- function(fields) {
  paste0("field=",fields,collapse="&")
}

# Function generates the nssp essence url based on resolution
# (zip or county), and a vector of geographies, plus a value
# for syndrome, given a category
gen_url <- function(
    geographies,
    syndrome, 
    syndrome_cat = c("ccdd", "synd", "subsynd"),
    res = c("zip", "county"),
    data_type = c("table", "details"),
    data_source = c("patient", "facility")
) {
  
  data_type = match.arg(data_type)
  res = match.arg(res)
  syndrome_cat = match.arg(syndrome_cat)
  data_source = match.arg(data_source)

  
  # if source is facility, then we cannot do zip and table builder
  if(data_source == "facility" && res == "zip" && data_type == "table") {
    cli::cli_abort(
      "Invalid combination: Table Builder Queries not available for zip/facility"
    )
  }
  
  
  # Lookups
  
  # --------------------------
  # Syndromic categories
  # --------------------------
  synd_bits <- list(
    "ccdd" = c("mgs" = "chiefcomplaintsubsyndromes", "cat" = "ccddCategory="),
    "synd" = c("mgs" = "essencesyndromes", "cat" = "medicalGrouping="),
    "subsynd" = c("mgs" = "chiefcomplaintsubsyndromes", "cat" = "medicalGrouping=")
  )
  # Set the syndrome specification
  sc <- paste0(
    "medicalGroupingSystem=",synd_bits[[syndrome_cat]]["mgs"], "&",
    synd_bits[[syndrome_cat]]["cat"], xml2::url_escape(tolower(syndrome))
  )
  
  
  # -------------------------------------------------
  # Data source, resolution, column fields, etc
  # -------------------------------------------------
  datasource <- get_datasource_structure(
    data_type=data_type, res=res, data_source = data_source
  )
  
  # -------------------------------------------------
  # Geographies, if passed
  # -------------------------------------------------
  if(!is.null(geographies)) {
    geographies = paste0(
      "geography=", xml2::url_escape(geographies), "&", collapse=""
    )
  } else geographies = ""
  
  # -------------------------------------------------
  # Base URL
  # -------------------------------------------------
  base_url <- get_base_url(data_type = data_type)  
  
  # -------------------------------------------------
  # Paste it all together, and return
  # -------------------------------------------------
  paste0(base_url,geographies,datasource,sc)
  
}

# Function that builds a portion of the url that dictates the data source
# the geography system, and the column fields, etc
get_datasource_structure <- function(data_type = c("table", "details"),
                                     res = c("zip", "county"),
                                     data_source = c("patient", "facility")) {
  data_type=match.arg(data_type)
  res=match.arg(res)
  data_source = match.arg(data_source)
  
  # if source is facility, then we cannot do zip and table builder
  if(data_source == "facility" && res == "zip" && data_type == "table") {
    cli::cli_abort(
      "Invalid combination: Table Builder Queries not available for zip/facility"
    )
  }
  

  # set the datasource 
  ds <- ifelse(data_source == "patient", "va_er", "va_hosp")
  
  # set the geography system
  if(data_type == "details") gs = "state"
  else {
    
    if(res == "zip") {
      
      # TODO: Currently, there seems to be no option to do table builder
      # query with zip code, and hospital. Therefore we can just set
      # gs and cdf, without worrying about ds
      gs = "zipcodelist"
      cf = "geographyzipcodelist"
    } else{
      
      if(ds == "va_hosp") {
        gs =  "hospitalregion"
        cf = "geographyhospitalregion"
      } else {
        gs = "region"
        cf = "geographyregion"
      }
    }
  }
  # Set the Data Source portions of URL
  datasource <- paste0("datasource=",ds,"&","geographySystem=",gs,"&")
  
  if(data_type == "table") {
    datasource = paste0(datasource, "columnField=", cf, "&")
  }
  
  return(datasource)
  
}

get_base_url <- function(data_type=c("table", "details")) {
  data_type=match.arg(data_type)
  base_start = format(Sys.Date()-7,"%d%b%Y")
  base_end = format(Sys.Date()-7-90, "%d%b%Y")
                  
  if(data_type == "table") {
    return(
      paste0(
        "https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder?",
        "startDate=", base_start, "&endDate=", base_end, "&percentParam=noPercent&aqtTarget=TableBuilder&",
        "detector=probrepswitch&timeResolution=daily&hasBeenE=1&rowFields=timeResolution&"
      )
    )
  } else {
    return(
      paste0(
        "https://essence.syndromicsurveillance.org/nssp_essence/api/dataDetails?",
        "startDate=", base_start, "&endDate=", base_end, "&percentParam=noPercent&aqtTarget=DataDetails&",
        "detector=probrepswitch&timeResolution=daily&hasBeenE=1&"
      )
    )
  }
}
