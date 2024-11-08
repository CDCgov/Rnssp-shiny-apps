# © 2024 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under 
# contracts no. 75D30120C07643, 75D30122C15442, 75D30124C19958

#----------------------------------------------------
# Scenario Detection App - Phase 1
# Authors:
#   Joshua Kimrey
#   Catherine Schwartz
#   Roseric Azondekon
#   Michael Sheppard
#----------------------------------------------------

#--------------------------Load libraries---------------------------------------
suppressPackageStartupMessages({
  packages <- c(
    "shiny", "shinyjs", "dplyr", "Rnssp", "purrr",
    "data.table", "lubridate", "shinycssloaders",
    "plotly", "shinyWidgets", "sf", "shinythemes",
    "janitor", "tidyverse", "leaflet", "leaflegend",
    "spdep", "shinydashboard", "stringr", "DT", 
    "kableExtra", "knitr", "htmltools", "openxlsx"
  )
})

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, library, character.only = TRUE)

if (length(setdiff("Rnssp", rownames(installed.packages()))) > 0) {
  devtools::install_github("cdcgov/Rnssp", upgrade = "never")
}
lapply("Rnssp", library, character.only = TRUE)

#------------------------------Load profile-------------------------------------
load_profile <- rstudioapi::showQuestion(
  "Scenario Detection App",
  "NSSP-ESSENCE Credentials are required to use this app!",
  "Load a profile File",
  "Supply User Credentials"
)

myProfile <- NULL
prof_file <- NULL

if (load_profile) {
  filtres <- matrix(c(
    "R images (*.RData,*.rda)", "Binary R files (*.rds)",
    "*.RData;*.rda", "*.rds"
  ), 2, 2)
  if (interactive() && .Platform$OS.type == "windows") {
    prof_file <- choose.files(filters = filtres)
  } else if (interactive() && .Platform$OS.type == "unix") {
    prof_file <- file.choose()
  } else if (!interactive()) {
    prof_file <- readline("Enter full path to the profile file: ")
  }
  if (!any(endsWith(prof_file, c(".rda", ".rds")))) {
    cli::cli_alert_danger("Failed to load. File provided must be either an {.field .rda} or {.field .rds} file")
  }
  
  if (all(endsWith(tolower(prof_file), ".rda"))) {
    myProfile <- get(load(prof_file))
  } else {
    myProfile <- prof_file %>%
      readRDS() %>%
      try(silent = TRUE)
  }
  if (all(class(myProfile) == "try-error")) {
    cli::cli_alert_danger("No or corrupt file loaded!")
    myProfile <- create_profile() %>%
      try(silent = TRUE)
    if (all(class(myProfile) == "try-error")) {
      cli::cli_abort("App stopped. No credentials provided!")
    }
  }
} else {
  myProfile <- create_profile() %>%
    try(silent = TRUE)
  if (all(class(myProfile) == "try-error")) {
    cli::cli_abort("App stopped. No credentials provided!")
  }
}

#--------------------Selection Options preparation/initialization---------------
# Load state selection widget options
state_helper <- state_sf %>%
  as.data.frame() %>%
  select(
    state_name = NAME, 
    state_abbr = STUSPS,
    state_fips = STATEFP
  )

states <- as.character(sort(state_helper$state_name))
states <- c("Select a state", states)

# Initialize default Date to be analyzed
EndDate_0 <- Sys.Date() %m-% days(1)

# Data mappings
field_names <- c("SubCategory_flat", "CCDDCategory_flat", "C_DiagnosisCode_ICD10_Flat",
                 "ICD_CCSR_flat")
nice_field_names <- c("Sub-syndrome", "CCDD Category", "ICD Diagnosis", "CCSR Category")
nice_to_field_list <- setNames(field_names, nice_field_names)

# convert "raw" labels to "nice" labels for render
sex_categories <- c("M", "F", "O", "U", "Z", "N")
sex_labels <- c("Male", "Female", "Unknown", "Unknown", "Unknown", "Unknown")
label_to_category_sex_list <- setNames(sex_labels, sex_categories)

age_categories <- c("00-04", "05-17", "18-44", "45-64", "65-1000", "Unknown")
age_labels <- c("0-4", "5-17", "18-44", "45-64", "65+", "Unknown")
label_to_category_age_list <- setNames(age_labels, age_categories)