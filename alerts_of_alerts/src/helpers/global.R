#----------------------------------------------------
# Alerts of Alerts App
# Authors:
#   Joshua Kimrey
#   Michael Sheppard
#   Raghav Ramachandran
#   Howard Burkom
#   Roseric Azondekon
#----------------------------------------------------

# load libraries
suppressPackageStartupMessages({
  packages <- c(
    "shiny", "shinyjs", "dplyr", "Rnssp", "purrr",
    "data.table", "lubridate", "shinycssloaders",
    "plotly", "shinyWidgets", "sf", "shinythemes",
    "janitor", "tidyverse", "leaflet", "leaflegend",
    "spdep", "shinydashboard", "htmltools",
    "leafsync", "knitr", "kableExtra", "mgcv", "gratia"
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

# Load profile
load_profile <- rstudioapi::showQuestion(
  "Alerts of Alerts App",
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
    myProfile <<- get(load(prof_file))
  } else {
    myProfile <<- prof_file %>%
      readRDS() %>%
      try(silent = TRUE)
  }
  if (all(class(myProfile) == "try-error")) {
    cli::cli_alert_danger("No or corrupt file loaded!")
    myProfile <<- create_profile() %>%
      try(silent = TRUE)
    if (all(class(myProfile) == "try-error")) {
      cli::cli_abort("App stopped. No credentials provided!")
    }
  }
} else {
  myProfile <<- create_profile() %>%
    try(silent = TRUE)
  if (all(class(myProfile) == "try-error")) {
    cli::cli_abort("App stopped. No credentials provided!")
  }
}

# Read in States
state_helper <<- state_sf %>%
  as.data.frame() %>%
  select(
    state_name = NAME, 
    state_abbr = STUSPS,
    state_fips = STATEFP
  )

states <- as.character(sort(state_helper$state_name))
states <<- c("Select a state", states, "All")

# # Read in CCDD Categories
ccdd_cats <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/ccddCategory" %>%
  get_api_data(profile = myProfile) %>%
  pluck("values") %>%
  pull("value") %>%
  try(silent = TRUE)

if (any(class(ccdd_cats) == "try-error")) {
  cli::cli_abort("App failed to establish connection with ESSENCE server!
                  Check your credentials and try again")
}

ccdd_cats <<- c("Select a CCDD Category", ccdd_cats)
