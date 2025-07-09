# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

##############################################
##  PACKAGE INSTALLATION AND IMPORTS
##############################################


suppressPackageStartupMessages({
  packages <- c(
    "shiny", "cli", "geosphere", "data.table", 
    "bslib", "bsicons", "DT", "ggplot2", "leaflet", 
    "leaflet.extras", "tigris", "sf",
    "shinyjs", "shinycssloaders", "plotly", "kableExtra"
  )
})

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(
    setdiff(packages, rownames(installed.packages())),
    repos = c(getOption("repos"), "https://cloud.r-project.org", "http://cran.rstudio.com/")
  )
}

lapply(packages, library, character.only = TRUE)

if (length(setdiff("Rnssp", rownames(installed.packages()))) > 0) {
  devtools::install_github("cdcgov/Rnssp", upgrade = "never")
}
lapply("Rnssp", library, character.only = TRUE)


#######################################
# Source the package assets
load("src/scd_package_assets/zipcodes.rda")
load("src/scd_package_assets/counties.rda")
for(f in list.files("src/scd_package_assets/", pattern=".R$", full.names = T,recursive = T)) {
  source(f)
}
rm(list=c("f"))  
#######################################

MAX_DATE_RANGE = 730


# SOURCE THE VARIOUS FILES
# Get the rstudio-based credentials functions
source("src/01_credentials.R")
  
# Get all the helpers and modules
for(grp in c("helpers", "modules")) {
  for(f in list.files(paste0("src/", grp),pattern=".R$",full.names = T,recursive = T)) {
    source(f)
  }
}
rm(list=c("f", "grp"))


########################################
##   GLOBAL CONFIGURATION AND CONSTANTS
########################################

ALLOW_SHINY_CREDENTIALS = FALSE
if(ALLOW_SHINY_CREDENTIALS) {
  CREDENTIALS = check_environ_profile("myProfile")
} else {
  CREDENTIALS = get_profile(title = "Spatio Temporal Clustering")
}

ZCTAS_FROM_TIGRIS <- FALSE
SPLINE_LIBRARY <- list(
  "Spline-0.001" = fread("ancillary_data/spline_001.csv"),
  "Spline-0.005" = fread("ancillary_data/spline_005.csv"),
  "Spline-0.01" = fread("ancillary_data/spline_01.csv"),
  "Spline-0.05" = fread("ancillary_data/spline_05.csv")
)
INTERP_FUNCTIONS = get_interpolation_functions(SPLINE_LIBRARY)

## theme variables
BOOT_PRESET = "cosmo"
THEME = bs_theme(version = 5, preset = BOOT_PRESET)
PRIMARY_COLOR = bs_get_variables(theme = THEME, varnames="primary")

# read in the zips fips intersections file
ZFI = fread("ancillary_data/zcta_fips_intersections.csv", colClasses = "character")

ALLOW_CUSTOM_URL = TRUE

## This is a way to globally set http version to 1.1
## but it is a bit like taking a sledgehammer.
#httr::set_config(config=httr::config(http_version = 2L))

