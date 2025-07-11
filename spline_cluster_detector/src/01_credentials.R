# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

### FUNCTIONS TO GET RSTUDIO-BASED CREDENTIALING AT APP LAUNCH

DEFAULT_FILE_FILTERS <- matrix(
  c("R images (*.rda)", "Binary R files (*.rds)",
    "*.rda", "*.rds"), nrow = 2
)

check_environ_profile <- function(environ_object_name) {
  if (
    exists(environ_object_name) &&
      "NSSPCredentials" %in% class(get(environ_object_name)) &&
      check_profile(get(environ_object_name))
  ) return(list(profile = get(environ_object_name), valid = TRUE))
  else list(profile = NULL, valid = FALSE)
}

get_profile <- function(
    title = "App Credentials",
    message = "NSSP-ESSENCE Credentials are required to use this app",
    check_environ = TRUE,
    environ_object_name = "myProfile"
    ) {

  if (check_environ) {
    p <- check_environ_profile(environ_object_name)
    if (p$valid) return(p)
  }

  provide_credentials <- rstudioapi::showQuestion(
    title = title,
    message = "App Mode (API Usage vs Local File Only):",
    "Utilize API to Input Data", "Use local data files only"
  )

  if (!provide_credentials) return(list(
    profile = NULL, valid = FALSE
  ))

  load_profile <- rstudioapi::showQuestion(
    title = title,
    message = message,
    ok = "Load a profile file",
    cancel = "Supply user name and password"
  )

  # If load profile selected
  if (load_profile == TRUE) profile <- get_profile_from_file()
  else profile <- Rnssp::create_profile()

  # Check profile
  if (!"NSSPCredentials" %in% class(profile) || !check_profile(profile)) {
    cli::cli_abort(
      "Credentials not valid; Check credentials file,
      user name and password, or connection. Aborting."
    )
  }

  list(profile = profile, valid = TRUE)

}

get_profile_from_file <- function(filters = DEFAULT_FILE_FILTERS) {
  pf <- get_profile_file_path(filters = filters)
  pf <- read_profile_file(pf)
}

get_profile_file_path <- function(filters = DEFAULT_FILE_FILTERS) {
  if (interactive()) {
    w <- .Platform$OS.type == "windows"
    if (w) choose.files(filters = filters, multi = FALSE)
    else file.choose()
  } else readline("Enter full path to the profile file: ")
}

read_profile_file <- function(path) {
  ew <- "Path to credentials not found, or invalid file"
  tryCatch(
    {
      if (tools::file_ext(path) == "rds") readRDS(path)
      else get(load(path))
    },
    error = \(e) cli::cli_abort(ew),
    warning = \(w) cli::cli_alert_warning(ew)
  )
}

check_profile <- function(
    profile,
    url = "https://essence.syndromicsurveillance.org/nssp_essence/"
    ) {
  # Simply checks if a profile returns a 200 response. Note that the profile
  # could technically still be "valid", but the server could be down, for example, and
  # this would return FALSE
  tryCatch(
    {
      # Note that the Rnssp package has an internal error that needS fixing.
      # Specifically, One cannot call The below function unless the Rnssp package
      # and/or one or more of its dependencies have been loaded. The current
      # unsatisfactory Workaround is to load magrittr.
      library(magrittr)
      resp <- Rnssp::get_api_response(url = url, profile = profile)
      return(resp$status == 200)
    },
    error = function(e) {
      print(e)
      return(FALSE)
    },
    warning = function(w) {
      print(w)
      return(FALSE)
    }
  )

}

