# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# Package check and handle function. 

package_handler <- function(
    inla_min = "24.06.27",
    epistemic_min = "1.6.0",
    auto_install=FALSE, 
    test_inla = TRUE,
    local_epistemic=FALSE
) {
  
  # 0. some global options
  options(timeout = 600) # - extend the timeout
  .libPaths(c(.Library, .libPaths())) # prioritize the global library
  
  # 1. get accessible packages
  accessible_pkgs <- accessible_packages()

  # 2. check inla
  check_inla(inla_min = inla_min,ac = accessible_pkgs)

  # 3. test inla
  if(test_inla) test_inla()
  
  # 4. check Rnssp
  check_rnssp(ac = accessible_pkgs)

  # 5. check other non-automatically installed packages
  check_other_uninstallable(ac = accessible_pkgs)

  custom_repo <- "http://cran.rstudio.com/"
  
  # 6. check installable packages
  check_installable(
    ac = accessible_pkgs, 
    auto_install = auto_install, 
    repos = custom_repo
  )

  # 7. check episemtic
  check_epistemic(
    epistemic_min = epistemic_min,
    ac = accessible_pkgs, 
    auto_install=auto_install, 
    repos=custom_repo, 
    local = local_epistemic
  )
  
  # If we have made it this far, all dependencies are available
  cat("all required packages and version(s) found.")
  
  invisible()
}

accessible_packages <- function() {
  # check all the names of installed packages accessible in .libPaths()
  lapply(
    .libPaths(), \(lp) installed.packages(lp) |> row.names()
  ) |> 
    unlist() |> 
    unique()
}
 
# function checks if a minimum version of inla is available
check_inla <- function(inla_min, ac = accessible_packages()) {
  
  # set the current version with a NULL value
  INLA_version = "0.0.0"
  
  # Update the current version to the one that is installed, if any
  if("INLA" %in% ac) INLA_version <- utils::packageVersion("INLA") 
  
  # if the current version is less than the minimum fail gracefully
  if(INLA_version < inla_min) {
    stop(paste0(
      "This app cannot be run until you install INLA version ",
      inla_min,
      " or greater.\n",
      "See https://r-inla.org/download/index.html for more information"
    ))
  }
  
  invisible()
}

test_inla <- function() {

  # Can a test INLA model be fit on this machine?
  
  failure_msg <- "a test run of INLA failed; app cannot be run"
  tryCatch(
    {
      set.seed(8783)
      x <- rnorm(50); y <- 1 + 2*x + rnorm(50)
      fit <- INLA::inla(y ~ x, data = data.frame(x = x, y = y))
      if(!fit$ok) stop(failure_msg)
    },
    error =  function(e) stop(failure_msg)
  )
  invisible()
}

check_rnssp <- function(ac = accessible_packages()) {
  if(!"Rnssp" %in% ac) {
    msg <- paste(
      "Note: Rnssp is not installed but required.\n",
      "It is not available on CRAN; to install see", 
      "https://cdcgov.github.io/Rnssp/"
    )   
    stop(msg, call. = FALSE)
  }
  invisible()
}

check_other_uninstallable <- function(ac = accessible_packages()) {
  
  # these packages cannot be installed automatically; they are too heavy
  # with numerous dependencies
  
  u_pkgs <- c(
    "plotly", "igraph","geojsonsf", "sf", "leaflet"
  )
  # update to those that aren't available:
  u_pkgs <- u_pkgs[!u_pkgs %in% ac]
  
  # if this contains any, we fail gracefully:
  if(length(u_pkgs)>0) {
    msg = paste0(
      "Install the following missing packages and try again:\n",
      paste0(u_pkgs, collapse = ",")
    )
    stop(msg, call.=FALSE)
  }
  invisible()
}

check_installable <- function(
    ac = accessible_packages(),
    auto_install=FALSE,
    repos = getOption("repos")
) {
  
  # Okay, this is the remainder of the dependencies
  ir_pkgs <- c(
    "shiny", "shinyjs", "cli", "data.table", "bslib",
    "bsicons", "lubridate","MMWRweek", "shinycssloaders", "gridExtra",
    "rlang", "reactable", "viridisLite", "rstudioapi", "magrittr", 
    "readr","ggplot2", "dplyr", "tidyr", "stringr", "htmltools", "jsonlite",
    "xml2", "zip", "purrr", "htmlwidgets", "RColorBrewer", 
    "leaflet.extras", "leafpop", "remotes", "pracma", "glue"
  )
  
  missing_required <- ir_pkgs[!ir_pkgs %in% ac]

  if(length(missing_required)>0) {
    if(auto_install) {
      cat(
        "The following required app dependencies are missing, and will be installed:\n",
        paste0(missing_required,collapse = ","), 
        "\n"
      )
      # Now install these!
      for(mr in missing_required) install.packages(
        mr, 
        repos=repos,
        lib = Sys.getenv("R_LIBS_USER")
      )
    } else {
      # auto-install declined; just report to user
      stop(
        "Install the following missing packages and try again:\n",
        paste0(missing_required, collapse = ",")
      )
    }
  }
  invisible()
}

check_epistemic <- function(
  epistemic_min, 
  ac=accessible_packages(),
  auto_install = FALSE,
  repos = getOption("repos"), 
  local=FALSE
) {
  
  # set a null current version
  epistemic_version = "0.0.0"
  
  # check if epistemic is already installed
  epistemic_installed <- "epistemic" %in% ac

  # update the current version if epistemic is installed
  if(epistemic_installed) epistemic_version <- utils::packageVersion("epistemic")
  
  if(epistemic_version < epistemic_min) {
    if(!auto_install) {
      stop(paste(
        "Either auto-install, or manually install epistemic version",
        epistemic_min,
        "or higher from github using pak, devtools, or",
        "remotes (e.g. remotes::install_github(\"mpanaggio/epistemic\")"
      ))
    }
    # Then we install with pak; however this might also not be available
    if(!"pak" %in% ac) {
      stop(paste(
        "epistemic (version: ", epistemic_min, "or higher) is not installed",
        "and must be installed from github, but pak package is not available.",
        " Install pak, devtools or remotes, and",
        "manually install using remotes::install_github(\"mpanaggio/epistemic\") or similar."
      ))
    } else {
      if(local==TRUE) {
        cat("Installing 'epistemic' package from local tarball")
        install.packages("src/epistemic_1_6_0.tar.gz", method="source")
      } else {
        cat("Installing 'epistemic' package from github repo")
        remotes::install_github(
          repo = "mpanaggio/epistemic",
          repos = repos,
          lib = Sys.getenv("R_LIBS_USER")
        )
      }
    }
  }
  invisible()

}