# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

#################################################################
# Run the package handler
source("src/04_package_handler.R")
INLA_MIN = "24.06.27" # Set the minimum INLA version
EPISTEMIC_MIN = "1.6.0" # Set the minimum epistemic version
AUTO_INSTALL = TRUE # Should missing (installable) packages be auto-installed?
TEST_INLA = TRUE # Should we test inla?

# Call the handler; any failures will abort app launch
package_handler(
  inla_min = INLA_MIN,
  epistemic_min = EPISTEMIC_MIN,
  auto_install = AUTO_INSTALL, 
  test_inla = TEST_INLA
)
#################################################################


# Load the libraries
library(shiny)
library(shinyjs)
library(cli)
library(data.table)
library(bslib)
library(bsicons)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(MMWRweek)
library(shinycssloaders)
library(INLA)
library(gridExtra)
library(rlang)
library(plotly)
library(geojsonsf)
library(igraph)
library(leaflet)
library(leaflet.extras)
library(leafpop)
library(reactable)
library(viridisLite)
library(sf)
library(epistemic)


# Profile
source("src/01_credentials.R")
ALLOW_SHINY_CREDENTIALS <- TRUE
# regardless of shiny credentials allowable or not, if this is running in rstudio
# set CREDENTIALS via the rstudio get_profile() function
if(rstudioapi::isAvailable() == TRUE) {
  CREDENTIALS = get_profile(title = "Bayesian Spatiotemporal Modeling")
} else {
  if(ALLOW_SHINY_CREDENTIALS) {
    CREDENTIALS <- check_environ_profile("myProfile")
  } else {
      cli::cli_abort("Is this app being run outside of RStudio? If so, the app must be configured to ALLOW SHINY CREDENTIALS")
  }
}



# Other key scripts, custom filters, and global UI tags

source("src/02_custom_filters.R")
source("src/03_global_ui_tags.R")


# Helpers and modules

# Source all helpers before modules so shared utilities are available
# everywhere without requiring per-file imports.
for(grp in c("helpers", "modules")) {
  for(f in list.files(paste0("src/", grp),pattern=".R$",full.names = T,recursive = T)) {
    source(f)
  }
}
rm(list=c("f", "grp"))


# Constants
BOOT_PRESET <- "pulse"
THEME <-  bs_theme(version = 5, preset = BOOT_PRESET,
                   "btn-padding-y" = ".25rem",
                   "btn-padding-x" = ".5rem",
                   "btn-font-size" = ".875rem") 
# Fix warning formatting for improved contrast in dark mode
THEME <- THEME |>
  bs_add_rules("
    /* =========================
       Shiny validation messages
       ========================= */

    .shiny-output-error-validation {
      font-weight: 600;
      padding: 0.5rem 0.75rem;
      border-radius: 0.375rem;
    }

    /* light mode */
    :root .shiny-output-error-validation {
      color: var(--bs-warning-text-emphasis);
      background-color: var(--bs-warning-bg-subtle);
      border: 1px solid var(--bs-warning-border-subtle);
    }

    /* dark mode */
    [data-bs-theme='dark'] .shiny-output-error-validation {
      color: var(--bs-warning-text-emphasis);
      background-color: var(--bs-warning-bg-subtle);
      border: 1px solid var(--bs-warning-border-subtle);
    }
  ")

# fix tooltip coloring for improved contrast in dark mode
THEME <- THEME |>
  bs_add_rules("
    /* Tooltip icon: neutral/accessible color and ensure SVG follows currentColor */
    .tooltip-icon {
      color: var(--bs-primary); /* default light-mode color; change if you prefer */
      opacity: 0.95;
      vertical-align: middle;
    }

    .tooltip-icon svg,
    .tooltip-icon use {
      fill: currentColor;
      stroke: none;
    }

    /* Dark-mode override: make tooltip icons high-contrast on dark backgrounds */
    [data-bs-theme='dark'] .tooltip-icon {
      color: var(--bs-body-color);  /* use body text color for best contrast */
    }

    /* Optional: if you want icons inside active tooltip triggers to use link color */
    [data-bs-theme='dark'] .tooltip-trigger .tooltip-icon {
      color: var(--bs-link-color);
    }
  ")


# Fix nav/tab formatting and coloring
THEME <- THEME |>
  bs_add_rules("
/* =========================
   NAV / TABS — ACCESSIBLE, CONTEXT AWARE
   Replaces prior tab/top-nav rules. Drop-in replacement.
   ========================= */

/* -------------------------
   Base tab appearance
   ------------------------- */
.nav .nav-link {
  color: var(--bs-body-color);
  font-weight: 500;
  padding: 0.5rem 0.75rem;
  border-radius: 0.375rem;
  margin-right: 0.25rem;
  transition: background-color 0.12s ease, color 0.12s ease;
}

/* -------------------------
   Active tab
   Light mode: white text on primary (fixes Pulse purple)
   Dark mode: dark text on bright primary (keeps earlier dark-mode behavior)
   ------------------------- */
:root .nav .nav-link.active,
:root .nav-pills .nav-link.active,
:root .nav-tabs .nav-link.active {
  background-color: var(--bs-primary);
  color: #FFFFFF;         /* white for dark primaries in light mode */
  font-weight: 600;
  border: none;
  box-shadow: 0 1px 0 rgba(0,0,0,0.06) inset;
}

/* dark mode active: primary may be bright -> use dark text for best contrast */
[data-bs-theme='dark'] .nav .nav-link.active,
[data-bs-theme='dark'] .nav-pills .nav-link.active,
[data-bs-theme='dark'] .nav-tabs .nav-link.active {
  background-color: var(--bs-primary);
  color: #0A0A0A;         /* dark text on bright primary */
  font-weight: 600;
  border: none;
}

/* Ensure icons inside active tabs inherit that color */
.nav .nav-link.active svg,
.nav .nav-link.active .bi,
.nav .nav-link.active i {
  color: inherit;
  fill: currentColor;
}

/* -------------------------
   Unselected tabs — clear, readable
   Light mode: black text (visible on light backgrounds)
   Dark mode: body color (light) but slightly muted
   ------------------------- */

/* strong general selector to beat other theme rules */
.nav.nav-tabs .nav-item > .nav-link:not(.active),
.nav.nav-pills .nav-item > .nav-link:not(.active),
.navbar .nav-item > .nav-link:not(.active),
.nav .nav-link:not(.active) {
  background: transparent !important;
  display: inline-block !important;
  visibility: visible !important;
  font-weight: 400 !important;
}

/* Light mode: explicit black for unselected tabs */
:root .nav.nav-tabs .nav-item > .nav-link:not(.active),
:root .nav.nav-pills .nav-item > .nav-link:not(.active),
:root .navbar .nav-item > .nav-link:not(.active),
:root .nav .nav-link:not(.active) {
  color: #000000 !important;
  opacity: 1 !important;
}

/* Dark mode: use body color (light) but slightly muted visually */
[data-bs-theme='dark'] .nav.nav-tabs .nav-item > .nav-link:not(.active),
[data-bs-theme='dark'] .nav.nav-pills .nav-item > .nav-link:not(.active),
[data-bs-theme='dark'] .navbar .nav-item > .nav-link:not(.active),
[data-bs-theme='dark'] .nav .nav-link:not(.active) {
  color: var(--bs-body-color) !important;
  opacity: 0.88 !important;
}

/* Ensure icons/SVGs inside unselected tabs inherit text color */
.nav .nav-link:not(.active) svg,
.nav .nav-link:not(.active) .bi,
.nav .nav-link:not(.active) i {
  color: inherit !important;
  fill: currentColor !important;
}

/* If elements have .text-muted, force them to inherit so they do not disappear */
.nav .nav-link:not(.active).text-muted,
.nav .nav-link:not(.active) .text-muted {
  color: inherit !important;
  opacity: inherit !important;
}

/* -------------------------
   Disabled / deactivated tabs: visually and functionally distinct
   ------------------------- */
.nav .nav-link.disabled,
.nav .nav-link:disabled,
.nav .nav-item > .nav-link.disabled {
  color: rgba(0,0,0,0.45) !important; /* light-mode fallback */
  opacity: 0.55 !important;
  cursor: not-allowed !important;
  pointer-events: none !important;
}

/* dark-mode disabled override (muted white) */
[data-bs-theme='dark'] .nav .nav-link.disabled,
[data-bs-theme='dark'] .nav .nav-link:disabled,
[data-bs-theme='dark'] .nav .nav-item > .nav-link.disabled {
  color: rgba(255,255,255,0.45) !important;
  opacity: 0.55 !important;
}

/* make sure icons inside disabled tabs look muted too */
.nav .nav-link.disabled svg,
.nav .nav-link.disabled .bi,
.nav .nav-link:disabled svg,
.nav .nav-link:disabled .bi {
  color: inherit !important;
  fill: currentColor !important;
}

/* -------------------------
   TOP NAVSET: always dark background (class = top-dark-nav)
   - background is fixed regardless of data-bs-theme
   - unselected tabs shown as light (unless disabled)
   ------------------------- */

/* top nav container background should be fixed and not flip with theme toggle */
.top-dark-nav {
  background-color: #121212 !important;
  color: #FFFFFF !important;
}

/* keep nav children on top of the dark header */
.top-dark-nav .nav,
.top-dark-nav .nav-tabs,
.top-dark-nav .nav-pills {
  background-color: transparent !important;
}

/* top nav unselected: visible (behave like dark-mode unselected) */
.top-dark-nav .nav-link:not(.active):not(.disabled),
.top-dark-nav .nav-item > .nav-link:not(.active):not(.disabled) {
  color: #FFFFFF !important;
  opacity: 0.92 !important;
  font-weight: 400 !important;
  pointer-events: auto !important;
}

/* top nav active: prominent */
.top-dark-nav .nav-link.active,
.top-dark-nav .nav-item > .nav-link.active {
  background-color: var(--bs-primary) !important;
  color: #FFFFFF !important;
  font-weight: 600 !important;
}

/* top nav disabled: muted and non-interactive */
.top-dark-nav .nav-link.disabled,
.top-dark-nav .nav-link:disabled {
  color: rgba(255,255,255,0.45) !important;
  opacity: 0.55 !important;
  pointer-events: none !important;
  cursor: not-allowed !important;
}

/* icons inside top nav tabs follow color */
.top-dark-nav .nav-link svg,
.top-dark-nav .nav-link .bi {
  color: inherit !important;
  fill: currentColor !important;
}

/* small hover cue for top nav (non-disabled) */
.top-dark-nav .nav-link:not(.active):not(.disabled):hover {
  text-decoration: underline;
  opacity: 1 !important;
}

/* -------------------------
   Keyboard focus visibility (accessible)
   ------------------------- */
.nav .nav-link:focus-visible {
  outline: 3px solid rgba(13,110,253,0.35);
  outline-offset: 2px;
}

/* =========================
   CARD HEADER — ACCENT (NO LINE)
   (keeps the earlier card-header-accent API)
   ========================= */
.card-header-accent {
  background-color: color-mix(in srgb, var(--bs-primary) 8%, transparent);
  color: var(--bs-body-color);
  font-weight: 600;
  padding: 0.75rem 1rem;
  background-clip: padding-box;
}

[data-bs-theme='dark'] .card-header-accent {
  background-color: color-mix(in srgb, var(--bs-primary) 14%, transparent);
}

/* FORCE: active tab text always white (applies globally and to top-dark-nav) */
.nav.nav-tabs .nav-item > .nav-link.active,
.nav.nav-pills .nav-item > .nav-link.active,
.nav-tabs .nav-link.active,
.nav-pills .nav-link.active,
.nav .nav-link.active,
.navbar .nav-link.active,
.top-dark-nav .nav-link.active,
.top-dark-nav .nav-item > .nav-link.active {
  color: #FFFFFF !important;
}

/* Ensure any SVG/icon inside the active tab inherits the white color */
.nav .nav-link.active svg,
.nav .nav-link.active .bi,
.top-dark-nav .nav-link.active svg,
.top-dark-nav .nav-link.active .bi {
  color: inherit !important;
  fill: currentColor !important;
}

")

# Modify appearance of light/dark mode toggle.
THEME <- THEME |>
  bs_add_rules("
/* =========================
   Mode switch – final tuning (lighter track + clearer unselected context)
   ========================= */

.mode-pill-wrap{
  display:inline-flex;
  align-items:center;
  gap:0.6rem;
  padding: 0.1rem 1.0rem;      /* a touch more internal padding */
  margin: 0 1.4rem;            /* a touch more space before/after */
  white-space:nowrap;
}

.mode-side{
  display:inline-flex;
  align-items:center;
  gap:0.35rem;
  pointer-events:none;
}

.mode-text{
  font-size:0.85rem;
  font-weight:600;
  line-height:1;
  position: relative;
  top: -0.02rem;
  opacity:0.9;                 /* selected side base */
}

.mode-icon{
  display:inline-flex;
  align-items:center;
  line-height:1;
  position: relative;
  top: -0.02rem;
  opacity:0.9;
}

.mode-icon .bi{
  width: 1rem;
  height: 1rem;
  vertical-align: middle;
}

/* Fixed slider track (lighter / more visible; does not change by mode) */
.mode-switch{
  position:relative;
  width: 3.2rem;
  height: 1.7rem;
  border-radius:999px;
  padding:0;
  cursor:pointer;
  display:inline-block;
  vertical-align: middle;

  /* LIGHTER track color */
  background: rgba(180,180,180,0.35);
  border: 1px solid rgba(160,160,160,0.45);
}

/* Thumb */
.mode-switch::after{
  content:'';
  position:absolute;
  top: 0.12rem;
  left: 0.12rem;
  width: 1.45rem;
  height: 1.45rem;
  border-radius:999px;
  background:#FFFFFF;
  box-shadow: 0 2px 10px rgba(0,0,0,0.25);
  transition: transform 0.18s ease;
}

/* Dark mode: move thumb right (only state change) */
html[data-bs-theme='dark'] .mode-switch::after{
  transform: translateX(1.5rem);
}

/* Context emphasis: make UNSELECTED side lighter (more distinct) */

/* DARK MODE: right selected, left unselected */
html[data-bs-theme='dark'] .mode-side.left  .mode-text,
html[data-bs-theme='dark'] .mode-side.left  .mode-icon { opacity:0.45; }

html[data-bs-theme='dark'] .mode-side.right .mode-text,
html[data-bs-theme='dark'] .mode-side.right .mode-icon { opacity:1; }

/* LIGHT MODE: left selected, right unselected */
html:not([data-bs-theme='dark']) .mode-side.left  .mode-text,
html:not([data-bs-theme='dark']) .mode-side.left  .mode-icon { opacity:1; }

html:not([data-bs-theme='dark']) .mode-side.right .mode-text,
html:not([data-bs-theme='dark']) .mode-side.right .mode-icon { opacity:0.45; }

/* Focus ring */
.mode-switch:focus-visible{
  outline: 3px solid rgba(13,110,253,0.35);
  outline-offset: 2px;
}
")

THEME <- bs_add_rules(THEME, "
  .reactable-top-controls {
    display: flex;
    align-items: flex-end;
    justify-content: space-between;
    gap: 0.75rem;
    flex-wrap: wrap;
    margin-bottom: 0.35rem;
  }

  .reactable-top-controls .form-group,
  .reactable-top-controls .mb-3 {
    margin-bottom: 0;
  }

  .reactable-top-controls .reactable-search-wrap {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    min-width: 240px;
    margin-left: auto;
  }

  .reactable-top-controls .reactable-search-wrap label {
    margin: 0;
    white-space: nowrap;
  }

  .reactable-top-controls .reactable-search-wrap .form-control {
    min-width: 240px;
  }

  .reactable-bottom-controls {
    margin-top: 0.75rem;
  }

  .rt-page-size-select,
  .rt-page-info,
  .rt-page-button {
    color: var(--bs-body-color) !important;
  }

  .rt-page-size-select {
    background-color: var(--bs-body-bg) !important;
    border-color: var(--bs-border-color) !important;
  }
")




SIDEBAR_WIDTH <- 300
DEFAULT_STATES <-  c("MD")
BUTTON_CLASS <- "btn-primary btn-sm"

# Adjacency matrix defaults
PHYS_ADJ_MATRIX <- "data/physical_adj_mat_rnssp.rds"
MOB_ADJ_MATRIX <-  "data/mobility_adj_mat.rds"

BS_REACTABLE_THEME <- reactable::reactableTheme(
  color           = "var(--bs-body-color)",
  backgroundColor = "var(--bs-body-bg)",
  borderColor = "transparent",
  stripedColor    = "var(--bs-tertiary-bg)",
  highlightColor  = "var(--bs-secondary-bg)",
  tableStyle = list(
    backgroundColor = "var(--bs-body-bg)"
  ),
  headerStyle = list(
    backgroundColor = "var(--bs-secondary-bg)",
    color           = "var(--bs-body-color)",
    borderColor = "transparent"
  ),
  rowStyle = list(
    borderColor = "transparent"
  )
)


