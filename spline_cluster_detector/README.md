# Spline-Based Cluster Detection Shiny App

### Description

An R Shiny app for visualizing the spline-based cluster classification/detection approach.
This app estimates spatio-temporal clusters of target events in a test interval
relative to the frequency of those events in a preceding baseline interval. The
method used to estimate observed and expected cases in each location, and to 
identify clusters is similar to the approach used in SaTScan™ 
(https://www.satscan.org/), but the statistical significance of the clusters is
determined using a set of classifiers that have learned the tri-variate relationship
between observed, log of the observed to expected ratio, and signficance of the
space-time permutation statistic. As estimation of the latter can be computationally
expensive, leveraging a trained classifier can provide an approximation of the statistic
in a fraction of time that would be necessary to run the full permutation-based
procedure. 
  
A table of location, dates, and counts of the event of interest is required as
input into the cluster identification approach. The app provides three method for
providing such an input:

1. upload a local file (.csv of .cas)
2. build a table builder or data details API using simple URL builder
3. input an ad-hoc url
  
Once data are uploaded, the app allows for data exploration (time series plots, 
summary statistics, heatmap), cluster identification (with details about each
cluster identified), map generation with clusters highlighted, and automatic
generation of html report for sharing results.

## Credentials

While the app will work with a local file upload option, Rnssp credentials are required to use the API version. Options include:

1.  Saving an rds file with credentials locally, and loading that file when asked
2.  Entering credentials via username and password when asked (i.e. within app when api request is made)
3.  Reading an rds file within a project-level `.Rprofile` file, to bypass the request for credentials each time the app is launched. To do this:

``` r
saveRDS(Rnssp::create_profile(), file="rnssp_creds.rds")
```

and then create the `.Rprofile` file within the root of this repo

``` r
myProfile <- readRDS("rnssp_creds.rds")
```

### Requirements

Requires the following libraries
```R
library(shiny)
library(cli)
library(geosphere)
library(data.table)
library(bslib)
library(bsicons)
library(DT)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(tigris)
library(sf)
library(shinyjs)
library(shinycssloaders)
library(Rnssp)
library(plotly)
library(kableExtra)
```

Note that the above required packages must be first installed (and updated to latest versions) in order to run this app

### Problems and Solutions noted by Users:
1. a common error regarding `tabset` construction will be raised if `bslib` package version is not updated to at least 0.9.0 (or higher)
2. not all the above packages are availalble at "https://rs-packages.biosense.wan/prod-cran/latest", so if you are installing the above pre-reqs on Biosense Posit, you may need to explicitly indicate the repo in the `install.packages()` function.  For example: try `install.packages("bsicons", repos = "https://cloud.r-project.org")` instead of `install.packages("bsicons")`


### Contacts:

-   Howard Burkom [Howard.Burkom\@jhuapl.edu](mailto:Howard.Burkom@jhuapl.edu)
-   Luke Mullany [Luke.Mullany\@jhuapl.edu](mailto:Luke.Mullany@jhuapl.edu)

### Copyright

© 2024 The Johns Hopkins University Applied Physics Laboratory LLC.<br></br> Development of this software was sponsored by the U.S. Government under contract no. 75D30124C19958
