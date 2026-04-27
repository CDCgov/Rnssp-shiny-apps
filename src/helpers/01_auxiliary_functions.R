# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# Null-coalescing helper used throughout the app to provide a default when a
# value is missing or empty.
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

counties_by_state <- function(states) {
  county_to_fips<-data.table::data.table(
    readRDS("data/Region_to_fips_mapping_dup_fips.rds")
  )
  county_to_fips$countyfips<-str_pad(as.character(county_to_fips$countyfips), width = 5, pad = "0", side = "left")
  pattern <- paste0("^(", paste(states, collapse = "|"), ")_")
  df <- county_to_fips |> filter(str_detect(Region, pattern))  |>
    arrange(Region)
  return(df$Region)
}

counties_from_fips <- function(fips) {
  county_to_fips<-data.table::data.table(
    readRDS("data/Region_to_fips_mapping_dup_fips.rds")
  )
  county_to_fips$countyfips<-str_pad(as.character(county_to_fips$countyfips), width = 5, pad = "0", side = "left")
  county_to_fips[CJ(countyfips = fips), on="countyfips", Region]
}

add_fips<-function(data){
  county_to_fips<-data.table::data.table(
    readRDS("data/Region_to_fips_mapping_dup_fips.rds")
  )
  county_to_fips[, countyfips:=stringr::str_pad(countyfips, width = 5, pad = "0", side = "left")]
  setnames(county_to_fips, old="Region", new="region")
  data <- merge(data, county_to_fips, by="region")
  
  # edge case: fips may be duplicated; 17031 is both IL_Chicago and IL_Cook
  # make all such Cook; sum over numerics; maintain col order
  data[countyfips == "17031", region:="IL_Cook"]
  data <- data[, lapply(.SD, sum),.(region, date, countyfips)][, .SD, .SDcols = names(data)]
  
  return(data)
}


 
# function takes data frame and string date column name
# and returns frame with that column replaced with the
# the date. Speed up, (relative to using sapply on
# the above function) grows with bigger datasets.
wk_to_date <- function(df, date_col) {
  d = df[[date_col]] |> unique()
  ndf = names(df)
  dlu = data.table(
    sapply(str_split(d,"-",), \(f) MMWRweek2Date(as.numeric(f[1]), as.numeric(f[2]),1)) |> as.IDate(),
    d
  ) |> setnames(new=c("_x", date_col))
  df = df[dlu, on=c(date_col)]
  
  # drop the original data column
  df[, x:=NULL, env=list(x=date_col)]
  
  # rename _x to datecol
  setnames(df, old="_x", new=date_col)
  
  # select cols in original order
  df[, .SD, .SDcols = ndf]
}

## LOAD ADJACENCY MATRICES
load_adj_matrix <- function(path) {
  if(tools::file_ext(path) == "csv") {
    return(read_adj_matrix_from_csv(path))
  }
  else if(tools::file_ext(path) == "rds") {
    return(readRDS(path))
  }
  else {
    cli::cli_abort("Only `rds` or `csv` allowed")
  }
}

read_adj_matrix_from_csv <- function(path) {
  am = data.table::fread(path, drop = 1, header=TRUE)
  as.matrix(am, rownames=names(am))
}

# read_mobility_adj_mat <- function(path = "data/mobility_adj_mat.csv") {
#   am = data.table::fread(path, drop = 1, header=TRUE)
#   as.matrix(am, rownames=names(am))
# }
# read_physical_adj_mat <- function(path = "data/us_county_adjacency.csv") {
#   am = data.table::fread(path, drop = 1, header=TRUE)
#   as.matrix(am, rownames=names(am))
# }


############################################
## ESSENCE QUERY TOOLS
############################################


get_county_codes <- function(){
  file_path<-"data/fips.json"
  # Load required package
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required. Please install it with install.packages('jsonlite').")
  }
  
  # Read and parse the JSON file
  json_data <- jsonlite::fromJSON(file_path)
  
  # Extract the "values" list and convert to data frame
  df <- as.data.frame(json_data$values, stringsAsFactors = FALSE)
  
  return(df)
}


make_table_builder_url<-function(
    start_date,end_date,time_resolution,geo_resolution,state_filter=NULL,county_filter=NULL,med_group_sys, categ_info=NULL){
  
  base_url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/tableBuilder/csv?"
  start_date<-format(as.Date(start_date), "%d%b%Y")
  end_date<-format(as.Date(end_date), "%d%b%Y")
  url<-paste0(base_url,"endDate=",end_date,"&startDate=",start_date)
  
  url<-paste0(url,"&aqtTarget=TableBuilder")
  url<-paste0(url,"&datasource=va_er")
  url<-paste0(url,"&detector=nodetectordetector")
  
  if (time_resolution=="daily"){
    url<-paste0(url,"&timeResolution=daily")
  } else if (time_resolution == "weekly"){
    url<-paste0(url,"&timeResolution=weekly")
  } else if (time_resolution == "monthly"){
    url<-paste0(url,"&timeResolution=monthly")
  } else if (time_resolution == "yearly"){
    url<-paste0(url,"&timeResolution=yearly")
  } else {
    stop(paste0("invalid time resoution:",time_resolution))
  }
  url<-paste0(url,"&rowFields=timeResolution")
  
  if (geo_resolution =="county"){
    url<-paste0(url,"&geographySystem=region")
    url<-paste0(url,"&columnField=geographyregion")
    
    if (!is.null(state_filter)) {
      if(is.null(county_filter)) {
        county_filter <- counties_by_state(state_filter)
      } else {
        county_filter <- counties_from_fips(county_filter)
      }
      
      url <- paste0(url,paste0("&geography=", gsub(" ","%20",tolower(county_filter)), collapse = ""))
    } 
  } else if (geo_resolution =="state"){
    url<-paste0(url,"&geographySystem=state")
    url<-paste0(url,"&columnField=geographystate")
    if (!is.null(state_filter)){
      url <- paste0(url,paste0("&geography=", tolower(state_filter), collapse = ""))
    }
  } else {
    stop(paste0("invalid geo resoution:",geo_resolution))
  }
  
  url<-paste0(url,"&medicalGroupingSystem=", med_group_sys)
  
  if (!is.null(categ_info)){

    url<-paste0(url,"&", categ_info[["cat_class"]], "=",categ_info[["cat_value"]])
  }
  return(url)
}

reshape_and_join <- function(df_single, df_all) {
  df_single_long <- df_single |>
    pivot_longer(-timeResolution, names_to = "region", values_to = "target")
  
  df_all_long <- df_all |>
    pivot_longer(-timeResolution, names_to = "region", values_to = "overall")
  
  df_joined <- df_single_long |>
    inner_join(df_all_long, by = c("timeResolution", "region")) |>
    rename("date"="timeResolution")
  
  return(df_joined)
}

reshape_and_join_dt <- function(df_single, df_all) {
  merge(
    melt(df_single, id="timeResolution", value.name="target", variable.name="region"),
    melt(df_all, id="timeResolution", value.name="overall", variable.name="region"),
    by=c("timeResolution", "region")
  ) |> setnames(new= c("date", "region", "target", "overall"))
}

get_data<-function(sd,ed,time_res,geo_res,state_filter=NULL,county_filter, med_group_sys, categ_info, profile){
  url_all <- make_table_builder_url(
    start_date=sd,
    end_date=ed,
    time_resolution=time_res,
    geo_resolution=geo_res,
    state_filter=state_filter,
    county_filter=county_filter,
    med_group_sys = med_group_sys
  )
  url_single <- make_table_builder_url(
    start_date=sd,
    end_date=ed,
    time_resolution=time_res,
    geo_resolution=geo_res,
    state_filter=state_filter,
    county_filter=county_filter,
    med_group_sys = med_group_sys,
    categ_info = categ_info
  )
  # Data Pull from ESSENCE
  data_all <- Rnssp::get_api_data(url_all, fromCSV = TRUE, profile=profile)
  data_single <- Rnssp::get_api_data(url_single, fromCSV = TRUE, profile=profile)
  
  
  setDT(data_all)
  setDT(data_single)
  merged <- reshape_and_join_dt(data_single, data_all)
  
  #if this is county level, add fips
  if(geo_res == "county") merged <- add_fips(merged)
  
  return(list(data = merged, url_all = url_all, url_single = url_single))
}
# Map internal column names to user-facing display labels, with special
# handling for plain numeric quantile column names.
map_table_names_to_display <- function(names, title_case = FALSE, quantile_suffix = NULL, keep_names = FALSE) {
  orig_names <- names
  map = list(
    "Date" = c("date"),
    "Region" = c("region"),
    "County FIPS" = c("countyfips"),
    "ED Visits (Target)" = c("target", "cases"),
    "ED Visits (Overall)" = c("overall"),
    "ED Visits (Expected)" = c("expected"), 
    "Denominator Source" = c("denominator_source"),
    "Predicted Quantile 0.025" = c("predicted_lower"),
    "Predicted Quantile 0.5" = c("predicted_median"),
    "Predicted Quantile 0.975" = c("predicted_upper")
  )
  # convert map to datatable for fast lookup via join
  map = rbindlist(lapply(map, data.table), id="display_name")
  # join, but retain order of initial names, so that we can return in that order
  result = map[data.table(V1=names)[, i:=.I], on="V1"]
  # return display name only
  result = result[is.na(display_name), display_name:=V1][order(i), display_name]
  
  # any names that have underscores should be converte
  result = sapply(result, \(r) gsub("_", " ", r) |> tools::toTitleCase(),USE.NAMES = FALSE)
  
  # Map plain quantile columns to "Posterior quantile q=x (Count)" or "Posterior quantile q=x (Count)"
  if (!is.null(quantile_suffix)) {
    is_quantile_num <- grepl("^(0(\\.\\d+)?|1)$", orig_names)
    result[is_quantile_num] <- sprintf(
      "Posterior Quantile q=%s (%s)",
      orig_names[is_quantile_num],
      quantile_suffix
    )
  }
  
  # convert to title case if requested (this is mainly useful for other columns)
  if (title_case) result <- tools::toTitleCase(result)
  if (keep_names) {
    names(result) <- orig_names
  } else {
    result <- unname(result)
  }
  result
}


# Given a data frame, identify which columns are those that should be 
# can be rounded, and which can be left as is because are integer. This
# is useful for feeding to formatRound() in DT. Will return column indices;
# set names to TRUE to get names instead of indices. Note that indices are 
# useful, because the colnames() parameter might have been used on the DT before
# formatRound() call, and so in this case it is better to use indices. 
non_integer_cols_to_round <- function(d, names=FALSE) {
  
  is_integer_vector <- function(x) {
    if (!is.numeric(x) || inherits(x, c("Date", "POSIXct", "POSIXlt", "POSIXt"))) {
      return(FALSE)
    }
    
    x <- x[!is.na(x)]
    if (!length(x)) return(TRUE)
    
    if (!all(is.finite(x))) return(FALSE)
    
    all(abs(x - round(x)) < sqrt(.Machine$double.eps)) &&
      all(abs(x) <= .Machine$integer.max)
  }

  cols_to_convert <- which(vapply(
    d,
    function(col) is.numeric(col) && !is_integer_vector(col),
    logical(1)
  ))
  if(names) cols_to_convert <- names(d)[cols_to_convert]
  
  cols_to_convert
}


# Build a consistently styled reactable with the app's shared filter widgets
# and display-name mapping.
build_standard_reactable <- function(
    df,
    table_id,
    digits = 2L,
    quantile_suffix = NULL,
    searchable = TRUE,
    filterable = TRUE,
    page_size = NULL,
    page_size_options = c(5, 10, 15, 25, 50, 100)
) {
  req(!missing(df), !missing(table_id))
  
  display_names <- map_table_names_to_display(
    names(df),
    quantile_suffix = quantile_suffix,
    keep_names = TRUE
  )
  if (is.null(names(display_names))) names(display_names) <- names(df)
  
  cols_to_round <- non_integer_cols_to_round(df, names = TRUE)
  digits <- max(0L, min(10L, as.integer(digits %||% 2L)))
  date_cols <- names(df)[vapply(df, inherits, logical(1), "Date")]
  
  col_defs <- lapply(names(df), function(col) {
    label <- display_names[[col]] %||% col
    is_num <- is.numeric(df[[col]])
    is_date <- col %in% date_cols
    is_rounded <- col %in% cols_to_round
    
    if (is_num) {
      reactable::colDef(
        name = label,
        align = "right",
        filterable = filterable,
        filterMethod = numeric_range_filter_method,
        filterInput = function(values, name) numeric_range_filter_input(values, name, table_id),
        format = if (is_rounded) reactable::colFormat(digits = digits) else NULL
      )
    } else if (is_date) {
      reactable::colDef(
        name = label,
        filterable = filterable,
        filterMethod = date_filter_method,
        filterInput = function(values, name) date_filter_input(values, name, table_id)
      )
    } else {
      reactable::colDef(
        name = label,
        filterable = filterable,
        filterMethod = checkbox_filter_method,
        filterInput = function(values, name) checkbox_filter_input(values, name, table_id)
      )
    }
  })
  names(col_defs) <- names(df)
  
  if (is.null(page_size)) {
    page_size <- if (nrow(df) > 0) min(nrow(df), 10L) else 10L
  }
  
  reactable::reactable(
    df,
    columns = col_defs,
    defaultPageSize = page_size,
    pageSizeOptions = page_size_options,
    searchable = searchable,
    filterable = filterable,
    highlight = TRUE,
    striped = TRUE,
    bordered = TRUE,
    resizable = TRUE,
    wrap = TRUE,
    defaultColDef = reactable::colDef(
      minWidth = 120,
      headerStyle = list(
        whiteSpace = "normal",
        wordBreak = "break-word",
        lineHeight = "1.2",
        paddingTop = "0.6rem",
        paddingBottom = "0.7rem",
        overflow = "visible"
      ),
      style = list(
        whiteSpace = "nowrap"
      )
    ),
    showPageSizeOptions = TRUE,
    fullWidth = TRUE,
    theme = BS_REACTABLE_THEME
  )
}


# Unpack a saved model archive into its stored model object and serialized UI
# values.
load_saved_model_file <- function(path) {

  # unpack the archive
  archive <- load_saved_object_from_file(path)
  model_object <- archive[["rds"]]
  
  if (!is.null(model_object$data_class)) {
    model_object$data_class <- normalize_loaded_data_class(model_object$data_class)
  }
  
  # return a list of objects (model object, model values)
  return(list(
    "model_object" = model_object,
    "model_values" = archive[["json"]]
  ))
  
}

# Unpack a saved query archive into its stored source data and serialized UI
# values.
load_saved_query_file <- function(path) {
  
  # unpack the archive
  archive <- load_saved_object_from_file(path)
  
  # return a list of objects (data, query_values)
  return(list(
    "data" = archive[["rds"]],
    "query_values" = archive[["json"]]
  ))
  
}

# Load one of the app's saved zip archives and return the underlying RDS and
# JSON payloads.
load_saved_object_from_file <- function(path) {
  # create temp folder
  tmpdir <- tempfile()
  dir.create(tmpdir)
  
  # unizip the path to the tempdir
  unzip(path, exdir = tmpdir)
  
  # get files from unzipped archive
  files <- list.files(tmpdir, full.names = TRUE)
  
  # identify the rds file (model object) and json file (model values)
  rds_file  <- files[grepl("\\.rds$", files, ignore.case = TRUE)]
  json_file <- files[grepl("\\.json$", files, ignore.case = TRUE)]
  
  # ensure that unzipped archive contains the expected files
  validate(
    need(length(rds_file) > 0, "No RDS file found in zip"),
    need(length(json_file) > 0, "No JSON file found in zip")
  )
  return(list(
    rds = readRDS(rds_file[1]),
    json=jsonlite::read_json(json_file[1], simplifyVector = TRUE)
  ))
  
}

# Normalize saved data-class objects onto the app's canonical field names so
# loaded models match the structure produced by current preprocessing.
normalize_loaded_data_class <- function(data_cls) {
  if (is.null(data_cls)) return(data_cls)
  
  if (is.null(data_cls$region_column) && !is.null(data_cls$region_col)) {
    data_cls$region_column <- data_cls$region_col
  }
  if (is.null(data_cls$date_column) && !is.null(data_cls$date_col)) {
    data_cls$date_column <- data_cls$date_col
  }
  
  data_cls$region_col <- NULL
  data_cls$date_col <- NULL
  sort_data_class_data(data_cls)
}

# Keep stored data-class rows in a stable order so downstream tables and plots
# see one consistent region/date ordering.
sort_data_class_data <- function(data_cls) {
  if (is.null(data_cls) || is.null(data_cls$data)) return(data_cls)

  dt <- data.table::as.data.table(data_cls$data)
  region_col <- data_cls$region_column %||% "countyfips"
  date_col <- data_cls$date_column %||% "date"
  sort_cols <- intersect(c(region_col, date_col), names(dt))
  if (length(sort_cols)) {
    if (region_col %in% sort_cols) dt[, (region_col) := as.character(get(region_col))]
    if (date_col %in% sort_cols && !inherits(dt[[date_col]], "Date")) {
      suppressWarnings(dt[, (date_col) := as.Date(get(date_col))])
    }
    data.table::setorderv(dt, sort_cols)
  }

  data_cls$data <- dt
  data_cls
}
  
# Function to place info circle tool tip on a input label
# l must be a two element list, first element holds the label
# 2nd element holds the tool tip message
labeltt <- function(l, ...) {
  tt <- bslib::tooltip(
    trigger = list(
      l[[1]],
      bsicons::bs_icon(name = "info-circle-fill", class = "tooltip-icon")
    ),
    p(l[[2]], style = "text-align:left;"),
    ...
  )
  
  # lets convert the result of tooltip function to character/HTML
  tt #htmltools::HTML(as.character(tt))
}

add_button_hover <- function(title,button) {
  div(title=title,button)
}

# Convert free-form text into a stable identifier fragment for feature IDs and
# other programmatic names.
slugify <- function(x) {
  x <- tolower(trimws(x %||% ""))
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "feature" else x
}
