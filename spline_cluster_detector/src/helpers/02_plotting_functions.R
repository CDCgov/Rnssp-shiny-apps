# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# Various functions to create the leaflet data
# and return plot

# Helper function to get the shape file on demand

get_shape_file_from_tigris <- function(st, level = c("zip", "county"), refresh = FALSE, use_cb=FALSE, zctas_from_tigris = TRUE) {
  level <- match.arg(level)
  
  # if zip is the level using zctas function, for this state
  if (level == "zip") {
    
    # if zctas from tigris is false, check for local file. If local
    # file does not exist, set zctas from tigris to TRUE
    if(zctas_from_tigris == FALSE) {
      local_zcta_name = paste0("state_zcta/", st, "_zcta.rds")
      if(!file.exists(local_zcta_name)) {
        zctas_from_tigris=TRUE
      }
    }
    
    if(zctas_from_tigris) {
      sh <- tigris::zctas(year = 2010, state = st, refresh = refresh)
      # add datum and fix column names
      sh <- sf::st_transform(sh, crs = "+proj=longlat +datum=WGS84")
    } else {
      sh <- readRDS(paste0("state_zcta/",st, "_zcta.rds"))
    }
    colnames(sh)[which(startsWith(colnames(sh), "ZCTA5CE"))] <- "GEOID"
  } else  {
    sh <- Rnssp::county_sf
    if(st!="US") {
      # reduce to state
      statefp <- Rnssp::state_sf[Rnssp::state_sf$STUSPS==st,"STATEFP"]$STATEFP |> as.character()
      sh <- Rnssp::county_sf[Rnssp::county_sf$STATEFP==statefp,]
    }
  }
  
  return(sh)

}

# Function to create hover labels. Pass a named list, where the names
# are the labels, and the values are the values for that label
create_custom_labels <- function(label_list) {
  fmt <- paste0(
    paste0(sapply(names(label_list), \(n) paste0("<strong>", n, "</strong>%s<br/>")), collapse = "")
  )
  lapply(do.call(sprintf, c(fmt, label_list)), htmltools::HTML)
}


# Wrapper function: pass the cluster_data, the state, and the level, and
# the leaflet data will be returned. Dispatches to either a county level
# logic or zip level logic
generate_leaflet_data <- function(
    cluster_data,
    state,
    distance_matrix,
    level = c("zip", "county"),
    zctas_from_tigris = TRUE
) {
  
  # get the level
  level <- match.arg(level)
  
  # Get the shape data
  shape_data <- get_shape_file_from_tigris(st = state, level = level,zctas_from_tigris = zctas_from_tigris)
  
  # call corresponding function
  if (level == "zip") ld <- generate_zip_leaflet_data(cluster_data, shape_data, distance_matrix)
  if (level == "county") ld <- generate_county_leaflet_data(cluster_data, shape_data, distance_matrix)
  
  # return the result
  return(ld)
}

# Here are the two functions, one for zip and one for county. They currently
# have the same signatures, which is probably important, and it is very
# important that they return the same structure to the wrapper function

generate_zip_leaflet_data <- function(cluster_data, shape_data, distance_matrix) {
  # generate the cluster centers frame, along with their constituent zip codes
  cluster_centers <- get_cluster_center_locations(
    cluster_data,
    distance_matrix
  )
  
  # merge the shape file to these cluster centers
  ld <- merge(shape_data, cluster_centers, by = "GEOID", all.x = TRUE)
  

  # merge individual location counts
  ld <- merge(
    ld,
    cluster_data$cluster_location_counts,
    by.x = c("GEOID","target"),
    by.y = c("location", "target"),
    all.x = TRUE
  )

  # replace NA
  ld[["target"]] = data.table::fifelse(is.na(ld[["target"]]), "0", ld[["target"]])
  
  for(col in c("observed", "expected", "nr_locs", "nr_days", "count")) {
    ld[[col]]=data.table::fifelse(is.na(ld[[col]]), 0, ld[[col]])
  }
  
  # add a character label for each zip code (cluster center or not)
  ld$label_center <- fifelse(ld$target == 0, "No Cluster", ld$target)

  return(ld)
}

generate_county_leaflet_data <- function(cluster_data, shape_data, distance_matrix) {
  # Add code to get the leaflet data, at county level, incorporating clusters
  # that might have been detected
  ld <- generate_zip_leaflet_data(
    cluster_data = cluster_data,
    shape_data = shape_data,
    distance_matrix = distance_matrix
  )

  return(ld)
}

# Helper function to generate the frame of cluster centers and their locations:
get_cluster_center_locations <- function(cluster_data, distance_matrix) {
  # unpack cluster_data
  cluster_alert_table <- cluster_data$cluster_alert_table
  # first convert the distance matrix to a data table
  dm <- data.table(distance_matrix, keep.rownames = "target")

  # merge the cluster_alert table
  cluster_center_locations <- 
    dm[cluster_alert_table[, .(target, distance_value, nr_days = as.integer(detect_date - date + 1))], 
       on = "target"]

  # melt it long
  cluster_center_locations <- melt(cluster_center_locations, 
                                   id.var = c("target", "distance_value", "nr_days"))

  # reduce to only those locations that are within the distance_value and select columns
  cluster_center_locations <- cluster_center_locations[
    value <= distance_value,
    .(target, GEOID = variable,nr_days)
  ]

  # merge back to the cluster_alert_table
  cluster_center_locations <- merge(
    cluster_center_locations,
    cluster_alert_table[, .(target, observed, expected, nr_locs)],
    by = "target"
  )

  return(cluster_center_locations)
}

prepare_labels_and_colors <- function(ld, level = c("zip", "county")) {
  level <- match.arg(level)

  cluster_center_labels <- unique(ld$label_center[ld$label_center != "No Cluster"])

  custom_palette <- colorFactor(
    palette = c("grey", rainbow(length(cluster_center_labels))), # Set No Cluster to grey, rest to rainbow colors
    domain = c("0", cluster_center_labels) # Include No Cluster in the domain
  )

  if (level == "zip") {
    labels <- create_custom_labels(label_list = list(
      "Zip:" = ld$GEOID,
      "Center Zip:" = ld$label_center,
      "Zips in Cluster:" = ld$nr_locs,
      "Cluster Obs. Count:" = ld$observed,
      "Cluster Exp. Count:" = round(ld$expected, 2),
      "Location Count:" = ld$count,
      "Nr. Cluster Days:" = ld$nr_days
    ))
  } else {
    labels <- create_custom_labels(label_list = list(
      "County:" = gen_display_name_from_fips(ld$GEOID),
      "Center County:" = gen_display_name_from_fips(ld$label_center, default = "No Cluster"),
      "Counties in Cluster:" = ld$nr_locs,
      "Cluster Obs. Count:" = ld$observed,
      "Cluster Exp. Count:" = round(ld$expected, 2),
      "Location Count:" = ld$count,
      "Nr. Cluster Days:" = ld$nr_days
    ))
  }

  return(list(
    custom_palette = custom_palette,
    labels = labels
  ))
}


# Here is a function that can be made more generic, and can return
# the plot. The data being passed here will be generated by either
# generate_zip_leaflet_data() or generate_county_leaflet_data()

generate_leaflet_plot <- function(leaflet_data, level = c("zip", "county"), ...) {
  labels_and_colors <- prepare_labels_and_colors(leaflet_data, level = level)

  # Currently, this does the same plot for both zip and county
  # TO: fix this to make it more flexible.
  p <- leaflet() |> 
    addPolygons(
      data = leaflet_data,
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 1.0,
      fillColor = ~ labels_and_colors[["custom_palette"]](target),
      label = labels_and_colors[["labels"]]
    ) |> 
    leaflet.extras::setMapWidgetStyle(list(background = "white")) |> 
    leaflet.extras::addFullscreenControl() |>  
    leaflet.extras::addResetMapButton() 

  return(list(plot = p, labels = labels_and_colors[["labels"]], colors = labels_and_colors[["custom_palette"]]))
}