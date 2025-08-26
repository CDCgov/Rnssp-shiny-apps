# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# Various functions to create the leaflet data
# and return plot

# Helper function to get the shape file on demand

get_shape_file_from_tigris <- function(st, level = c("zip", "county"), refresh = FALSE, use_cb = FALSE, zctas_from_tigris = TRUE) {
  level <- match.arg(level)

  # if zip is the level using zctas function, for this state
  if (level == "zip") {
    # if zctas from tigris is false, check for local file. If local
    # file does not exist, set zctas from tigris to TRUE
    if (zctas_from_tigris == FALSE) {
      local_zcta_name <- paste0("state_zcta/", st, "_zcta.rds")
      if (!file.exists(local_zcta_name)) {
        zctas_from_tigris <- TRUE
      }
    }

    if (zctas_from_tigris) {
      sh <- tigris::zctas(year = 2010, state = st, refresh = refresh)
      # add datum and fix column names
      sh <- sf::st_transform(sh, crs = "+proj=longlat +datum=WGS84")
    } else {
      sh <- readRDS(paste0("state_zcta/", st, "_zcta.rds"))
    }
    colnames(sh)[which(startsWith(colnames(sh), "ZCTA5CE"))] <- "GEOID"
    colnames(sh)[which(startsWith(colnames(sh), "NAME"))] <- "NAME"
  } else  {
    sh <- Rnssp::county_sf
    if (st != "US") {
      # reduce to state
      statefp <- Rnssp::state_sf[Rnssp::state_sf$STUSPS == st, "STATEFP"]$STATEFP |> as.character()
      sh <- Rnssp::county_sf[Rnssp::county_sf$STATEFP == statefp, ]
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
  
  # create a factor 
  # Derive ordered cluster labels
  cluster_center_labels <- ld |>
    dplyr::filter(label_center != "No Cluster") |>
    dplyr::arrange(-nr_days, -observed) |>
    dplyr::pull(label_center) |>
    unique()
  
  # Add a mapped factor for coloring
  ld <- ld |>
    dplyr::mutate(
      label_factor = dplyr::case_when(
        label_center == "No Cluster" ~ "0",
        TRUE ~ label_center
      ),
      label_factor = factor(label_factor, levels = c("0", cluster_center_labels))
    )
  
  ld <- dplyr::arrange(ld, -nr_days, -observed)
  # return the result
  attr(ld, "cluster_center_labels") <- cluster_center_labels
  return(ld)
}

# Here are the two functions, one for zip and one for county. They currently
# have the same signatures, which is probably important, and it is very
# important that they return the same structure to the wrapper function

generate_zip_leaflet_data <- function(cluster_data, shape_data, distance_matrix) {
  
  # generate the cluster centers frame, along with their constituent zip codes
  cluster_centers <- get_cluster_center_locations(cluster_data)
  
  # merge the shape file to these cluster centers
  ld <- merge(shape_data, cluster_centers, by = "GEOID", all.x = TRUE)

  # merge individual location counts
  ld <- merge(
    ld,
    cluster_data$cluster_location_counts,
    by.x = c("GEOID", "target"),
    by.y = c("location", "target"),
    all.x = TRUE
  )

  # replace NA
  ld[["target"]] <- data.table::fifelse(is.na(ld[["target"]]), "0", ld[["target"]])

  for (col in c("observed", "expected", "nr_locs", "nr_days", "count")) {
    ld[[col]] <- data.table::fifelse(is.na(ld[[col]]), 0, ld[[col]])
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

# replace the previous with a simpler function
get_cluster_center_locations <- function(cluster_data) {
  
  cluster_center_locations <- merge(
    cluster_data$cluster_alert_table[
      ,.(target, nr_days = as.integer(detect_date-date+1), observed, expected, nr_locs)
    ],
    cluster_data$cluster_location_counts[, .(target, GEOID=location)], 
    by = c("target")
  )
  
}

scaled_palette_from_labels <- function(
    labels,
    palette = c("Blues", "Greens", "Purples","plasma", "inferno", "magma", "viridis", "cividis"),
    gradient_fraction = 1.0,
    reverse = TRUE
) {
  palette <- match.arg(palette)
  
  n_colors <- length(labels)
  oversample_n <- 100
  
  if (palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    # Use RColorBrewer
    max_colors <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
    base_colors <- RColorBrewer::brewer.pal(min(max_colors, 9), palette)
    full_gradient <- colorRampPalette(base_colors)(oversample_n)
  } else {
    # Use viridisLite
    palette_fun <- get(palette, envir = asNamespace("viridisLite"))
    full_gradient <- palette_fun(oversample_n)
  }
  
  if (reverse) {
    full_gradient <- rev(full_gradient)
  }
  
  # Slice fraction of the gradient
  slice_end <- ceiling(oversample_n * gradient_fraction)
  sliced_gradient <- full_gradient[1:slice_end]
  
  # Sample final colors
  sampled_indices <- round(seq(1, length(sliced_gradient), length.out = n_colors))
  final_colors <- sliced_gradient[sampled_indices]
  
  return(final_colors)
}

prepare_colors <- function(cluster_center_labels, ...) {
  
  color_args = list(...)

  colors <- do.call(
    scaled_palette_from_labels,
    c(list(labels = cluster_center_labels),color_args)
  )

  custom_palette <- colorFactor(
    palette = c("grey", colors),
    domain = c("0", cluster_center_labels) # Include No Cluster in the domain
  )
  
  custom_palette

}

prepare_labels <- function(ld, level = c("zip", "county")) {

  level <- match.arg(level)

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
  
  labels
}



## Generate plot

# generate_leaflet_plot <- function(
#     leaflet_data,
#     level = c("zip", "county"),
#     color_options = list(
#       palette = "Blues",
#       reverse = TRUE,
#       gradient_fraction = 0.5
#     ),
#     ...
# ) {
#   level <- match.arg(level)
#   
#   # --- Step 1: Get cluster center labels safely ---
#   cluster_center_labels <- attr(leaflet_data, "cluster_center_labels")
#   if (is.null(cluster_center_labels) || length(cluster_center_labels) == 0) {
#     stop("Missing or empty 'cluster_center_labels' attribute in leaflet_data.")
#   }
#   
#   # --- Step 2: Prepare popup/hover labels for polygons ---
#   labels <- prepare_labels(leaflet_data, level = level)
# 
#   # --- Step 3: Generate colors for the cluster labels ---
#   default_color_options <- list(
#     palette = "Blues",
#     reverse = TRUE,
#     gradient_fraction = 0.5
#   )
#   
#   colors <- do.call(
#     prepare_colors,
#     c(
#       list(cluster_center_labels = cluster_center_labels),
#       modifyList(default_color_options, color_options)
#     )
#   )
#   
#   # --- Step 4: Get user-friendly names for tooltips (e.g., county names) ---
#   # Ensure center_names and colors are aligned with cluster_center_labels
#   center_info <- leaflet_data |>
#     dplyr::filter(GEOID %in% cluster_center_labels) |>
#     dplyr::distinct(GEOID, NAME) |>
#     dplyr::mutate(label_center = factor(GEOID, levels = cluster_center_labels)) |>
#     dplyr::arrange(label_center)
#   
#   # Defensive check
#   if (nrow(center_info) != length(cluster_center_labels)) {
#     stop("Mismatch between cluster_center_labels and available NAME values in leaflet_data.")
#   }
#   
#   # Step 5 - Build segment divs with tooltips
#   segment_divs <- paste(sprintf(
#     "<div title='%s' style='flex: 1; height: 100%%; background-color: %s; cursor: help;'></div>",
#     center_info$NAME,
#     colors(as.character(center_info$label_center))
#   ), collapse = "\n")
#   
#   # --- Step 6: Assemble the leaflet plot ---
#   p <- leaflet() |>
#     addPolygons(
#       data = leaflet_data,
#       color = "black",
#       weight = 1,
#       opacity = 1,
#       fillOpacity = 1.0,
#       fillColor = ~ colors(label_factor),
#       label = labels
#     ) |>
#     addControl(
#       html = htmltools::HTML(sprintf("
#         <style>
#           .leaflet-control.info.legend {
#             background: transparent !important;
#             box-shadow: none !important;
#             border: none !important;
#             padding: 0 !important;
#             margin: 0 !important;
#           }
#         </style>
# 
#         <div class='legend-container' style='text-align: center; font-family: sans-serif;'>
#           <div class='text-body fw-bold mb-1' style='font-size: 12px;'>
#             Clusters Ordered by Date and Size
#           </div>
#           <div style='
#             width: 200px;
#             height: 20px;
#             display: flex;
#             border: 1px solid var(--bs-border-color);
#             border-radius: 4px;
#             overflow: hidden;
#           '>
#             %s
#           </div>
#         </div>
#       ", segment_divs)),
#       position = "topright"
#     ) |>
#     leaflet.extras::setMapWidgetStyle(list(background = "var(--bs-body-bg)")) |>
#     leaflet.extras::addFullscreenControl() |>
#     leaflet.extras::addResetMapButton()
#   
#   return(list(
#     plot = p,
#     labels = labels,
#     colors = colors
#   ))
# }

generate_leaflet_plot <- function(
    leaflet_data,
    level = c("zip", "county"),
    color_options = list(
      palette = "Blues",
      reverse = TRUE,
      gradient_fraction = 0.5
    ),
    ...
) {
  level <- match.arg(level)
  
  cluster_center_labels <- attr(leaflet_data, "cluster_center_labels")
  if (is.null(cluster_center_labels) || length(cluster_center_labels) == 0) {
    stop("Missing or empty 'cluster_center_labels' attribute in leaflet_data.")
  }
  
  # Apply colors directly to each row
  leaflet_data <- assign_cluster_colors(
    leaflet_data,
    palette = color_options$palette %||% "Blues",
    reverse = color_options$reverse %||% TRUE,
    gradient_fraction = color_options$gradient_fraction %||% 0.5
  )
  
  labels <- prepare_labels(leaflet_data, level = level)
  
  # # Extract only cluster center rows and their assigned colors
  # # Step 1: pick a single NAME per label_center
  # # Use the geometry row where GEOID == label_center
  # center_names <- leaflet_data |>
  #   dplyr::filter(label_center != "No Cluster", GEOID == label_center) |>
  #   dplyr::distinct(label_center, NAME)
  # 
  # # Step 2: join this to the cluster_center_labels and fill_color
  # center_info <- tibble::tibble(label_center = cluster_center_labels) |>
  #   dplyr::left_join(center_names, by = "label_center") |>
  #   dplyr::left_join(
  #     leaflet_data |> dplyr::distinct(label_center, fill_color),
  #     by = "label_center"
  #   )
  # 
  # # Optional: warning if any NAMEs are missing
  # if (any(is.na(center_info$NAME))) {
  #   warning("Some cluster centers have no matching NAME (could not resolve via GEOID == label_center).")
  # } 
  # 
  # Generate the segmented color legend with tooltips
  
  # Horizontal Legend
  # segment_divs <- paste(sprintf(
  #   "<div title='%s' style='flex: 1; height: 100%%; background-color: %s; cursor: help;'></div>",
  #   center_info$NAME,
  #   center_info$fill_color
  # ), collapse = "\n")
  # 
  
  # Vertical Legend
  # segment_divs <- paste(
  #   sprintf(
  #     "<div title='%s' style='height: 20px; width: 100%%; background-color: %s; cursor: help;'></div>",
  #     center_info$NAME,
  #     center_info$fill_color
  #   ),
  #   collapse = ""
  # )
  
  p <- leaflet() |>
    addPolygons(
      data = leaflet_data,
      color = "black",
      weight = 1,
      opacity = 1,
      fillOpacity = 1.0,
      fillColor = ~ fill_color,
      label = labels
    ) |>
    # For now, I'm going to comment out the legend, but retain
    # it in case we want in the future
    # addControl(
    #   html = htmltools::HTML(sprintf("
    #     <style>
    #       .leaflet-control.info.legend {
    #         background: transparent !important;
    #         box-shadow: none !important;
    #         border: none !important;
    #         padding: 0 !important;
    #         margin: 0 !important;
    #       }
    #     </style>
    # 
    #     <div class='legend-container' style='text-align: center; font-family: sans-serif;'>
    #       <div class='text-body fw-bold mb-1' style='font-size: 12px;'>
    #         Clusters Ordered by Date and Size
    #       </div>
    #       <div style='
    #         width: 20px;
    #         height: auto;
    #         border: 1px solid var(--bs-border-color);
    #         border-radius: 4px;
    #         overflow: hidden;
    #       '>
    #         %s
    #       </div>
    #     </div>
    #   ", segment_divs)),
    #   position = "topright"
    # ) |>
    leaflet.extras::setMapWidgetStyle(list(background = "var(--bs-body-bg)")) |>
    leaflet.extras::addFullscreenControl() |>
    leaflet.extras::addResetMapButton()
  
  return(list(
    plot = p,
    labels = labels,
    colors = leaflet_data$fill_color
  ))
}


assign_cluster_colors <- function(ld, palette = "Blues", reverse = TRUE, gradient_fraction = 1.0) {
  # Get unique, ordered cluster centers by severity
  cluster_centers <- ld |>
    dplyr::filter(label_center != "No Cluster") |>
    dplyr::distinct(label_center, nr_days, observed) |>
    dplyr::arrange(desc(nr_days), desc(observed))
  
  unique_centers <- cluster_centers$label_center
  n_centers <- length(unique_centers)
  
  # Build interpolated palette
  if (n_centers == 0) {
    ld$fill_color <- "grey"
    return(ld)
  }
  
  if (palette %in% rownames(RColorBrewer::brewer.pal.info)) {
    max_colors <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
    base_colors <- RColorBrewer::brewer.pal(min(max_colors, 9), palette)
    full_gradient <- colorRampPalette(base_colors)(100)
  } else {
    palette_fun <- get(palette, envir = asNamespace("viridisLite"))
    full_gradient <- palette_fun(100)
  }
  
  if (reverse) {
    full_gradient <- rev(full_gradient)
  }
  
  slice_end <- ceiling(100 * gradient_fraction)
  sliced_gradient <- full_gradient[1:slice_end]
  
  # Assign one color per unique cluster center
  color_indices <- round(seq(1, length(sliced_gradient), length.out = n_centers))
  cluster_colors <- sliced_gradient[color_indices]
  
  color_map <- data.frame(
    label_center = unique_centers,
    fill_color = cluster_colors,
    stringsAsFactors = FALSE
  )
  
  # Merge color assignments back onto full dataset
  ld <- ld |>
    dplyr::left_join(color_map, by = "label_center") |>
    dplyr::mutate(fill_color = dplyr::if_else(label_center == "No Cluster", "grey", fill_color))
  
  return(ld)
}


