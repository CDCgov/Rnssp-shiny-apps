# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# County Select Module. Contains both a ui and server component. 
# The UI component will show a selectize (for counties) and a map
# The server component maintains a persistent store of counties between
# multiple uses of the module. The server also allows for manipulating
# the map direclty using point and click (select/deselect counties, or
# use polygon or circle drawing to select regions of map

named_geoids <- function(df) setNames(df[["GEOID"]], df[["NAME"]])

county_selector_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # We will need to work on the size of the map to find the 
    # best way to show the map given different possible states
    tags$style("
      .leaflet-selected-pane { pointer-events: none !important; }
      #county_map { max-height: 600px; }
    "),
    div(
      # title for the module contents
      # add buttons for selecting all and deselecting all
      layout_columns(
        col_widths = c(4, 4, 4),
        input_task_button(ns("select_all"),   "Select All"),
        input_task_button(ns("deselect_all"), "Deselect All"), 
        input_task_button(ns("add_neighbors"), "Add Neighbors")
      ),
      # Here is the actual map itself; I'm making width maximal, but constraining
      # the height
      div(leafletOutput(ns("county_map"), height = "600px", width = "100%")),
      # add a selectize for the counties. This could be problematic for
      # many counties
      selectizeInput(
        inputId = ns("county_list"),
        label   = "Selected counties:",
        choices  = NULL,
        multiple = TRUE,
        options  = list(
          placeholder = "Type to search counties...",
          plugins     = list("remove_button") 
        ),
        # make the seletize wide, so that when showing those selected it
        # spreads out a bit
        width = "95%"
      ),
    ),
    # add a button to return and close the modal.
    div(style = "text-align: right;",input_task_button(ns("done"), "Done"))
  )
}


# This is the server component. It received the sf object containing the
# geometries of all the counties in the states seleected, plus a 
# reactive that indicates to the server that the user has opened the 
# modal
county_selector_server <- function(id, geoms, grv, open_trigger, adj_mat) {
  moduleServer(id, function(input, output, session) {
    
    # get the session namespace
    ns <- session$ns
    
    county_names <- reactive({
      k = tibble(geoms()) |> mutate(GEOID = as.character(GEOID)) |> select(NAME, GEOID)
      setNames(k$GEOID, k$NAME)
    })
    
    geoms_3857 <- reactive({
      st_transform(geoms(), 3857)
    })

    # this is for setting the center of the initial map. However, 
    # I do wonder if we should use a bounding box, and set that as the bounds?
    geoms_ctr <- reactive({
      st_coordinates(st_centroid(st_union(geoms())))
    })
    
    # here is a reactive bounding box
    geoms_bbox <- reactive({
      bb = sf::st_bbox(geoms())  
      unname(c(bb["xmin"], bb["ymin"], bb["xmax"], bb["ymax"]))
    })
    
    selected_names <- reactive({
      sel <- input$county_list
      if (is.null(sel)) character(0) else sel
    })
    
    # Whenever the user changes the selectize, push it into the global store
    observe({
      grv$selected_counties <- county_names()[which(county_names() %in% selected_names())]
    }) |> bindEvent(selected_names(),  ignoreInit = TRUE)
    

    # Helper function to repaint
    repaint_selected <- function(sel) {
      sel_geoms <- geoms()
      sel_geoms <- if (length(sel)) sel_geoms[sel_geoms$GEOID %in% sel, ] else sel_geoms[0, ]
      leafletProxy(ns("county_map")) |>
        clearGroup("selected") |>
        addPolygons(
          data        = sel_geoms,
          fillColor   = "orange",
          color       = "black",
          weight      = 2,
          fillOpacity = 0.6,
          options     = pathOptions(pane = "selected", interactive = FALSE),
          group       = "selected"
        )
    }
    
    build_drawn_geom <- function(evt) {
      if (is.null(evt)) return(NULL)
      feature <- if (!is.null(evt$feature)) evt$feature else evt
      
      gtype <- feature$geometry$type
      props <- feature$properties
      
      is_point   <- is.character(gtype) && tolower(gtype[1]) == "point"
      has_radius <- !is.null(props$radius) && is.finite(as.numeric(props$radius))
      
      if (is_point && has_radius) {
        lon <- as.numeric(feature$geometry$coordinates[[1]])
        lat <- as.numeric(feature$geometry$coordinates[[2]])
        radius_m <- as.numeric(props$radius)
        
        center_4326 <- st_sfc(st_point(c(lon, lat)), crs = 4326)
        center_3857 <- st_transform(center_4326, 3857)
        circle_3857 <- st_buffer(center_3857, dist = radius_m)
        return(st_make_valid(circle_3857))
      }
      
      gj <- jsonlite::toJSON(feature, auto_unbox = TRUE)
      g  <- tryCatch(geojsonsf::geojson_sf(gj), error = function(e) NULL)
      if (is.null(g)) return(NULL)
      g  <- st_make_valid(g)
      st_transform(g, 3857)
    }
    
    # function to draw the maps toolbar and set the various options
    # notice that the group is named so that we can later clear it
    add_draw_toolbar <- function(map) {
      map |>
        addDrawToolbar(
          targetGroup = "drawn",
          circleOptions   = drawCircleOptions(
            shapeOptions = drawShapeOptions(weight = 2)
          ),
          polygonOptions  = drawPolygonOptions(
            showArea     = TRUE,
            shapeOptions = drawShapeOptions(weight = 2, fillOpacity = 0.2)
          ),
          editOptions     = editToolbarOptions(edit = FALSE, remove = FALSE),
          rectangleOptions= FALSE,
          polylineOptions = FALSE,
          markerOptions   = FALSE,
          circleMarkerOptions = FALSE
        )
    }
    
    # When the open_trigger increments, we want to fire this up.. drawing
    # the map, but respecting county selections
    observe({
      req(geoms(), county_names())
      
      cn       <- county_names()
      
      # use persisted selection if available; otherwise select all
      sel <- grv$selected_counties
      
      # update selectize
      updateSelectizeInput(
        session,
        "county_list",
        choices  = cn,
        selected = sel, 
      )
      
      # precompute selected sf for initial draw
      geom_sel <- if (length(sel)) geoms()[geoms()$GEOID %in% sel, ] else geoms()[0, ]
      
      output$county_map <- renderLeaflet({
        m <- leaflet(geoms(), options = leafletOptions(
          preferCanvas = TRUE,
          minZoom = 3,
          maxZoom = 12
        ))

        if(grv$includes_alaska_hawaii == FALSE) m <- addProviderTiles(m,"CartoDB.Positron")
        
        m |> 
          addMapPane("base",     zIndex = 410) |>
          addMapPane("selected", zIndex = 420) |>
          addPolygons(
            fillColor   = "lightblue",
            weight      = 1,
            opacity     = 1,
            color       = "white",
            dashArray   = "3",
            fillOpacity = 0.6,
            highlight   = highlightOptions(
              weight      = 3,
              color       = "#666",
              fillOpacity = 0.7,
              bringToFront = TRUE
            ),
            label = ~NAME,
            layerId = ~GEOID,
            options = pathOptions(pane = "base")
          ) |>
          addPolygons(
            data        = geom_sel,
            fillColor   = "orange",
            color       = "black",
            weight      = 2,
            fillOpacity = 0.6,
            options     = pathOptions(pane = "selected", interactive = FALSE),
            group       = "selected"
          ) |>
          add_draw_toolbar() |>
          addLegend(
            position = "bottomright",
            colors = c("orange", "lightblue"),
            labels = c("Selected", "Unselected"),
            opacity = 1
          ) |> 
          enable_draggable_legend() |> 
          flyToBounds(
            lng1 = geoms_bbox()[1],
            lat1 = geoms_bbox()[2],
            lng2 = geoms_bbox()[3],
            lat2 = geoms_bbox()[4]
          )
    
      })
    }) |> bindEvent(open_trigger(), ignoreInit = TRUE)
    
    # Repaint when selection changes
    observe({
      req(geoms())
      repaint_selected(selected_names())
    }) |> bindEvent(geoms(), selected_names())
    
    # If the map has been clicked, we have to update the selectize!
    observe({
      # get the click object
      click <- input$county_map_shape_click
      if (is.null(click$id)) return()
      
      # get the current selected names
      cur <- selected_names()
      # get the new names; which is union with current if new, else deselect via setdiff
      new <- if (click$id %in% cur) setdiff(cur, click$id) else sort(union(cur, click$id))
      
      # update the seletize with the new selection
      updateSelectizeInput(session, "county_list", selected = new)
    }) |> bindEvent(input$county_map_shape_click)
    
    # If the map has been drawn on, we have to update the selectize!
    observe({
      # get the drawn object (use _draw_new_feature)
      feat <- input$county_map_draw_new_feature
      if (is.null(feat)) return()
      
      # get critical information about the feature, using the helper function
      geom_3857 <- build_drawn_geom(feat)
      # if nothing returned, return NULL
      if (is.null(geom_3857)) return()
      
      
      # If multiple geometries, union into a single shape
      if (inherits(geom_3857, "sf") && nrow(geom_3857) > 1) {
        geom_3857 <- st_union(geom_3857)
      }
      
      # get the hits (i.e counties), via intersection
      hit_mat   <- st_intersects(geoms_3857(), geom_3857, sparse = FALSE)
      hit       <- if (is.matrix(hit_mat)) apply(hit_mat, 1, any) else as.logical(hit_mat)
      new_names <- named_geoids(geoms()[hit, ])
      
      # union the hits with those that are currently selected
      new_sel <- sort(union(selected_names(), new_names))
      
      # update the selectize with the new selection
      updateSelectizeInput(session, "county_list", selected = new_sel)
      
      # Let's also drop that feature that was drawn. i.e. we don't need the
      # circle or polygon drawn by the user, once we have used it to
      # identify the counties
      leafletProxy(ns("county_map")) |>
        removeDrawToolbar(clearFeatures = TRUE) |>
        add_draw_toolbar()
    }) |> bindEvent(input$county_map_draw_new_feature)
    
    # Observe the select all / deselect all buttons
    observe(updateSelectizeInput(session, "county_list", selected = county_names())) |>
      bindEvent(input$select_all)
    
    observe(updateSelectizeInput(session, "county_list", selected = character(0))) |> 
      bindEvent(input$deselect_all)
    
    # Observe the add neighbors buttons:
    observe({
      # require that county_names is not empty
      req(input$county_list)
      nb <- get_neighbors(input$county_list, adj_mat)
      # add these neighbors to the county list
      new_selected <- union(input$county_list, nb)
      updateSelectizeInput(session, "county_list", selected = new_selected)
      
    }) %>% bindEvent(input$add_neighbors)

    # When the done button is hit, update the grv done reactive
    observe(grv$modal_done <- grv$modal_done + 1) |> bindEvent(input$done)
    
  })
}


# add neighbors helper function. Given a vector of geoids, and an
# adjacency matrix, return the unique set of geoids from the ajj
# matrix that are valued 1 in the rows() define by the vector
get_neighbors <- function(geoids, mat) {
  nb = lapply(geoids, \(v) names(mat[v,])[which(mat[v,]==1)]) %>% 
    unlist() |>
    unique()
  
  return(unlist(nb))
}
