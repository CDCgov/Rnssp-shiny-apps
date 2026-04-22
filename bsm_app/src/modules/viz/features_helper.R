# THIS FILE IS JUST TO TEST AND ILLUSTRATE HOW TO USE THE FEATURE_SELECTOR_PANEL

test_with_multiple <- TRUE
dummy_page_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    title = "Dummy Page",
    value = ns("dummy_page"),
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = SIDEBAR_WIDTH * 2,
        feature_sidepanel_ui(ns("feature_side"),allow_multiple = test_with_multiple)
      ),
      bslib::card(
        bslib::card_header("Selected features"),
        bslib::card_body(verbatimTextOutput(ns("debug")))
      )
    )
  )
}

dummy_page_server <- function(id, feature_store, im) {
  moduleServer(id, function(input, output, session) {
    
    get_store <- function() {
      if (is.function(feature_store)) feature_store() else feature_store
    }
    
    # keep store synced with current model output
    observe({
      req(im$data_cls)
      
      fs <- get_store()
      req(!is.null(fs))
      
      if (is.function(fs$sync_base_columns)) {
        fs$sync_base_columns(
          data = if (!is.null(im$posterior)) im$posterior else im$data_cls$data,
          data_cls = im$data_cls
        )
      }
      
      if (is.function(fs$register_default_calculated_features)) {
        fs$register_default_calculated_features()
      }
    })
    
    filters <- feature_sidepanel_server(
      "feature_side",
      feature_store = get_store,
      allow_multiple = test_with_multiple
    )
    
    output$debug <- renderPrint({
      fs <- get_store()
      filt <- filters()
      
      list(
        im_model_exists = !is.null(im$model),
        im_data_cls_exists = !is.null(im$data_cls),
        im_posterior_exists = !is.null(im$posterior),
        im_nforecasts = im$nforecasts,
        
        data_cls_names = if (!is.null(im$data_cls)) names(im$data_cls) else NULL,
        data_cls_data_cols = if (!is.null(im$data_cls) && !is.null(im$data_cls$data)) names(im$data_cls$data) else NULL,
        data_cls_nrow = if (!is.null(im$data_cls) && !is.null(im$data_cls$data)) nrow(im$data_cls$data) else NULL,
        
        posterior_cols = if (!is.null(im$posterior)) names(im$posterior) else NULL,
        posterior_nrow = if (!is.null(im$posterior)) nrow(im$posterior) else NULL,
        posterior_head = if (!is.null(im$posterior)) utils::head(as.data.frame(im$posterior), 3) else NULL,
        
        feature_order = fs$rv$order,
        feature_count = length(fs$rv$order),
        features_df = if (is.function(fs$features_df)) fs$features_df() else fs$features_df,
        
        filter_scale = filt$filter_scale,
        filter_type = filt$filter_type,
        selected = filt$selected_features,
        add_menu_choices = filt$add_menu_choices
      )
    })
    
    filters
  })
}
