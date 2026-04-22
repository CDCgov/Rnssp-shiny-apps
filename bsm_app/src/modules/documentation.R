# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# This is the main documentation ui
documentation_ui <- function(id) {
  uiOutput(outputId = NS(id)("app_documentation"))
}

# Helper function to return the widget for the table of contents
documentation_toc_ui <- function() {
  toc_sections <- list(
    list(
      title = "Basic User Guide",
      href = "#basic-user-guide",
      children = list(
        list(title = "Loading Data", href = "#loading-data"),
        list(title = "Fitting a Default Model", href = "#fitting-a-default-model"),
        list(title = "Reviewing Results", href = "#reviewing-results"),
        list(title = "Choosing a Visualization", href = "#choosing-a-visualization")
      )
    ),
    list(
      title = "Advanced User Guide",
      href = "#advanced-user-guide",
      children = list(
        list(title = "Adding Covariates", href = "#adding-covariates"),
        list(title = "Advanced Model Specification", href = "#advanced-model-specification"),
        list(title = "Saved and Exported Files", href = "#saved-and-exported-files")
      )
    ),
    list(title = "Tutorial", href = "#tutorial"),
    list(title = "Troubleshooting", href = "#troubleshooting"),
    list(
      title = "Developer Guide",
      href = "#developer-guide",
      children = list(
        list(title = "High-Level App Structure", href = "#high-level-app-structure"),
        list(title = "Data Flow", href = "#data-flow"),
        list(title = "Feature Metadata and Stored Columns", href = "#feature-metadata-and-stored-columns")
      )
    ),
    list(title = "Glossary", href = "#glossary")
  )
  
  toc_item_ui <- function(item) {
    child_items <- item$children %||% list()
    
    tags$li(
      tags$a(item$title, href = item$href, class = "documentation-toc-link"),
      if (length(child_items)) {
        tags$ul(
          class = "documentation-toc-sublist",
          lapply(child_items, function(child) {
            tags$li(
              tags$a(child$title, href = child$href, class = "documentation-toc-sublink")
            )
          })
        )
      }
    )
  }
  
  tags$nav(
    class = "documentation-toc",
    tags$div(class = "documentation-toc-title", "Contents"),
    tags$ul(
      class = "documentation-toc-list",
      lapply(toc_sections, toc_item_ui)
    )
  )
}

# Helper function, receives some content (say some htm from markdonw-to-html pipeline)
# and returns as ui tag
documentation_layout_ui <- function(content) {
  
  # return the tags; first we add some css, mainly to style the documentation
  # table of contents
  
  tags$div(
    class = "documentation-layout",
    tags$style(HTML("
      .documentation-layout {
        display: grid;
        grid-template-columns: minmax(220px, 260px) minmax(0, 1fr);
        gap: 1.5rem;
        align-items: start;
      }

      .documentation-toc {
        position: sticky;
        top: 0;
        align-self: start;
        padding: 0.75rem 0.875rem;
        border: 1px solid var(--bs-border-color);
        border-radius: 0.5rem;
        background: var(--bs-body-bg);
      }

      .documentation-toc-title {
        font-size: 0.8rem;
        font-weight: 700;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        margin-bottom: 0.75rem;
        color: var(--bs-secondary-color);
      }

      .documentation-toc-list,
      .documentation-toc-sublist {
        list-style: none;
        margin: 0;
        padding: 0;
      }

      .documentation-toc-list > li + li {
        margin-top: 0.65rem;
      }

      .documentation-toc-sublist {
        margin-top: 0.35rem;
        padding-left: 0.85rem;
      }

      .documentation-toc-sublist > li + li {
        margin-top: 0.3rem;
      }

      .documentation-toc-link,
      .documentation-toc-sublink {
        color: var(--bs-body-color);
        text-decoration: none;
        line-height: 1.25;
      }

      .documentation-toc-link {
        font-weight: 600;
      }

      .documentation-toc-sublink {
        font-size: 0.95rem;
        color: var(--bs-secondary-color);
      }

      .documentation-toc-link:hover,
      .documentation-toc-sublink:hover {
        color: var(--bs-link-color);
        text-decoration: underline;
      }

      .documentation-content {
        min-width: 0;
      }

      @media (max-width: 991.98px) {
        .documentation-layout {
          grid-template-columns: 1fr;
        }

        .documentation-toc {
          position: static;
        }
      }
    ")),
    
    # then we add the table of content links themselves
    documentation_toc_ui(),
    
    # then we add the actual content passed
    tags$div(class = "documentation-content", content)
  )
}

register_documentation_resources <- local({
  registered <- FALSE
  
  function() {
    if (registered) return(invisible(NULL))
    
    shiny::addResourcePath(
      prefix = "documentation-screenshots",
      directoryPath = normalizePath("src/documentation/screenshots", winslash = "/", mustWork = TRUE)
    )
    
    shiny::addResourcePath(
      prefix = "documentation-tutorial-screenshots",
      directoryPath = normalizePath("src/documentation/tutorial_screenshots", winslash = "/", mustWork = TRUE)
    )
    
    registered <<- TRUE
    invisible(NULL)
  }
})

render_documentation_content <- function(path) {
  if (requireNamespace("markdown", quietly = TRUE)) {
    return(htmltools::HTML(
      markdown::markdownToHTML(
        file = path,
        fragment.only = TRUE,
        options = c("-embed_resources") # adding this to avoid warnings
      )
    ))
  }
  
  doc_text <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  
  if (requireNamespace("commonmark", quietly = TRUE)) {
    return(htmltools::HTML(
      commonmark::markdown_html(doc_text)
    ))
  }
  
  tags$div(
    class = "alert alert-warning",
    tags$strong("Markdown rendering package not installed."),
    tags$p(
      "Install the ",
      tags$code("markdown"),
      " package for fully formatted documentation. Showing plain text for now."
    ),
    tags$pre(
      style = "white-space: pre-wrap; margin-bottom: 0;",
      doc_text
    )
  )
}

# this is the ui for the tutorial page ui (it is serverless)
tutorial_page_ui <- function() {
  register_documentation_resources()
  
  page_fillable(
    theme = THEME,
    title = "BSM Tutorial",
    global_ui_tags,
    tags$style(HTML("
      @media print {
        .tutorial-toolbar {
          display: none !important;
        }

        .tutorial-page img {
          max-width: 100% !important;
          page-break-inside: avoid;
        }

        .tutorial-page figure,
        .tutorial-page h3,
        .tutorial-page h4 {
          page-break-inside: avoid;
        }
      }
    ")),
    div(
      class = "container py-4 tutorial-page",
      style = "max-width: 1100px;",
      div(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2 mb-3 tutorial-toolbar",
        tags$div(
          tags$h2("Bayesian Spatiotemporal Modeling Tutorial", class = "mb-1"),
          tags$p(
            class = "text-muted mb-0",
            "Use this window alongside the main app so the walkthrough stays visible while you work."
          )
        ),
        div(
          class = "d-flex gap-2 flex-wrap",
          tags$button(
            type = "button",
            class = "btn btn-primary btn-sm",
            onclick = "window.print();",
            "Download PDF"
          ),
          tags$a(
            href = "./",
            class = "btn btn-primary btn-sm",
            "Return to Main App"
          )
        )
      ),
      uiOutput("tutorial_page_content")
    )
  )
}


# Here is the main server function for the documentation ui
# Note that the documentatioin ui has one object (a uiOutput, called
# app_documentation, which we render server-side)

documentation_server <- function(id) {
  moduleServer(id,function(input, output, session) {
    
    # register resources
    register_documentation_resources()
  
    # render the uiOutput here  
    output$app_documentation <- renderUI(
      # simply pass the rendered documentation md to the
      # documentent_layout_ui helper function
      documentation_layout_ui(
        render_documentation_content("src/documentation/documentation.md")
      )
    )
  })
}
