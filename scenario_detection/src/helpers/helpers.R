# © 2024 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under 
# contracts no. 75D30120C07643, 75D30122C15442, 75D30124C19958

#----------------------------------------------------
# Scenario Detection App - Phase 1
# Authors:
#   Joshua Kimrey
#   Catherine Schwartz
#   Roseric Azondekon
#   Michael Sheppard
#----------------------------------------------------

# Help Popup
helpPopup <- function(
    id,
    word = "methods",
    title,
    content,
    placement = c("right", "top", "left", "bottom"),
    trigger = c("click", "hover", "focus", "manual"),
    icon_name = "question-circle",
    icon_style = "color:red; cursor:pointer;") {
  
  tagList(
    singleton(
      tags$head(
        tags$script(
          HTML("
            $(function() {
              $('[data-toggle=\"popover\"]').popover({ html : true, sanitize: false });
            });")
        )
      )
    ),
    HTML(id),
    tags$span(
      shiny::icon(name = icon_name, class = "shinyhelper-icon", style = icon_style),
      word,
      style = paste("margin-left:10px;", icon_style),
      `data-toggle` = "popover",
      title = title,
      `data-content` = content,
      `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok = TRUE)[1]
    )
  )
}

