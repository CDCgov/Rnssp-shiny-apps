# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

#############################################################
## MODULE FOR CREDENTIALS
## (note this is UI-less)
#############################################################

credServer <- function(id, profile, valid_profile) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      observe({
        if(!valid_profile()) showModal(credentials_modal(ns=ns))
      })
      
      observe({
        # Process Credentials Dialog Submission
        if(!is.null(input$cred_user) && input$cred_user!="") {
          prof <- create_profile(username = input$cred_user, password=input$cred_pwd)
        }
        # Now, check for validity
        if(!is.null(prof) && check_profile(prof)) {
          profile(prof)
          valid_profile(TRUE)
          removeModal()
        } else showModal(credentials_modal(ns=ns,failed=TRUE))
      }) |> bindEvent(input$submit_creds)
      
    }
  )
}

#############################################################
## MODULE HELPER FUNCTIONS
## 1 - FUNCTION TO CHECK IF PROFILE IS VALID
## 2 = FUNCTION TO CREATE THE MODAL
#############################################################

check_profile <- function(profile, url = "https://essence.syndromicsurveillance.org/nssp_essence/") {
  # Simply checks if a profile returns a 200 response. Note that the profile
  # could technically still be "valid", but the server could be down, for example, and
  # this would return FALSE
  tryCatch(
    {
      resp <- get_api_response(url=url, profile=profile)
      return(resp$status == 200)
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  
}

credentials_modal <- function(ns = NS(), failed=FALSE) {
  
  modalDialog(
    title = "Enter NSSP API Credentials",
    if(failed == TRUE) div(p("Invalid Credentials", style="color:red")),
    textInput(ns("cred_user"), "Username"),
    passwordInput(ns("cred_pwd"), "Password"),
    easyClose = T,
    footer = tagList(
      actionButton(ns("submit_creds"), "Submit Credentials"),
      modalButton("Cancel")
    )
  )
}
