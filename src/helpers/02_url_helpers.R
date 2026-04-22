# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

get_categorical_values <- function(profile=NULL) {
  
  if(is.null(profile)) return(NULL)
  
  #########################################################
  comb_url <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/combinedCategory"
  url <- "https://essence.syndromicsurveillance.org/nssp_essence/servlet/SyndromeDefinitionsServlet_CCDD?action=getCCDDTerms"
  ##################################################   Note CCDD name change!
  
  combinedCategories <- comb_url |> Rnssp::get_api_data(profile = profile)
  ccddterms <- Rnssp::get_api_data(url, profile = profile)
  
  if(!is.null(combinedCategories) && !is.null(ccddterms)) {
    
    combinedCategories <- combinedCategories |>       
      purrr::pluck("values") |> 
      dplyr::rename(combined_category = display) |> 
      dplyr::left_join(
        ccddterms |> 
          purrr::pluck("categories") |> 
          dplyr::select(combined_category = category, query_logic = definition) |> 
          dplyr::mutate(combined_category = paste("CCDD", combined_category)), 
        by = "combined_category"
      ) |> 
      dplyr::mutate(dplyr::across(where(is.character), ~tidyr::replace_na(., "")))
    
    
    setDT(combinedCategories)
    ccdd_cats <- combinedCategories[grepl("^CCDD", combined_category), combined_category]
    ccdd_cats <- gsub("^CCDD ", "", ccdd_cats, perl=T)
    syndromes <- combinedCategories[grepl("^SYNDROME", combined_category), combined_category]
    syndromes <- gsub("SYNDROME ", "", syndromes)
    subsyndromes <- combinedCategories[grepl("^SUBSYNDROME", combined_category), combined_category]
    subsyndromes <- gsub("SUBSYNDROME ", "", subsyndromes)
  } 
    
  return(list(
    ccdd_cats = ccdd_cats,
    syndromes = syndromes,
    subsyndromes = subsyndromes
  ))
  
}
