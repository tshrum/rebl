#' Get Likelihood Ratio Test
#'
#' Function that gets a Likelihood Ratio test based on a split criterion. First, it filters
#' to make sure that variable has no NAs (necessary for LRTest), then it runs
#' a new `RM()`, then runs `LRTest()` with the split criterion given.
#'
#' @param survey Survey data frame
#' @param var A variable to use to filter for NAs and as split criterion (unquoted variable name)
#' @param rebl_names Vector of REBL item names. This can be all items, or a subset
#'
#' @return An LR test output from the eRm package
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' lr_result <- get_lr_test(survey_data, gender, rebl_item_names)
#' }
get_lr_test <- function(survey, var, rebl_names) {
  
  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>% 
    dplyr::filter(!is.na(!!rlang::ensym(var)))
  
  cat('\nFiltered (Step 1/3)')
  
  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>% 
    dplyr::select(dplyr::any_of(rebl_names)) %>% 
    eRm::RM()
  
  cat('\nRM Complete (Step 2/3)')
  
  # Now LR test, with split criterion as criterion
  lr_test <- eRm::LRtest(model, splitcr = no_na_survey[[rlang::as_string(rlang::ensym(var))]])
  
  cat('\nLR Test Complete (Step 3/3)\n')
  
  return(lr_test)
}