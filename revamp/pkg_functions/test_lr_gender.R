#' Likelihood Ratio test by gender
#'
#' @description Likelihood ratio test for invariance of eRm model splitting
#'  sample by gender. NOTE: can we have this function run splits by different
#'  variables?
#' @param survey Dataframe of containing REBL item responses
#' @param rebl_names A character vector of the names of the REBL items to be used
#'  to filter the df to only REBL items.
#' @param var Currently only works for gender. Might add to this.
#' @returns Test output from eRm::LRtest()
#' @export
#' @importFrom dplyr filter select any_of
#' @importFrom eRm RM LRtest
#' @examples
test_lr_gender <- function(survey,
                           rebl_names,
                           var = gender) {

  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>%
    dplyr::filter(!is.na(!!ensym(var)))

  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>%
    dplyr::select(dplyr::any_of(rebl_names)) %>%
    eRm::RM()

  # Now LR test, with split criterion as criterion
  lr_test <- eRm::LRtest(model, splitcr = no_na_survey[[as_string(ensym(var))]])

  return(lr_test)
}
