#' Recode REBL Items
#'
#' Recoding values of REBL items based on which option is the Pro-Environmental Behavior (PEB).
#' This applies to surveys 2a and 2b where responses need to be converted from "Yes"/"No" to
#' 1/0 based on whether the response represents pro-environmental behavior.
#'
#' @param df Dataframe with cases excluded
#'
#' @return Same dataframe with "Yes" and "No" coded to 1 and 0 as factors based 
#'   on which one is the PEB
#' @export
#'
#' @details Will intentionally throw an error if any of the required columns don't exist.
#'   The function loads REBL item coding information from '5_objects/cleaning/rebl_item_coding.rds'
#'   to determine which items should be standard coded vs reverse coded.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' recoded_data <- recode_rebl(raw_survey_data)
#' }
recode_rebl <- function(df) {
  
  # Load data frame of rebl items and coding
  rebl_coding <- readRDS('5_objects/cleaning/rebl_item_coding.rds') %>% 
    dplyr::select(rebl_item, peb_coding) %>% 
    unique()
  
  # Get standard
  standard_questions <- rebl_coding %>% 
    dplyr::filter(peb_coding == "standard") %>% 
    dplyr::pull(rebl_item)
  
  # Get reversed
  reversed_questions <- rebl_coding %>% 
    dplyr::filter(peb_coding == "reverse") %>% 
    dplyr::pull(rebl_item)
  
  # Recode in two groups. Note that we are recoding the forage BS check here 
  # manually because we did not include it as a REBL item originally because
  # it is duplicated.
  dat <- df |>
    dplyr::mutate(dplyr::across(dplyr::any_of(c(standard_questions, 'foodForageBSCheck')),
                  ~ dplyr::case_match(., "Yes" ~ 1, "No" ~ 0)),
           dplyr::across(dplyr::any_of(reversed_questions),
                  ~ dplyr::case_match(., "Yes" ~ 0, "No" ~ 1)))
  
  return(dat)
}