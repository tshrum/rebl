#' Get Conditional REBL Items
#'
#' Function to explore iterative conditional fit results, see which items are 
#' shared between surveys 2a and 2b, and check their p values.
#'
#' @param survey_2a Conditional fit output from `boot_fit()` for survey 2a
#' @param survey_2b Conditional fit output from `boot_fit()` for survey 2b
#'
#' @return A dataframe that contains:
#'   \itemize{
#'     \item REBL items that were shared between surveys 2a and 2b
#'     \item p values for those items from survey 2a
#'     \item p values for those items from survey 2b
#'   }
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' shared_items <- get_cond_rebl_items(fit_2a, fit_2b)
#' }
get_cond_rebl_items <- function(survey_2a, survey_2b){
  
  survey_2a <- tibble::rownames_to_column(survey_2a, var = 'rebl_item')
  survey_2b <- tibble::rownames_to_column(survey_2b, var = 'rebl_item')
  
  df <- dplyr::inner_join(survey_2a, survey_2b, by = 'rebl_item', suffix = c('_2a', '_2b')) %>% 
    dplyr::select(rebl_item, dplyr::matches('in.pkorr'), dplyr::matches('out.pkorr')) %>% 
    dplyr::arrange(in.pkorr_2a, in.pkorr_2b)
  
  return(df)
}