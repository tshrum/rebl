#' Recode Attention Checks
#'
#' Function to recode attention checks. Note that Survey 1 only has 2, while
#' Surveys 2a and 2b have 4. The first two checks in each survey pass with NA,
#' while the second two checks only (Survey 2a and 2b) pass with 'Agree'.
#'
#' @param df Survey dataframe with attention checks uncoded
#'
#' @return Survey dataframe with attention checks recoded as 1 for pass and 0 for fail
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' recoded_data <- recode_attention_checks(survey_data)
#' }
recode_attention_checks <- function(df) {
  df %>%
    dplyr::mutate(dplyr::across(dplyr::any_of(
      c('attentionCheck1', 'attentionCheck2')
    ), ~ dplyr::case_match(.x, NA ~ 1, .default = 0)),
    dplyr::across(dplyr::any_of(
      c('attentionCheck3', 'attentionCheck4', 'attentionCheck5')
    ), ~ dplyr::case_match(.x, 'Agree' ~ 1, NA ~ 0, .default = 0)))
}