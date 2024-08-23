#' Recode Attention Checks
#' 2024-04-13
#' 
#' Function to recode attention checks. Note that Survey 1 only has 2, while
#' Surveys 2a and 2b have 4. The first two checks in each survey pass with NA,
#' while the second two checks only (Survey 2a and 2b) pass with 'Agree'.
#' 
#' Inputs
#'    1. Survey DF with attention checks uncoded
#'    
#' Outputs
#'    1. Survey DF with attention checks recoded as 1 for pass and 0 for fail



# Packages ----------------------------------------------------------------


pacman::p_load(dplyr)



# Function ----------------------------------------------------------------


recode_attention_checks <- function(df) {
  df %>%
    mutate(across(any_of(
      c('attentionCheck1', 'attentionCheck2')
    ), ~ case_match(.x, NA ~ 1, .default = 0)),
    across(any_of(
      c('attentionCheck3', 'attentionCheck4', 'attentionCheck5')
    ), ~ case_match(.x, 'Agree' ~ 1, NA ~ 0, .default = 0)))
}
