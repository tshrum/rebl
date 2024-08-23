#' get conditional rebl items
#' 2024-04-24
#' 
#' Function to explore iterative conditional fit results, see which items are 
#' shared between surveys 2a and 2b, and check their p values.
#' 
#' Inputs
#'    1. Two conditional fit outputs from `boot_fit()`.
#'    
#' Outputs
#'    1. A df that contains:
#'      1.1. REBL items that were shared between surveys 2a and 2b
#'      1.2. p values for those items from survey 2a
#'      1.3. p values for those items from survey 2b



# Packages ----------------------------------------------------------------


pacman::p_load(dplyr,
               purrr)



# Function ----------------------------------------------------------------


get_cond_rebl_items <- function(survey_2a, survey_2b){
  
  survey_2a <- rownames_to_column(survey_2a, var = 'rebl_item')
  survey_2b <- rownames_to_column(survey_2b, var = 'rebl_item')
  
  df <- inner_join(survey_2a, survey_2b, by = 'rebl_item', suffix = c('_2a', '_2b')) %>% 
    select(rebl_item, matches('in.pkorr'), matches('out.pkorr')) %>% 
    arrange(in.pkorr_2a, in.pkorr_2b)
  
  return(df)
}