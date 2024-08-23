#' get lr test
#' 2024-04-21
#' 
#' Function that gets an LR test based on a split criterion. First, it filters
#' to make sure that variable has no NAs (necessary for LRTest), then it runs
#' a new `RM()`, then runs `LRTest()` with the split criterion given.

#' Inputs
#'    1. Survey DF
#'    2. A variable to use to filter for NAs and as split criterion. Note that
#'      this should be without quotes, like a tidyverse argument.
#'    3. Need list of rebl names. This can be all, or a subset.

#' Outputs
#'    1. An LR test output.



# Packages ----------------------------------------------------------------


pacman::p_load(dplyr,
               rlang,
               eRm)



# Function ----------------------------------------------------------------


get_lr_test <- function(survey, var, rebl_names) {
  
  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>% 
    filter(!is.na(!!ensym(var)))
  
  cat('\nFiltered (Step 1/3)')
  
  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>% 
    select(any_of(rebl_names)) %>% 
    RM()
  
  cat('\nRM Complete (Step 2/3)')
  
  # Now LR test, with split criterion as criterion
  lr_test <- LRtest(model, splitcr = no_na_survey[[as_string(ensym(var))]])
  
  cat('\nLR Test Complete (Step 3/3)\n')
  
  return(lr_test)
}

cat('\nLoaded LR Test\n')