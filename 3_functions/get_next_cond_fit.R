# get next conditional fit
# 2024.04.16

#' This function takes bootstrapped fit statistics from iarm:boot_fit, removes
#' items with the most significant fit stats, reruns the cml rasch model, and 
#' runs another round of conditional fit statistics. It reduces the number of 
#' items in each iteration. This is set up to run in parallel and work across
#' all three surveys at once, saving to a list, where each element is an 
#' iteration that contains fit stats for each survey. It is a 2 part function.
#' Firt we run it once with `get_cond_fit`, then from there we keep running
#' `get_next_cond_fit` for more iterations. 
#' 
#' Inputs
#'    1. A dataframe of conditional fit statistics
#'    2. A list of all clean surveys to use to rerun rasch model

#' Outputs
#'    1. A new dataframe of conditional fit statistics with reduced items



# Packages ----------------------------------------------------------------


pacman::p_load(iarm,
               dplyr,
               eRm,
               purrr,
               furrr)



# For one DF at a time ----------------------------------------------


get_cond_fit <- function(fit, 
                         survey, 
                         n_boot = 1000, 
                         seed = 42) {

  # Keep less significant items, grab names of items
  # Functionally removing items where BOTH out and in pkorr == 0
  keep_items <- fit %>%
    filter(out.pkorr > 0 | in.pkorr > 0) %>%
    row.names()

  # Run new rasch model with keepers
  rm <- survey %>%
    select(any_of(keep_items)) %>%
    RM()
  
  # Set seed for bootstrap
  set.seed(seed)
  
  # Run bootstrapped fit statistics with new set
  boot_fit(object = rm, B = n_boot)[[1]] %>%
    as.data.frame()
}



# get_next_cond_fit -------------------------------------------------------


# Maps using get_cond_fit 
get_next_cond_fit <- function(prev_fit,
                              surveys,
                              n_boot = 1000,
                              config = config,
                              seed = 42) {
  # Just mapping over the get cond fit function above
  future_map2(prev_fit, surveys, ~ {
    get_cond_fit(.x, .y, n_boot = n_boot, seed = seed)
  }, options = config)
}