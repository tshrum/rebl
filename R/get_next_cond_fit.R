#' Iterative Conditional Fit Functions
#'
#' Functions for running iterative conditional fit statistics with bootstrapped
#' fit statistics from iarm::boot_fit, removing items with significant fit stats,
#' and rerunning the CML Rasch model with reduced items.
#'
#' @name conditional-fit-functions
NULL

#' Get Conditional Fit Statistics
#'
#' Takes bootstrapped fit statistics, removes items with the most significant fit stats,
#' reruns the CML rasch model, and runs another round of conditional fit statistics.
#'
#' @param fit A dataframe of conditional fit statistics
#' @param survey Survey data to use for rerunning rasch model
#' @param n_boot Number of bootstrap samples (default: 1000)
#' @param seed Random seed for reproducibility (default: 42)
#'
#' @return New dataframe of conditional fit statistics with reduced items
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' new_fit <- get_cond_fit(previous_fit, survey_data)
#' }
get_cond_fit <- function(fit, 
                         survey, 
                         n_boot = 1000, 
                         seed = 42) {

  # Keep less significant items, grab names of items
  # Functionally removing items where BOTH out and in pkorr == 0
  keep_items <- fit %>%
    dplyr::filter(out.pkorr > 0 | in.pkorr > 0) %>%
    row.names()

  # Run new rasch model with keepers
  rm <- survey %>%
    dplyr::select(dplyr::any_of(keep_items)) %>%
    eRm::RM()
  
  # Set seed for bootstrap
  set.seed(seed)
  
  # Run bootstrapped fit statistics with new set
  iarm::boot_fit(object = rm, B = n_boot)[[1]] %>%
    as.data.frame()
}

#' Get Next Conditional Fit (Parallel)
#'
#' Maps over the get_cond_fit function to run iterative conditional fit
#' across multiple surveys in parallel.
#'
#' @param prev_fit List of previous fit statistics for each survey
#' @param surveys List of survey datasets
#' @param n_boot Number of bootstrap samples (default: 1000)
#' @param config Future configuration for parallel processing
#' @param seed Random seed for reproducibility (default: 42)
#'
#' @return List of conditional fit results for each survey
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage with parallel processing
#' library(furrr)
#' plan(multisession)
#' next_fit <- get_next_cond_fit(prev_fit, survey_list, config = config)
#' }
get_next_cond_fit <- function(prev_fit,
                              surveys,
                              n_boot = 1000,
                              config = config,
                              seed = 42) {
  # Just mapping over the get cond fit function above
  furrr::future_map2(prev_fit, surveys, ~ {
    get_cond_fit(.x, .y, n_boot = n_boot, seed = seed)
  }, options = config)
}