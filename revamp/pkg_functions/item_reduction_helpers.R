#' Item Reduction Helper Functions
#'
#' Functions to make item reduction by consistency and stepwise LR tests easier,
#' seeing as we are now trying out a bunch of iterations.
#'
#' @name item-reduction-functions
NULL

#' Reduce Items by Stepwise LR Test
#'
#' Uses stepwise likelihood ratio test to identify items that can be removed
#' while maintaining model fit across multiple surveys.
#'
#' @param models Either an eRm object or list of eRm objects
#' @param criterion Criterion for stepwise selection (default: list('LRtest'))
#' @param verbose Logical, whether to show detailed output (default: TRUE)
#' @param maxstep Maximum number of steps (default: NA)
#'
#' @return Vector of shared items that remain after stepwise reduction
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' shared_items <- reduce_by_stepwise_lr(rasch_models)
#' }
reduce_by_stepwise_lr <- function(models,
                                  criterion = list('LRtest'),
                                  verbose = TRUE,
                                  maxstep = NA) {
  # If given an individual model, turn it into a list
  if (!is.list(models)) {
    assertthat::validate_that(any(stringr::str_detect(class(models), 'eRm')),
                  msg = 'Input must be either an eRm object of a list of eRm objects')
    models <- list(models)
  }
  
  
  # Now treat as list and apply to all
  tictoc::tic()
  lr_outputs <- purrr::imap(models, ~ {
    cat('\n~~~ Starting', .y, 'at:', format(Sys.time(), '%X'), '~~~\n')
    
    eRm::stepwiseIt(object = .x,    
               criterion = criterion,
               verbose = verbose,
               maxstep = maxstep)
  })
  tictoc::toc()
  
  # Now get shared items
  shared_items <- intersect(colnames(lr_outputs[[1]]$X), 
                            colnames(lr_outputs[[2]]$X))
  
  return(shared_items)
}

#' Reduce Items by Consistency
#'
#' Filter items based on consistency between survey parts, keeping only items
#' that fall within specified consistency bounds.
#'
#' @param rebl_items Vector of REBL item names to evaluate
#' @param clean_surveys List of clean survey datasets
#' @param lower_bound Lower bound for consistency proportion (default: 0.75)
#' @param upper_bound Upper bound for consistency proportion (default: 0.95)
#'
#' @return Dataframe of items meeting consistency criteria
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' consistent_items <- reduce_by_consistency(rebl_items, surveys, 0.8, 0.9)
#' }
reduce_by_consistency <- function(rebl_items,
                                  clean_surveys,
                                  lower_bound = 0.75,
                                  upper_bound = 0.95) {
  
  # Filter down both surveys to only the people who were in both parts
  shared_ids <- intersect(clean_surveys[[1]]$prolificID, clean_surveys[[2]]$prolificID)
  filtered <- purrr::map(clean_surveys, ~ {
    .x %>% 
      dplyr::filter(prolificID %in% shared_ids) %>% 
      dplyr::arrange(prolificID)
    })
  
  # Make consistency data frame, where each cell is 1 if consistent, 0 if not
  consistency <- purrr::map(rebl_items, ~ {
    dplyr::case_when(filtered[[1]][.x] == filtered[[2]][.x] ~ 1,
              .default = 0)
  }) %>% 
    as.data.frame() %>%
    stats::setNames(rebl_items)
  
  # Get item consistency
  item_consistency <- consistency %>% 
    dplyr::select(dplyr::matches(rebl_items)) %>% 
    colSums() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column() %>% 
    stats::setNames(c('rebl_item', 'total_consistent')) %>% 
    dplyr::mutate(prop_consistent = round(total_consistent / (nrow(filtered[[1]])), 3)) %>% 
    dplyr::arrange(prop_consistent)
  
  # Filter by given bounds
  chosen_items <- item_consistency %>% 
      dplyr::filter(prop_consistent >= lower_bound & prop_consistent < upper_bound)
  
  return(chosen_items)
}

#' Get Item Consistency
#'
#' Calculate consistency proportions for REBL items between survey parts.
#'
#' @param rebl_items Vector of REBL item names to evaluate
#' @param clean_surveys List of clean survey datasets
#'
#' @return Dataframe with item consistency proportions
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' consistency_stats <- get_consistency(rebl_items, surveys)
#' }
get_consistency <- function(rebl_items,
                            clean_surveys) {
  # Filter down both surveys to only the people who were in both parts
  shared_ids <- intersect(clean_surveys[[1]]$prolificID, clean_surveys[[2]]$prolificID)
  filtered <- purrr::map(clean_surveys, ~ {
    .x %>%
      dplyr::filter(prolificID %in% shared_ids) %>%
      dplyr::arrange(prolificID)
  })
  
  # Make consistency data frame, where each cell is 1 if consistent, 0 if not
  consistency <- purrr::map(rebl_items, ~ {
    dplyr::case_when(filtered[[1]][.x] == filtered[[2]][.x] ~ 1, .default = 0)
  }) %>%
    as.data.frame() %>%
    stats::setNames(rebl_items)
  
  # Get item consistency
  item_consistency <- consistency %>%
    dplyr::select(dplyr::matches(rebl_items)) %>%
    colSums() %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    stats::setNames(c('rebl_item', 'total_consistent')) %>%
    dplyr::mutate(prop_consistent = round(total_consistent / (nrow(filtered[[1]])), 3)) %>%
    dplyr::arrange(prop_consistent) %>% 
    dplyr::select(-total_consistent)
  
  return(item_consistency)
}

#' Get Rasch Models
#'
#' Create Rasch models for specified REBL items across clean surveys.
#'
#' @param rebl_items Vector of REBL item names
#' @param clean_surveys List of clean survey datasets
#'
#' @return List of Rasch models, one for each survey
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' models <- get_rm(final_items, clean_surveys)
#' }
get_rm <- function(rebl_items, clean_surveys) {
  purrr::map(clean_surveys, ~ {
    .x %>% 
      dplyr::select(dplyr::all_of(rebl_items)) %>%
      eRm::RM()
  })
}

#' Get Unidimensionality Test (Single Survey)
#'
#' Run unidimensionality test for REBL items on a single survey.
#'
#' @param rebl_items Vector of REBL item names
#' @param clean_surveys Single survey dataset
#'
#' @return NPtest results for unidimensionality
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage for single survey
#' uni_test <- get_uni_test(rebl_items, survey_data)
#' }
get_uni_test <- function(rebl_items, clean_surveys) {
  results <- clean_surveys %>% 
    dplyr::select(dplyr::all_of(rebl_items)) %>% 
    as.matrix() %>% 
    eRm::NPtest(method = 'MLoef')
  return(results)
}

#' Get Unidimensionality Test (Multiple Surveys)
#'
#' Run unidimensionality tests for REBL items across multiple surveys.
#'
#' @param rebl_items Vector of REBL item names
#' @param clean_surveys List of clean survey datasets
#'
#' @return List of NPtest results, one for each survey
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage for multiple surveys
#' uni_tests <- get_uni_test_multiple(rebl_items, survey_list)
#' }
get_uni_test_multiple <- function(rebl_items, clean_surveys) {
  set <- deparse(substitute(rebl_items))
  cat('\n~~~ Starting', set, 'at', format(Sys.time(), '%X'), '~~~')
  tictoc::tic()
  results <- purrr::imap(clean_surveys, ~ {
    cat('\nStarting', .y)
    .x %>%
      dplyr::select(dplyr::all_of(rebl_items)) %>%
      as.matrix() %>%
      eRm::NPtest(method = 'MLoef')
  })
  tictoc::toc()
  return(results)
}