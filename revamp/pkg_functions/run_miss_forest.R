#' MissForest Imputation Functions
#'
#' Functions for running missForest algorithm for data imputation with hyperparameter
#' tuning, selecting best runs, and combining imputed data back with original datasets.
#'
#' @name missforest-functions
NULL

#' Run MissForest Imputation
#'
#' Runs missForest algorithm for data imputation using a hyperparameter tuning grid
#' rather than choosing specific values of mtry and ntree. Default is to run parallel
#' and set seed as 42 with each iteration.
#'
#' @param df A data frame (cannot be a tibble!) that contains missing data. All 
#'   vars must either be factors or numeric. No vars can have a single output.
#' @param tuning_grid Data frame with mtry and ntree columns for hyperparameter tuning
#' @param verbose Logical, whether to show progress (default: TRUE)
#' @param variablewise Logical, whether to compute error estimates per variable (default: FALSE)
#' @param parallelize Character, parallelization method (default: 'variables')
#' @param seed Integer, random seed for reproducibility (default: 42)
#' @param ... Additional arguments passed to missForest
#'
#' @return List containing imputed datasets for each parameter combination
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' tuning_grid <- expand.grid(mtry = c(2, 4, 6), ntree = c(100, 500))
#' results <- run_miss_forest(data_with_nas, tuning_grid)
#' }
run_miss_forest <- function(df,
                            tuning_grid = tuning_grid,
                            verbose = TRUE,
                            variablewise = FALSE,
                            parallelize = 'variables',
                            seed = 42,
                            ...) {
  
  # Initialize results list
  results <- list()
  
  for (i in 1:nrow(tuning_grid)) {
    
    # Set seed before every run
    set.seed(seed)
    
    tictoc::tic()
    
    # Run missForest using parameters from tuning grid, parallelize across vars
    imp <- missForest::missForest(df,
                      mtry = tuning_grid$mtry[i],
                      ntree = tuning_grid$ntree[i],
                      verbose = verbose,
                      variablewise = variablewise,
                      parallelize = parallelize,
                      ...)
    
    # Print a message after each iteration so we know what's happening
    cat('\n~~~~~~~ Run', i, 'of', nrow(tuning_grid), 'complete ~~~~~~~\n\n')
    
    tictoc::toc()
    
    # Save imp to results list
    results[[i]] <- imp
    
  }
  
  return(results)
}

#' Choose Best MissForest Runs
#'
#' Helper function to pull the best runs out of the list of all the imputations
#' based on lowest error rates.
#'
#' @param imp_surveys List of imputation results from run_miss_forest
#' @param tuning_grid Data frame with hyperparameter combinations used
#' @param variablewise Logical, whether variable-wise errors were computed (default: TRUE)
#'
#' @return List of best imputation results with lowest error rates
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage  
#' best_results <- choose_best_runs(imp_results, tuning_grid)
#' }
choose_best_runs <- function(imp_surveys,
                             tuning_grid = full_grid,
                             variablewise = TRUE) {
  if (variablewise == TRUE) {
  ## For variablewise -----
    
    results <- purrr::map2(imp_surveys, names(imp_surveys), ~ {
      
      # Get mean PFC and add to each output
      imp_surveys <- purrr::map(.x, \(imp_output) {
        imp_output[['mean_PFC']] <- mean(imp_output$OOBerror)
        imp_output[['mean_nonzero_PFC']] <- mean(
          imp_output$OOBerror[imp_output$OOBerror != 0]
          )
        return(imp_output)
      })
      
      # Index of which run was the best (to then pull parameter values)
      index <- which.min(purrr::map_dbl(imp_surveys, \(single_imp) single_imp$mean_PFC))
      
      # Yoink mtry and ntree based on index
      mtry <- tuning_grid$mtry[index]
      ntree <- tuning_grid$ntree[index]
      
      # Get error of best run
      error <- imp_surveys[[index]]$mean_PFC
      
      cat(
        '\nThe best run for ',
        .y,
        ' was number ',
        index,
        ' with a PFC error of ',
        error,
        '. mtry = ',
        mtry,
        ', and ntree = ',
        ntree,
        '.\n',
        sep = ''
      )
      
      # Add values of mtry and ntree to result list - not kept anywhere else
      # Otherwise we will lose which hyper parameters we used
      imp_surveys[[index]][['tuning']] <- tuning_grid[index, ]
      
      return(imp_surveys[[index]])
      
    })
    
    return(results)
    
  } else if (variablewise == FALSE) {
    
    ## For not variablewise -----
    results <- purrr::map2(imp_surveys, names(imp_surveys), ~ {
      index <- which.min(purrr::map_dbl(.x, \(single_imp) single_imp$OOBerror))
      mtry <- tuning_grid$mtry[index]
      ntree <- tuning_grid$ntree[index]
      error <- .x[[index]]$OOBerror
      
      cat(
        '\nThe best run for ',
        .y,
        ' was number ',
        index,
        ' with a PFC error of ',
        error,
        '. mtry = ',
        mtry,
        ', and ntree = ',
        ntree,
        '.\n',
        sep = ''
      )
      
      # Add values of mtry and ntree to result list - not kept anywhere else
      # Otherwise we will lose which hyper parameters we used
      .x[[index]][['tuning']] <- tuning_grid[index, ]
      
      # return(best_imputation)
      return(.x[[index]])
      
    })
    
    return(results)
    
  }
}

#' Add Imputed Data to Original Datasets
#'
#' Helper function to put the imputed data back into the original data.
#' Removes demographics from imputed datasets (we don't want imputed demographics),
#' then combines with original survey data.
#'
#' @param og_dfs List of original surveys before imputation
#' @param best_imp_outputs List of best outputs from missForest algorithm
#'
#' @return List of new, imputed surveys with readout of NAs before and after
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' final_data <- add_imputed_to_og(original_surveys, best_imputations)
#' }
add_imputed_to_og <- function(og_dfs,
                              best_imp_outputs) {
  
  # First we have to pull out just the imputed data frame from the list
  best_imp_dfs <- purrr::map(best_imp_outputs, ~ .x$ximp)

  # Remove demographics from best imputed datasets because we don't want 
  # to have imputed race or income, etc.
  imputed_vars_only <- purrr::map(best_imp_dfs, ~ {
    .x |>
      dplyr::select(-dplyr::any_of(
        c(
          'age',
          'gender',
          'race',
          'education',
          'rurality',
          'income',
          'children',
          'election',
          'politics'
        )
      ))
  })
  
  # Change imputed vars into numeric from factor. Factor was needed for miss
  # Forest, but we need numeric for analyses. We are also subtracting one 
  # because R is 1-indexed, so converting back to numeric turns 0s and 1s into 
  # 1s and 2s. But we want the original 0s and 1s for analysis. 
  imputed_vars_only <- purrr::map(imputed_vars_only, ~ {
    .x |>
      dplyr::mutate(
        dplyr::across(dplyr::matches('ccStatements'), as.character),
        dplyr::across(dplyr::matches('^values'), ~ as.numeric(.x) - 2),
        dplyr::across(dplyr::where(is.factor), ~ as.numeric(.x) - 1))
  })
  
  # Record number of missing in original dataset to compare later
  og_missing <- purrr::map(og_dfs, ~ sum(is.na(.x)))
  
  # Get names of imputed variables
  imp_var_names <- purrr::map(imputed_vars_only, names)
  
  # Remove the vars from surveys that were imputed so we don't have doubles
  og_dfs <- purrr::map2(og_dfs, imp_var_names, ~ {
    .x |> 
      dplyr::select(-dplyr::any_of(.y))
  })
  
  # Recombine og_dfs with imputed_vars_only to get complete set
  result <- purrr::map2(og_dfs, imputed_vars_only, ~ {
    dplyr::bind_cols(.x, .y)
  })
  
  # Get number of missing in result to compare to og_dfs
  result_missing <- purrr::map(result, ~ sum(is.na(.x)))
  
  # Readout about missing data before and after
  cat(
    '\nOriginal surveys had', 
    paste(og_missing, collapse = '/'),
    'NAs.\nThe new datasets have', 
    paste(result_missing, collapse = '/'),
    'NAs.\n'
  )
  
  return(result)
}

#' Add Imputed Data to Original Datasets (Keep Politics)
#'
#' Alternative version that keeps imputed politics variable while removing other demographics.
#'
#' @param og_dfs List of original surveys before imputation
#' @param best_imp_outputs List of best outputs from missForest algorithm
#'
#' @return List of new, imputed surveys with politics included
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage for EWE analysis
#' final_data <- add_imp_to_og_keep_politics(original_surveys, best_imputations)
#' }
add_imp_to_og_keep_politics <- function(og_dfs, best_imp_outputs) {
  # First we have to pull out just the imputed data frame from the list
  best_imp_dfs <- purrr::map(best_imp_outputs, ~ .x$ximp)
  
  # Remove demographics from best imputed datasets because we don't want
  # to have imputed race or income, etc.
  imputed_vars_only <- purrr::map(best_imp_dfs, ~ {
    .x |>
      dplyr::select(-dplyr::any_of(
        c(
          'age',
          'gender',
          'race',
          'education',
          'rurality',
          'income',
          'children',
          'election'
        )
      ))
  })
  
  # Change imputed vars into numeric from factor. Factor was needed for miss
  # Forest, but we need numeric for analyses. We are also subtracting one
  # because R is 1-indexed, so converting back to numeric turns 0s and 1s into
  # 1s and 2s. But we want the original 0s and 1s for analysis.
  imputed_vars_only <- purrr::map(imputed_vars_only, ~ {
    .x |>
      dplyr::mutate(
        dplyr::across(dplyr::matches('ccStatements'), as.character),
        dplyr::across(dplyr::matches('^values'), ~ as.numeric(.x) - 2),
        dplyr::across(dplyr::where(is.factor), ~ as.numeric(.x) - 1)
      )
  })
  
  # Record number of missing in original dataset to compare later
  og_missing <- purrr::map(og_dfs, ~ sum(is.na(.x)))
  
  # Get names of imputed variables
  imp_var_names <- purrr::map(imputed_vars_only, names)
  
  # Remove the vars from surveys that were imputed so we don't have doubles
  og_dfs <- purrr::map2(og_dfs, imp_var_names, ~ {
    .x |>
      dplyr::select(-dplyr::any_of(.y))
  })
  
  # Recombine og_dfs with imputed_vars_only to get complete set
  result <- purrr::map2(og_dfs, imputed_vars_only, ~ {
    dplyr::bind_cols(.x, .y)
  })
  
  # Get number of missing in result to compare to og_dfs
  result_missing <- purrr::map(result, ~ sum(is.na(.x)))
  
  # Readout about missing data before and after
  cat(
    '\nOriginal surveys had',
    paste(og_missing, collapse = '/'),
    'NAs.\nThe new datasets have',
    paste(result_missing, collapse = '/'),
    'NAs.\n'
  )
  
  return(result)
}