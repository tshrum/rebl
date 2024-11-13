# wrangle imputed data
# 2024-04-09

# Here we are taking the list of full outputs from missForest imputation, 
# identifying the runs with the lowest error, removing the variables that we 
# wanted as predictors but did not want to impute (demographics), and combining
# the imputed variables back into the original survey data that had details like
# prolificID.

#' Inputs:
#' 1. List of all outputs of missForest imputation for all surveys
#' 2. Tuning grid from imputation

#' Outputs:
#'  1. List of all surveys with clean imputed data ready for analysis
#'  2. List of all imputation outputs with and grids for posterity
#'  3. Varwise errors for outputs



# Load Data ---------------------------------------------------------------


# Now we have to get the one with lowest error for each survey
pacman::p_load(dplyr,
               purrr,
               skimr)

# Load miss forest functions (includes helpers for wrangling)
source('3_functions/run_miss_forest.R')

# Load list of all surveys, prepped but before imputation
surveys <- readRDS('5_objects/cleaning/all_surveys_excluded.rds')

# Load list of all imputation outputs for all surveys
imputed_surveys <- readRDS('5_objects/imputation/all_survey_imp_outputs.rds')

# Load tuning grid from imputation.R. We will use it to preserve parameters
# for reporting.
tuning_grid <- readRDS('5_objects/imputation/full_grid.rds')



# Choose Best Runs --------------------------------------------------------


# Going through all imputed datasets and choosing the best ones based on PFC

# Check out imputation outputs
get_str(imputed_surveys)

# Select the run with lowest error for each survey. Hyperparameters used will
# be added to list output for each survey.
best_imp_outputs <- choose_best_runs(imputed_surveys, tuning_grid)
# Note that survey 1 caps out, but 2a and 2b do fine with the grid

get_str(best_imp_outputs)
# This is the single best output for each survey

# Check out errors and tuning parameters
map(best_imp_outputs, ~ .x$OOBerror)
map(best_imp_outputs, ~ .x$mean_PFC)
map(best_imp_outputs, ~ .x$mean_nonzero_PFC)
map(best_imp_outputs, ~ .x$tuning)
map(best_imp_outputs, ~ get_str(.x$ximp))

# Save varwise errors for reporting
varwise_errors <- map(best_imp_outputs, ~ {
    data.frame(
      var = names(.x$ximp),
      OOBerror = .x$OOBerror
    )
})



# Recombine Imputed Data with Surveys -------------------------------------


#' Function to keep the imputed data vars and recombine with old original vars
#' that were not imputed, like demographics, ewe questions, prolificID. 
all_surveys_imputed <- add_imputed_to_og(surveys, best_imp_outputs)

#' Now just put prolificIDs in alphabetical order in all three surveys so 2a and
#' 2b will be in the same order.
all_surveys_imputed <- map(all_surveys_imputed, ~ {
  .x %>% 
    arrange(prolificID)
})

map(all_surveys_imputed, ~ sum(is.na(.x)))
map(all_surveys_imputed, get_str)
# Good, has all imputed data without any missing, plus original data with IDs
# This is nice clean imputed data for analyses!

#' One more thing to do here is to remove lunchOwn and foodFishConventional
#' from survey 1. They had too much missing data to impute. We want to remove 
#' them so they don't cause problems in next step - Rasch modeling
all_surveys_imputed[[1]] <-  all_surveys_imputed[[1]] %>% 
  select(-matches('foodOwnLunch|foodFishConventional'))



# Save and clear data -----------------------------------------------------


# Save best imputed datasets, error, and tuning parameters
saveRDS(best_imp_outputs, '5_objects/imputation/all_survey_best_imp_outputs.rds')

# Save this set in root clean folder to use in all analyses
saveRDS(all_surveys_imputed, '2_clean/all_surveys_imputed.rds')

# Save varwise errors as csv and rds for reporting
saveRDS(varwise_errors, '5_objects/imputation/varwise_errors.rds')
walk2(varwise_errors, names(varwise_errors), ~ {
  .x %>% 
    write_csv(paste0('6_outputs/imputation/varwise_errors_',
                     .y,
                     '.csv'))
})

# Remove objects from environment
clear_data()
