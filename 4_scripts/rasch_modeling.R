#' rasch_modeling
#' 2024-08-21

#' Here we are taking the clean, imputed datasets for all four surveys, running
#' rasch models on them, evaluating them with a battery of tests, and getting
#' a heap of outputs, figures, fit stats, etc.

#' Inputs:
#'    1. A list of all three clean, imputed, surveys
#'    2. Vector of all REBL items to make it easier to pull only those



# Load and Wrangle Data ---------------------------------------------------


pacman::p_load(dplyr,
               purrr,
               eRm,
               skimr,
               furrr,
               parallelly,
               tictoc)

# Load REBL item names to only grab those for Rasch modeling
all_rebl_items <- readRDS('5_objects/all_rebl_item_names.rds')

# Load functions for evaluating Rasch model and getting outputs
source('3_functions/rasch_test_functions.R')
source('3_functions/get_rasch_outputs.R')

# Load surveys 1, 2a, 2b (but not 3 because it is the test set)
surveys <- readRDS('2_clean/all_surveys_imputed.rds') %>% 
  .[! names(.) %in% 'survey_3']

# Convert to matrix and keep only the rebl items from each survey
surveys_rebl <- map(surveys, ~ {
  .x %>% 
    select(any_of(all_rebl_items)) %>% 
    as.matrix()
})



# CML ---------------------------------------------------------------------


# Run CML model on all surveys using eRm
# Running this in parallel.

# Export objects and packages to workers, set up workers, run models, end
config <- furrr_options(globals = 'surveys_rebl',
                        packages = c('eRm'))
plan(multisession, workers = availableCores(omit = 1))
rasch_models <- future_map(surveys_rebl, RM, .options = config)
plan(sequential)



# Evaluate Rasch Models ---------------------------------------------------


# Run suite of tests on Rasch models. This also runs in parallel
all_tests <- test_rasch_model(surveys, all_rebl_items, rasch_models)
# ~ 10 minute run time



# Outputs -----------------------------------------------------------------


# Folder paths for plots and csvs
plot_path <- '7_plots/cml/all_items/'
csv_path <- '6_outputs/cml/all_items/'

results <- get_rasch_outputs(
  models = rasch_models,
  surveys = surveys,
  plot_path = plot_path,
  csv_path = csv_path,
  rebl_items = all_rebl_items
)
# PNGs and CSVs are saved to folder paths automatically

get_str(results)
# Contains person fit and item fit dataframes for each survey



# Save and clear ----------------------------------------------------------


# Save all tests in one list
saveRDS(all_tests, '5_objects/cml/all_items/all_model_tests.rds')

# All model outputs in one list:
saveRDS(rasch_models, '5_objects/cml/all_items/rasch_models.rds')

clear_data()
