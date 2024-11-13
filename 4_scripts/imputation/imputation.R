# imputation
# 2024-11-13

# Using missForest to impute prepped datasets

# Inputs: 
# 1. All three prepped survey datasets (all_surveys_prepped.rds)

# Outputs:
# 1. List of full outputs from all runs of missForest on all three surveys.
#   This will then be wrangled in the wrangle_imputed_data.R script to
#   get nice clean datasets for analysis.

# Notes:
# 1. This takes around 1.5 hours to run with current tuning grid.
# 2. Currently (2024-04-09) we saved a PRELIM set of imputation outputs to 
#   work with. This will 100% require some tweaking as to what vars we want to 
#   impute and with which predictors. Moving ahead with this for now though.
# 3. See last section for things that can be improved in imputation.



# Load Data ---------------------------------------------------------------


pacman::p_load(
  dplyr,
  missForest,
  foreach,
  doParallel,
  purrr,
  tictoc,
  skimr
)

# Load helper functions
source('3_functions/run_miss_forest.R')
source('3_functions/helper_functions/print_time.R')

# Load all surveys
surveys <- readRDS('5_objects/cleaning/all_surveys_excluded.rds')



# Prep All Surveys --------------------------------------------------------

## Reduce to Relevant Columns ----------------------------------------------


map(surveys, get_str)
# And we need everything to be either factor or numeric for missForest
# Also missForest does not tolerate tibbles? Must be OG data frame

# Use matching pattern to remove groups of questions that shouldn't be here
# for this, like all ewe questions, 
matching_pattern <- paste(
  c(
    '^ewe',
    'ccBelief',     # ewe questions
    'efficacy5',
    'efficacy6',    # First 4 efficacies are Roberts 1996, last 2 are ewe stuff
    'enviroID4',    # First 3 enviroIDs are van der Werff, last 1 is ewe stuff
    '^zip$',
    'race\\w+',     # Removing race dummies (raceWhite), but keeping race
    'BIPOC',        # Redundant with race
    'attribution',
    'psychDist',
    '^worry\\d{1}', # Removing worry1, worry2, etc, but not ccWorry
    'socialNorms',
    'Other',        # Get rid of genderOther too - too many NAs
    'fashNatural',
    'fashSynthetic',
    'fashRecyclable',
    'fashShoesResolable'
  ),
  collapse = '|'
)

# Make a new set of survey data with just vars that are relevant for imputation.
# We don't want prolific ID or start date to be used to impute other vars, for
# example. We will keep some things here that we probably don't want to be 
# imputed, like demographics. We can still use them as predictors. They will get
# imputed, but we can combine imputed data back into this set so we leave race
# and such the same. 
surveys_vars_to_impute <- map(surveys, ~ {
  
  .x |> 
    select(-c(StartDate:prolificID),
           -any_of(matches(matching_pattern)))
})

map(surveys_vars_to_impute, get_str)



## Explore Missingness -----------------------------------------------------


# Skim for missing data. Items with very high missingness are not good for
# missForest, but also shouldn't be imputed anyway.
(skims <- map(surveys_vars_to_impute, skim))

# Get vector of just the ones that are < 60% complete
(bad_items <- map(skims, ~ {
  .x %>% 
    filter(complete_rate < 0.6) %>% 
    pull(skim_variable)
}) %>% 
  unlist() %>% 
  as.vector())

#' We will remove these both because they mess with missForest and also because
#' they will be trash items anyway. Note that they are only bad in survey 1. In 
#' survey 2, foodOwnLunch does fine. Weird that there are so many missing in 
#' survey 1.

# Remove bad items from survey 1

surveys_vars_to_impute[[1]] <- surveys_vars_to_impute[[1]] %>% 
  select(-bad_items)

map(surveys_vars_to_impute, get_str)



## Clean up DF -------------------------------------------------------------


# Now for the data to work with, we need them to be factor and a data frame,
# not a tibble. I've never seen this matter before!
surveys_ready_to_impute <- map(surveys_vars_to_impute, ~ {
  .x |> 
      mutate(across(everything(), as.factor)) |> 
      as.data.frame()
})
  
map(surveys_ready_to_impute, get_str)



# Prep for Parallel and Forests -------------------------------------------


# Make a tuning grid to try different combinations of hyperparameters and 
# select the one with the lowest estimated error. For now we are keeping this
# as a little baby grid to make sure things run smoothly before we finish
# fiddling with cleaning the data.

# Tuning grid
full_grid <- expand.grid(
  mtry = c(7, 10, 15, 20),
  ntree = c(100, 250, 500, 1000)
)

#' Set number of cores. Note that this is hard-coded to 7 to reproduce the 
#' original results.
n_cores <- 7



# Imputation --------------------------------------------------------------


# Make backend for parallel processing with one less core than available
cluster <- makeCluster(n_cores)
registerDoParallel(cluster)

tic()
imputed_surveys <- map2(
  surveys_ready_to_impute, names(surveys_ready_to_impute), ~ {

    print_time(paste('Starting', .y, 'at'))

    run_miss_forest(
      df = .x,
      tuning_grid = full_grid,
      parallelize = 'variables',
      variablewise = TRUE,
      verbose = FALSE,
      seed = 42
    )
  })

toc()
# Take 1: 1 hr 3 minutes for survey 1. 1 hr 20 minutes total
# Take 2: 1 hr 30 minutes total

# End parallel backend
stopImplicitCluster()

get_str(imputed_surveys)



# Save and Clear ----------------------------------------------------------


# Save full output, all imputations, with errors and everything
saveRDS(imputed_surveys, '5_objects/imputation/all_survey_imp_outputs.rds')

# Also going to save the tuning grid to be used in next script
saveRDS(full_grid, '5_objects/imputation/full_grid.rds')

# Clear environment of objects
remove_data_objects()



# Link to Imputation Testing ----------------------------------------------


# See here for imputation testing:
'4_scripts/imputation/imputation_testing.R'