# Imputation
# 2024-08-21 update

#' Using missForest to impute prepped datasets. Note that for surveys 1, 2a, and
#' 2b, the predictors are all REBL items, demographics, and enviro scales (for
#' survey 1). In Survey 3, we are only working with the 24 final REBL items and
#' demographics. In the next script, we will throw out the imputed demographics
#' and replace them with the unimputed demograhics because there really isn't a
#' way to choose which variables get imputed in this function. They are either
#' in the dataset or not. This is essentially the same as making them 
#' predictors, but not imputing them.

# Inputs: 
# 1. All prepped survey datasets (all_surveys_prepped.rds)

# Outputs:
# 1. List of full outputs from all runs of missForest on all surveys. This will
#   then be wrangled in the wrangle_imputed_data.R script to get nice clean
#   datasets for analysis. Also includes a copy of the tuning grid and OOB
#   error to report. 



# Load Data ---------------------------------------------------------------


pacman::p_load(
  dplyr,
  missForest,
  foreach,
  doParallel,
  parallelly,
  doParallel,
  doRNG,
  purrr,
  furrr,
  tictoc,
  skimr
)

# Load miss_forest function
source('3_functions/run_miss_forest.R')

# Load all surveys
surveys <- readRDS('5_objects/cleaning/all_surveys_excluded.rds')

# Vector of 'front matter' - qualtrics things. Will use this later to remove 
# them from surveys.
front_matter <- surveys[[1]] %>% 
  select(StartDate:prolificID) %>% 
  names()



# Prep All Surveys --------------------------------------------------------
## Reduce to Relevant Columns ----------------------------------------------


# Use matching pattern to remove groups of questions that shouldn't be here
# for this, like EEWE questions. We don't want them either to be imputed NOR to 
# be used as predictors to impute other variables. 
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
    '^att[0-9]',
    'psychDist',
    '^pd[0-9]',
    '^eewe[0-9]',
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
    select(-any_of(front_matter),
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
#' survey 2, foodOwnLunch does fine.

# Remove bad items from survey 1
surveys_vars_to_impute[[1]] <- surveys_vars_to_impute[[1]] %>% 
  select(-any_of(bad_items))

map(surveys_vars_to_impute, get_str)



## Clean up DF -------------------------------------------------------------


# Convert everything to factor for missForest, and convert to DF (not tibble)
surveys_ready_to_impute <- map(surveys_vars_to_impute, ~ {
  .x |> 
      mutate(across(everything(), as.factor)) |> 
      as.data.frame()
})
map(surveys_ready_to_impute, get_str)



# Imputation Runs ---------------------------------------------------------


# Relatively small tuning grid
tuning_grid <- expand.grid(
  mtry = c(7, 10, 15),
  ntree = c(100, 250, 500)
)

tic()
registerDoParallel(cores = availableCores(omit = 1))
getDoParWorkers()
registerDoRNG(seed = 1.618)
foreach(i = 1:3) %dorng% sqrt(i)

outputs <- imap(surveys_ready_to_impute, \(survey, name) {
  print_time(paste0('Starting ', name, ' at:'))
  result <- map(1:nrow(tuning_grid), \(row) {
    missForest(
      xmis = survey,
      ntree = tuning_grid$ntree[row],
      mtry = tuning_grid$mtry[row],
      variablewise = TRUE,
      verbose = FALSE,
      parallelize = 'variables'
    )
  })
  print_time('\nDone!')
  return(result)
})

stopImplicitCluster()
toc()



# Imputation Runs 2 ---------------------------------------------------------


# Running with just ntree = 500 and mtry = 15 to save time to make it easier
# to reproduce.
tic()
registerDoParallel(cores = availableCores(omit = 1))
getDoParWorkers()
registerDoRNG(seed = 1.618)
foreach(i = 1:3) %dorng% sqrt(i)

print_time('Starting imputation run at:')
outputs <- imap(surveys_ready_to_impute, \(survey, name) {
  print_time(paste0('Starting ', name, ' at:'))
  result <- missForest(
    xmis = survey,
    ntree = 500,
    mtry = 15,
    variablewise = TRUE,
    verbose = FALSE,
    parallelize = 'variables'
  )
  print_time('\nDone!')
  return(result)
})

stopImplicitCluster()
toc()
# 8 minutes



# Save and Clear ----------------------------------------------------------


# Save full output, all imputations, with errors and everything
saveRDS(outputs, '5_objects/imputation/all_survey_imp_outputs.rds')

# Also going to save the tuning grid to be used in next script
saveRDS(tuning_grid, '5_objects/imputation/quick_grid.rds')

# Clear environment of objects
clear_data()
