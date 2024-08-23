# Imputation
# 2024-08-07 update

# Using missForest to impute prepped datasets

# Inputs: 
# 1. All prepped survey datasets (all_surveys_prepped.rds)

# Outputs:
# 1. List of full outputs from all runs of missForest on all surveys. This will
#   then be wrangled in the wrangle_imputed_data.R script to get nice clean
#   datasets for analysis.



# Load Data ---------------------------------------------------------------


pacman::p_load(dplyr,
               missForest,
               foreach,
               doParallel,
               parallelly,
               doParallel,
               doRNG,
               purrr,
               furrr,
               tictoc,
               skimr)

# Load miss_forest function
source('3_functions/run_miss_forest.R')

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
  select(-any_of(bad_items))

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

# First a small test grid to make sure things work
mini_grid <- expand.grid(
  mtry = c(5, 6),
  ntree = c(25, 50)
)

# But also a bigger grid for the real run
full_grid <- expand.grid(
  mtry = c(7, 10, 15, 20),
  ntree = c(100, 250, 500, 1000)
)

# Check number of cores
(n_cores <- availableCores(omit = 1))
# Will use 1 less than we have to keep some capacity to do other things



# # Mini Par Vars Varwise -----------------------------------------------
# 
# 
# # Make backend for parallel processing with one less core than available
# cluster <- makeCluster(n_cores - 1)
# registerDoParallel(cluster)
# 
# # Add a timer for funsies
# print_time()
# tic()
# 
# imputed_surveys <- map2(
#   surveys_ready_to_impute, names(surveys_ready_to_impute), ~ {
# 
#     cat('\n\n======= Starting',
#         .y,
#         'at',
#         format(Sys.time(), '%I:%M:%S %p'),
#         '=======\n')
# 
#     run_miss_forest(
#       df = .x,
#       tuning_grid = mini_grid,
#       parallelize = 'variables',
#       variablewise = TRUE,
#       verbose = FALSE
#     )
#   })
# 
# # End Timer
# toc()
# # 129 seconds with parallel vars
# # 121 seconds second time
# 
# # End parallel backend
# stopImplicitCluster()
# 
# get_str(imputed_surveys)
# 
# 
# 
# # Better Test -------------------------------------------------------------
# 
# 
# # Test parallel RNG (From Stekhoven 2011)
# print_time()
# registerDoParallel(cores = n_cores)
# getDoParWorkers()
# registerDoRNG(seed = 1.618)
# foreach(i = 1:3) %dorng% sqrt(i)
# 
# tic()
# r1 <- imap(surveys_ready_to_impute[-1], ~ {
#   print_time(paste('Starting', .y, 'at'))
#   run_miss_forest(
#     df = .x,
#     tuning_grid = mini_grid,
#     parallelize = 'variables',
#     variablewise = TRUE,
#     verbose = FALSE,
#     seed = NULL
#   )
# })
# toc()
# stopImplicitCluster()
# 
# get_str(all_imputations)
# 
# 
# ## Compare -----------------------------------------------------------------
# 
# 
# # Save this to compare to other runs
# saveRDS(all_imputations, 'temp/test_run_2.rds')
# 
# r1 <- readRDS('temp/test_run_1.rds')
# r2 <- readRDS('temp/test_run_2.rds')
# 
# identical(r1, r2)
# get_str(r1)
# 
# 
# get_str(r1$survey_3[[1]]$OOBerror)
# identical(
#   r1$survey_3[[1]]$OOBerror,
#   r2$survey_3[[1]]$OOBerror
# )
# Comparison(
#   r1$survey_3[[1]]$OOBerror,
#   r2$survey_3[[1]]$OOBerror
# )
# 
# df <- data.frame(
#   r1$survey_3[[1]]$OOBerror,
#   r2$survey_3[[1]]$OOBerror
# )
# 
# # Compare values
# identical(
#   r1$survey_3[[1]]$ximp,
#   r2$survey_3[[1]]$ximp
# )
# all.equal(
#   r1$survey_3[[3]]$ximp,
#   r2$survey_3[[3]]$ximp
# )
# 

# # Single Test -------------------------------------------------------------
# 
# set.seed(42)
# out2 <- run_miss_forest(
#   df = surveys_ready_to_impute$survey_3,
#   tuning_grid = mini_grid,
#   parallelize = 'no',
#   variablewise = TRUE,
#   verbose = FALSE,
#   seed = 42
# )
# 
# get_str(out1)
# out1[[1]]$OOBerror
# out1[[2]]$OOBerror
# 
# 
# # REWORK ------------------------------------------------------------------
# 
# 
# set.seed(42)
# out2 <- missForest(surveys_ready_to_impute$survey_3,
#            mtry = mini_grid$mtry[1],
#            ntree = mini_grid$ntree[1],
#            verbose = TRUE,
#            variablewise = TRUE)
# out1$OOBerror
# out2$OOBerror
# identical(
#   out1$OOBerror,
#   out2$OOBerror
# )
# 
# # OKAY THIS WORKS
# 
# 
# 
# ## MAP IT ------------------------------------------------------------------
# 
# 
# set.seed(42)
# out2 <- map(seq_along(1:nrow(mini_grid)), ~ {
#   missForest(
#     surveys_ready_to_impute$survey_3,
#     mtry = mini_grid$mtry[.x],
#     ntree = mini_grid$ntree[.x],
#     verbose = TRUE,
#     variablewise = TRUE
#   )
# })
# 
# out1[[1]]$OOBerror
# out2[[1]]$OOBerror
# identical(
#   out1[[1]]$OOBerror,
#   out2[[1]]$OOBerror
# )
# # STILL WORKS
# 
# 
# ## PARALLEL ----------------------------------------------------------------
# 
# 
# print_time()
# registerDoParallel(cores = n_cores)
# getDoParWorkers()
# registerDoRNG(seed = 1.618, once = FALSE)
# foreach(i = 1:3) %dorng% sqrt(i)
# 
# tic()
# out2 <- imap(surveys_ready_to_impute[2:3], ~ {
#   print_time(paste('Starting', .y, 'at'))
#   run_miss_forest(
#     df = .x,
#     tuning_grid = mini_grid,
#     parallelize = 'variables',
#     variablewise = TRUE,
#     verbose = FALSE,
#     seed = NULL
#   )
# })
# toc()
# stopImplicitCluster()
# 
# get_str(out1)
# out1$survey_2a[[1]]$OOBerror
# out2$survey_2a[[1]]$OOBerror
# # Nope
# 
# 
# 
# 
# # EXAMPLE -----------------------------------------------------------------
# 
# 
# # Install and load necessary libraries
# library(doParallel)
# library(foreach)
# 
# # Register the parallel backend
# no_cores <- detectCores() - 1
# cl <- makeCluster(no_cores)
# registerDoParallel(cl)
# 
# # Set up the reproducible random seed
# set.seed(12345)
# seeds <- sample.int(1e6, no_cores)
# 
# # Define the function
# my_function <- function(seed, x) {
#   set.seed(seed + x)  # Set a unique seed for each iteration
#   rnorm(1)            # Generate a random number
# }
# 
# # Run the parallel computation with foreach
# results <- foreach(i = 1:10, .combine = c) %dopar% {
#   seed <- seeds[(i %% no_cores) + 1]
#   my_function(seed, i)
# }
# 
# # Stop the cluster
# stopCluster(cl)
# 
# # Print results
# print(results)
# 
# 
# # FURRR -------------------------------------------------------------------
# 
# 
# # config <- furrr_options(globals = 'cml_models',
# #                         packages = c('purrr', 'iarm', 'eRm', 'dplyr'),
# #                         seed = 42)
# 
# set.seed(42)
# map(1:10, ~ rnorm(1))
# # Works fine setting it once like this
# 
# plan(multisession, workers = availableCores(omit = 1))
# config <- furrr_options(seed = 42)
# out2 <- future_map(1:10, ~ rnorm(1), .progress = TRUE, .options = config)
# # THIS WORKS
# 
# out1
# out2
# identical(out1, out2)
# 
# 
# 
# ## Try with furrr ----------------------------------------------------------
# 
# 
# 
# plan(multisession, workers = availableCores(omit = 1))
# config <- furrr_options(seed = 42)
# 
# out2 <- future_map(surveys_ready_to_impute[2:3], ~ {
#   missForest(
#     xmis = .x,
#     mtry = 3,
#     ntree = 3,
#     # mtry = mini_grid$mtry[.x],
#     # ntree = mini_grid$ntree[.x],
#     verbose = TRUE,
#     variablewise = TRUE
#   )
# }, .options = config, .progress = TRUE)
# 
# plan(sequential)
# 
# get_str(out1)
# identical(
#   out1$survey_2a$OOBerror,
#   out2$survey_2a$OOBerror
# )
# # STILL WORKS
# 
# 
# 
# ## Now with real grid ------------------------------------------------------
# 
# 
# tic()
# plan(multisession, workers = availableCores(omit = 1))
# config <- furrr_options(seed = 42)
# 
# out2 <- future_map(surveys_ready_to_impute[2:3], ~ {
#   map(1:nrow(mini_grid), \(row) {
#     missForest(
#       xmis = .x,
#       mtry = mini_grid$mtry[row],
#       ntree = mini_grid$ntree[row],
#       verbose = TRUE,
#       variablewise = TRUE
#     )
#   })
# }, .options = config, .progress = TRUE)
# 
# plan(sequential)
# tic()
# 
# get_str(out1$survey_2a[[1]]$OOBerror)
# 
# identical(
#   out1$survey_2a[[1]]$OOBerror,
#   out2$survey_2a[[1]]$OOBerror
# )
# 
# get_str(out2)
# out1$survey_2a[[2]]$OOBerror
# out2$survey_2a[[2]]$OOBerror
# # THIS WORKS
# 
# 
# Test Run ---------------------------------------------------------------


print_time()
tic()
plan(multisession, workers = availableCores(omit = 1))
config <- furrr_options(seed = 42)

out2 <- future_map(surveys_ready_to_impute[2:3], ~ {
  map(1:nrow(mini_grid), \(row) {
    missForest(
      xmis = .x,
      mtry = mini_grid$mtry[row],
      ntree = mini_grid$ntree[row],
      verbose = FALSE,
      variablewise = TRUE
    )
  })
}, .options = config, .progress = TRUE)

plan(sequential)
tic()

identical(out1, out2)



# ***REAL THING --------------------------------------------------------------


# Doing each survey in parallel, not each set of imputations
print_time('Starting whole shebang at:')
tic()
plan(multisession, workers = availableCores(omit = 1))
config <- furrr_options(seed = 42)

all_imputations <- future_map(surveys_ready_to_impute, \(survey) {
  map(1:nrow(full_grid), \(row) {
    missForest(
      xmis = survey,
      mtry = full_grid$mtry[row],
      ntree = full_grid$ntree[row],
      verbose = FALSE,
      variablewise = TRUE
    )
  })
}, .options = config, .progress = TRUE)

plan(sequential)
toc()
print_time('Finished whole shebang at:')
# 3 hours...


get_str(all_imputations)
saveRDS(all_imputations, 'temp/all_imputations.rds')
saveRDS(all_imputations, '5_objects/imputation/new_imputations/all_imputations.rds')




# Faster Run --------------------------------------------------------------


# Make this more manageable

# First a small test grid to make sure things work
mini_grid <- expand.grid(
  mtry = c(5, 6),
  ntree = c(25, 50)
)

# But also a bigger grid for the real run
full_grid <- expand.grid(
  mtry = c(7, 10, 15),
  ntree = c(100, 250, 500)
)

# Check number of cores
(n_cores <- availableCores(omit = 1))

print_time('Starting whole shebang at:')
tic()
plan(multisession, workers = availableCores(omit = 1))
config <- furrr_options(seed = 42)

all_imputations <- future_map(surveys_ready_to_impute, \(survey) {
  map(1:nrow(full_grid), \(row) {
    missForest(
      xmis = survey,
      mtry = full_grid$mtry[row],
      ntree = full_grid$ntree[row],
      verbose = FALSE,
      variablewise = TRUE
    )
  })
}, .options = config, .progress = TRUE)

plan(sequential)
toc()
print_time('Finished whole shebang at:')



# Try Simple Parallel -----------------------------------------------------



registerDoParallel(cores = n_cores)
getDoParWorkers()
registerDoRNG(seed = 1.618)
foreach(i = 1:3) %dorng% sqrt(i)

ntree <- 2000
mtry <- 25

{
  tic()
    seq <- missForest(
      xmis = surveys_ready_to_impute[[4]],
      ntree = ntree,
      mtry = mtry,
      variablewise = TRUE,
      verbose = FALSE
    )
  toc()
}

{
  tic()
  registerDoParallel(cores = n_cores)
  getDoParWorkers()
  registerDoRNG(seed = 1.618)
  foreach(i = 1:3) %dorng% sqrt(i)
    par <- missForest(
      xmis = surveys_ready_to_impute[[4]],
      ntree = ntree,
      mtry = mtry,
      variablewise = TRUE,
      verbose = FALSE,
      parallelize = 'forests'
    )
  stopImplicitCluster()
  toc()
}  

# 7.5s 7.78s
# 10.9s 12s
# 12.21s 10.82
# 13.5s 11.4s (at 1000 and 25)
# 20.7s 15.9s
# 28.9s 19.8s (2000 and 25 variablewise) 18.1s (forest)

# Definitely does better 


# Check consistency parallel ----------------------------------------------


tic()
registerDoParallel(cores = availableCores(omit = 1))
getDoParWorkers()
registerDoRNG(seed = 1.618)
foreach(i = 1:3) %dorng% sqrt(i)
par4 <- missForest(
  xmis = surveys_ready_to_impute[[4]],
  ntree = 100,
  # mtry = mtry,
  variablewise = TRUE,
  verbose = FALSE,
  parallelize = 'variables'
)
stopImplicitCluster()
toc()

get_str(par1)

identical(par1, par2)
# Works

identical(par2, par3)
# Noooope not when we do by variables

identical(par3, par4)
# But doing variables twice works fine


# Consistency with mapping ------------------------------------------------


tic()
registerDoParallel(cores = availableCores(omit = 1))
getDoParWorkers()
registerDoRNG(seed = 1.618)
foreach(i = 1:3) %dorng% sqrt(i)

par2 <- imap(surveys_ready_to_impute[-1], \(survey, name) {
  print_time(paste0('Starting ', name, ' at:'))
  missForest(
    xmis = survey,
    ntree = 100,
    # mtry = mtry,
    variablewise = TRUE,
    verbose = FALSE,
    parallelize = 'variables'
  )
  print_time('\nDone!')
})

stopImplicitCluster()
toc()

identical(par1, par2)
# Works!!



# Quick Run No 1 ----------------------------------------------------------


full_grid <- expand.grid(
  mtry = c(7, 10, 15),
  ntree = c(100, 250, 500)
)

tic()
registerDoParallel(cores = availableCores(omit = 1))
getDoParWorkers()
registerDoRNG(seed = 1.618)
foreach(i = 1:3) %dorng% sqrt(i)

quick_run <- imap(surveys_ready_to_impute[2], \(survey, name) {
  print_time(paste0('Starting ', name, ' at:'))
  result <- map(1:nrow(full_grid), \(row) {
    missForest(
      xmis = survey,
      ntree = full_grid$ntree[row],
      mtry = full_grid$mtry[row],
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
# 413 seconds for surveys 2:4, full grid of 9

# identical(par1, par2)
# Works!!

# 70s for extra short run with just survey [2]
saveRDS(quick_run, '5_objects/imputation/new_imputations/all_imputations_quick.rds')



# # Full Par Vars Varwise -----------------------------------------
# 
# 
# # Make backend for parallel processing with one less core than available
# cluster <- makeCluster(n_cores - 1)
# registerDoParallel(cluster)
# 
# # Add a timer for funsies
# tic()
# 
# imputed_surveys <- map2(
#   surveys_ready_to_impute, names(surveys_ready_to_impute), ~ {
# 
#     cat('\n\n======= Starting',
#         .y,
#         'at',
#         format(Sys.time(), '%I:%M:%S %p'),
#         '=======')
# 
#     run_miss_forest(
#       df = .x,
#       tuning_grid = full_grid,
#       parallelize = 'variables',
#       variablewise = TRUE,
#       verbose = FALSE,
#       seed = 42
#     )
#   })
# 
# # End Timer
# toc()
# # Take 1: 1 hr 3 minutes for survey 1. 1 hr 20 minutes total
# # Take 2: 1 hr 30 minutes total
# 
# # End parallel backend
# stopImplicitCluster()
# 
# get_str(imputed_surveys)



# Save and Clear ----------------------------------------------------------


# Save full output, all imputations, with errors and everything
saveRDS(imputed_surveys, '5_objects/imputation/all_survey_imp_outputs.rds')

# Also going to save the tuning grid to be used in next script
saveRDS(full_grid, '5_objects/imputation/full_grid.rds')

# Clear environment of objects
clear_data()



# Link to Imputation Testing ----------------------------------------------


# See here for imputation testing:
'4_scripts/imputation/imputation_testing.R'