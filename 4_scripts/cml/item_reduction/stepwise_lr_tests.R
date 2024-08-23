#' stepwise LR tests
#' 2024-08-08 update
#' 
#' Using `eRm::stepwiseIt()` function to reduce number of REBL items that came
#' from conditional fits. We first have to rerun new Rasch models with that
#' reduced set of items though. Note that we are not running these item 
#' reduction scripts on survey 3 (test set). 
#' 
#' Inputs:
#'    1. Reduced set of items (38) from conditional fit procedure
#'    
#' Outputs:
#'    1. New `RM()` output for reduced set of items.
#'    2. A further reduced set of items to be explored for item consistency.



# Load Packages and Data --------------------------------------------------


pacman::p_load(dplyr,
               eRm,
               purrr,
               tictoc)


# Similar vector from 05-04 reductions to compare
rebl_items <- readRDS('5_objects/cml/item_reduction/rebl_items_cond_fit_38.rds')

# Load surveys 2a and 2b
surveys <- readRDS('2_clean/all_surveys_imputed.rds') %>% 
  .[names(.) %in% c('survey_2a', 'survey_2b')]



# Wrangle -----------------------------------------------------------------


# Reduce Surveys to just the rebl items from conditional fits
dat <- map(surveys, ~ select(.x, any_of(rebl_items)))

str(dat)


  
# Rasch Model -------------------------------------------------------------


# Run a new Rasch model with reduced set of items. Map over both surveys.
models <- map(dat, ~ {
  set.seed(42)
  RM(.x)
})

get_str(models)

# See if LR tests pass here:
map(models, LRtest)
# $survey_2a
# Andersen LR-test: 
#   LR-value: 65.026 
# Chi-square df: 37 
# p-value:  0.003 
# 
# $survey_2b
# Andersen LR-test: 
#   LR-value: 36.123 
# Chi-square df: 37 
# p-value:  0.51 

# Survey 2a fails, 2b passes

# Save these
saveRDS(models, '5_objects/cml/item_reduction/cond_fit_reduced_models.rds')



# Stepwise Elimination ----------------------------------------------------


# Map it over both surveys.
tic()
lr_outputs <- imap(models, ~ {
  print_time(paste0('Starting ', .y, ' at:'))
  stepwiseIt(.x,    
             criterion = list("LRtest"),
             verbose = TRUE,
             maxstep = NA)
})
toc()

# 2a: removed packPaperTowel_r, foodNonDairyMilk_
# 2b: removed nothing



# Explore LR Output -------------------------------------------------------


# Check it out
get_str(lr_outputs)
get_str(lr_outputs[[1]])

# Eliminated items
stepwise_lr_eliminations <- list(
  lr_outputs[[1]]$it.elim,
  lr_outputs[[2]]$it.elim
)

# Items from each survey
colnames(lr_outputs[[1]]$X)
colnames(lr_outputs[[2]]$X)

# Intersect to get shared items (in the event that some were removed)
shared_items <- intersect(colnames(lr_outputs[[1]]$X), 
                          colnames(lr_outputs[[2]]$X))
shared_items
# Down to 36



## Try Another LR Test -----------------------------------------------------


test_dat <- map(surveys, ~ select(.x, any_of(shared_items)))
map(test_dat, get_str)

test_models <- map(test_dat, RM)
(test_lr <- map(test_models, LRtest))
# $survey_2a
# Andersen LR-test: 
#   LR-value: 46.453 
# Chi-square df: 35 
# p-value:  0.093 
# 
# $survey_2b
# Andersen LR-test: 
#   LR-value: 30.391 
# Chi-square df: 35 
# p-value:  0.69

# Both pass here, but 2a is creeping pretty close to failing

# Check visual goodness of fit
map(test_lr, plotGOF)
# Both look great



# Save and Clear ----------------------------------------------------------


# Save these items
saveRDS(shared_items, '5_objects/cml/item_reduction/rebl_items_step_lr_36.rds')

# Keep the items eliminated from the process too
saveRDS(stepwise_lr_eliminations,
        '5_objects/cml/item_reduction/stepwise_lr_eliminations.rds')

# Save the lr outputs for posterity
saveRDS(lr_outputs, '5_objects/cml/item_reduction/step_lr_outputs.rds')

# Clear
clear_data()
