#' Rasch Model and Tests
#' 2024-08-24 update
#' 
#' In this script we take the final set of 24 items (or whatever is given), run
#' the final rasch model, and check fit and assumptions, including:
#'  1. PCAR to check for unidimensionality
#'  2. LR test for measurement invariance across median split and demo splits
#'  3. gofIRT tests - R2, ROC, accuracy, specificity, etc.
#'  4. Separation Reliability
#'  5. Local Independence
#'  
#'  Notes:
#'  - Individual tests are done in each section first, or skip to the end to 
#'      test everything at once.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  eRm,
  pwrRasch,
  psych,
  stringr,
  DescTools,
  purrr
)

# Load wrappers and convenience functions for Rasch tests
source('3_functions/rasch_test_functions.R')

# Load clean surveys 2a, 2b, and 3
surveys <- readRDS('2_clean/all_surveys_imputed.rds') %>% 
  .[!names(.) %in% 'survey_1']

# Names of REBL items (26 with consistent items, 22 without)
final_items <- readRDS('2_clean/rebl_items_final.rds')



# Rasch Model -------------------------------------------------------------


#' Run RM with final set of items. Will use this model in tests below. Note that
#' we are doing this on BOTH survey 2a and 2b at once, keeping results as a 
#' list.
models <- map(surveys, ~ {
  .x %>% 
    select(all_of(final_items)) %>% 
    RM()
})

map(models, get_str)



# gofIRT ------------------------------------------------------------------


# Series of tests from Mair et al 2008. Not a lot of guidelines on 
# interpretation though.
gof <- map(models, ~ {
  .x %>%
    person.parameter() %>%
    gofIRT()
})

map(gof, summary)



# PCAR --------------------------------------------------------------------


#' Test of unidimensionality. First eigenvalue below 2.0 means there is no 
#' pattern to the residuals, thus single dimension. See Boone and Staver 2020
#' Chapter 2, Aryadoust et al 2021. 
#' Methods from: https://bookdown.org/chua/new_rasch_demo2/MD-fit.html#evaluating-measurement-quality-from-the-perspective-of-rasch-measurement-theory

# How many factors to extract? Looking at both 2a and 3
map(list(surveys$survey_2a, surveys$survey_3), \(survey) {
  results <- list()
  results$vss <- survey %>% 
    select(all_of(final_items)) %>% 
    VSS(n = 5, rotate = 'Promax')
  results$parallel <- survey %>% 
    select(all_of(final_items)) %>% 
    fa.parallel()
  return(results)
})
# Suggets up to 5f/4c for 2a, 4f/3c for 3

# Let's go with 4 components - again using survey 3 as test set, not diagnostic
pcar <- map(models, ~ test_uni_pcar(.x, rotate = 'Promax', n_factors = 4))
pcar[[1]]
pcar[[2]]
pcar[[3]]

# Check rotation
pcar[[1]]$rotation
# Promax is oblique, good good



# LR Tests -----------------------------------------------------------------


# Test for measurement invariance. Default is median split of raw score
(lr <- map(models, LRtest))

# LR tests for invariance by various splits. Cutpoints shown in outputs. Can 
# only do these on survey 2a because it has demographics.
(lr_gender <- test_lr_gender(surveys$survey_2a, final_items))
(lr_income <- test_lr_income(surveys$survey_2a, final_items))
(lr_rurality <- test_lr_rurality(surveys$survey_2a, final_items))

lr_tests <- map(list(surveys$survey_2a, surveys$survey_3), \(survey) {
  out <- list()
  out$lr_gender <- test_lr_gender(survey, final_items)
  out$lr_income <- test_lr_income(survey, final_items)
  out$lr_rurality <- test_lr_rurality(survey, final_items)
  return(out)
})

get_str(lr_tests)



# Get Fits ----------------------------------------------------------------


# Plain old item fits
fits <- map(models, get_fits)
get_str(fits)



# Separation Reliability --------------------------------------------------


#' See Aryadoust et al 2021 for an insufficient explanation. > 0.8 is ideal
#' Note that the documentation advises against using this. May not be necessary
#' to include.
map(models, ~ {
  .x %>% 
    person.parameter() %>% 
    SepRel()
})



# Local Independence ------------------------------------------------------


#' Residuals should be uncorrelated once the latent factor is accounted for.
#' See Aryadoust et al 2021. Haven't found much in the way of guidelines for 
#' how many correlated residuals are reasonable.
q3 <- map(surveys, ~ test_local_dependence(.x, final_items))
map(q3, get_str)



# Everything --------------------------------------------------------------


# Run all tests on both surveys at once. This runs in parallel
all_tests <- test_rasch_model(surveys, final_items, models)



# Maps --------------------------------------------------------------------


# Pathway and item maps
iwalk(surveys[names(surveys) %in% c('survey_2a', 'survey_3')], \(survey, name) {
  model <- survey %>% 
    select(all_of(final_items)) %>% 
    RM()
  
  plotPWmap(model, mainitem = name)
  plotPImap(model, sorted = TRUE, main = name)
})
# survey 3 has some clumping, but fits better than 2a



# Save and Clear ----------------------------------------------------------


# Rasch model with final items
saveRDS(models, '5_objects/cml/final_items/rasch_model_final_items.rds')

# Save Rasch tests - to be put into paper
saveRDS(all_tests, '6_outputs/cml/final_items/rasch_tests.rds')

clear_data()
