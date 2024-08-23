#' REBL Table of Contents
#' 2024-08-23 update

#' This is the table of contents for the REBL repo. See README.md for 
#' details on navigation and repo structure. 

#' Scripts are organized in the document outline  Use `CTRL / CMD + Left Click`
#' on a file path to open it, or put the cursor on the file path and hit `F2`. 

#' Run the the housekeeping script before doing anything else. It loads helper
#' functions that are used throughout the project.



# Housekeeping ------------------------------------------------------------


# Startup script - run this first
source('4_scripts/housekeeping.R')

# README file
knitr::knit('README.rmd')



# Main Workflow -----------------------------------------------------------
## Cleaning ----------------------------------------------------------------


# Renaming columns for all surveys
source('4_scripts/cleaning/all_surveys_rename_vars.R')

# Data Prep
source('4_scripts/cleaning/all_surveys_data_prep.R')

# Exclusions for all surveys
source('4_scripts/cleaning/all_survey_exclusions.R')



## Missing Data ------------------------------------------------------------


# Explore missing data
'4_scripts/cleaning/explore_missing_data.R'

# Imputation with missForest across all surveys
source('4_scripts/imputation/imputation.R')

# Wrangle imputation runs - get clean datasets for analysis
source('4_scripts/imputation/wrangle_imputed_data.R')



## Preliminary Model -------------------------------------------------------


#' First we just run each of the IRT models with all items on surveys 1, 2a, and
#' 2b, but not survey 3. We are holding survey 3 aside as a validation set, so 
#' not doing any preliminary things with it.
source('4_scripts/rasch_modeling.R')



## Item Reduction ----------------------------------------------------------


# Conditional Item Fit iterations
source('4_scripts/cml/item_reduction/conditional_item_fit.R')

# Stepwise LR reductions
source('4_scripts/cml/item_reduction/stepwise_lr_tests.R')

# Reduction by consistency (and qualitative cuts at the end)
# This is where we define the final scale!
source('4_scripts/cml/item_reduction/item_reduction_consistency.R')



## Final Model -------------------------------------------------------------


# Rasch model with final items and run tests of invariance, unidimensionality.
# Bringing test_set (survey 3) back into the mix here
source('4_scripts/cml/final_items/rasch_model_and_tests.R')

# Rasch-related outputs: plots, fits, tables, cov matrix, etc.
source('4_scripts/cml/final_items/rasch_outputs.R')

# Test linking and rescaling between 2a and 2b, plots of reliability, and also
# making clean data frames of each survey. Most anything that involves comparing
# 2a and 2b lives here.
source('4_scripts/cml/final_items/test_linking.R')



## Other Analyses and Outputs ----------------------------------------------


# Regression and correlations with donation measure
source('4_scripts/cml/final_items/regressions.R')

#' Any other outputs for paper: exclusions, missForest info, demographics
source('4_scripts/other_outputs.R')
