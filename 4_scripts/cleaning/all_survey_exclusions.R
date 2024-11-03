# all_survey_exclusions.R
# 2024-08-07 update

# Taking renamed data and excluding cases based on date, attention checks, 
# recaptcha, duration, and eventually NAs to filter out bad responses.

# Inputs: all prepped surveys from all_surveys_prepped.rds

# Outputs: surveys filtered by quality cases: all_surveys_excluded.rds. Ready
# for imputation



# Load Data ---------------------------------------------------------------


pacman::p_load(dplyr,
               stringr,
               lubridate,
               tibble,
               purrr)

# Load function for excluding cases. More info about use in that script.
source('3_functions/exclude_cases.R')

# Load list of all renamed surveys
surveys <- readRDS('5_objects/cleaning/all_surveys_prepped.rds')



# Survey 1 ----------------------------------------------------------------


# Completion is much worse in survey 1 than 2a or 2b. Cutoff is low for now?

#' Note that there were a couple of questions that were missing in the pilot,
#' including buying a second hand item and checking tire pressure. For now we 
#' are throwing out pilot? Can revisit this. Pilot starts 2024-05-11, full study
#' starts 2024-07-15

#' Reasonable NA Count
#'    1 for BIPOC because it double counts +
#'    2 for genderOtherText and raceOtherText 
#'    = 3 reasonable NAs

surveys[[1]] <- exclude_cases(
  df = surveys[[1]],
  start_date = '2022-07-15',
  end_date = '2022-07-17',
  bs_check_1 = NULL,
  bs_check_2 = NULL,
  vegan_meat_bs_check = FALSE,
  attention_check_cutoff = 2,
  duration_percentile = 0.05,
  reasonable_na_count = 3,
  completion_cutoff = 0.75
)



# Survey 2a ---------------------------------------------------------------


#' Note that the pilot is hinky here. Several different questions were missing,
#' including ziplocbags and foodForage. Most importantly, there were only 2 
#' attention checks in the pilot, while there were 4 in the full survey. So, 
#' we are ditching the pilot here for now. Full study starts on 2024-05-02.

#' Reasonable NA Count
#'    2 for genderOtherText and raceOtherText +
#'    1 for BIPOC because it would double count race
#'    = 3 reasonable NAs

surveys[[2]] <- exclude_cases(
  df = surveys[[2]],
  start_date = '2023-05-02',
  end_date = '2023-05-16',
  bs_check_1 = 'foodForage_',
  bs_check_2 = 'foodForageBSCheck',
  vegan_meat_bs_check = TRUE,
  attention_check_cutoff = 3,
  duration_percentile = 0.05,
  reasonable_na_count = 3,
  completion_cutoff = 0.95
)



# Survey 2b ---------------------------------------------------------------


# Surveys still come in through September?? Think about a reasonable cutoff...
# Note this for the EWE questions. Not the same time of year. 

#' Also, the pilot study here was missing a bunch of EWE questions. Fine for 
#' REBL, but will have to deal with it in the EWE study, probably by throwing
#' out the pilot.

#' Reasonable NA Count
#'    12 specific EWE questions +
#'    4 specific EWE attribution questions +
#'    2 for genderOtherText and raceOtherText +
#'    1 more for BIPOC because it would double count race +
#'    = 19 reasonable NAs

surveys[[3]] <- exclude_cases(
  df = surveys[[3]],
  start_date = '2023-04-20',
  end_date = '2050-01-01', 
  bs_check_1 = 'foodForage_',
  bs_check_2 = 'foodForageBSCheck',
  vegan_meat_bs_check = TRUE,
  attention_check_cutoff = 3,
  duration_percentile = 0.05,
  reasonable_na_count = 19,
  completion_cutoff = 0.95
)



# Survey 3 ----------------------------------------------------------------


#' Treating this test set differently than the others. Just going to filter 
#' down to the 24 final items, demographics, and whatever else we need to be 
#' able to exclude cases, like start date, end date, attention checks, BS
#' checks, meat checks. So the only reasonable NAs will then be gender_other and
#'race_other (2). 

# Pull in final list of 24 items
final_items <- readRDS('2_clean/rebl_items_final.rds')

# Select relevant columns
surveys[[4]] <- surveys[[4]] %>% 
  select(StartDate:prolificID,
         all_of(final_items),
         matches('attentionCheck|Forage|Beef|Meat|CowMilk'),
         age:last_col())

# filter after 2023-10-05 at 9am. First one at 9:19am
surveys[[4]] <- exclude_cases(
  df = surveys[[4]],
  start_date = '2023-10-05',
  end_date = '2024-01-01',
  bs_check_1 = 'foodForage_',
  bs_check_2 = 'foodForageBSCheck',
  vegan_meat_bs_check = TRUE,
  attention_check_cutoff = 3,
  duration_percentile = 0.05,
  reasonable_na_count = 2,
  completion_cutoff = 0.95
)

# Now remove all the other extraneous columns
surveys[[4]] <- surveys[[4]] %>% 
  select(prolificID, all_of(final_items), age:last_col())



# Wrap up -----------------------------------------------------------------


# Check it out
map(surveys, dim)
map(surveys, get_str)

# Get rid of attention checks and BS Check (but keep first forage question)
surveys <- map(surveys, ~ select(.x, -matches('attention|foodForageBSCheck')))

# Save and clear
saveRDS(surveys, '5_objects/cleaning/all_surveys_excluded.rds')

# Clear data objects
clear_data()