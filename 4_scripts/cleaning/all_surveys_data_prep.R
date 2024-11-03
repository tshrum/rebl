# all_surveys_data_prep
# 2024-08-08 update

# Data Prep for all three surveys. Note that survey 1 is pretty different than 
# 2a, 2b, and 3. Keeping 1 mostly manual here... But others are smooth with 
# custom recode functions. We also take a look at missing data here and try to 
# decide reasonable cutoffs for NAs in the exclusions script.

# Inputs: List of all named surveys (1, 2a, 2b, 3)

# Outputs: List of all clean and recoded surveys ready for exclusions.



# Load Data ---------------------------------------------------------------


# Packages
pacman::p_load(readr,
               stringr,
               tibble,
               dplyr,
               lubridate,
               purrr)

# Load recode and remove seasonal functions
function_list <- list.files('3_functions', 
                            pattern = 'recode', 
                            full.names = TRUE)
invisible(map(function_list, source))

# Pull surveys after renaming, also remove Qualtrics trash in first row or two
# that either copies the column name or has some weird import gibberish
surveys <- readRDS('5_objects/cleaning/all_surveys_renamed.rds') %>% 
  map(~ filter(.x, str_detect(StartDate, 'Import|Start', negate = TRUE)))

map(surveys, get_str)



# Recode 1 ----------------------------------------------------------------


# Survey 1 doesn't recode nicely because it is very different than 2a and 2b.
# For now, doing what we can with the pre-made function and copying in the
# rest manually. This recodes REBL as 1/0 based on PEB and cleans up demos 
# (except for race - TBD on whether we want dummies or categorical).
get_str(surveys[[1]])

surveys[[1]] <- surveys[[1]] %>% 
  recode_rebl() %>% 
  recode_likert() %>% 
  recode_demos() %>% 
  recode_attention_checks() 

get_str(surveys[[1]])



# Recode 2a ---------------------------------------------------------------


# Use custom to functions to recode REBL as 1/0 based on PEB
# And also all Likert questions from 0 to 6 and binary 0 or 1
# Also clean up demographics
get_str(surveys[[2]])

surveys[[2]] <- surveys[[2]] |> 
  recode_rebl() |> 
  recode_likert() |> 
  recode_demos() %>% 
  recode_attention_checks()

get_str(surveys[[2]])



# Recode 2b ---------------------------------------------------------------


get_str(surveys[[3]])
# No demos here, so we just need rebl and likert

surveys[[3]] <- surveys[[3]] |> 
  recode_rebl() |>
  recode_likert() %>% 
  recode_attention_checks()

get_str(surveys[[3]])



# Recode 3 ----------------------------------------------------------------


get_str(surveys[[4]])

surveys[[4]] <- surveys[[4]] |> 
  recode_rebl() |>
  recode_likert() %>% 
  recode_demos() %>% 
  recode_attention_checks()



# Clean up Classes --------------------------------------------------------


# # Making sure all classes are what they should be
# map(surveys, get_str)
# 
# # Make StartDate and EndDate posix. But 3 is different than others.
# # First do 1, 2a, 2b:
# surveys[1:3] <- map(surveys[1:3], ~ mutate(
#   .x,
#   across(matches('Date'), ~ ymd_hms(.x))
# ))
# 
# # Now just survey 3
# surveys$survey_3 <- surveys$survey_3 %>%
#   mutate(across(matches('Date'), ~ mdy_hm(.)))

# All
surveys <- map(surveys, ~ {
  .x %>% 
    mutate(across(matches('Date'), ~ mdy_hm(.)))
})

# Make recaptcha numeric for all surveys
surveys <- map(surveys, ~ {
  mutate(.x, Q_RecaptchaScore = as.numeric(Q_RecaptchaScore))
})



# One-Offs ----------------------------------------------------------------

# Unique data prep shenanigans for individual surveys in this section. 


## Survey 1 Race -----------------------------------------------------------


# Let's also manually recode race for survey 1, since it didn't get covered in
# the recode_demos() function. It may cause problems if it is only 1 and NA,
# since there is no real variation. Let's make it 1 and 0 
surveys[[1]] <- surveys[[1]] |> 
  mutate(across(c(raceWhite:raceOther), 
                ~ case_match(
                  .x,
                  NA ~ 0,
                  .default = 1
                  )))

# Create a new twoOrMoreRaces dummy
# Then add a categorical race variable
surveys[[1]] <- surveys[[1]] %>%
  mutate(
    
    # First a twoOrMoreRace binary dummy
    twoOrMoreRaces = case_when(
      rowSums(
        select(.,
              all_of(
                c('raceWhite', 
                  'raceBlack', 
                  'raceHispanicLatino',
                  'raceAsian',
                  'raceNative',
                  'raceIslander',
                  'raceOther')))) >= 2 ~ 1,
      .default = 0
    ),
    
    # Now a categorical race variable
    race = case_when(
      twoOrMoreRaces == 1 ~ 'twoOrMoreRaces',
      raceWhite == 1 ~ 'white',
      raceBlack == 1 ~ 'black',
      raceHispanicLatino == 1 ~ 'hispanicLatino',
      raceAsian == 1 ~ 'asian',
      raceNative == 1 ~ 'native',
      raceIslander == 1 ~ 'islander',
      raceOther == 1 ~ 'other'
    )
  )



## BIPOC Variable ----------------------------------------------------------


# Add Binary BIPOC variable for 1, 2a, and 3. No demos in 2b.
surveys[names(surveys) != 'survey_2b'] <- map(
  surveys[names(surveys) != 'survey_2b'], 
  \(x) {
    x %>%
      mutate(BIPOC = case_when(is.na(race) ~ NA,
                               str_detect(race, 'hite') ~ 0,
                           .default = 1))
})
    
# Check them out
map(surveys, get_str)

# Check tables of race and bipoc
map(surveys[c(1, 2, 4)], ~ {
  get_table(.x$race)
  get_table(.x$BIPOC)
})



## Survey 2a Dictator Game ------------------------------------------------


# Recoding dictator game with 10 for PEB - all money for trees
# 0 for keeping all money
# This is gross code but whatever I guess. Actually it tears me up but I can't
# think of a better way other than writing out each response

responses <- sort(unique(surveys[[2]]$dictatorGame))

surveys[[2]] <- surveys[[2]] %>% 
  mutate(dictatorGame = case_match(
    dictatorGame,
    responses[1] ~ 0,
    responses[2] ~ 1,
    responses[3] ~ 10, # NOTE: this looks wrong but is right because alphabet
    responses[4] ~ 2,
    responses[5] ~ 3,
    responses[6] ~ 4,
    responses[7] ~ 5,
    responses[8] ~ 6,
    responses[9] ~ 7,
    responses[10] ~ 8,
    responses[11] ~ 9
  ))



# Remove Extraneous Columns -----------------------------------------------


# Check it out
map(surveys, get_str)

# Get vector of seasonal heating and cooling questions
# Indexed by first and last question in each survey (2a and 2b are same though)
seasonal_questions <- c(
  surveys[[1]] %>% select(season:homeFan) %>% names(),
  surveys[[2]] %>% select(homeHeatOrCool:homeFan) %>% names()
) %>% unique()

# Remove them from all surveys As well as a bunch of other things we don't need.
# Including Qualtrics junk and a broken question Hemp questions in 2b were not
# in pilot, so we can't use then unless we ditch the pilot too
surveys <- map(surveys, ~ {
  .x %>%
    select(
      -c(
        any_of(seasonal_questions),
        matches('Recipient'),
        matches('LocationL'),
        UserLanguage,
        matches('External'),
        ResponseId,
        matches('IPAddress'),
        DistributionChannel,
        matches('broken'),
        matches('^hemp')
      )
    )
})

map(surveys, get_str)
map(surveys, names)



# Save and clear ----------------------------------------------------------


# Save prepped surveys to be sent to exclusions
saveRDS(surveys, '5_objects/cleaning/all_surveys_prepped.rds')
clear_data()
