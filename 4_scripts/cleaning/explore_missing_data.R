#' Explore Missing Data
#' 2024-07-17
#' 
#' Exploring missing data after exclusions. Pros and cons - it's weird to 
#' look at missing data after cases have been removed in the exclusions script,
#' but if we don't do exclusions first, we will end up with bots, test runs, 
#' etc. Ideally we would manually 'clean' the data to explore missingness. Put
#' this on the list of things to think about for later or never.



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  naniar,
  skimr
)

# Load all three surveys
surveys <- readRDS('5_objects/cleaning/all_surveys_excluded.rds')

# Only choose REBL items and demos, remove optional text variables
survey_1 <- surveys[[1]] %>% 
  select(waterShortShower_:politics, -genderOther, -raceOtherText)



# Survey 1 with demos -----------------------------------------------------


get_str(survey_1)
skim(survey_1)

prop_complete(survey_1)
miss_var_summary(survey_1)
miss_case_summary(survey_1)

# group by politics
survey_1 %>% 
  group_by(politics) %>% 
  miss_var_summary()

# Tables of missingness
miss_var_table(survey_1)
miss_case_table(survey_1)

vis_miss(survey_1) 
vis_miss(survey_1, sort_miss = TRUE)
vis_miss(survey_1, cluster = TRUE)
# 12.4% missing overall - reporting this in paper

gg_miss_case(survey_1)
gg_miss_var(survey_1, facet = politics, show_pct = TRUE)
# Liberals and very liberal have more missing than cons, very cons, some cons

survey_1 %>% 
  mutate(politics = factor(politics)) %>% 
  gg_miss_fct(fct = politics)
# Very conservative - more likely to miss trans questions, less likely food q's




# REBL Only ---------------------------------------------------------------


## Survey 1
just_rebl <- survey_1 %>% 
  select(1:fashShoesResolable)
names(just_rebl)

vis_miss(just_rebl)
vis_miss(just_rebl, sort_miss = TRUE)
vis_miss(just_rebl, cluster = TRUE)
# 17.4% of REBL items missing. Looks like whole blocks were missed 


## Survey 2a
rebl_2a <- surveys[[2]] %>% 
  select(waterWashingNotFull_r:purchTriedSecondHand_)
get_str(rebl_2a)

vis_miss(rebl_2a)

rebl_2b <- surveys[[2]] %>% 
  select(waterWashingNotFull_r:purchTriedSecondHand_)
get_str(rebl_2b)

vis_miss(rebl_2b)
