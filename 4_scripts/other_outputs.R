#' Other Outputs
#' 2024-08-14 update

#' Using script to put together any other outputs for the paper that are not 
#' specifically Rasch-related. Includes:
#'  1. Demographics table for 2a and 2b
#'  2. Missing data stats
#'  3. Imputation stats



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stringr,
  purrr,
  flextable,
  ggplot2,
  crosstable,
  flextable,
  stargazer,
  xtable,
  naniar,
  ggpubr
)



# Demographics -----------------------------------------------------------


#' Making a single demo table for both Survey 1 and 2a. Note that this comes 
#' out pretty nice as docx, but loses formatting when saved as an image, and I 
#' cannot for the life of me get it to come out in latex properly. Going through
#' xtable might be possible, but I couldn't get that to work either. So for now,
#' we are saving as docx, then opening in docx, and saving as image. It's the
#' fucking worst and I hate it.

#' Pulling data after exclusions, before imputations. We are going to take the
#' UNCODED responses after variable naming and join them with the excluded
#' surveys to get the right people but go back to the uncoded responses so we
#' don't have to recode 1s and 0s back into worded responses. Does this make
#' sense? Maybe.



## Wrangle Demos ----------------------------------------------------------


#' Pull form both renamed and excluded data, survey 1 and 2a
uncoded <- readRDS('5_objects/cleaning/all_surveys_renamed.rds')[1:2] |> 
  map(~ .x[-(1:2), ])
map(uncoded, get_str)

surveys <- readRDS('5_objects/cleaning/all_surveys_excluded.rds')[1:2]
map(surveys, get_str)

## Join them, just demos and prolificIDs

#' Survey 1 - Note there are a couple of dupes in the raw survey that we are 
#' filtering out here.
survey_1 <- uncoded[[1]] %>% 
  select(prolificID, age:gender, rurality:politics) %>% 
  right_join(select(surveys[[1]], prolificID, race), by = join_by(prolificID)) %>% 
  mutate(survey = 1)

# Now same for survey_2
survey_2 <- uncoded[[2]] %>% 
  select(prolificID, age:gender, rurality:politics) %>% 
  right_join(select(surveys[[2]], prolificID, race), by = join_by(prolificID)) %>% 
  mutate(survey = 2)
get_str(survey_1)
get_str(survey_2)

# Combine surveys
survey_both <- rbind(survey_1, survey_2)
get_str(survey_both)



## Demos - Flextable -------------------------------------------------------


#' Now that data frames are arranged, we will make a table. The ultimate output
#' is flextable, but we use crosstable to get nice formatting by demographic

# Default formatting for flextable. We will also apply apa_theme later
set_flextable_defaults(
  font.family = 'Times',
  font.size = 10,
  text.align = 'center',
  border.width = 0.5,
  border.color = 'black',
  table_align = 'left'
)

# Format factor labels, make cross table and convert to flextable
demos <- survey_both %>%
  select(age, gender, race, income, politics, survey) %>%
  setNames(str_to_title(names(.))) %>% 
  mutate(Race = factor(
    Race,
    levels = c(
      'asian',
      'black',
      'hispanicLatino',
      'native',
      'white',
      'twoOrMoreRaces',
      'other'
    ),
    labels = c(
      'Asian',
      'Black',
      'Hispanic or Latino',
      'Native or Indigenous',
      'White',
      'Two or More Races',
      'Other'
    ),
    ordered = TRUE
  ),
  Politics = factor(Politics,
                    levels = c('Very conservative',
                               'Conservative',
                               'Somewhat conservative',
                               'Moderate',
                               'Somewhat liberal',
                               'Liberal',
                               'Very liberal')),
  ) %>%
  crosstable(
    showNA = 'ifany',
    by = 'Survey',
    label = TRUE,
    percent_pattern = "{n} ({p_col})"
  ) %>% 
  as_flextable() %>% 
  flextable::align(align = 'center', part = 'all') %>% 
  font(fontname = 'Times', part = 'all') %>% 
  fontsize(size = 10, part = 'all') %>% 
  autofit(add_w = 0, add_h = 0) %>% 
  set_header_labels(values = list(
    `1` = 'Survey 1',
    `2` = 'Survey 2'
    )
  ) %>% 
  flextable::compose(i = 1, j = 1, part = "header", value = as_paragraph("Demographic")) %>% 
  flextable::compose(i = 1, j = 2, part = "header", value = as_paragraph("Category"))

# Check it out
demos

#' Save as word doc. Note that if we save to image, it loses its apa formatting 
#' work around for now is opening it in word and saving as image... I hate it
save_as_docx(demos, path = '6_outputs/demographic_table_flex.docx')



## Demos - kableExtra ------------------------------------------------------


# This one is better. It can go straight to proper latex format.
survey_both %>%
  select(age, gender, race, income, politics, survey) %>%
  setNames(str_to_title(names(.))) %>% 
  mutate(
    Race = factor(
      Race,
      levels = c(
        'asian',
        'black',
        'hispanicLatino',
        'native',
        'white',
        'twoOrMoreRaces',
        'other'
      ),
      labels = c(
        'Asian',
        'Black',
        'Hispanic or Latino',
        'Native or Indigenous',
        'White',
        'Two or More Races',
        'Other'
      ),
      ordered = TRUE
    ),
    Politics = factor(
      Politics,
      levels = c(
        'Very conservative',
        'Conservative',
        'Somewhat conservative',
        'Moderate',
        'Somewhat liberal',
        'Liberal',
        'Very liberal'
      )
    ),
    Income = factor(
      Income,
      levels = c(
        'Less than $25,000',
        '$50,000 - $100,000',
        '$100,000 - $200,000',
        'More than $200,000'
      )
    )
  ) %>%
  crosstable(
    showNA = 'ifany',
    by = 'Survey',
    label = TRUE,
    percent_pattern = "{n} ({p_col})",
    percent_digits = 1
  ) %>% 
  select(-1) %>%
  setNames(c(' ', ' ', 'Study 1', 'Study 2 Part 1')) %>%
  kbl(format = 'latex',
      booktabs = TRUE,
      align = 'c',
      caption = 'Demographic Table',
      position = 'p',
      label = 'demographics'
      ) %>%
  kable_styling() %>% 
  add_header_above(c(" " = 2, 'Study' = 2)) %>% 
  column_spec(1, bold = T) %>%
  collapse_rows(
    columns = 1,
    latex_hline = 'major',
    row_group_label_position = "first"
  ) %>% 
  save_kable(
    file = '6_outputs/demographic_table.tex'
  )



# Missing Data ------------------------------------------------------------
## Older Take --------------------------------------------------------------


# Get % missing of REBL items for each survey, after exclusions
surveys <- readRDS('5_objects/cleaning/all_surveys_excluded.rds')
all_rebl_item_names <- readRDS("5_objects/all_rebl_item_names.rds")

percent_missing <- map(surveys, ~ {
  df <- .x %>% 
    select(any_of(all_rebl_item_names))
  missing <- sum(is.na(df))
  total <- prod(dim(df))
  percent_missing <- missing / total * 100
  return(percent_missing)
})

percent_missing
# $survey_1
# [1] 16.99294
# 
# $survey_2a
# [1] 0.04201681
# 
# $survey_2b
# [1] 0.06082961



## Naniar ------------------------------------------------------------------


# Load all three surveys
surveys <- readRDS('5_objects/cleaning/all_surveys_excluded.rds')


## Survey 1

# Only choose REBL items and demos, remove optional text variables
survey_1 <- surveys[[1]] %>% 
  select(waterShortShower_:politics, -genderOther, -raceOtherText)

just_rebl <- survey_1 %>% 
  select(1:fashShoesResolable)

names(just_rebl)

vis_miss(just_rebl, cluster = TRUE)
# 17.4% of REBL items missing. Looks like whole blocks were missed 
# Reporting this 


## Survey 2a
rebl_2a <- surveys[[2]] %>% 
  select(waterWashingNotFull_r:purchTriedSecondHand_)
get_str(rebl_2a)

vis_miss(rebl_2a)
# < 0.1%

rebl_2b <- surveys[[2]] %>% 
  select(waterWashingNotFull_r:purchTriedSecondHand_)
get_str(rebl_2b)

vis_miss(rebl_2b)
# < 0.1%



# Imputation Stats --------------------------------------------------------


# Pulling accuracy of missForest imputation to report in paper

# Load imputation data
imputed_surveys <- readRDS('5_objects/imputation/all_survey_imp_outputs.rds')
tuning_grid <- readRDS('5_objects/imputation/quick_grid.rds')
source('3_functions/run_miss_forest.R')

# Recreate total mean error and hyperparameters
tuning_grid
invisible(choose_best_runs(imputed_surveys, tuning_grid))

# Get varwise errors
varwise_errors <- readRDS('5_objects/imputation/varwise_errors.rds')
varwise_errors

#' Total mean error. Note these are across all vars, even if there was nothing
#' to impute. Thus, error looks lower than it really should be.
map(varwise_errors, ~ mean(.x$OOBerror))
# $survey_1
# [1] 0.2381137
# 
# $survey_2a
# [1] 0.08462873
# 
# $survey_2b
# [1] 0.05298111
# 
# $survey_3
# [1] 0.07071828

# Get mean error only of vars that had missing data to begin with
map(varwise_errors, ~ {
  .x %>% 
    filter(OOBerror != 0) %>%
    pull(OOBerror) %>% 
    mean()
})
# $survey_1
# [1] 0.2681123
# 
# $survey_2a
# [1] 0.2638425
# 
# $survey_2b
# [1] 0.2046998
# 
# $survey_3
# [1] 0.3333862

# Reporting these ones instead.


# REBL and Donation Histograms --------------------------------------------


dat <- readRDS("2_clean/clean_dataframes/survey_2a.rds")
get_str(dat)

# REBL Histogram
rebl_hist <- dat %>% 
  ggplot(aes(x = rebl_score)) +
  geom_histogram(
    fill = 'grey',
    color = 'black',
    binwidth = 0.3
  ) +
  theme_classic() +
  labs(
    x = 'REBL Score',
    y = 'Count'
  ) +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(breaks = seq(-5, 4))
rebl_hist

# Donation Hist
donation_hist <- dat %>% 
  mutate(dictatorGame = factor(dictatorGame)) %>% 
  ggplot(aes(x = dictatorGame)) +
  geom_bar(
    fill = 'grey',
    color = 'black',
    stat = 'count'
  ) +
  theme_classic() +
  labs(
    x = 'Donation',
    y = 'Count'
  ) +
  theme(text = element_text(size = 16))


arranged <- ggarrange(plotlist = list(rebl_hist, donation_hist),
                      ncol = 1,
                      nrow = 2)

arranged

ggsave(
  filename = '7_plots/cml/final_items/rebl_donation_hist.png',
  plot = arranged,
  width = 3000,
  height = 2500,
  units = 'px',
  dpi = 300
)


# Save and Clear ----------------------------------------------------------


# Nothing really even to save, just pulling data as needed from above.
# Other outputs like demographic table are already written to docx above.
clear_data()



