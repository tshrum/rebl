#' final_items_rasch_model_and_outputs
#' 2024-08-03 update

#' All Rasch models for the final list of 24 items

#' Note that I'm running all of this for survey 2a and 2b even though we don't
#' really care about 2b. This is partly because I figure we may as well have to
#' it to compare out of curiosity, but also because I already set up the 
#' function to work on lists and it's easier to keep 2b in than run it with 
#' only 2a. But hey it will probably pay off sometime when we inevitably decide
#' we do want to look at 2b results.

#' Inputs
#'    1. List of final rebl items from consistency script
#'    2. Clean surveys 2a and 2b

#' Outputs
#'    1. For cml models, all outputs including:
#'        - Histogram of REBL score for each survey individually and combined
#'        - PW Maps
#'        - PI Maps
#'        - ICC plots
#'        - Person Fit
#'        - Item Fit
#'        - REBL item table with etas, betas, means
#'    2. Clean full DFs including REBL items, REBL scores, demos. One just for 
#'        survey 2a, and one for the inner join of 2a and 2b.



# Packages and Data -------------------------------------------------------


pacman::p_load(
  dplyr,
  purrr,
  eRm,
  tidyr,
  tibble,
  tictoc,
  furrr,
  stringr,
  stargazer,
  readr,
  knitr,
  kableExtra,
  xtable
)

# Pull in surveys 2a, 2b, and 3 to run Rasch model one last time
surveys <- readRDS('2_clean/all_surveys_imputed.rds') %>% 
  .[names(.) != 'survey_1']

# REBL item names to pull in the item fit section
final_items <- readRDS('2_clean/rebl_items_final.rds')

# Rasch output function
source('3_functions/get_rasch_outputs.R')

# List for all model results
results <- list()



# Wrangle -----------------------------------------------------------------


#' Before we make figures, let's get rid of the suffixes on the REBL names so 
#' they don't show up in the model outputs.
clean_item_names <- str_remove_all(final_items, '_[a-z]*')

surveys <- map(surveys, \(x) {
  names(x) <- str_split_i(names(x), '_', 1)
  return(x)
})

map(surveys, get_str)
clean_item_names



# Rasch Model -------------------------------------------------------------


#' These are the same rasch models we made in the `rasch_model_and_tests` script
#' but we are running it again with clean names so we don't have to deal with 
#' removing the suffixes
models <- map(surveys, ~ {
  .x %>%
    select(all_of(clean_item_names)) %>%
    RM()
})
# This call throws an error but works the second time every time?

get_str(models)



# Outputs -----------------------------------------------------------------


# Folder paths for plots and csvs
plot_path <- '7_plots/cml/final_items/'
csv_path <- '6_outputs/cml/final_items/'

#' Get all outputs! So clean. So modular. Such reproduction. CSVs and plots are
#' automatically saved in folders 6 and 7. Person and item fit outputs are saved
#' and also sent to the results object here for playing around with. We are 
#' doing this with 
results <- get_rasch_outputs(
  models = models,
  surveys = surveys,
  plot_path = plot_path,
  csv_path = csv_path,
  rebl_items = clean_item_names
)



## Latex Fit Tables --------------------------------------------------------


# Pulling out fit tables and converting to latex

# Person fit for survey 2a
results[[1]][[1]] %>% 
  select(p.fit,
         p.outfitMSQ:p.infitZ) %>% 
  stargazer(
    type = 'latex',
    title = 'Person Fit',
    style = 'default',
    summary = FALSE,
    out = paste0(csv_path, 'person_fit_2a.tex'),
    font.size = 'scriptsize'
  )

# Item Fit
map(results[[2]], get_str)
results[[2]][[1]] %>% 
  select(rebl_item, 
         i.fit,
         i.outfitMSQ:last_col()) %>% 
  stargazer(
    type = 'latex',
    title = 'REBL Item Fit',
    style = 'default',
    summary = FALSE,
    out = paste0(csv_path, 'item_fit_2a.tex'),
    font.size = 'scriptsize'
  )



## Etas --------------------------------------------------------------------


# We are taking betas and multiplying by -1 to get etas Then add proportion
# answered "correctly"
eta_df <- bind_cols(str_split_i(names(models$survey_2a$betapar), ' ', 2),
                    models$survey_2a$betapar*-1, 
                    models$survey_2a$se.beta) %>% 
  setNames(c('Item', 'Eta', 'SE'))

# Get proportion correct from survey 2a
props <- surveys$survey_2a %>% 
  select(all_of(clean_item_names)) %>% 
  apply(MARGIN = 2, FUN = mean) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  setNames(c('Item', 'Mean'))
  
# Now Join
eta_df <- eta_df %>% 
  full_join(props, by = 'Item') %>% 
  arrange(Eta) %>% 
  mutate(across(Eta:Mean, ~ format(round(., 3), nsmall = 3)))

stargazer(
  eta_df,
  type = 'latex',
  summary = FALSE,
  out = paste0(csv_path, 'eta_table_2a.tex'),
  title = 'Etas and SEs for REBL Items'
)



## VCOV Matrix -------------------------------------------------------------


# Get vcov matrix for model 2a, put it into a functional format
vcov(models$survey_2a) %>% 
  as.data.frame() %>% 
  round(3) %>% 
  format(nsmall = 3) %>% 
  # Cut off names at 10 characters to make it narrower
  setNames(str_sub(names(models$survey_2a$etapar), end = 10)) %>% 
  `rownames<-`(names(models$survey_2a$etapar)) %>% 
  kbl(
    format = 'latex',
    align = 'c',
    booktabs = TRUE,
    caption = 'Variance-Covariance Matrix for REBL Items',
    label = 'vcov_matrix_long'
  ) %>% 
  kable_styling(
    latex_options = c('hold_position', 'scale_down'),
    font_size = 5,
    bootstrap_options = 'condensed'
  ) %>% 
  landscape() %>% 
  save_kable(
    file = paste0(csv_path, 'vcov_table_2a_kable.tex')
  )


# REBL Item Table -----------------------------------------------------------


# Getting a table of short REBL names and full question text

# Pull final list of items, then pull text from rebl item coding file
final <- readRDS("2_clean/rebl_items_final.rds")
all <- readRDS('5_objects/cleaning/rebl_item_coding.rds')

# Filter for final to get final set with text, then xtable to save as latex
rebl_text <- all %>% 
  filter(rebl_item %in% final) %>% 
  select(rebl_item, question_text) %>% 
  mutate(
    question_text = str_split_i(question_text, ' - ', 2),
    rebl_item = str_remove(rebl_item, '_')) %>%
  filter(!str_detect(question_text, 'focused on')) %>% # Getting rid of dupe
  setNames(c('REBL Item',
             'In the last week, have you...')) %>% 
  xtable(
    label = 'rebl_text',
    align = 'ccp{7cm}',
    caption = 'REBL Items and Text'
  ) %>% 
  print(
    type = 'latex',
    file = '6_outputs/cml/final_items/rebl_text.tex',
    size = 'footnotesize'
  )



# DIF ---------------------------------------------------------------------


# First run LR test on survey 2a
lr <- LRtest(models$survey_2a)

# DIF plot based on LR test by median split
png(
  '7_plots/cml/final_items/item_dif_survey_2a.png',
  res = 1000,
  height = 6,
  width = 6,
  unit = 'in',
  pointsize = 10
)
par(mar = c(5, 10, 1, 1))
plotDIF(lr,
        main = '',
        xlab = 'Item Difficulty')
dev.off()



# Save and Clear ----------------------------------------------------------


# Save person and item fit results
saveRDS(results, '5_objects/cml/final_items/rasch_outputs.rds')

clear_data()
