#' Test Linking
#' 2024-08-18

#' Contents
#'  - Test linking with plink to rescale scores for 2b in terms of 2a
#'  - Bootstrapping standard errors for linking constants
#'  - Making clean DFs with all questions and scores for posterity
#'  - Heat Map for REBL score in 2a and 2b
#'  - Pearson correlations for REBL score in 2a and 2b
#'  - Bland Altman plot for 2a and 2b
#'  - DIF plot for 2a and 2b



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  plink,
  purrr,
  stringr,
  eRm,
  ggplot2,
  viridis,
  knitr,
  kableExtra,
  broom,
  furrr,
  parallelly,
  tibble,
  tictoc
)

# Load final Rasch models for surveys 2a and 2b only
models <- readRDS('5_objects/cml/final_items/rasch_model_final_items.rds') %>% 
  .[names(.) %in% c('survey_2a', 'survey_2b')]

# Source read_erm function (modification of plink::read.erm)
source('3_functions/read_erm.R')

# Source modified ba_plot function from Karun and Puranik 2021
source('3_functions/ba_plot.R')

# Load clean surveys, fits to get clean DFs with all info including scores
fits <- readRDS('5_objects/cml/final_items/rasch_outputs.rds')
surveys <- readRDS('2_clean/all_surveys_imputed.rds')

# Clean REBL names
rebl_items <- readRDS("2_clean/rebl_items_final.rds")

# Start list for results
results <- list()



# Test Linking ------------------------------------------------------------


#' This method is designed for equating two tests that have some number of 
#' common items between them. For our purposes, we are comparing 2a and 2b, so
#' all the items are common. We will rescale REBL scores from 2b in terms of 2a
#' to make comparisons.

# First define a matrix showing which items are in common (all of them)
common <- matrix(c(1:24, 1:24), 24, 2)

#' Set up parameters for plink. x is a list of model outputs as class irt.pars.
#' Note that we had to modify plink::read.erm and saved as read_erm to remove 
#' some arguments to make it work.
pars <- combine.pars(
  x = list(read_erm(x = models[[1]]), read_erm(x = models[[2]])),
  common = common,
  grp.names = c('survey_2a', 'survey_2b')
)

#' Define a list of thetas from survey 2a and 2b
thetas <- map(models, ~ coef(person.parameter(.x)))

# Link 2a and 2b using Stocking Lord method (SL) while also including thetas
out <- plink(
  x = pars,
  rescale = 'SL',
  ability = thetas
)

# Check it
out$link@constants$SL
summary(out)
#   Linking Constants
# 
# A         B
# Mean/Mean     1.000000  0.000000
# Mean/Sigma    0.983888  0.000000
# Haebara       0.988726 -0.000455
# Stocking-Lord 0.992000  0.001609
# 
# Ability Descriptive Statistics
# 
# survey_2a survey_2b
# Mean   -1.0189   -1.0018
# SD      1.0109    0.9959
# Min    -4.5872   -4.5984
# Max     2.5423    2.1694

# Pull out new thetas for survey_2b
new_thetas <- link.ability(out)
names(new_thetas)

# Check if new thetas are same as old thetas
identical(new_thetas$survey_2a, thetas[[1]]) # Same
identical(new_thetas$survey_2b, thetas[[2]]) # Rescaled

# Compare means between 2b and rescaled 2b
old <- mean(thetas[[2]])
new <- mean(new_thetas$survey_2b)
cat(paste0('Old: ', old, '\nNew: ', new))
# Old: -1.01146179311042
# New: -1.00176109876553

# % Difference
(new - old) / (old) * 100
# -0.959% change

# Put results into object to save
test_linking_results <- list(
  output = out
)



## Bootstrap SEs -----------------------------------------------------------


# Clean up the two surveys to run models out of
boot_surveys <- surveys[names(surveys) %in% c('survey_2a', 'survey_2b')] %>%
  map( ~ .x %>% select(any_of(rebl_items)))

# Prep common item matrix
common <- matrix(c(1:24, 1:24), 24, 2)

# Future options
config <- furrr_options(
  globals = c('boot_surveys', 'common', 'read_erm', 'print_time'),
  packages = c('eRm', 'plink', 'dplyr', 'tibble'),
  seed = 42
)

print_time()
tic()
n_boot = 1000
set.seed(42)
plan(multisession, workers = availableCores(omit = 1))

constants <- future_map(1:n_boot, \(i) {
  # Resample from surveys, run model, read_erm for info for
  boot_mods <- map(boot_surveys, ~ {
    .x %>%
      slice_sample(n = nrow(.x), replace = TRUE) %>%
      RM() %>%
      read_erm()
  })
  
  # Get pars object for plink
  pars <- combine.pars(
    x = list(boot_mods[[1]], boot_mods[[2]]),
    common = common,
    grp.names = c('survey_2a', 'survey_2b')
  )
  
  # Get constants
  out <- plink(x = pars, rescale = 'SL')
  cons <- out$link@constants$SL %>% 
    t() %>%
    as.data.frame()
  
  return(cons)
  
}, .options = config, .progress = TRUE) %>% 
  list_rbind()
plan(sequential)
toc()
# 8.3 minutes

# Quantiles
quantiles <- map(constants, ~ {
  quantile(.x, probs = c(0.025, 0.975), digits = 3)
}) %>%
  as.data.frame()

se <- map(quantiles, ~ ((.x[[2]] - .x[[1]]) / 3.92)) %>% 
  bind_cols()

std_err <- quantiles %>% 
  bind_rows(se) %>% 
  rownames_to_column() %>% 
  setNames(c(' ', 'survey_2a', 'survey_2b')) %>% 
  {.[3, 1] <- 'std.err'; .} %>% 
  mutate(across(where(is.numeric), ~ round(., 3)))

# Save these
test_linking_results$boot_constants <- constants
test_linking_results$boot_std_err <- std_err



# Make Clean DFs ----------------------------------------------------------


#' Putting together a clean DF with final REBL items from surveys with REBL
#' scores, rescaled REBL scores (for 2b only), and demographics. Will make
#' individual dataframes for 2a, 2b, and 3, and also an inner join dataframe of
#' 2a and 2b.
clean_dataframes <- imap(surveys[-1], \(survey, name) {
  survey %>% 
    inner_join(fits$person_fit[[name]], by = 'prolificID')
})

# Add rescaled rebl scores (thetas) to 2b
clean_dataframes$survey_2b <- new_thetas$survey_2b %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column() %>% 
  setNames(c('person', 'rebl_score_link_sl')) %>% 
  inner_join(clean_dataframes$survey_2b, by = 'person') %>% 
  relocate(rebl_score_link_sl, .after = last_col())

# Remove person column from all DFs
clean_dataframes <- map(clean_dataframes, ~ select(.x, -person))

# Now another df for inner join between 2a and 2b
clean_dataframes$survey_2a_2b_inner <- inner_join(
  clean_dataframes$survey_2a, 
  clean_dataframes$survey_2b, 
  by = 'prolificID', 
  suffix = c('.2a', '.2b')
)



# REBL Score Heatmap -------------------------------------------------------


# Heatmap using scores from 2a and 2b (not rescaled)
heatmap <- clean_dataframes$survey_2a_2b_inner %>%
  ggplot(aes(x = rebl_score.2a, y = rebl_score.2b)) +
  geom_bin_2d(bins = 20) +
  theme_bw() +
  scale_fill_viridis() +
  labs(
    x = 'REBL Score Study 2a',
    y = 'REBL Score Study 2b',
    fill = 'Count'
  ) +
  scale_x_continuous(breaks = -5:5) +
  scale_y_continuous(breaks = -5:5) +
  geom_abline(intercept = 0, slope = 1, lwd = 1)

ggsave(
  filename = '7_plots/cml/final_items/rebl_t1t2_heatmap.png',
  plot = heatmap,
  dpi = 1000,
  width = 5,
  height = 4,
  units = 'in'
)



# REBL Correlations -------------------------------------------------------


# Using full dataset with rebl scores from both 2a and 2b
get_str(clean_dataframes$survey_2a_2b_inner)

# Get Pearson correlation between scores in 2a and 2b
results$rebl_cor_2a_2b <- cor.test(
  clean_dataframes$survey_2a_2b_inner$rebl_score.2a, 
  clean_dataframes$survey_2a_2b_inner$rebl_score.2b,
  method = 'pearson'
)
results$rebl_cor_2a_2b
# t = 33.614, df = 808, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.7332634 0.7908653
# sample estimates:
#   cor 
# 0.7635793 

# Same thing for 2b and rescaled 2b
results$rebl_cor_2b_link_sl <- cor.test(
  clean_dataframes$survey_2a_2b_inner$rebl_score.2b, 
  clean_dataframes$survey_2a_2b_inner$rebl_score_link_sl,
  method = 'pearson'
)
results$rebl_cor_2b_link_sl
# t = Inf, df = 808, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   1 1
# sample estimates:
#   cor 
# 1  
# Pretty strong correlation - checks out!

# Save a latex table (not added to paper, just reporting in text for now)
results$rebl_cor_2a_2b %>% 
  broom::tidy() %>%
  as.data.frame() %>% 
  select(-method, -alternative) %>% 
  mutate(across(where(is.numeric), ~ format(round(., 3), nsmall = 3))) %>% 
  kbl(format = 'html', booktabs = TRUE)



# Bland Altman ------------------------------------------------------------


# Saving a BA plot for 2a and 2b from modified function
png(
  filename = '7_plots/cml/final_items/ba_plot.png',
  width = 7,
  height = 6,
  units = 'in',
  res = 500
)

ba_plot(
  clean_dataframes$survey_2a_2b_inner$rebl_score.2a, 
  clean_dataframes$survey_2a_2b_inner$rebl_score.2b
)

dev.off()



# Save and Clear ----------------------------------------------------------


# Save clean dataframes individually
iwalk(clean_dataframes, \(df, name) {
  saveRDS(df, paste0('2_clean/clean_dataframes/', name, '.rds'))
})

# Also save the list of all clean dataframes in one object
saveRDS(clean_dataframes, '2_clean/clean_dataframes/all_surveys_list.rds')

# Save bootstrapped link stats
saveRDS(test_linking_results, '6_outputs/cml/test_linking/test_link_results.rds')

# Clear data
clear_data()
