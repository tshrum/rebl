#' Regressions
#' 2024-08-21

#' Two sets of regressions.
#' 1. Tobit regression on donation analysis. Four different runs:
#'  a. Donation ~ REBL
#'  b. Donation ~ REBL + demos
#'  b. Donation ~ REBL + demos + enviro scales
#'  b. Donation ~ demos + enviro scales
#' 2. Correlation Matrix - each enviro scale with REBL and log donation
#'  This was a set of regressions before. 
#' 3. Series of regressions with log donation score or REBL score as dependent
#'  var, with enviro scales as predictors



# Housekeeping ------------------------------------------------------------


pacman::p_load(
  dplyr,
  stargazer,
  stringr,
  purrr,
  DHARMa,
  AER,
  fitdistrplus,
  ggplot2,
  GGally,
  ggcorrplot,
  knitr,
  kableExtra,
  rstatix
)

conflicted::conflicts_prefer(
  AER::tobit
)

# Load Data. All demos and scales are in 2a.
dat <- readRDS('2_clean/clean_dataframes/survey_2a.rds')



# Wrangle -----------------------------------------------------------------


get_str(dat)
# Demos we need: age, gender, education, rurality (dummy?), income, politics.

#' Scales:
#'  enviroID 1, 2, 3 - not 4
#'  greenGlow 1, 2, 3, 4
#'  efficacy 1, 2, 3, 4 - not 5
#'  ccBelief 1, 2, 3, 4, 5 - Note: This one written by Chris

#' Note that enviroID4 and efficacy5 were added, not part of original scales

# Check out donation:
get_table(dat$dictatorGame)
hist(dat$dictatorGame)

# Get averages of scales. Do not include items that were added by Chris.
dat <- dat %>% 
  mutate(enviroID = rowMeans(select(., matches('^enviroID[1-3]')), na.rm = TRUE),
         greenGlow = rowMeans(select(., matches('^greenGlow')), na.rm = TRUE),
         efficacy = rowMeans(select(., matches('^efficacy[1-4]')), na.rm = TRUE),
         ccBelief = rowMeans(select(., matches('^ccBelief')), na.rm = TRUE))
get_str(dat)

# Put politics back into numeric and divide income by 1000
dat <- dat %>% 
  mutate(
    politics = case_when(
      str_detect(politics, '^Very l') ~ 1,
      str_detect(politics, '^Liberal') ~ 2,
      str_detect(politics, 'Somewhat l') ~ 3,
      str_detect(politics, 'Mod') ~ 4,
      str_detect(politics, 'Somewhat c') ~ 5,
      str_detect(politics, '^Cons') ~ 6,
      str_detect(politics, '^Very c') ~ 7,
      .default = NA
    ),
    income = income / 1000,
    log_donation = ifelse(log(dictatorGame) == -Inf, 0, log(dictatorGame))
  )



# Check Distribution ------------------------------------------------------


# Exploring distribution of donation game variable

plotdist(dat$dictatorGame, demp = TRUE)
descdist(dat$dictatorGame, boot = 1000)

descdist(sqrt(dat$dictatorGame), boot = 1000)
hist(sqrt(dat$dictatorGame))

descdist((dat$dictatorGame)^(1/3), boot = 1000)
hist((dat$dictatorGame)^(1/3))



# AER Tobit ---------------------------------------------------------------


## REBL only -----
tobit_1 <- tobit(
  dictatorGame ~ rebl_score,
  left = 0,
  right = 10,
  data = dat,
  dist = 'gaussian'
)

summary(tobit_1)
hist(residuals(tobit_1))
tobit_1 %>% 
  residuals() %>% 
  plot()
# Oh shit these actually aren't half bad

plot(fitted(tobit_1), residuals(tobit_1),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted values")
abline(h = 0, col = "red")
# This bad though

qqnorm(residuals(tobit_1))
qqline(residuals(tobit_1))
# This is not ideal.


## Add Demos -----
tobit_2 <- tobit(
  dictatorGame ~ rebl_score + age + gender + education + rurality + income + 
    politics,
  left = 0,
  right = 10,
  data = dat,
  dist = 'gaussian'
)

summary(tobit_2)
residuals(tobit_2) %>% hist()
tobit_2 %>% 
  residuals() %>% 
  plot()


## Add Scales -----
tobit_3 <- tobit(
  dictatorGame ~ rebl_score + age + gender + education + rurality + income + 
    politics + enviroID + greenGlow + efficacy + ccBelief,
  left = 0,
  right = 10,
  data = dat,
  dist = 'gaussian'
)

summary(tobit_3)
residuals(tobit_3) %>% hist()
tobit_3 %>% 
  residuals() %>% 
  plot()

plot(fitted(tobit_3), residuals(tobit_3),
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs Fitted values")
abline(h = 0, col = "red")
# Yeah nope

qqnorm(residuals(tobit_3))
qqline(residuals(tobit_3))
# Decent


## Not REBL -----
tobit_4 <- tobit(
  dictatorGame ~ age + gender + education + rurality + income + politics + 
    enviroID + greenGlow + efficacy + ccBelief,
  left = 0,
  right = 10,
  data = dat,
  dist = 'gaussian'
)

summary(tobit_4)
residuals(tobit_4) %>% hist()
tobit_4 %>% 
  residuals() %>% 
  plot()


## Compare Fit -----

models <- mget(c(
  'tobit_1',
  'tobit_2',
  'tobit_3',
  'tobit_4'
))

data.frame(
  AIC = map_dbl(models, AIC),
  BIC = map_dbl(models, BIC)
)
# Model 3 fits best for AIC, 4 is just barely better for BIC



## Pseudo R2 ---------------------------------------------------------------


# McFadden R2 is: 1 - (ln(likelihood of fitted)) / (ln(likelihood of null))

# Check logLik
map(models, logLik)

# Define null model
tobit_null <- tobit(
  dictatorGame ~ 1,
  left = 0,
  right = 10,
  data = dat,
  dist = 'gaussian'
)

# Define log likelihood of null model
loglik_tobit_null <- logLik(tobit_null)

# Calculate. Loglik already took the natural log, so we don't need that
pseudos <- map(models, ~ 1 - (logLik(.x) / loglik_tobit_null)) %>% 
  unlist()
pseudos



## Stargazer ---------------------------------------------------------------


stargazer(
  models,
  out = '6_outputs/cml/final_items/regression_donation_by_rebl.tex',
  type = 'latex',
  # column.labels = c('REBL Only', 'With Demos', 'With Scales', 'No REBL'),
  column.sep.width = '1pt',
  omit.stat = c('wald', 'll'),
  digits = 3,
  add.lines = list(
    c("AIC", round(map_dbl(models, AIC), 1)),
    c('BIC', round(map_dbl(models, BIC), 1)),
    c('Log Lik', map_dbl(models, ~ .x$loglik[2] %>% round(1))),
    c('Pseudo R2', format(round(pseudos, 3), nsmall = 3))
  ),
  covariate.labels = c(
    'REBL',
    'Age',
    'Gender',
    'Education',
    'Suburban',
    'Urban',
    'Income',
    'Politics',
    'EnviroID',
    'GreenGlow',
    'Efficacy',
    'CCBelief'
  ),
  digits.extra = 0,
  align = TRUE,
  dep.var.labels = 'Donation',
  font.size = 'scriptsize',
  no.space = FALSE,
  notes.align = 'r',
  notes.append = TRUE,
  notes = c("Pseudo R-squared by McFadden")
)



# Log Donation ------------------------------------------------------------
## Correlation Plot --------------------------------------------------------


# all scales against REBL and log donation
cor_dat <- dat %>% 
  mutate(log_donation = ifelse(dictatorGame == 0, 0, log(dictatorGame))) %>% 
  select(enviroID, greenGlow, efficacy, ccBelief, rebl_score, log_donation)

make_plot <- function(data, mapping, ...){
  ggplot(data = data, mapping = mapping) + 
    geom_jitter(height = 0.25, width = 0.25, alpha = 0.4) + 
    geom_smooth(method = "lm", color = "blue", se = FALSE)
}

cor_plot <- ggpairs(cor_dat, lower = list(continuous = make_plot)) +
  theme_bw()

ggsave(
  cor_plot,
  filename = '7_plots/cml/final_items/correlation_plot.png',
  dpi = 300,
  height = 6,
  width = 6,
  units = 'in'
)



## Correlation Matrix ------------------------------------------------------


cor_dat %>% 
  setNames(c(
    'EnviroID',
    'Green Glow',
    'Efficacy',
    'CC Belief',
    'REBL Score',
    'Log Donation'
  )) %>% 
  cor_mat() %>% 
  cor_mark_significant(
    cutpoints = c(0, 0.01, 0.05, 0.1),
    symbols = c('***', '**', '*')
  ) %>%
  {names(.)[1] <- ''; .} %>% 
  .[, -ncol(.)] %>% 
  kbl(
    'latex',
    label = 'log_donation_regression',
    caption = 'Correlation Table',
    booktabs = TRUE
  ) %>% 
  kable_styling(
    font_size = 10
  ) %>% 
  save_kable(
    file = '6_outputs/cml/final_items/correlation_table.tex',
    keep_tex = TRUE
  )


## Many Regressions --------------------------------------------------------


get_str(dat)

lms <- list(
  lm(rebl_score ~ greenGlow, data = dat),
  lm(log_donation ~ greenGlow, data = dat),
  lm(rebl_score ~ enviroID, data = dat),
  lm(log_donation ~ enviroID, data = dat),
  lm(rebl_score ~ efficacy, data = dat),
  lm(log_donation ~ efficacy, data = dat),
  lm(rebl_score ~ ccBelief, data = dat),
  lm(log_donation ~ ccBelief, data = dat)
)

stargazer(
  lms,
  type = 'latex',
  out = '6_outputs/cml/final_items/log_donation_regressions.tex',
  title = 'REBL and Log Donation Regressions',
  dep.var.labels = rep(c('REBL', 'Log Donation'), 4),
  covariate.labels = c('Green Glow', 'Enviro ID', 'Efficacy', 'CC Belief'),
  font.size = 'scriptsize',
  column.sep.width = '1pt'
)



# Clear -------------------------------------------------------------------


clear_data()
