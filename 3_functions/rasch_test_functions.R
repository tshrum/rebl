#' Rasch Test Functions
#' 2024-08-21

#' Series of functions to make it easier to evaluate fit of Rasch model.
#' Includes tests of unidimensionality, goodness of fit, invariance by median
#' split and several demographic splits, separation reliability, and test for
#' local dependence. There is also a function to run everything together (except
#' for local dependence because it is slow).



# Packages ----------------------------------------------------------------


pacman::p_load(
  eRm,
  rlang,
  dplyr,
  psych,
  purrr,
  furrr,
  parallelly
)


# Test Unidimensionality --------------------------------------------------


# Methods from https://bookdown.org/chua/new_rasch_demo2/MD-fit.html#conduct-a-pca-of-standardized-residual-correlations
# Boone and Staver 2020 Chapter 2: eigenvalue less than 2 means unidimensional


test_uni_pcar <- function(model, 
                          n_vars = length(model$se.beta), 
                          rotate = 'Promax',
                          n_factors = 5) {
  results <- list()

  pca <- model %>% 
    person.parameter() %>% 
    itemfit() %>% 
    .$st.res %>% 
    pca(nfactors = n_factors, rotate = rotate)
  
  eigenvalues <- c(pca$values[1:10])
  y_limit <- ifelse(max(eigenvalues) > 2.1, max(eigenvalues), 2.1)
  plot(eigenvalues,
       ylab = "Eigenvalues",
       xlab = "Item Number",
       type = "b",
       ylim = c(1, y_limit)
  )
  abline(h = 2, col = 'red', lwd = 2, lty = 2)
  
  return(pca)
}



# Get Fits ----------------------------------------------------------------


# Get both item and person fit statistics from Rasch model
get_fits <- function(model) {
  results <- list()
  pp <- person.parameter(model)
  results$person_fit <- personfit(pp)
  results$item_fit <- itemfit(pp)
  return(results)
}



# Test LR by Group --------------------------------------------------------


test_lr_gender <- function(survey, 
                           rebl_names,
                           var = gender) {
  
  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>% 
    filter(!is.na(!!ensym(var)))
  
  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>% 
    select(any_of(rebl_names)) %>% 
    RM()
  
  # Now LR test, with split criterion as criterion
  lr_test <- LRtest(model, splitcr = no_na_survey[[as_string(ensym(var))]])
  
  return(lr_test)
}


# This is just rural vs everything else
test_lr_rurality <- function(survey, 
                             rebl_names, 
                             var = rurality, 
                             dummy_name = rural_dummy) {
  
  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>% 
    filter(!is.na(!!ensym(var))) %>% 
    mutate(rural_dummy = case_match(
      rurality,
      'Rural' ~ 1,
      NA ~ NA,
      .default = 0
    ))
  
  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>% 
    select(any_of(rebl_names)) %>% 
    RM()
  
  # Now LR test, with split criterion as criterion
  lr_test <- LRtest(model, splitcr = no_na_survey[[as_string(ensym(var))]])
  
  cat('\nNote that this test rural vs suburban + urban which is an unbalanced split:\n')
  print(get_table(no_na_survey$rurality))
  return(lr_test)
}


test_lr_income <- function(survey, 
                             rebl_names, 
                             var = income, 
                             dummy_name = rural_dummy) {
  
  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>% 
    filter(!is.na(!!ensym(var))) %>% 
    mutate(income_dummy = case_when(
      income >= 75000 ~ 1,
      is.na(income) ~ NA, 
      .default= 0
    ))
  
  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>% 
    select(any_of(rebl_names)) %>% 
    RM()
  
  # Now LR test, with split criterion as criterion
  lr_test <- LRtest(model, splitcr = no_na_survey[[as_string(ensym(var))]])
  
  cat('\nSplit income on >75k = 1, <75k = 0.\n')
  print(get_table(no_na_survey$income))
  return(lr_test)
}



# NPTests -----------------------------------------------------------------


test_local_dependence <- function(survey, rebl_items) {
  
  results <- list()
  
  matrix <- survey %>% 
    select(any_of(rebl_items)) %>% 
    as.matrix()
  
  # Return raw results of test
  results$q3_test <- NPtest(matrix, method = 'Q3h')
  
  # Also calculate the proportion that are above abs(0.3)
  results$coef_above_03 <- results$q3_test$Q3hmat %>% 
    .[!is.na(.)] %>% 
    .[. > abs(0.3)] %>% 
    mean()
  
  return(results)
}



# Combine Them All --------------------------------------------------------


test_rasch_model <- function(surveys, rebl_items, models) {
  
  # Results list
  results <- list()
  
  # Define models
  if (is.null(models)) {
    models <- map(surveys, ~ {
      .x %>% 
        select(any_of(rebl_items)) %>% 
        RM()
    })
  }
  
  plan(multisession, workers = availableCores(omit = 1))
  results <- map2(surveys, models, \(survey, model) {
    out <- list()
    out$uni_pcar <- test_uni_pcar(model)
    out$gof <- gofIRT(person.parameter(model))
    out$fits <- get_fits(model)
    out$sep_rel <- SepRel(person.parameter(model))
    out$lr <- LRtest(model)
    
    # Only do demos when there are demo questions
    if ('age' %in% names(survey)) {
      out$lr_demos <- list(
        lr_gender = test_lr_gender(survey, rebl_items),
        lr_income = test_lr_income(survey, rebl_items),
        lr_rurality = test_lr_rurality(survey, rebl_items)
      )
    }
    return(out)
  })
  plan(sequential)
  
  return(results)
}