#' Rasch Test Functions
#'
#' Series of functions to make it easier to evaluate fit of Rasch model.
#' Includes tests of unidimensionality, goodness of fit, invariance by median
#' split and several demographic splits, separation reliability, and test for
#' local dependence. There is also a function to run everything together.
#'
#' @name rasch-test-functions
NULL

#' Test Unidimensionality with PCA of Residuals
#'
#' Methods from Boone and Staver 2020 Chapter 2: eigenvalue less than 2 means unidimensional.
#' Conducts a PCA of standardized residual correlations.
#'
#' @param model A Rasch model object from eRm
#' @param n_vars Number of variables (default: length of model$se.beta)
#' @param rotate Rotation method for PCA (default: 'Promax')
#' @param n_factors Number of factors to extract (default: 5)
#'
#' @return PCA results object with eigenvalue plot
#' @export
#'
#' @examples
#' \dontrun{
#' # Test unidimensionality
#' uni_results <- test_uni_pcar(rasch_model)
#' }
test_uni_pcar <- function(model, 
                          n_vars = length(model$se.beta), 
                          rotate = 'Promax',
                          n_factors = 5) {
  results <- list()

  pca <- model %>% 
    eRm::person.parameter() %>% 
    eRm::itemfit() %>% 
    .$st.res %>% 
    psych::pca(nfactors = n_factors, rotate = rotate)
  
  eigenvalues <- c(pca$values[1:10])
  y_limit <- ifelse(max(eigenvalues) > 2.1, max(eigenvalues), 2.1)
  plot(eigenvalues,
       ylab = "Eigenvalues",
       xlab = "Item Number",
       type = "b",
       ylim = c(1, y_limit)
  )
  graphics::abline(h = 2, col = 'red', lwd = 2, lty = 2)
  
  return(pca)
}

#' Get Fit Statistics
#'
#' Get both item and person fit statistics from Rasch model.
#'
#' @param model A Rasch model object from eRm
#'
#' @return List containing person_fit and item_fit statistics
#' @export
#'
#' @examples
#' \dontrun{
#' # Get fit statistics
#' fits <- get_fits(rasch_model)
#' }
get_fits <- function(model) {
  results <- list()
  pp <- eRm::person.parameter(model)
  results$person_fit <- eRm::personfit(pp)
  results$item_fit <- eRm::itemfit(pp)
  return(results)
}

#' Test LR by Gender
#'
#' Test likelihood ratio by gender grouping.
#'
#' @param survey Survey dataset
#' @param rebl_names Vector of REBL item names
#' @param var Variable to use for grouping (default: gender, unquoted)
#'
#' @return LR test results
#' @export
#'
#' @examples
#' \dontrun{
#' # Test LR by gender
#' lr_gender <- test_lr_gender(survey_data, rebl_items)
#' }
test_lr_gender <- function(survey, 
                           rebl_names,
                           var = gender) {
  
  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>% 
    dplyr::filter(!is.na(!!rlang::ensym(var)))
  
  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>% 
    dplyr::select(dplyr::any_of(rebl_names)) %>% 
    eRm::RM()
  
  # Now LR test, with split criterion as criterion
  lr_test <- eRm::LRtest(model, splitcr = no_na_survey[[rlang::as_string(rlang::ensym(var))]])
  
  return(lr_test)
}

#' Test LR by Rurality
#'
#' Test likelihood ratio by rurality (rural vs everything else).
#'
#' @param survey Survey dataset
#' @param rebl_names Vector of REBL item names
#' @param var Variable to use for grouping (default: rurality, unquoted)
#' @param dummy_name Name for dummy variable (default: rural_dummy, unquoted)
#'
#' @return LR test results with rurality split summary
#' @export
#'
#' @examples
#' \dontrun{
#' # Test LR by rurality
#' lr_rural <- test_lr_rurality(survey_data, rebl_items)
#' }
test_lr_rurality <- function(survey, 
                             rebl_names, 
                             var = rurality, 
                             dummy_name = rural_dummy) {
  
  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>% 
    dplyr::filter(!is.na(!!rlang::ensym(var))) %>% 
    dplyr::mutate(rural_dummy = dplyr::case_match(
      rurality,
      'Rural' ~ 1,
      NA ~ NA,
      .default = 0
    ))
  
  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>% 
    dplyr::select(dplyr::any_of(rebl_names)) %>% 
    eRm::RM()
  
  # Now LR test, with split criterion as criterion
  lr_test <- eRm::LRtest(model, splitcr = no_na_survey[[rlang::as_string(rlang::ensym(var))]])
  
  cat('\nNote that this test rural vs suburban + urban which is an unbalanced split:\n')
  print(get_table(no_na_survey$rurality))
  return(lr_test)
}

#' Test LR by Income
#'
#' Test likelihood ratio by income level (split at $75k).
#'
#' @param survey Survey dataset
#' @param rebl_names Vector of REBL item names
#' @param var Variable to use for grouping (default: income, unquoted)
#' @param dummy_name Name for dummy variable (default: income_dummy, unquoted)
#'
#' @return LR test results with income split summary
#' @export
#'
#' @examples
#' \dontrun{
#' # Test LR by income
#' lr_income <- test_lr_income(survey_data, rebl_items)
#' }
test_lr_income <- function(survey, 
                             rebl_names, 
                             var = income, 
                             dummy_name = income_dummy) {
  
  # Filter out NAs (necessary for LR test)
  # Need this half step so we can grab split criterion vector later
  no_na_survey <- survey %>% 
    dplyr::filter(!is.na(!!rlang::ensym(var))) %>% 
    dplyr::mutate(income_dummy = dplyr::case_when(
      income >= 75000 ~ 1,
      is.na(income) ~ NA, 
      .default= 0
    ))
  
  # Run new RM with filtered DF, only rebl items
  model <- no_na_survey %>% 
    dplyr::select(dplyr::any_of(rebl_names)) %>% 
    eRm::RM()
  
  # Now LR test, with split criterion as criterion
  lr_test <- eRm::LRtest(model, splitcr = no_na_survey[[rlang::as_string(rlang::ensym(var))]])
  
  cat('\nSplit income on >75k = 1, <75k = 0.\n')
  print(get_table(no_na_survey$income))
  return(lr_test)
}

#' Test Local Dependence
#'
#' Test for local dependence using Q3 statistic.
#'
#' @param survey Survey dataset
#' @param rebl_items Vector of REBL item names
#'
#' @return List containing Q3 test results and proportion of coefficients above |0.3|
#' @export
#'
#' @examples
#' \dontrun{
#' # Test local dependence (this can be slow)
#' local_dep <- test_local_dependence(survey_data, rebl_items)
#' }
test_local_dependence <- function(survey, rebl_items) {
  
  results <- list()
  
  matrix <- survey %>% 
    dplyr::select(dplyr::any_of(rebl_items)) %>% 
    as.matrix()
  
  # Return raw results of test
  results$q3_test <- eRm::NPtest(matrix, method = 'Q3h')
  
  # Also calculate the proportion that are above abs(0.3)
  results$coef_above_03 <- results$q3_test$Q3hmat %>% 
    .[!is.na(.)] %>% 
    .[. > abs(0.3)] %>% 
    mean()
  
  return(results)
}

#' Test Rasch Model Comprehensively
#'
#' Run all Rasch model tests together (except local dependence which is slow).
#' Tests unidimensionality, goodness of fit, invariance, separation reliability,
#' and demographic differences.
#'
#' @param surveys List of survey datasets
#' @param rebl_items Vector of REBL item names
#' @param models List of Rasch models (optional, will be created if NULL)
#'
#' @return List of comprehensive test results for each survey
#' @export
#'
#' @examples
#' \dontrun{
#' # Comprehensive model testing
#' test_results <- test_rasch_model(survey_list, rebl_items)
#' }
test_rasch_model <- function(surveys, rebl_items, models) {
  
  # Results list
  results <- list()
  
  # Define models
  if (is.null(models)) {
    models <- purrr::map(surveys, ~ {
      .x %>% 
        dplyr::select(dplyr::any_of(rebl_items)) %>% 
        eRm::RM()
    })
  }
  
  furrr::plan(furrr::multisession, workers = parallelly::availableCores(omit = 1))
  results <- purrr::map2(surveys, models, \(survey, model) {
    out <- list()
    out$uni_pcar <- test_uni_pcar(model)
    out$gof <- eRm::gofIRT(eRm::person.parameter(model))
    out$fits <- get_fits(model)
    out$sep_rel <- eRm::SepRel(eRm::person.parameter(model))
    out$lr <- eRm::LRtest(model)
    
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
  furrr::plan(furrr::sequential)
  
  return(results)
}