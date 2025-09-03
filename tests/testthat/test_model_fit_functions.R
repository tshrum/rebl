
# LR Tests ----------------------------------------------------------------


# test_that("test_lr_gender returns test output", {
#   # Need to grab final_items and load them here
#   data('example', package = 'rebl')
#   out <- test_lr_gender(surveys$survey_2a, final_items)
#   expect_equal('eRm', class(out))
# })
#
# test_that("test_lr_income returns test output", {
#   # Need to grab final_items and load them here
#   data('example', package = 'rebl')
#   out <- test_lr_income(surveys$survey_2a, final_items)
#   expect_equal('eRm', class(out))
# })
#
# test_that("test_lr_rurality returns test output", {
#   # Need to grab final_items and load them here
#   data('example', package = 'rebl')
#   example$respondent_id <- NULL
#   out <- test_lr_rurality(example, final_items)
#   expect_equal('eRm', class(out))
# })



# Model Fit ---------------------------------------------------------------


test_that("get_fits gets fits", {
  data('example', package = 'rebl')
  example$respondent_id <- NULL
  model <- RM(example)
  expect_s3_class(model, 'eRm')

  fit <- get_fits(model)
  expect_s3_class(fit$person_fit, 'pfit')
  expect_s3_class(fit$item_fit, 'ifit')
})

