test_that("test_lr_gender returns test output", {
  # Need a real example dataset
  # Double check class too
  out <- test_lr_gender(surveys$survey_2a, final_items)
  expect_equal('eRm', class(out))
})

test_that("test_lr_income returns test output", {
  # Need a real example dataset
  # Double check class too
  out <- test_lr_income(surveys$survey_2a, final_items)
  expect_equal('eRm', class(out))
})

test_that("test_lr_rurality returns test output", {
  # Need a real example dataset
  # Double check class too
  out <- test_lr_rurality(surveys$survey_2a, final_items)
  expect_equal('eRm', class(out))
})

test_that("get_fits gets fits", {
  data('example', package = 'rebl')
  example$respondent_id <- NULL
  model <- RM(example)
  fit <- get_fits(model)
})
