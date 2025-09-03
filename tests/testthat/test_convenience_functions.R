
# Convenience Functions ---------------------------------------------------


test_that("get_str runs without error", {
  capture.output(expect_no_error(get_str(iris)))
})

test_that("print_time runs without error", {
  capture.output(expect_no_error(print_time()))
})

test_that("get_table runs without error", {
  capture.output(expect_no_error(get_table(iris)))
})

test_that("get_size runs without error", {
  capture.output(expect_no_error(get_size(iris)))
})

test_that("clear_data clears environment of values", {
  x <- 7
  clear_data()
  values <- setdiff(ls(envir = .GlobalEnv), lsf.str(envir = .GlobalEnv))
  expect_equal(length(values), 0)
})
