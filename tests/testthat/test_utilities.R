# # Comprehensive Tests for Convenience Functions
#
# # get_size() Tests --------------------------------------------------------
#
# test_that("get_size handles different object types", {
#   # Test with different data types
#   expect_no_error(get_size(iris))
#   expect_no_error(get_size(list(a = 1:100, b = letters)))
#   expect_no_error(get_size(matrix(1:100, nrow = 10)))
#   expect_no_error(get_size(c("a", "b", "c")))
#   expect_no_error(get_size(factor(c("a", "b", "c"))))
# })
#
# test_that("get_size handles NULL and empty objects", {
#   expect_no_error(get_size(NULL))
#   expect_no_error(get_size(numeric(0)))
#   expect_no_error(get_size(character(0)))
#   expect_no_error(get_size(list()))
# })
#
#
# # get_str() Tests ---------------------------------------------------------
#
# test_that("get_str works with data frames", {
#   expect_no_error(capture.output(get_str(iris)))
#   expect_no_error(capture.output(get_str(iris, level = 1)))
#   expect_no_error(capture.output(get_str(iris, all_cols = FALSE)))
# })
#
# test_that("get_str works with lists", {
#   test_list <- list(a = 1:10, b = letters[1:5], c = list(d = 1:3))
#   expect_no_error(capture.output(get_str(test_list)))
#   expect_no_error(capture.output(get_str(test_list, level = 1)))
#   expect_no_error(capture.output(get_str(test_list, all_cols = FALSE)))
# })
#
# test_that("get_str handles different object types", {
#   expect_no_error(capture.output(get_str(1:100)))
#   expect_no_error(capture.output(get_str(matrix(1:20, nrow = 4))))
#   expect_no_error(capture.output(get_str(factor(letters[1:5]))))
#   expect_no_error(capture.output(get_str(c("a", "b", "c"))))
# })
#
# test_that("get_str handles edge cases", {
#   expect_no_error(capture.output(get_str(NULL)))
#   expect_no_error(capture.output(get_str(numeric(0))))
#   expect_no_error(capture.output(get_str(data.frame())))
#   expect_no_error(capture.output(get_str(list())))
# })
#
# test_that("get_str returns invisibly", {
#   result <- capture.output(get_str(iris))
#   expect_null(get_str(iris))
# })
#
#
# # clear_data() Tests ------------------------------------------------------
#
# test_that("clear_data removes values but keeps functions", {
#   # Setup test environment - save current state
#   old_objects <- ls(envir = .GlobalEnv)
#
#   # Create test objects
#   assign("test_var", 123, envir = .GlobalEnv)
#   assign("test_df", data.frame(x = 1:3), envir = .GlobalEnv)
#   assign("test_list", list(a = 1, b = 2), envir = .GlobalEnv)
#
#   # Create a test function
#   assign("test_func", function(x) x + 1, envir = .GlobalEnv)
#
#   # Count functions before clearing
#   before_funcs <- lsf.str(envir = .GlobalEnv)
#
#   # Clear data
#   clear_data()
#
#   # Count after clearing
#   after_funcs <- lsf.str(envir = .GlobalEnv)
#
#   # Functions should be preserved
#   expect_true("test_func" %in% after_funcs)
#
#   # Data objects should be removed
#   current_objects <- ls(envir = .GlobalEnv)
#   expect_false("test_var" %in% current_objects)
#   expect_false("test_df" %in% current_objects)
#   expect_false("test_list" %in% current_objects)
#
#   # Clean up the test function
#   if ("test_func" %in% ls(envir = .GlobalEnv)) {
#     rm(test_func, envir = .GlobalEnv)
#   }
# })
#
# test_that("clear_data runs without error", {
#   # Should not error when called
#   expect_no_error(clear_data())
# })
#
# test_that("clear_data preserves functions correctly", {
#   # Create a test function
#   assign("test_preserve_func", function(x) x^2, envir = .GlobalEnv)
#
#   # Clear data
#   clear_data()
#
#   # Function should still exist
#   expect_true("test_preserve_func" %in% ls(envir = .GlobalEnv))
#   expect_true("test_preserve_func" %in% lsf.str(envir = .GlobalEnv))
#
#   # Clean up
#   if ("test_preserve_func" %in% ls(envir = .GlobalEnv)) {
#     rm(test_preserve_func, envir = .GlobalEnv)
#   }
# })
#
# test_that("functions handle special values correctly", {
#   # Test with Inf, -Inf, NaN
#   special_vector <- c(1, 2, Inf, -Inf, NaN, NA)
#
#   expect_no_error(get_size(special_vector))
#   expect_no_error(get_table(special_vector))
#   expect_no_error(capture.output(get_str(special_vector)))
# })
