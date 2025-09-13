test_that("get_rasch_model validates inputs correctly", {
  # Create minimal test data
  test_df <- data.frame(
    respondent_id = 1:5,
    item1 = c(1, 0, 1, 0, 1),
    item2 = c(0, 1, 0, 1, 0)
  )

  rebl_items <- c("item1", "item2")

  # Test input validation - non-data.frame
  expect_error(
    get_rasch_model(list(), "respondent_id", rebl_items),
    "df must be a data.frame"
  )

  # Test input validation - non-character id
  expect_error(
    get_rasch_model(test_df, 123, rebl_items),
    "id variable must be a character"
  )

  # Test input validation - id not in df
  expect_error(
    get_rasch_model(test_df, "nonexistent_id", rebl_items),
    "id variable is not found in the df"
  )

  # Test input validation - no rebl_items in df
  expect_error(
    get_rasch_model(test_df, "respondent_id", c("nonexistent")),
    "None of the rebl_items are found as columns"
  )
})

test_that("get_rasch_model handles missing items with warning", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  test_df <- data.frame(
    respondent_id = as.character(1:10),
    item1 = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)
  )

  # Test single missing item
  rebl_items_single <- c("item1", "item2", "missing_item")
  expect_warning(
    model <- get_rasch_model(test_df, "respondent_id", rebl_items_single, type = 'mml_con'),
    "1 REBL item\\(s\\) not included in data frame: missing_item"
  )
  expect_s3_class(model, "rasch")

  # Test multiple missing items
  rebl_items_multiple <- c("item1", "item2", "missing_item1", "missing_item2", "missing_item3")
  expect_warning(
    model2 <- get_rasch_model(test_df, "respondent_id", rebl_items_multiple, type = 'mml_con'),
    "3 REBL item\\(s\\) not included in data frame: missing_item1, missing_item2, missing_item3"
  )
  expect_s3_class(model2, "rasch")
})

test_that("get_rasch_model creates valid Rasch model", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Create test data with sufficient observations for Rasch model
  set.seed(42)
  test_df <- data.frame(
    respondent_id = as.character(1:20),
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE),
    item3 = sample(c(0, 1), 20, replace = TRUE)
  )

  rebl_items <- c("item1", "item2", "item3")

  # Create model
  model <- get_rasch_model(test_df, "respondent_id", rebl_items, type = 'mml_con')

  # Check that it's a valid rasch object
  expect_s3_class(model, "rasch")
})

test_that("get_rasch_model works with character and numeric IDs", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Test with character IDs
  test_df_char <- data.frame(
    id = as.character(1:15),
    item1 = rep(c(1, 0, 1), 5),
    item2 = rep(c(0, 1, 0), 5)
  )

  rebl_items <- c("item1", "item2")
  model_char <- get_rasch_model(test_df_char, "id", rebl_items, type = 'mml_con')
  expect_s3_class(model_char, "rasch")

  # Test with numeric IDs converted to character
  test_df_num <- data.frame(
    participant = 1:15,
    item1 = rep(c(1, 0, 1), 5),
    item2 = rep(c(0, 1, 0), 5)
  )

  model_num <- get_rasch_model(test_df_num, "participant", rebl_items, type = 'mml_con')
  expect_s3_class(model_num, "rasch")
})

test_that("get_rasch_model handles edge cases", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Test with minimum viable data
  min_df <- data.frame(
    id = as.character(1:10),
    item1 = c(rep(0, 5), rep(1, 5)),
    item2 = c(rep(1, 5), rep(0, 5))
  )

  model <- get_rasch_model(min_df, "id", c("item1", "item2"), type = 'mml_con')
  expect_s3_class(model, "rasch")

  # Test with all items present but some missing from rebl_items list
  all_items_df <- data.frame(
    id = as.character(1:10),
    item1 = c(rep(0, 5), rep(1, 5)),
    item2 = c(rep(1, 5), rep(0, 5)),
    item3 = rep(c(0, 1), 5)
  )

  # Only specify some items
  partial_items <- c("item1", "item3")
  model_partial <- get_rasch_model(all_items_df, "id", partial_items, type = 'mml_con')
  expect_s3_class(model_partial, "rasch")
  expect_equal(ncol(model_partial$X), 2)
})
