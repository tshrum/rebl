test_that("id_rebl_items works with basic patterns", {
  # Create test data frame
  test_df <- data.frame(
    respondent_id = 1:5,
    food_item1 = c("Yes", "No", "Yes", "No", "Yes"),
    food_item2 = c("No", "Yes", "No", "Yes", "No"),
    pack_item1 = c("Yes", "Yes", "No", "No", "Yes"),
    water_item1 = c("No", "No", "Yes", "Yes", "No"),
    other_var = c("A", "B", "C", "D", "E")
  )
  
  # Test basic pattern matching
  food_items <- id_rebl_items(test_df, "^food")
  expect_equal(food_items, c("food_item1", "food_item2"))
  
  # Test multiple patterns with OR
  multi_items <- id_rebl_items(test_df, "^food|^pack|^water")
  expect_length(multi_items, 4)
  expect_true(all(c("food_item1", "food_item2", "pack_item1", "water_item1") %in% multi_items))
})

test_that("id_rebl_items handles edge cases", {
  # Empty data frame
  empty_df <- data.frame()
  expect_equal(id_rebl_items(empty_df, "test"), character(0))
  
  # No matching patterns
  test_df <- data.frame(a = 1, b = 2)
  expect_equal(id_rebl_items(test_df, "nonexistent"), character(0))
  
  # Test with ignore.case
  test_df <- data.frame(Food_Item = 1, PACK_item = 2)
  items_case <- id_rebl_items(test_df, "food", ignore.case = TRUE)
  expect_equal(items_case, "Food_Item")
})

test_that("id_rebl_items validates inputs", {
  test_df <- data.frame(a = 1, b = 2)
  
  # Non-data.frame input
  expect_error(id_rebl_items(list(a = 1), "pattern"))
  expect_error(id_rebl_items(matrix(1:4, nrow = 2), "pattern"))
  
  # Non-character pattern
  expect_error(id_rebl_items(test_df, 123))
  # Note: Multiple patterns don't error but warn - testing this separately
})

test_that("recode_rebl converts Yes/No to 1/0", {
  # Create test data
  test_df <- data.frame(
    respondent_id = 1:3,
    item1 = c("Yes", "No", "Yes"),
    item2 = c("No", "Yes", "No"),
    other_var = c("A", "B", "C")
  )
  
  rebl_items <- c("item1", "item2")
  result <- recode_rebl(test_df, rebl_items)
  
  # Check that Yes/No are converted to 1/0
  expect_equal(result$item1, c(1, 0, 1))
  expect_equal(result$item2, c(0, 1, 0))
  
  # Check that other columns are unchanged
  expect_equal(result$other_var, test_df$other_var)
  expect_equal(result$respondent_id, test_df$respondent_id)
})

test_that("recode_rebl handles missing items with warning", {
  test_df <- data.frame(
    respondent_id = 1:3,
    item1 = c("Yes", "No", "Yes")
  )
  
  # Test with missing items
  rebl_items <- c("item1", "missing_item")
  expect_warning(
    result <- recode_rebl(test_df, rebl_items),
    "Some REBL items missing"
  )
  
  # Should still work for existing items
  expect_equal(result$item1, c(1, 0, 1))
})

test_that("recode_rebl validates inputs", {
  test_df <- data.frame(item1 = c("Yes", "No"))
  
  # Non-data.frame input
  expect_error(recode_rebl(list(item1 = "Yes"), "item1"))
  
  # Non-character rebl_items
  expect_error(recode_rebl(test_df, 123))
  expect_error(recode_rebl(test_df, factor("item1")))
})

test_that("id_reversed_rebl_items works correctly", {
  # Test basic functionality
  rebl_items <- c("food_access", "food_quality", "pack_lunch", "water_access")
  reversed <- id_reversed_rebl_items(rebl_items, "^food")
  expect_equal(reversed, c("food_access", "food_quality"))
  
  # Test with no matches
  no_match <- id_reversed_rebl_items(rebl_items, "^nonexistent")
  expect_equal(no_match, character(0))
})

test_that("reverse_code_rebl_items works correctly", {
  # Create test data with 0/1 coded items
  test_df <- data.frame(
    respondent_id = 1:3,
    item1 = c(1, 0, 1),
    item2 = c(0, 1, 0),
    other_var = c("A", "B", "C")
  )
  
  reversed_items <- c("item1")
  result <- reverse_code_rebl_items(test_df, reversed_items)
  
  # Check that item1 is reversed
  expect_equal(result$item1, c(0, 1, 0))
  
  # Check that other items are unchanged
  expect_equal(result$item2, test_df$item2)
  expect_equal(result$other_var, test_df$other_var)
})