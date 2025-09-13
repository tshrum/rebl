test_that("fix_lumped_ltm_scores handles basic lumping scenario", {
  # Skip if ltm not available
  skip_if_not_installed("ltm")
  
  # Create test data with duplicate response patterns
  test_df <- data.frame(
    id = c("1", "2", "3", "4", "5"),
    item1 = c(1, 0, 1, 0, 1),
    item2 = c(1, 0, 1, 0, 1),
    item3 = c(0, 1, 0, 1, 0)
  )
  
  rebl_items <- c("item1", "item2", "item3")
  
  # Create mock scores object similar to ltm output
  mock_scores <- list(
    score.dat = data.frame(
      item1 = c(1, 0),
      item2 = c(1, 0), 
      item3 = c(0, 1),
      z1 = c(1.5, -1.5),  # Mock ability scores
      se.z1 = c(0.5, 0.5)
    )
  )
  
  result <- fix_lumped_ltm_scores(test_df, mock_scores, rebl_items)
  
  # Check structure
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 5)  # Should match original df
  expect_true("id" %in% names(result))
  expect_true("z1" %in% names(result))
  expect_true(all(result$id %in% c("1", "2", "3", "4", "5")))
})

test_that("fix_lumped_ltm_scores preserves original ordering", {
  # Skip if ltm not available  
  skip_if_not_installed("ltm")
  
  # Create test data
  test_df <- data.frame(
    id = c("P003", "P001", "P005", "P002", "P004"),
    item1 = c(1, 0, 1, 0, 1),
    item2 = c(0, 1, 0, 1, 0)
  )
  
  rebl_items <- c("item1", "item2")
  
  # Mock scores
  mock_scores <- list(
    score.dat = data.frame(
      item1 = c(1, 0),
      item2 = c(0, 1),
      z1 = c(1.2, -0.8)
    )
  )
  
  result <- fix_lumped_ltm_scores(test_df, mock_scores, rebl_items)
  
  # Check that original order is preserved by ID
  expect_equal(result$id[1], "P003")  # First in original data
  expect_equal(result$id[2], "P001")  # Second in original data
  expect_equal(nrow(result), 5)
})

test_that("fix_lumped_ltm_scores handles missing patterns gracefully", {
  # Skip if ltm not available
  skip_if_not_installed("ltm")
  
  # Create test data
  test_df <- data.frame(
    id = c("1", "2", "3"),
    item1 = c(1, 0, 1),
    item2 = c(1, 0, 0)
  )
  
  rebl_items <- c("item1", "item2")
  
  # Mock scores with only partial patterns
  mock_scores <- list(
    score.dat = data.frame(
      item1 = c(1, 0),
      item2 = c(1, 0),
      z1 = c(2.0, -1.0)
    )
  )
  
  result <- fix_lumped_ltm_scores(test_df, mock_scores, rebl_items)
  
  # Should handle gracefully
  expect_true(is.data.frame(result))
  expect_true(any(is.na(result$z1)))  # Pattern (1,0) not in mock_scores
})

test_that("fix_lumped_ltm_scores removes Obs column when present", {
  # Skip if ltm not available
  skip_if_not_installed("ltm")
  
  test_df <- data.frame(
    id = c("1", "2"),
    item1 = c(1, 0),
    item2 = c(1, 0)
  )
  
  rebl_items <- c("item1", "item2")
  
  # Mock scores with Obs column (common in ltm output)
  mock_scores <- list(
    score.dat = data.frame(
      item1 = c(1, 0),
      item2 = c(1, 0),
      Obs = c(1, 1),  # Should be removed
      z1 = c(1.5, -1.5)
    )
  )
  
  result <- fix_lumped_ltm_scores(test_df, mock_scores, rebl_items)
  
  # Obs column should be removed
  expect_false("Obs" %in% names(result))
  expect_true("z1" %in% names(result))
})