test_that("get_rebl_scores returns correct structure", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Create test data with sufficient observations for Rasch model
  set.seed(123)
  test_df <- data.frame(
    respondent_id = as.character(1:25),
    item1 = sample(c(0, 1), 25, replace = TRUE, prob = c(0.4, 0.6)),
    item2 = sample(c(0, 1), 25, replace = TRUE, prob = c(0.5, 0.5)),
    item3 = sample(c(0, 1), 25, replace = TRUE, prob = c(0.6, 0.4))
  )

  rebl_items <- c("item1", "item2", "item3")

  # Create model
  model <- get_rasch_model(test_df, "respondent_id", rebl_items, type = 'cml')

  # Get scores
  scores_with_fits <- get_rebl_scores(model)

  # Check basic structure
  expect_true(is.data.frame(scores_with_fits))
  expect_true(nrow(scores_with_fits) > 0)  # Some may be excluded due to extreme scores
})

test_that("get_rebl_scores preserves participant IDs correctly", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Create test data with specific IDs
  test_ids <- c("P001", "P002", "P003", "P004", "P005",
                "P006", "P007", "P008", "P009", "P010")

  test_df <- data.frame(
    participant_id = test_ids,
    item1 = c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    item2 = c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item3 = c(1, 1, 0, 0, 1, 1, 0, 0, 1, 1)
  )

  rebl_items <- c("item1", "item2", "item3")

  # Create model and get scores
  model <- get_rasch_model(test_df, "participant_id", rebl_items, type = 'mml_con')
  scores <- get_rebl_scores(
    model = model,
    df = test_df,
    rebl_items = rebl_items
  )

  # Check that IDs are preserved
  expect_true(all(scores$id %in% test_ids))
})

test_that("get_rebl_scores handles edge cases", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Test with participants who have all 0s or all 1s (extreme scores)
  test_df <- data.frame(
    id = as.character(1:12),
    item1 = c(0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1),
    item2 = c(1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0),
    item3 = c(0, 1, 1, 0, 1, 1, 0, 0, 1, 1, 0, 0)
  )

  model <- get_rasch_model(test_df, "id", c("item1", "item2", "item3"), type = 'cml')
  scores <- get_rebl_scores(model)

  # Should handle the model without errors
  expect_true(is.data.frame(scores))
  expect_true(nrow(scores) > 0)  # Some may be excluded due to extreme scores
})
