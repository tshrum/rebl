test_that("get_rebl_scores returns correct structure with fits", {
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
  model <- get_rasch_model(test_df, "respondent_id", rebl_items)

  # Test with fits (default)
  scores_with_fits <- get_rebl_scores(model, include_fits = TRUE)

  # Check basic structure
  expect_true(is.data.frame(scores_with_fits))
  expect_true("rebl_score" %in% names(scores_with_fits))
  expect_true("id" %in% names(scores_with_fits))
  expect_true(nrow(scores_with_fits) > 0)  # Some may be excluded due to extreme scores

  # Should have fit statistics (more than just id and rebl_score)
  expect_true(ncol(scores_with_fits) > 2)

  # Check that fit statistics are included
  fit_cols <- c("p.fit", "outfit", "infit", "z.infit", "z.outfit", "disc")
  expect_true(any(fit_cols %in% names(scores_with_fits)))
})

test_that("get_rebl_scores returns correct structure without fits", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Create test data
  set.seed(456)
  test_df <- data.frame(
    respondent_id = as.character(1:20),
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE),
    item3 = sample(c(0, 1), 20, replace = TRUE)
  )

  rebl_items <- c("item1", "item2", "item3")

  # Create model
  model <- get_rasch_model(test_df, "respondent_id", rebl_items)

  # Test without fits
  scores_only <- get_rebl_scores(model, include_fits = FALSE)

  # Check structure
  expect_true(is.data.frame(scores_only))
  expect_equal(ncol(scores_only), 2)  # Only id and rebl_score
  expect_true(all(c("id", "rebl_score") %in% names(scores_only)))
  expect_true(nrow(scores_only) > 0)  # Some may be excluded due to extreme scores
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
  model <- get_rasch_model(test_df, "participant_id", rebl_items)
  scores <- get_rebl_scores(model)

  # Check that IDs are preserved (some may be excluded due to extreme scores)
  expect_true(all(scores$id %in% test_ids))
  expect_true(length(unique(scores$id)) > 0)
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

  model <- get_rasch_model(test_df, "id", c("item1", "item2", "item3"))
  scores <- get_rebl_scores(model)

  # Should handle the model without errors
  expect_true(is.data.frame(scores))
  expect_true(nrow(scores) > 0)  # Some may be excluded due to extreme scores
  expect_true(all(is.numeric(scores$rebl_score)))
})

test_that("get_rebl_scores default parameter works", {
  browser()
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Create simple test data - more variable to avoid extreme scores
  set.seed(999)
  test_df <- data.frame(
    id = as.character(1:20),
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE),
    item3 = sample(c(0, 1), 20, replace = TRUE),
    item4 = sample(c(0, 1), 20, replace = TRUE),
    item5 = sample(c(0, 1), 20, replace = TRUE)
  )

  model <- get_rasch_model(test_df, "id", c("item1", "item2"))

  # Test default (should include fits)
  scores_default <- get_rebl_scores(model)
  expect_true(ncol(scores_default) > 2)  # More than just id and rebl_score

  # Test explicit TRUE
  scores_explicit <- get_rebl_scores(model, include_fits = TRUE)
  expect_equal(ncol(scores_default), ncol(scores_explicit))
})
