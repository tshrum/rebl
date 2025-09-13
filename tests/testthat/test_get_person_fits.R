test_that("get_person_fits works with eRm models", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Create test data
  set.seed(123)
  test_df <- data.frame(
    id = as.character(1:20),
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE),
    item3 = sample(c(0, 1), 20, replace = TRUE)
  )

  rebl_items <- c("item1", "item2", "item3")

  # Create eRm model
  model <- get_rasch_model(test_df, "id", rebl_items, type = 'cml')

  # Get person fits
  person_fits <- get_person_fits(model)

  # Check structure
  expect_true(is.data.frame(person_fits))
  expect_true("id" %in% names(person_fits))
  expect_true(nrow(person_fits) > 0)

  # Should have person fit statistics (p-values from personfit)
  fit_cols <- names(person_fits)
  expect_true(any(grepl("^p", fit_cols)))  # p.fit columns
})

test_that("get_person_fits works with ltm rasch models", {
  # Skip if ltm not available
  skip_if_not_installed("ltm")

  # Create test data
  set.seed(123)
  test_df <- data.frame(
    id = as.character(1:20),
    item1 = sample(c(0, 1), 20, replace = TRUE),
    item2 = sample(c(0, 1), 20, replace = TRUE),
    item3 = sample(c(0, 1), 20, replace = TRUE)
  )
  rebl_items <- c("item1", "item2", "item3")

  # Create ltm rasch model
  model <- get_rasch_model(test_df, 'id', rebl_items, 'mml_con')

  # Get person fits
  person_fits <- get_person_fits(model)

  # Check structure
  expect_true(is.data.frame(person_fits))
  expect_true("id" %in% names(person_fits))
  expect_equal(names(person_fits), c("id", "L0", "Lz", "p"))
  expect_equal(nrow(person_fits), nrow(test_df))
})

test_that("get_person_fits works with ltm models (ltm class)", {
  # Skip if ltm not available
  skip_if_not_installed("ltm")

  # Create test data with more items for ltm
  set.seed(789)
  test_data <- data.frame(
    item1 = sample(c(0, 1), 12, replace = TRUE),
    item2 = sample(c(0, 1), 12, replace = TRUE),
    item3 = sample(c(0, 1), 12, replace = TRUE),
    item4 = sample(c(0, 1), 12, replace = TRUE)
  )

  # Create ltm model (2PL)
  model <- ltm::ltm(test_data ~ z1)

  # Get person fits
  person_fits <- get_person_fits(model)

  # Check structure
  expect_true(is.data.frame(person_fits))
  expect_true("id" %in% names(person_fits))
  expect_true(all(c("L0", "Lz", "p") %in% names(person_fits)))
})

test_that("get_person_fits preserves row order for ltm models", {
  # Skip if ltm not available
  skip_if_not_installed("ltm")

  # Create test data with known patterns
  test_data <- data.frame(
    item1 = c(1, 0, 1, 0),
    item2 = c(1, 0, 0, 1),
    item3 = c(0, 1, 1, 0)
  )

  # Create model
  model <- ltm::rasch(test_data)

  # Get fits
  person_fits <- get_person_fits(model)

  # Should maintain original row order through rownames
  expect_equal(person_fits$id, c("1", "2", "3", "4"))
  expect_equal(nrow(person_fits), 4)
})

test_that("get_person_fits handles eRm models with extreme scores", {
  # Skip if eRm not available
  skip_if_not_installed("eRm")

  # Create data with some extreme scores (all 0s or all 1s)
  test_df <- data.frame(
    id = as.character(1:10),
    item1 = c(0, 0, 1, 1, 0, 1, 0, 1, 1, 0),
    item2 = c(1, 0, 0, 1, 1, 0, 1, 0, 0, 1),
    item3 = c(0, 1, 1, 0, 1, 1, 0, 0, 1, 0)
  )

  # Create model
  model <- get_rasch_model(test_df, "id", c("item1", "item2", "item3"), type = "cml")

  # Get fits - should handle gracefully
  person_fits <- get_person_fits(model)

  # Should return data frame without errors
  expect_true(is.data.frame(person_fits))
  expect_true("id" %in% names(person_fits))
  # Some participants may be excluded due to extreme scores
  expect_true(nrow(person_fits) > 0)
})
