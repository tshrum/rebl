# Example workflow using REBL package functions


# Cleaning ----------------------------------------------------------------

# Example data frame
str(raw_example)

# Identify REBL items
(rebl_items <- id_rebl_items(raw_example, '^(?!res).*', perl = TRUE))

# Recode REBL items from Yes/No to 1/0
df <- recode_rebl(raw_example, rebl_items)
str(df)

# To recode reversed REBL items, first identify them.
# Just using all food items as an example
(reversed_items <- id_reversed_rebl_items(rebl_items, '^food'))

# Now recode the reversed items in the df
df <- reverse_code_rebl_items(df, reversed_items)
str(df)



# REBL scores -------------------------------------------------------------


# Run Rasch Model
model <- get_rasch_model(df, 'respondent_id', rebl_items)
str(model)

# Throws warning if some items are not found in df
model <- get_rasch_model(df, 'respondent_id', c(rebl_items, 'ffff', 'zzzz'))
str(model)

# Get a df of REBL scores and person fit statistics
rebl_scores <- get_rebl_scores(model)
str(rebl_scores)

# Only REBL scores, no fits
rebl_only <- get_rebl_scores(model, include_fits = FALSE)
str(rebl_only)

# Add linking the plink. Return scaled and unscaled scores. Check Shiny
# Note that setting up the common items among scales will be tricky. Have to
# make a matrix showing common items.
# v1 <- c("a", "b", "c", "d")
# v2 <- c("b", "d", "e", "a")
#
# # Convert to numeric codes
# (v1_num <- match(v1, sort(unique(c(v1, v2)))))
# (v2_num <- match(v2, sort(unique(c(v1, v2)))))
#
# idx <- which(outer(v1_num, v2_num, "=="), arr.ind = TRUE)
# cbind(v1_num[idx[,1]], v2_num[idx[,2]])
#
# # common <- intersect(v1, v2)
# # mat <- cbind(common, common)
# # mat
#
# #      common common
# # [1,] "a"    "a"
# # [2,] "b"    "b"
# # [3,] "d"    "d"
#
# matches <- expand.grid(v1 = v1, v2 = v2)
# matches$v1 <- as.character(matches$v1)
# matches$v2 <- as.character(matches$v2)
# matches <- matches[matches$v1 == matches$v2, ]
# matches
#
# # Convert to matrix
# as.matrix(matches)

# Model Validation --------------------------------------------------------


# Item fits
# get_item_fits()

# Invariance: LR test - score, gender, rurality, income
# Unidimensionality



