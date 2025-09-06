# Prepare example dataset as modification of survey 2a


df <- read.csv('revamp/example.csv')
str(df)

# Sample 100 rows
set.seed(42)
df <- df %>%
  slice_sample(n = 100) %>%
  mutate(respondent_id = paste0('p', 1:100))
str(df)

# Add some noise
random_change <- function(col, seed = 42) {
  change <- rbinom(length(col), 1, 0.4)
  flipped <- ifelse(
    change == 1,
    case_when(
      col == 1 ~ 0,
      col == 0 ~ 1,
      TRUE ~ NA_real_
    ),
    col
  )
  flipped
}

set.seed(42)
out <- df %>%
  mutate(across(!respondent_id, ~ random_change(.x, seed = 42))
)
# out == df

# Make sure there are some vegans
# out$foodVegan[1:3] <- 1

example <- out
str(example)

usethis::use_data(example, overwrite = TRUE)
