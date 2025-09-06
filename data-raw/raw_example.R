# Making 'raw' example dataset from the existing example.
# Should have yes/no REBL items
str(example)
raw_example <- example %>%
  mutate(across(!respondent_id, ~ case_when(
    .x == 1 ~ 'Yes',
    .x == 0 ~ 'No',
    .default = NA_character_
  )))
str(raw_example)

usethis::use_data(raw_example, overwrite = TRUE)
