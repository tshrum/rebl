# recode_rebl
# 2024.04.07

# Recoding values of REBL items based on which option is the PEB. This will only
# apply to surveys 2a and 2b I think, but I suppose is worth lumping together.

# Input: Dataframe with cases excluded

# Output: Same dataframe with "Yes" and "No" coded to 1 and 0 as factors based 
# on which one is the PEB.

# Notes: Will intentionally throw an error if any of the columns don't exist.



# Packages ----------------------------------------------------------------


pacman::p_load(dplyr)



# Function ----------------------------------------------------------------


recode_rebl <- function(df) {
  
  # Load data frame of rebl items and coding
  rebl_coding <- readRDS('5_objects/cleaning/rebl_item_coding.rds') %>% 
    select(rebl_item, peb_coding) %>% 
    unique()
  
  # Get standard
  standard_questions <- rebl_coding %>% 
    filter(peb_coding == "standard") %>% 
    pull(rebl_item)
  
  # Get reversed
  reversed_questions <- rebl_coding %>% 
    filter(peb_coding == "reverse") %>% 
    pull(rebl_item)
  
  # Recode in two groups. Note that we are recoding the forage BS check here 
  # manually because we did not include it as a REBL item originally because
  # it is duplicated.
  dat <- df |>
    mutate(across(any_of(c(standard_questions, 'foodForageBSCheck')),
                  ~ case_match(., "Yes" ~ 1, "No" ~ 0)),
           across(any_of(reversed_questions),
                  ~ case_match(., "Yes" ~ 0, "No" ~ 1)))
  
  return(dat)
  
}

