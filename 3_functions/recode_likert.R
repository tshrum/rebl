# recode_likert
# 2024.04.07

# Automatically look for column names containing efficacy, ccBelief, enviroID,
# greenGlow, worry, attribution, socialNorms, eweExpGen, psychDist, or hemp and
# recode them as 7pt or binary as appropriate.

# Note that this really only applies to surveys 2a and 2b. Different procedures
# needed for survey 1 and ewe validation set.

# Input: DF that contains any of these categories of Likert questions

# Output: Same DF but recoded numeric 0 to 6 or 0 to 1 for binary



# Load Packages -----------------------------------------------------------


pacman::p_load(dplyr)



# Function ----------------------------------------------------------------


recode_likert <- function(df) {
  
  # Define search parameters
  pattern_11pt <- paste('personalResponsibility')
  
  pattern_7pt <- paste(
    'enviroID',
    'greenG',
    'effi',
    'ccB',
    'eweExpGen',
    'attributionGen3',
    'attributionGen4',
    'attributionGen5',
    'attributionSpec',
    'worry\\d{1}',
    'psych',
    'socialNorms',
    'personalNorm',
    '^pd',
    '^att\\d{1}',
    '^eewe',
    sep = '|'
  )
  
  pattern_5pt <- paste('ccWorry')
  
  pattern_binary <- paste('attributionGen1',
                          'attributionGen2',
                          'extremeWeatherF',
                          'eweExpSpec',
                          sep = '|')
  
  # Mutate across those matching columns to recode
  dat <- df |>
    mutate(
      
      across(
        matches(pattern_7pt),
        ~ case_match(
          .x,
          "Strongly disagree" ~ 0,
          "Disagree" ~ 1,
          "Somewhat disagree" ~ 2,
          "Neither agree nor disagree" ~ 3,
          "Somewhat agree" ~ 4,
          "Agree" ~ 5,
          "Strongly agree" ~ 6
        )
      ),
      
      across(matches(pattern_binary),
             ~ case_match(.x,
                          'Yes' ~ 1,
                          'No' ~ 0)),
      
      across(
        matches(pattern_11pt),
        ~ case_match(.x,
                     '0 (Not at all)' ~ '0',
                     '10 (A great deal)' ~ '10',
                     .default = .x
                     ) %>% as.numeric()
      ),
      
      across(
        matches(pattern_5pt),
        ~ case_match(
          .x,
          'Not at all worried' ~ 0,
          "Not very worried" ~ 1,
          "Somewhat worried" ~ 2,
          "Very worried" ~ 3,
          "Extremely worried" ~ 4,
          .default = NA
          )
      ),
      
      across(
        matches('^values'),
        ~ case_when(
          str_detect(.x, 'Opposed') ~ -1,
          str_detect(.x, 'Not') ~ 0,
          .x == '1' ~ 1,
          .x == '2' ~ 2,
          .x == '3' ~ 3,
          .x == '4' ~ 4,
          .x == '5' ~ 5,
          .x == '6' ~ 6,
          str_detect(.x, 'Of') ~ 7,
          .default = NA
        )
      )
      
    )
  
  return(dat)
  
}
