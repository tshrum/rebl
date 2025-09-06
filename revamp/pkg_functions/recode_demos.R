#' Recode Demographics
#'
#' Recode demographic questions for surveys 2a and 2b. Not sure the questions
#' are the same on surveys 1 and ewe validation set... will have to explore that.
#'
#' @param df Survey dataframe
#'
#' @return Dataframe with demographics recoded
#' @export
#'
#' @details 
#' Leaving rurality, election, and politics as categorical for now. 
#' Age and income are currently midpoints
#' Education is binary (0 = no bachelor's degree, 1 = bachelor's degree or higher)
#' Gender is binary (0 = male, 1 = female/other)
#' Race is handled differently for survey 1 vs others
#'
#' @examples
#' \dontrun{
#' # Example usage
#' recoded_data <- recode_demos(survey_data)
#' }
recode_demos <- function(df) {
  
  dat <- df |>
    dplyr::mutate(
      age = dplyr::case_match(
        age,
        "18-24" ~ 21,
        "25-34" ~ 29.5,
        "35-44" ~ 39.5,
        "45-54" ~ 49.5,
        "55-64" ~ 59.5,
        "65-74" ~ 69.5,
        "75 or older" ~ 75,
        .default = NA_real_
      ),
      
      gender = dplyr::case_match(
        gender,
        "Female" ~ 1,
        "Male" ~ 0,
        "Other" ~ 1,
        .default = NA_real_
      ),
      
      education = dplyr::case_match(
        education,
        "Some High School" ~ 0,
        "High School Diploma" ~ 0,
        "Some College" ~ 0,
        "Associate's Degree" ~ 0,
        "Trade School" ~ 0,
        "Bachelor's Degree" ~ 1,
        "Master's Degree" ~ 1,
        "Doctorate (Ph.D., J.D., M.D., etc.)" ~ 1,
        .default = NA_real_
      ),
      
      income = dplyr::case_match(
        income,
        "Less than $25,000" ~ 12500,
        "$25,000 - $50,000" ~ 37500,
        "$50,000 - $100,000" ~ 75000,
        "$100,000 - $200,000" ~ 150000,
        "More than $200,000" ~ 200000,
        .default = NA_real_
      ),
      
      children = dplyr::case_match(
        children,
        "No" ~ 0,
        "Yes, younger than 18" ~ 1,
        "Yes, older than 18" ~ 2
      ),
    )
  
  if ('race' %in% names(dat)) {

    dat <- dat |>
      dplyr::mutate(
        race = dplyr::case_match(
          race,
          "American Indian or Alaska Native" ~ "native",
          "Asian or Asian American" ~ "asian",
          "Black or African American" ~ "black",
          "Hispanic or Latino" ~ "hispanic",
          "Other" ~ "other",
          "White or Caucasian" ~ "white",
          NA ~ NA,
          .default = "two_or_more",
        )
      )
    
  } else {

    cat('\nNOTE: Can\'t recode race for survey 1 with recode_demos function',
        'because it is already in dummy variables.\n',
        sep = ' ')

  }
  
  return(dat)
}