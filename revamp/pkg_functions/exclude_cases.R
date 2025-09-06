#' Exclude Cases from Survey Data
#'
#' Turning case exclusion into a function that can apply to any REBL survey. It
#' removes the 2 rows of Qualtrics nonsense, removes survey previews, and filters
#' by consent, start date, end date, and duration.
#'
#' @param df Survey dataframe
#' @param start_date Start date ('YYYY-MM-DD') inclusive
#' @param end_date End date ('YYYY-MM-DD') inclusive  
#' @param bs_check_1 Variable name as character for first BS check
#' @param bs_check_2 Variable name as character for second BS check
#' @param vegan_meat_bs_check Logical, whether to apply vegan/meat consistency check
#' @param attention_check_cutoff Integer, minimum number of attention checks to pass
#' @param reasonable_na_count Numeric, reasonable number of NAs for completion calculation
#' @param completion_cutoff Numeric (0 to 1), minimum completion rate
#' @param duration_percentile Numeric (0 to 1), percentile cutoff for duration filtering
#'
#' @return New filtered dataframe, along with a readout of how many were filtered at each step
#' @export
#'
#' @details 
#' It requires the same variable names! Make sure they are named the same way.
#' It requires user to figure out a reasonable number of NAs for that survey.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' clean_data <- exclude_cases(
#'   df = raw_survey,
#'   start_date = "2024-01-01",
#'   end_date = "2024-12-31",
#'   bs_check_1 = "foodForage",
#'   bs_check_2 = "foodForageBSCheck", 
#'   attention_check_cutoff = 2,
#'   completion_cutoff = 0.8,
#'   duration_percentile = 0.05
#' )
#' }
exclude_cases <- function(df,
                          start_date = NULL,
                          end_date = NULL,
                          bs_check_1 = NULL,
                          bs_check_2 = NULL,
                          vegan_meat_bs_check = NULL,
                          attention_check_cutoff = NULL,
                          reasonable_na_count = NULL,
                          completion_cutoff = NULL,
                          duration_percentile = NULL)
{

  cat('\n~~~~~ Output from exclude_cases ~~~~~\n')
  
  # Initial Dimensions
  cat('\nInitial dimensions are',
      nrow(df),
      'rows and',
      ncol(df),
      'columns.\n')
  
  # Lose Javascript nonsense -----
  # If there is gross javascript shit in 2nd row, lose first 2 rows.
  # Legacy of Qualtrics output. First row is repeat of name, 2nd row is Java
  if(stringr::str_detect(df[2, 1], 'ImportId') == TRUE) {
    
    df <- df[-c(1:2), ]
    
    cat(
      '\n',
      nrow(df),
      ' rows remain after removing 2 rows of Qualtrics junk.\n',
      sep = ''
      )
  } else {
    
    cat('\nQualtrics Javascript junk already removed!\n')
    
  }
  
  # Previews -----
  df <- df |> 
    dplyr::filter(Status != 'Survey Preview')
  
  cat(
    '\n',
    nrow(df),
    ' recruits began the study (not Survey Previews).\n',
    sep = ''
    )

  
  # Recaptcha -----
  df <- df |> 
    dplyr::filter(Q_RecaptchaScore > 0.5)
  
  cat(
    '\n',
    nrow(df),
    ' participants were not bots! (Recaptcha score > 0.5).\n',
    sep = ''
  )
  
  # Consent -----
  df <- dplyr::filter(df, consent == 'Yes')
  cat(
    '\n*',
    nrow(df),
    ' participants consented to the study.*\n',
    sep = ''
  )
  
  # Preserve number who consented for later
  consented <- df
  
  
  # start_date and end_date -----

  if (!is.null(start_date) & !is.null(end_date)) {

    # Convert to posix
    df <- df |>
      dplyr::mutate(dplyr::across(c(StartDate, EndDate), lubridate::as_date))
    
    # Filter by start and end dates
    df <-
      dplyr::filter(df, EndDate <= lubridate::ymd(end_date) & StartDate >= lubridate::ymd(start_date))
    
    cat(
      '\n',
      nrow(df),
      ' participants took the survey between ',
      start_date,
      ' and ',
      end_date,
      '.\n',
      sep = ''
    )
  } else {
    
    cat('\nDid not filter by date.\n')
    
  }
  
  # Save number who started to calculate percentage
  started_in_window <- nrow(df)

  # How many completed -----
  df <- dplyr::filter(df, Finished == 'TRUE')
  # Note that this is a character TRUE, not boolean TRUE
  
  # Completion rate
  completed <- nrow(df)
  completion_rate <- round(completed/started_in_window, 3)
  
  cat(
    '\n',
    nrow(df),
    ' participants finished the survey, for a ',
    completion_rate,
    ' completion rate.\n',
    sep = ''
  )
  
  # Attention Checks -----
  df <- df |> 
    dplyr::mutate(pass_count = rowSums(dplyr::across(dplyr::starts_with('attention')))) |> 
    dplyr::filter(pass_count >= attention_check_cutoff)
  
  cat(
    '\n',
    nrow(df),
    ' participants passed at least ',
    attention_check_cutoff,
    ' out of the ',
    sum(stringr::str_count(names(df), 'ttention')),
    ' attention checks.\n',
    sep = ''
  )
  
  # BS Check -----
  if (!is.null(bs_check_1) & !is.null(bs_check_2)) {
    
    df <- df |> 
      dplyr::filter(df[[bs_check_1]] == df[[bs_check_2]])
    
    cat(
      '\n',
      nrow(df),
      ' participants remain after foraging BS check (',
      bs_check_1,
      ' == ',
      bs_check_2,
      ').\n',
      sep = ''
    )
    
  } else {
    
    cat(
      '\nNo foraging BS check provided.\n'
    )
    
  }
  
  # Vegan Meat BS Check -----
  # Remove people who said they were vegan but ate meat
  # Also remove people who said they didn't eat meat but then said they did
  if (vegan_meat_bs_check == TRUE) {
    
    # Need branching paths - no cowmilk question in test set
    if ('foodCowMilk_r' %in% names(df)) {
      df <- df |> 
        dplyr::filter(!(foodVegan_ == 1 & (foodCowMilk_r == 0 |
                                    foodBeef_r == 0 |
                                    foodMeat_r == 0 |
                                    foodMeatEveryDay_r == 0))) |> 
        dplyr::filter(!(foodMeat_r == 1 & (foodBeef_r == 0 | 
                                    foodMeatEveryDay_r == 0)))
    } else {
      df <- df |> 
        dplyr::filter(!(foodVegan_ == 1 & (foodBeef_r == 0 |
                                    foodMeat_r == 0 |
                                    foodMeatEveryDay_r == 0))) |> 
        dplyr::filter(!(foodMeat_r == 1 & (foodBeef_r == 0 | 
                                    foodMeatEveryDay_r == 0)))
    }
    
    
    cat(
      '\n',
      nrow(df),
      ' participants remain after filtering by vegan/meat and "no meat"/meat ',
      'BS checks.\n',
      sep = ''
    )
  } else {
    
    cat(
      '\nNo vegan/meat BS check provided.\n'
    )
    
  }
  
  
  
  # Completion -----
  if (!is.null(reasonable_na_count) & !is.null(completion_cutoff)) {
    
    # Filter by proportion complete
    df <- df %>%
      dplyr::mutate(
        na_count = rowSums(
          is.na(.[, (which(names(.) == "prolificID") + 1):(ncol(.) - 1)])
          ),
        prop_complete = (ncol(.) - na_count + reasonable_na_count) / ncol(.)) %>% 
      dplyr::filter(prop_complete >= completion_cutoff)
    
    cat(
      '\n',
      nrow(df),
      ' participants remain after filtering by completion (proportion complete >= ',
      completion_cutoff,
      '). This accounts for ',
      reasonable_na_count,
      ' reasonable NAs. Note we are only counting from REBL items ',
      'through end of demographics.\n',
      sep = ''
    )
    
  } else {
    cat(
      '\nNot filtering by question completion (NAs).\n'
    )
  }
  
  
  # Duration -----
  df <- df |> 
    dplyr::mutate(duration = as.numeric(duration)) |> 
    dplyr::filter(duration > stats::quantile(as.numeric(consented$duration), 
                               duration_percentile,
                               na.rm = TRUE))
  
  cat(
    '\n',
    nrow(df),
    ' participants remain after removing the fastest ',
    duration_percentile * 100,
    '%. Note that this is based on the speed of those in the original, ',
    'unfiltered dataset.\n',
    sep = ''
  )
  
  
  # Final readout -----
  kept <- round(((nrow(df)/nrow(consented)) * 100), 2)
  cat(
    '\nOut of the original ',
    nrow(consented),
    ' participants (not bots, not previews, did consent), we kept ',
    nrow(df),
    ' (',
    kept,
    '%).\n\n',
    sep = ''
  )
  
  
  # Remove extraneous columns and return -----
  df <- df |>
    dplyr::select(-(pass_count:dplyr::last_col()))
  
  return(df)
}