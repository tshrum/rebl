#' Visualize Missingness by Demographics
#'
#' Create visualization plots showing missingness patterns across demographic variables.
#' This function creates scatter plots for education, race, gender, and income variables
#' showing their missingness patterns.
#'
#' @param df Dataframe containing demographic variables and missingness indicators
#' @param var Variable to plot on x-axis (unquoted variable name)
#'
#' @return List of ggplot objects showing missingness patterns for each demographic variable
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' miss_plots <- vis_mis_by_demo(survey_data, some_variable)
#' }
vis_mis_by_demo <- function(df, var){
  
  # enquo for some reason
  var <- rlang::enquo(var)
  
  # education
  a <- ggplot2::ggplot(df, 
         ggplot2::aes(x = !!var, 
             y = education, 
             color = education_NA)) + 
    ggplot2::geom_jitter()
  
  # race
  b <- ggplot2::ggplot(df, 
         ggplot2::aes(x = !!var, 
             y = race, 
             color = race_NA)) + 
    ggplot2::geom_jitter()
  
  # gender
  c <- ggplot2::ggplot(df, 
         ggplot2::aes(x = !!var,
             y = gender, 
             color = gender_NA)) + 
    ggplot2::geom_jitter()
  
  # income
  d <- ggplot2::ggplot(df, 
         ggplot2::aes(x = !!var,
             y = income, 
             color = income_NA)) + 
    ggplot2::geom_jitter()
  
  plots <- list(a, b, c, d)
  plots
}