#' Get REBL Scores
#'
#' @description Extract person parameters (REBL scores) from a fitted Rasch model.
#'   Optionally includes person fit statistics to assess how well each person's
#'   response pattern fits the Rasch model.
#'
#' @param model A fitted eRm Rasch model object from [get_rasch_model()]
#' @param include_fits Logical indicating whether to include person fit statistics
#'   in the output (default: TRUE)
#'
#' @returns A dataframe with participant IDs, REBL scores, and optionally person
#'   fit statistics. When include_fits is TRUE, includes columns for outfit,
#'   infit, and other fit measures from eRm::personfit()
#' @seealso [get_rasch_model()], [id_rebl_items()], [recode_rebl()],
#'   [reverse_code_rebl_items()]
#' @export
#'
#' @importFrom eRm person.parameter personfit
#' @importFrom stats coef setNames
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr %>% inner_join
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#'   # Fit model and get scores
#'   model <- get_rasch_model(df, "id", rebl_items)
#'   scores <- get_rebl_scores(model, include_fits = TRUE)
#'
#'   # Get scores without fit statistics
#'   scores_only <- get_rebl_scores(model, include_fits = FALSE)
#' }
get_rebl_scores <- function(model, include_fits = TRUE) {

  # Assertions
  assertthat::assert_that(
    'Rm' %in% class(model),
    msg = paste('model must be an eRm Rasch model object, not a', class(model))
  )
  assertthat::assert_that(
    'logical' %in% class(include_fits),
    msg = paste('include_fits must be logical, not a', class(include_fits))
  )
  assertthat::assert_that(
    length(include_fits) == 1,
    msg = 'include_fits must be a single logical value'
  )

  # Extract person parameters from Rasch model
  pp <- eRm::person.parameter(model)

  # Get df of REBL scores (person parameters) and ids
  rebl_scores <- stats::coef(pp) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    stats::setNames(c('id', 'rebl_score'))

  # Include fit statistics
  if (include_fits) {
    pfit <- eRm::personfit(pp)[1:7] %>%
      .[names(.) != 'st.res']

    # Join to REBL scores
    rebl_scores <- pfit %>%
      as.data.frame() %>%
      tibble::rownames_to_column('id') %>%
      dplyr::inner_join(rebl_scores, by = 'id') %>%
      dplyr::select(id, rebl_score, everything())
  }
  return(rebl_scores)
}
