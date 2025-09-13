#' Get Rasch Model
#'
#' @description Fit a Rasch model to REBL items using the eRm package. This
#'   creates a Rasch model object that can be used to calculate person
#'   parameters (REBL scores) and item parameters.
#'
#' @param df A dataframe containing the REBL items and participant IDs
#' @param id A character string specifying the column name containing
#'   participant IDs
#' @param rebl_items A character vector of REBL item column names to include in
#'   the model. Get this with `id_rebl_items()` if you don't have it handy.
#' @param type A string. `cml` = conditional maximum likelihood, `mml_con` =
#'   constrained maximum marginal likelihood, `mml_uncon` = unconstrained
#'   maximum marginal likelihoood, `tpl` = two parameter logistic model, `tpm` =
#'   three parameter logistic model. See details for more info.
#'
#' @returns An eRm Rasch model object that can be passed to other functions like
#'   [get_rebl_scores()]
#' @details Add some more details about what this means.
#'
#' @export
#' @seealso [id_rebl_items()], [id_reversed_rebl_items()],
#'   [reverse_code_rebl_items()], [get_rebl_scores()]
#'
#' @importFrom dplyr %>% select all_of any_of
#' @importFrom tibble column_to_rownames
#' @importFrom eRm RM
#' @importFrom assertthat assert_that
#' @importFrom ltm rasch ltm tpm
#'
#' @examples
#' \dontrun{
#'   # Get REBL items
#'   rebl_items <- id_rebl_items(raw_example, '^(?!res).*')
#'
#'   # Fit Rasch model
#'   model <- get_rasch_model(raw_example, "id", rebl_items)
#' }
get_rasch_model <- function(
    df,
    id,
    rebl_items,
    type = c(
      'cml',
      'mml_constrained',
      'mml_con',
      'mml_unconstrained',
      'mml_uncon',
      '2pl',
      'tpm'
    )
) {

  # Check inputs
  assertthat::assert_that(
    'data.frame' %in% class(df),
    msg = paste('df must be a data.frame, not a', class(df))
  )
  assertthat::assert_that(
    class(id) == 'character',
    msg = paste('id variable must be a character, not a', class(id))
  )
  assertthat::assert_that(
    id %in% names(df),
    msg = 'id variable is not found in the df'
  )
  assertthat::assert_that(
    length(names(df)[names(df) %in% rebl_items]) > 0,
    msg = 'None of the rebl_items are found as columns in the df.'
  )
  assertthat::assert_that(
    type != '2pl',
    msg = 'Use tpl instead of 2pl!'
  )
  if (any(!rebl_items %in% names(df))) {
    missing <- setdiff(rebl_items, names(df))
    n_miss <- length(missing)
    miss_format <- paste0(missing, collapse = ', ')
    warning(n_miss, ' REBL item(s) not included in data frame: ', miss_format)
  }

  # Prepare df
  df <- df %>%
    dplyr::select(dplyr::all_of(id), dplyr::any_of(rebl_items)) %>%
    tibble::column_to_rownames(id)

  # Run appropriate model
  model <- switch(
    type,

    cml = eRm::RM(df),

    mml_constrained = ltm::rasch(df, constraint = cbind(length(df) + 1, 1)),
    mml_con = ltm::rasch(df, constraint = cbind(length(df) + 1, 1)),

    mml_unconstrained = ltm::rasch(df),
    mml_uncon = ltm::rasch(df),

    mml_2pl = ltm::ltm(df ~ z1),

    mml_tpm = ltm::tpm(df, type = 'rasch', max.guessing = 1)
  )

  return(model)
}

