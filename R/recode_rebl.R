#' Recode REBL Items
#'
#' @description Recode REBL items from Yes/No to 1/0. Does not deal with reverse
#' coding. See [reverse_code_rebl_items()] for that.
#'
#' @param df A dataframe that includes all the REBL items
#' @param rebl_items A character vector of all REBL items. You can use
#'   [id_rebl_items()] to get this.
#'
#' @return Same dataframe with "Yes" and "No" coded to 1 and 0 as factors based
#'   on which one is the PEB
#' @details Will show a warning if any of the REBL items provided do not appear
#'   in the data frame.
#'
#' @seealso [id_rebl_items()], [id_reversed_rebl_items()],
#'   [reverse_code_rebl_items()], [get_rasch_model()]
#' @export
#'
#' @importFrom dplyr %>% mutate across any_of case_match
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#'   rebl_items <- id_rebl_items(raw_example, '^(?!res).*')
#'   df <- recode_rebl(raw_example, rebl_items)
#' }
recode_rebl <- function(df, rebl_items) {
  # Check inputs
  assertthat::assert_that(
    'data.frame' %in% class(df),
    msg = paste('df must be a data.frame, not a', class(df))
  )
  assertthat::assert_that(
    'character' %in% class(rebl_items),
    msg = paste('rebl_items must be a character vector, not a', class(rebl_items))
  )
  assertthat::assert_that(
    length(names(df)[names(df) %in% rebl_items]) > 0,
    msg = 'None of the rebl_items are found as columns in the df.'
  )

  out <- df %>%
    stringr::str_trim() %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(rebl_items), ~ dplyr::case_match(., "Yes" ~ 1, "No" ~ 0)
      )
    )
  if (any(!rebl_items %in% names(df))) {
    missing <- setdiff(names(df), rebl_items)
    warning('\n*Some REBL items missing from data frame:', missing)
  }
  return(out)
}
