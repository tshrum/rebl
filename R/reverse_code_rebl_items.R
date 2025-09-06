#' Reverse Code REBL Items
#'
#' @description Reverse code specified REBL items by flipping 1s to 0s and 0s to 1s.
#'   This is typically done for items where "No" represents the pro-environmental
#'   behavior rather than "Yes".
#'
#' @param df A dataframe containing the REBL items to be reverse coded
#' @param reversed_items A character vector of REBL item names that need to be
#'   reverse coded
#'
#' @returns A dataframe with the specified items reverse coded (1s become 0s, 
#'   0s become 1s, NAs remain NA)
#' @seealso [id_rebl_items()], [id_reversed_rebl_items()], 
#'   [recode_rebl()], [get_rasch_model()]
#' @export
#'
#' @importFrom dplyr mutate across any_of case_when
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#'   # Identify items that need reverse coding
#'   reversed_items <- id_reversed_rebl_items(rebl_items, "pattern")
#'   df_reversed <- reverse_code_rebl_items(df, reversed_items)
#' }
reverse_code_rebl_items <- function(df, reversed_items) {
  # Assertions
  assertthat::assert_that(
    'data.frame' %in% class(df),
    msg = paste('df must be a data.frame, not a', class(df))
  )
  assertthat::assert_that(
    'character' %in% class(reversed_items),
    msg = paste('reversed_items must be a character vector, not a', class(reversed_items))
  )
  assertthat::assert_that(
    length(names(df)[names(df) %in% reversed_items]) > 0,
    msg = 'None of the reversed_items are found as columns in the df.'
  )
  
  df %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(reversed_items),
        ~ dplyr::case_when(
          .x == 1 ~ 0,
          .x == 0 ~ 1,
          .default = NA
        )
      )
    )
}
