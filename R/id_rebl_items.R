#' Identify REBL Items
#'
#' @description Create a vector of your REBL items from a data frame using a
#'   regex matching pattern.
#' @param df A data frame containing all the REBL items
#' @param pattern A regex matching pattern
#' @param ... Additional arguments passed to `grepl()`
#'
#' @returns A character vector of the names of your REBL items.
#' @details
#' This is a convenience function, but not essential in the workflow. If you
#' already have a vector of your REBL item names, that works just as well. Using
#' this function might be easier or harder depending on how items are named. If
#' they are something like `rebl_*` this would work quite well. Otherwise, like
#' in the example, we might have to use some "OR" operators to represent them
#' all.
#'
#' A vector of your REBL items will be required later in the workflow, however
#' you choose to get it.
#'
#' @seealso [id_reversed_rebl_items()], [reverse_code_rebl_items()],
#'   [get_rasch_model()], [get_rebl_scores()]
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' data(example)
#' (id_rebl_items(example, 'lunch')
#' (id_rebl_items(example, 'lunch', ignore.case = TRUE))
#' (id_rebl_items(example, '^food')
#' (id_rebl_items(example, '^food|^pack|^home|^water|^social')
#' }
id_rebl_items <- function(df, pattern, ...) {
  assertthat::assert_that(
    'data.frame' %in% class(df),
    msg = paste('df must be a data.frame, not a', class(df))
  )
  assertthat::assert_that(
    'character' %in% class(pattern),
    msg = paste('pattern must be a character string, not a', class(pattern))
  )
  assertthat::assert_that(
    length(pattern) == 1,
    msg = 'pattern must be a single character string'
  )

  names(df)[grepl(pattern, names(df), ...)]
}

