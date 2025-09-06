#' Identify Reversed REBL Items
#'
#' @description Filter a vector of REBL items to identify only those that need
#'   reverse coding using a regex matching pattern.
#' @param rebl_items A character vector of REBL item names
#' @param pattern A regex matching pattern to identify items that need reverse coding
#' @param ... Additional arguments passed to `grepl()`
#'
#' @returns A character vector of REBL item names that need reverse coding.
#' @details
#' This is a convenience function for identifying which REBL items need reverse
#' coding based on a pattern. For example, if items that need reverse coding
#' have "reverse" or "neg" in their names, you can use a pattern to identify them.
#' The function filters the provided vector of REBL items to return only those
#' matching the pattern.
#'
#' @seealso [id_rebl_items()], [reverse_code_rebl_items()], 
#'   [get_rasch_model()], [get_rebl_scores()]
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#'   # First get all REBL items
#'   rebl_items <- id_rebl_items(example, '^rebl_')
#'   
#'   # Then identify which ones need reverse coding
#'   reversed_items <- id_reversed_rebl_items(rebl_items, 'reverse')
#'   reversed_items <- id_reversed_rebl_items(rebl_items, '_neg$')
#'   reversed_items <- id_reversed_rebl_items(rebl_items, 'item[135]$')
#' }
id_reversed_rebl_items <- function(rebl_items, pattern, ...) {
  # Assertions
  assertthat::assert_that(
    'character' %in% class(rebl_items),
    msg = paste('rebl_items must be a character vector, not a', class(rebl_items))
  )
  assertthat::assert_that(
    'character' %in% class(pattern),
    msg = paste('pattern must be a character string, not a', class(pattern))
  )
  assertthat::assert_that(
    length(pattern) == 1,
    msg = 'pattern must be a single character string'
  )
  
  rebl_items[grepl(pattern, rebl_items, ...)]
}
