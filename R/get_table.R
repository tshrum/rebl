#' Get Table with NAs
#'
#' Wrapper for table() that includes NA values by default (useNA = 'always').
#'
#' @param x Vector or factor to create table from
#'
#' @return Table object showing frequencies including NA values
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' get_table(c(1, 2, 1, NA, 2, 1))
#' }
get_table <- function(x) {
    table(x, useNA = 'always')
}