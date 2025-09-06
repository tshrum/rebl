#' Get Object Size
#'
#' Wrapper around format(object.size(x)) to get it in Kb, Mb or Gb depending on size.
#'
#' @param x An R object to get the size of
#'
#' @return A formatted string showing the object size in appropriate units
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage
#' get_size(mtcars)
#' get_size(1:1000000)
#' }
get_size <- function(x) {
  
  size <- object.size(x)
  
  if (size < 1e6) {
    units <- 'Kb'
  } else if (size < 1e9) {
    units <- 'Mb'
  } else if (size >= 1e9) {
    units <- 'Gb'
  }
  
  format(size, units = units)
}