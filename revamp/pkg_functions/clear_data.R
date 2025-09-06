#' Clear Values from Global Environment
#'
#' Function to remove values from the global environment,
#' useful for cleaning up after scripts while preserving loaded functions.
#'
#' @export
#' @examples
#' \dontrun{
#' # Create some test objects
#' x <- 1:10
#' y <- "test"
#'
#' # Remove data objects but keep functions
#' clear_data()
#' }
clear_data <- function() {
  rm(
    list = setdiff(
      ls(envir = .GlobalEnv),
      lsf.str(envir = .GlobalEnv)
    ),
    envir = .GlobalEnv
  )
}
