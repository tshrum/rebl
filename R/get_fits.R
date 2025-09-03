#' Get person and item fits
#'
#' @description Using an eRm model, get person parameter fit and item fit
#'   objects.
#' @param model An eRm model object.
#' @returns A list containing person fits and item fits.
#' @export
#' @importFrom eRm person.parameter personfit itemfit
#' @seealso [test_lr_gender()], [test_lr_rurality()], [test_lr_race()]
#' @examples
#' \dontrun{
#'   model <- eRm::RM(df)
#'   fits <- get_fits(model)
#' }
get_fits <- function(model) {
  results <- list()
  pp <- eRm::person.parameter(model)
  results$person_fit <- eRm::personfit(pp)
  results$item_fit <- eRm::itemfit(pp)
  return(results)
}
