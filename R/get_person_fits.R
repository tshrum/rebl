#' Get Person Fits
#'
#' @description Extract person fit statistics from fitted psychometric models.
#'   Person fit statistics assess how well each individual's response pattern
#'   conforms to the model expectations. Works with both eRm and ltm model objects.
#'
#' @param model A fitted psychometric model object from either eRm (Rasch models)
#'   or ltm package (rasch, ltm, tpm models)
#'
#' @returns A data frame containing person fit statistics with participant IDs.
#'   For eRm models: includes outfit, infit statistics from eRm::personfit().
#'   For ltm models: includes L0, Lz test statistics and p-values from ltm::person.fit().
#' @seealso [get_rebl_scores()], [get_rasch_model()]
#' @importFrom eRm person.parameter personfit
#' @importFrom ltm person.fit
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr bind_cols mutate %>% everything
#' @export
#'
#' @examples
#' \dontrun{
#'   # With eRm model
#'   model_erm <- get_rasch_model(df, "id", rebl_items)
#'   fits_erm <- get_person_fits(model_erm)
#'
#'   # With ltm model
#'   model_ltm <- ltm::rasch(df[rebl_items])
#'   fits_ltm <- get_person_fits(model_ltm)
#' }
get_person_fits <- function(model) {
  model_class <- class(model)

  if ('eRm' %in% model_class) {

    person_fits <- eRm::person.parameter(model) %>%
      eRm::personfit() %>%
      .[grepl('^p', names(.))] %>%
      as.data.frame() %>%
      tibble::rownames_to_column('id')

  } else if (any(c('rasch', 'ltm', 'tpm') %in% model_class)) {

    fit_outs <- model %>%
      ltm::person.fit()

    # Get unique response patterns from the fitted model
    # These correspond to the collapsed patterns in fit_outs
    unique_patterns <- unique(model$X)
    
    # Create data frame with fit statistics and unique response patterns
    model_fits <- fit_outs$Tobs %>%
      as.data.frame() %>%
      dplyr::bind_cols(fit_outs$p.values, .name_repair = 'minimal') %>%
      setNames(c('L0', 'Lz', 'p')) %>%
      dplyr::bind_cols(unique_patterns, .name_repair = 'minimal') %>%
      dplyr::mutate(
        uid = apply(unique_patterns, 1, paste, collapse = "_")
      )

    # Create original data with unique identifiers
    original_data <- model$X %>%
      as.data.frame() %>%
      tibble::rownames_to_column('id') %>%
      dplyr::mutate(
        uid = apply(.[, -1], 1, paste, collapse = "_")  # Exclude 'id' column
      )

    # Merge back to get all original participants
    person_fits <- merge(original_data, model_fits, by = 'uid', all.x = TRUE) %>%
      dplyr::select(id, L0, Lz, p) %>%
      dplyr::arrange(as.numeric(id))
  }

  return(person_fits)
}
