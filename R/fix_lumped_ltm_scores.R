#' Fix lumped ltm scores
#'
#' @description Resolves issue where ltm model returns fewer observations than
#'   input data due to response patterns being collapsed into groups. This function
#'   maps the model scores back to the original participant observations using
#'   unique response pattern identifiers.
#'
#' @param df A data frame containing the original response data with participant IDs
#' @param scores A list object returned by ltm::factor.scores() containing score.dat
#' @param rebl_items Character vector of column names representing REBL items
#'
#' @returns A data frame with participant IDs, REBL scores ordered by original row position
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate select last_col %>% arrange any_of
#' @keywords internal
fix_lumped_ltm_scores <- function(df, scores, rebl_items) {

  # Create unique identifier in the original df
  df_id <- df %>%
    tibble::rownames_to_column('row_id') %>%
    dplyr::mutate(
      uid = apply(.[, c(rebl_items)], 1, paste, collapse = "_")
    )

  # Create unique identifier in the model output df
  model_id <- scores$score.dat %>%
    dplyr::mutate(
      uid = apply(.[, rebl_items], 1, paste, collapse = "_")
    )

  # Put the back together, just keep original data and rebl scores
  rebl_scores <- merge(df_id, model_id, by = 'uid', all.x = TRUE) %>%
    dplyr::select(-uid) %>%
    dplyr::arrange(as.numeric(row_id)) %>%
    dplyr::select(-row_id, -dplyr::any_of('Obs'))

  return(rebl_scores)
}
