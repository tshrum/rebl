#' Test unidimensionality with PCAR method
#'
#' @description Test construct unidimensionality using Principal Components
#'   Analysis R(?) (PCAR).
#'
#' @param model An eRm model object.
#' @param rotate Name of rotation to use for PCAR. Passed to `pca()`.
#' @param n_factors Number of factors to extract
#'
#' @returns A list containing the PCA model. Side effect of generating a scree
#'   plot.
#' @export
#' @importFrom eRm RM
#' @importFrom psych pca
#' @import dplyr
#'
#' @seealso [get_fits()], [test_lr_gender()], [test_lr_rurality()],
#'   [test_lr_race()]
#'
#' @examples
#' \dontrun{
#'   model <- eRm::RM(df)
#'   test_uni_pcar(
#'     model = model,
#'     rotate = 'Promax',
#'     n_factors = 7
#'   )
#' }
test_uni_pcar <- function(model,
                          rotate = 'Promax',
                          n_factors = 5) {
  results <- list()

  pca <- model %>%
    person.parameter() %>%
    itemfit() %>%
    .$st.res %>%
    pca(
      nfactors = n_factors,
      rotate = rotate
    )

  eigenvalues <- c(pca$values[1:10])
  y_limit <- ifelse(max(eigenvalues) > 2.1, max(eigenvalues), 2.1)
  plot(
    eigenvalues,
    ylab = "Eigenvalues",
    xlab = "Item Number",
    type = "b",
    ylim = c(1, y_limit)
  )
  abline(h = 2, col = 'red', lwd = 2, lty = 2)

  return(pca)
}
