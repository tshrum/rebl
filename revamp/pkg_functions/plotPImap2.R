#' Enhanced Person-Item Map Plot
#'
#' Modified version of plotPImap from eRm package with customizable margins.
#' This function creates person-item maps for Rasch models with enhanced
#' formatting options.
#'
#' @param object A Rasch model object from eRm
#' @param item.subset Items to include, either "all" or vector of item names/indices
#' @param sorted Logical, whether to sort items by difficulty (default: FALSE)
#' @param main Main title for the plot (default: "Person-Item Map")
#' @param latdim Label for latent dimension (default: "Latent Dimension")
#' @param pplabel Label for person parameter distribution (default: "Person\\nParameter\\nDistribution")
#' @param cex.gen General character expansion factor (default: 0.7)
#' @param xrange Range for x-axis, NULL for automatic (default: NULL)
#' @param warn.ord Logical, whether to warn about threshold ordering (default: TRUE)
#' @param warn.ord.colour Color for threshold ordering warnings (default: "black")
#' @param irug Logical, whether to show item rug plot (default: TRUE)
#' @param pp Person parameters object, NULL for automatic (default: NULL)
#' @param margins Margin settings as vector c(bottom, left, top, right) (default: c(2.5, 4, 0, 1))
#'
#' @return Creates a person-item map plot
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plotPImap2(rasch_model)
#' 
#' # With custom margins and formatting
#' plotPImap2(rasch_model, 
#'           sorted = TRUE,
#'           margins = c(3, 11, 0, 1),
#'           cex.gen = 0.8)
#' }
plotPImap2 <- function (object,
                        item.subset = "all",
                        sorted = FALSE,
                        main = "Person-Item Map",
                        latdim = "Latent Dimension",
                        pplabel = "Person\nParameter\nDistribution",
                        cex.gen = 0.7,
                        xrange = NULL,
                        warn.ord = TRUE,
                        warn.ord.colour = "black",
                        irug = TRUE,
                        pp = NULL,
                        margins = c(2.5, 4, 0, 1))
{
  def.par <- graphics::par(no.readonly = TRUE)
  if ((object$model == "LLTM") || (object$model == "LRSM") ||
      (object$model == "LPCM"))
    stop("Item-Person Map are computed only for RM, RSM, and PCM!")
  if (object$model == "RM" || max(object$X, na.rm = TRUE) <
      2) {
    dRm <- TRUE
    threshtable <- cbind(object$betapar, object$betapar) *
      -1
    rownames(threshtable) <- substring(rownames(threshtable), first = 6, last = 9999)
  }
  else {
    dRm <- FALSE
    threshtable <- eRm::thresholds(object)$threshtable[[1]]
  }
  tr <- as.matrix(threshtable)
  if (is.character(item.subset)) {
    if (length(item.subset) > 1 && all(item.subset %in%
                                       rownames(threshtable)))
      tr <- tr[item.subset, ]
    else if (length(item.subset) != 1 || !(item.subset ==
                                           "all"))
      stop(
        "item.subset misspecified. Use 'all' or vector of at least two valid item indices/names."
      )
  }
  else {
    if (length(item.subset) > 1 && all(item.subset %in%
                                       1:nrow(tr)))
      tr <- tr[item.subset, ]
    else
      stop(
        "item.subset misspecified. Use 'all' or vector of at least two valid item indices/names."
      )
  }
  if (sorted)
    tr <- tr[order(tr[, 1], decreasing = FALSE), ]
  loc <- as.matrix(tr[, 1])
  tr <- as.matrix(tr[, -1])
  if (is.null(pp))
    suppressWarnings(pp <- eRm::person.parameter(object))
  else if ((!("ppar" %in% class(pp))) || !identical(pp$X, object$X))
    stop("pp is not a person.parameter object which matches the main Rasch data object!")
  theta <- unlist(pp$thetapar)
  tt <- table(theta)
  ttx <- as.numeric(names(tt))
  yrange <- c(0, nrow(tr) + 1)
  if (is.null(xrange))
    xrange <- range(c(tr, theta), na.rm = T)
  nf <- graphics::layout(matrix(c(2, 1), 2, 1, byrow = TRUE), heights = c(1, 3), T)
  
  # mar1 -----
  graphics::par(mar = c(margins))
  plot(
    xrange,
    yrange,
    xlim = xrange,
    ylim = yrange,
    main = "",
    ylab = "",
    type = "n",
    yaxt = "n",
    xaxt = "n"
  )
  graphics::axis(
    2,
    at = 1:nrow(tr),
    labels = rev(rownames(tr)),
    las = 2,
    cex.axis = cex.gen
  )
  graphics::axis(
    1,
    at = seq(floor(xrange[1]), ceiling(xrange[2])),
    cex.axis = cex.gen,
    padj = -1.5
  )
  graphics::mtext(latdim, 1, 1.2, cex = cex.gen + 0.1)
  if (irug == TRUE) {
    y.offset <- nrow(tr) * 0.0275
    tr.rug <- as.numeric(tr)
    if (any(is.na(tr.rug)))
      tr.rug <- tr.rug[-which(is.na(tr.rug))]
    graphics::segments(tr.rug,
             rep(yrange[2], length(tr.rug)) + y.offset,
             tr.rug,
             rep(yrange[2], length(tr.rug)) + 100)
  }
  warn <- rep(" ", nrow(tr))
  for (j in 1:nrow(tr)) {
    i <- nrow(tr) + 1 - j
    assign("trpoints", tr[i, !is.na(tr[i, ])])
    npnts <- length(trpoints)
    if (!dRm && !all(sort(trpoints) == trpoints))
      ptcol = warn.ord.colour
    else
      ptcol = "black"
    if (npnts > 1)
      graphics::points(
        sort(trpoints),
        rep(j, npnts),
        type = "b",
        cex = 1,
        col = ptcol
      )
    if (dRm) {
      graphics::lines(xrange * 1.5, rep(j, 2), lty = "dotted")
    }
    else {
      if (npnts > 1)
        graphics::text(
          sort(trpoints),
          rep(j, npnts),
          (1:npnts)[order(trpoints)],
          cex = cex.gen,
          pos = 1,
          col = ptcol
        )
      if (!all(sort(trpoints) == trpoints))
        warn[j] <- "*"
    }
    graphics::points(loc[i],
           j,
           pch = 20,
           cex = 1.5,
           col = ptcol)
  }
  if (warn.ord)
    graphics::axis(
      4,
      at = 1:nrow(tr),
      tick = FALSE,
      labels = warn,
      hadj = 2.5,
      padj = 0.7,
      las = 2
    )
  
  # mar2 -----
  graphics::par(mar = c(0, margins[2], 3, margins[4]))
  plot(
    ttx,
    tt,
    type = "n",
    main = main,
    axes = FALSE,
    ylab = "",
    xlim = xrange,
    ylim = c(0, max(tt))
  )
  graphics::points(
    ttx,
    tt,
    type = "h",
    col = "gray",
    lend = 2,
    lwd = 5
  )
  graphics::mtext(pplabel, 2, 0.5, las = 2, cex = cex.gen)
  graphics::box()
  graphics::par(def.par)
}