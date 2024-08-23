#' ba_plot
#' 2024-08-14

#' Slightly modified function from Karun and Puranik 2021. Mostly just 
#' changed some formatting - margins, text locations, axis limits.
#' https://cegh.net/article/S2213-3984(21)00139-1/fulltext

ba_plot <- function(m1, m2) {
  
  means <- (m1 + m2) / 2
  diffs <- m1 - m2
  mdiff <- mean(diffs)
  sddiff <- sd(diffs)
  
  # Positioning of labels on the lines
  xmax = max(means)
  xmin = min(means)
  x1 = (xmax - xmin) * 0.10
  x2 = xmax - x1
  
  # Adding space for legend
  ylimh <- mdiff + (3.5 * sddiff)
  yliml <- mdiff - (3 * sddiff)
  
  # Plot data
  par(mar = c(5, 4, 1, 1) + 0.1)
  set.seed(42)
  plot(
    jitter(diffs, amount = 0.1) ~ jitter(means, amount = 0.1),
    xlab = "Mean of REBL Scores",
    ylab = "Difference in REBL Scores",
    col = alpha('royalblue4', 0.4),
    pch = 19,
    ylim = c(min(diffs) - 0.25, max(diffs) + 0.25),
    xlim = c(xmin - 0.25, xmax + 0.75)
  )
  text(xmax + 0.4, mdiff + 0.1, "Mean")
  abline(h = 0)
  
  # Standard deviations lines & legend
  abline(h = mdiff + 1.96 * sddiff,
         col = 'red',
         lty = 2)
  text(xmax + 0.4, (mdiff + 1.96 * sddiff) + 0.1, "Upper Limit")
  abline(h = mdiff - 1.96 * sddiff,
         col = 'red',
         lty = 2)
  text(xmax + 0.4, (mdiff - 1.96 * sddiff) + 0.1, "Lower Limit")
  paramX = round(mdiff, 2)
  paramY = round(sddiff, 2)
  UL = mdiff + 1.96 * sddiff
  UL = round(UL, 2)
  LL = mdiff - 1.96 * sddiff
  LL = round(LL, 2)
  expr <- vector("expression", 4)
  expr[[1]] <- bquote(Mean[diff] == .(paramX))
  expr[[2]] <- bquote(SD[diff] == .(paramY))
  expr[[3]] <- bquote(LL == .(LL))
  expr[[4]] <- bquote(UL == .(UL))
  legend("topright", bty = "n", legend = expr)
}