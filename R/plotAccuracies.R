#' Plot accuracies of all methods and their error bars. Used in analysis, not used for tutorial.
#'
#' @param
#' @return
#'
#'
#'
#' @examples
#'
#'

plotAccuracies <- function(accuracies, n){
  require(ggplot2)

  accuracies$Method <- paste0(accuracies$FS, ", ", accuracies$ML)

  plotAccuracy <- ggplot(data = accuracies, aes(x = accuracy, y = Method, color = ML)) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmax = accuracy + ((1.96*accuracySD)/sqrt(n)), xmin = accuracy - ((1.96*accuracySD)/sqrt(n)))) +
    ggtitle("Cross Validated Accuracy by Method")+
    theme(text = element_text(size=16, color = "black"),
          axis.text = element_text(color = "black"),
          plot.background = element_rect(fill = "transparent",colour = NA))

  plotAccuracy

  ggsave(filename = "AccuracyofMethodsPlot.png", plot = plotAccuracy, bg = "transparent" )
}
