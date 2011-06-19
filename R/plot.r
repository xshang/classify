#' Plots the classification results for classify simulations.
#'
#' Generates a ggplot2 graph (boxplots) for the classification methods
#' for each data set.
#'
#' @param obj Object from 'classify' that contains error rate results.
#' @param ymax The maximum error rate to include on the error rate plots.
#'
#' @return list_plots list of plots by data set.
plot_classify <- function(obj, ymax = 1, ...) {
  list_plots <- dlply(obj$results, .(data_set), function(d_results) {
    p <- ggplot(d_results, aes(x = method, y = error_rate))
    p <- p + ylim(c(0, ymax)) + geom_boxplot(color = I("#3366FF"))
    p <- p + xlab("") + ylab("Error Rate")
    p <- p + theme_set(theme_bw())
    p
  })
  names(list_plots) <- unique(obj$results$data_set)
  list_plots
}
