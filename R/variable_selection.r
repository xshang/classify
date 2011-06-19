#' Interface for a variety of variable selection methods.
#'
#' list_classifiers is a list of classifiers, where each classifier is passed in as a list.
#' Each classifier must specify a 'classifier' option to indicate the function to call
#' to build the classification rule. By default, classify will use the generic function
#' predict. An optional 'predict' can be added to list_classifiers to override the default.
#' Other options???
#'
#' @param d TODO
#' @param vs TODO
#' @param ... TODO
#'
#' @return d TODO
var_sel <- function(d, vs_method, ...) {
  VS_METHODS <- c("anova")
  vs_method <- match.arg(vs_method, VS_METHODS)
  if(vs_method == "anova") {
    d <- var_sel_anova(d, ...)
  } else {
    stop("Only the ANOVA variable selection method has been implemented.")
  }
  d
}

#' Variable selection via analysis of variance (ANOVA).
#' 
#' For each variable in x, we calculate the F statistic with the hypothesis
#' that the means are equal for each class.
#' The q variables with largest values of the F statistic are kept.
#' The remaining p - q variables are dropped.
#' 
#' @param d TODO
#' @param q The number of variables to select.
#'
#' @param d TODO
var_sel_anova <- function(d, q = 30) {
  p <- ncol(d$x)
  F_stats <-  apply(d$x, 2, function(col) {
    summary(aov(col ~ d$y))[[1]][["F value"]][1]
  })
  F_stat_ranks <- rank(F_stats, ties = "random")

  kept_vars <- which(F_stat_ranks > p - q)

  d$x <- d$x[, kept_vars]
  d
}
