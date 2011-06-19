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
var_sel <- function(x, y, vs_method, ...) {
  VS_METHODS <- c("anova")
  vs_method <- match.arg(vs_method, VS_METHODS)
  if(vs_method == "anova") {
    var_sel_results <- var_sel_anova(d, ...)
  } else {
    stop("Only the ANOVA variable selection method has been implemented.")
  }
  var_sel_results 
}

#' Variable selection via analysis of variance (ANOVA).
#' 
#' For each variable in x, we calculate the F statistic with the hypothesis
#' that the means are equal for each class.
#' The q variables with largest values of the F statistic are kept.
#' The remaining p - q variables are dropped.
#' 
#' @param x TODO
#' @param y TODO
#' @param q The number of variables to select.
#'
#' @return d TODO
var_sel_anova <- function(x, y, q = 30) {
  p <- ncol(x)
  F_stats <-  apply(x, 2, function(col) {
    summary(aov(col ~ y))[[1]][["F value"]][1]
  })
  F_stat_ranks <- rank(F_stats, ties = "random")

  kept_vars <- which(F_stat_ranks > p - q)
  
  list(
    kept = kept_vars,
    dropped = dropped_vars,
    F_stats = F_stats
  )
}
