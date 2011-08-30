#' Interface for a variety of variable selection methods.
#'
#' list_classifiers is a list of classifiers, where each classifier is passed in as a list.
#' Each classifier must specify a 'classifier' option to indicate the function to call
#' to build the classification rule. By default, classify will use the generic function
#' predict. An optional 'predict' can be added to list_classifiers to override the default.
#' Other options???
#'
#' @param x data matrix with N observations and p features.
#' @param y vector of length N with class labels for each of the N observations.
#' @param vs_method Variable selection method to use
#' @param ... TODO
#'
#' @return list with variable selection information
var_sel <- function(x, y, vs_method, ...) {
  VS_METHODS <- c("anova")
  vs_method <- match.arg(vs_method, VS_METHODS)
  if(vs_method == "anova") {
    var_sel_results <- var_sel_anova(x, y, ...)
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
#' If there is a tie in the F statistics, we rank the first column higher.
#' 
#' @param x data matrix with N observations and p features.
#' @param y vector of length N with class labels for each of the N observations.
#' @param q The number of variables to select.
#'
#' @return list with indices of the selected variables, dropped variables, and
#' the F statistics for each column.
var_sel_anova <- function(x, y, q = 30) {
  p <- ncol(x)
	stopifnot(q > 0)
	stopifnot(p >= q)
  F_stats <-  apply(x, 2, function(col) {
    summary(aov(col ~ y))[[1]][["F value"]][1]
  })
  F_stat_ranks <- rank(F_stats, ties = "first")

  kept_vars <- which(F_stat_ranks > p - q)
  dropped_vars <- which(F_stat_ranks <= p - q)

  list(
    kept = as.vector(kept_vars),
    dropped = as.vector(dropped_vars),
    F_stats = as.vector(F_stats)
  )
}

#' Variable selection via ratio of Between-group and Within-group (BW) sum of squares.
#' 
#' For each variable in x, we calculate the BW statistic from Dudoit,
#' Fridyland, and Speed (2002).
#' The q variables with largest values of the BW statistic are kept.
#' The remaining p - q variables are dropped.
#'
#' If there is a tie in the BW statistics, we rank the first column higher.
#' 
#' @export
#' @param x data matrix with N observations and p features.
#' @param y vector of length N with class labels for each of the N observations.
#' @param q The number of variables to select.
#'
#' @reference Dudoit, S., Fridyland, J., and Speed, T. (2002). "Comparison of Discrimination Methods for the Classification of Tumors Using Gene Expression Data," Journal of the American Statistical Association, 97, 457, 77-87.
#' @return list with indices of the selected variables, dropped variables, and
#' the BW statistics for each column.
var_sel_BW <- function(x, y, q = 30) {
	p <- ncol(x)
	stopifnot(q > 0)
	stopifnot(p >= q)
	y <- factor(y)
	var_means <- colMeans(x)
	
	class_out <- foreach(cl=levels(y)) %do% {
		x_cl <- x[which(y == cl), ]
		xbar_cl <- colMeans(x_cl)
		wss_cl <- rowSums(apply(x_cl, 1, function(x_i) (x_i - xbar_cl)^2))
		list(xbar = xbar_cl, wss = wss_cl)
	}
  WSS <- colSums(do.call(rbind, lapply(class_out, function(cl) cl$wss)))
	BSS <- colSums(do.call(rbind, lapply(class_out, function(cl) (cl$xbar - var_means)^2)))
	BW <- BSS / WSS
  BW_ranks <- rank(BW, ties = "first")

  kept_vars <- which(BW_ranks > p - q)
  dropped_vars <- which(BW_ranks <= p - q)

  list(
    kept = as.vector(kept_vars),
    dropped = as.vector(dropped_vars),
    BW = as.vector(BW)
  )
}

