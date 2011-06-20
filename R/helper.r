#' Partitions data for cross-validation
#'
#' @param y a vector of the labels of the training data
#' @param k leave out how many observations per fold in cross-validation?
#' @return list the indices of observations to hold out by fold..
cv_partition <- function(y, num_folds = 5) {
  n <- length(y)
  folds <- split(sample(seq_len(n), n), gl(n = ceiling(n / k), k = k, length = n))
  names(folds) <- paste("Fold", names(folds), sep = "")
  folds
}
