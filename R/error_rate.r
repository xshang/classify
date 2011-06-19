#' Interface for a variaety of error rate estimators.
#'
#' list_classifiers is a list of classifiers, where each classifier is passed in as a list.
#' Each classifier must specify a 'classifier' option to indicate the function to call
#' to build the classification rule. By default, classify will use the generic function
#' predict. An optional 'predict' can be added to list_classifiers to override the default.
#' Other options???
#'
#' @param d TODO
#' @param list_classifiers TODO
#' @param est TODO
#' @param ... TODO
#'
#' @return error_results TODO
est_error <- function(d, list_classifiers, est, var_sel = NULL, ...) {
  ERROR_EST <- c("split", "cv", "boot", "632", "632+", "apparent")
  est <- match.arg(est, ERROR_EST)
  if(est == "split") {
    error_results <- error_split(d, list_classifiers, var_sel, ...)
  } else if(est == "cv") {
    error_results <- error_cv(d, list_classifiers, var_sel, ...)
  } else if(est == "boot") {
    error_results <- error_boot(d, list_classifiers, var_sel, ...)
  } else if(est == "632") {
    error_results <- error_632(d, list_classifiers, var_sel, ...)
  } else if(est == "632+") {
    error_results <- error_632plus(d, list_classifiers, var_sel, ...)
  } else if(est == "apparent") {
    error_results <- error_apparent(d, list_classifiers, var_sel, ...)
  }

  error_results
}

#' Classification error rate estimation via cross-validation
#'
#' TODO
#'
#' @param d TODO
#' @param list_classifiers TODO
#' @param num_folds TODO
#'
#' @return error_results TODO
error_cv <- function(d, list_classifiers, num_folds = 5, ...) {
  stop("The cross-validation error rate estimator has not been implemented yet.")
}

#' Classification error rate estimation via bootstrapping
#'
#' TODO
#'
#' @param d TODO
#' @param list_classifiers TODO
#' @param num_reps TODO
#'
#' @return error_results TODO
error_boot <- function(d, list_classifiers, num_reps = 20, ...) {
  stop("The bootstrapping error rate estimator has not been implemented yet.")
}

#' Classification error rate estimation via the 632 estimator
#'
#' TODO
#'
#' @param d TODO
#' @param list_classifiers TODO
#' @param num_folds TODO
#'
#' @return error_results TODO
error_632 <- function(d, list_classifiers, num_folds = 5, ...) {
  stop("The 632 error rate estimator has not been implemented yet.")
}

#' Classification error rate estimation via the 632+ estimator
#'
#' TODO
#'
#' @param d TODO
#' @param list_classifiers TODO
#' @param num_folds TODO
#'
#' @return error_results TODO
error_632plus <- function(d, list_classifiers, num_folds = 5, ...) {
  stop("The 632+ error rate estimator has not been implemented yet.")
}

#' Classification error rate estimation via the apparent error rate estimator.
#'
#' TODO
#'
#' @param d TODO
#' @param list_classifiers TODO
#'
#' @return error_results TODO
error_apparent <- function(d, list_classifiers, ...) {
  stop("The apparent error rate estimator has not been implemented yet.")
}

#' Classification error rate estimation via repeated random splitting of data
#'
#' list_classifiers is a list of classifiers, where each classifier is passed in as a list.
#' Each classifier must specify a 'classifier' option to indicate the function to call
#' to build the classification rule. By default, classify will use the generic function
#' predict. An optional 'predict' can be added to list_classifiers to override the default.
#' Other options???
#'
#' @param d TODO
#' @param list_classifiers TODO
#' @param split_pct The percentage of observations allocated as training data.
#' @param num_splits The number of splits performed for error rate estimation.
#'
#' @return error_results TODO
error_split <- function(d, list_classifiers, split_pct = 0.5, num_splits = 50,
vs_method = NULL, ...) {
    N <- nrow(d$x)
    list_sim <- foreach(i = icount(num_splits)) %dopar% {
      # Partition the data sets.
      obs <- seq_len(N)
      train_obs <- sort(sample(obs, split_pct * N))
      test_obs <- which(!(obs %in% train_obs))

      # Variable selection
      kept_vars <- seq_len(ncol(d$x))
      if(!is.null(vs_method)) {
        kept_vars <- var_sel(d$x, d$y, vs_method, ...)$kept
      }
      train_x <- d$x[train_obs, kept_vars]
      train_y <- d$y[train_obs]
      test_x <- d$x[test_obs, kept_vars]
      test_y <- d$y[test_obs]

      list_pred <- foreach(cl = list_classifiers) %do% {
        cl_attrib <- get_cl_attrib(cl)

        # Call the classifier and prediction functions.
        cl_out <- cl_attrib$cl_train(x = train_x, y = train_y, ...)
        test_pred <- cl_attrib$cl_predict(object = cl_out, newdata = test_x)
        list(
          method = cl_attrib$cl_method,
          vs_method = vs_method,
          train_obs = train_obs,
          test_obs = test_obs,
          test_class = test_y,
          test_pred = test_pred, 
          error = mean(test_pred != test_y)
        )
      }
      list_pred
    }
    error_results <- do.call(rbind, lapply(list_sim, function(sim_iter) {
      # NOTE: Sim is a list of the results FOR EACH classifier
      do.call(rbind, lapply(sim_iter, function(sim_cl) {
        cbind(sim_cl$method, sim_cl$error)
      }))
    }))
    error_results <- data.frame(error_results, stringsAsFactors = FALSE)
    names(error_results) <- c("method", "error_rate")
    # The error rate gets coerced to a factor in the above cbind() call.
    error_results$error_rate <- as.numeric(error_results$error_rate)
    error_results
}
