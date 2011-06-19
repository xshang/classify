#' classify
#'
#' classify is a classification framework intended to streamline the classification performance
#' of classifiers in various settings with numerous error rate estimators
#' (e.g. cross-validation, .632, .632+, etc).
#'
#' list_classifiers is a list of classifiers, where each classifier is passed in as a list.
#' Each classifier must specify a 'classifier' option to indicate the function to call
#' to build the classification rule. By default, classify will use the generic function
#' predict. An optional 'predict' can be added to list_classifiers to override the default.
#' Other options???
#'
#' Example:
#' list_classifiers = list(
#'   classifier = "rda",
#'   predict = "predict_rda"
#' )
#'
#' @param list_data TODO
#' @param list_classifiers TODO
#' @param est TODO
#' @param seed TODO
#'
#' @return list_results list of results
classify <- function(list_data, list_classifiers, est = "split", seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)

  sim_results <- foreach(d = list_data, .combine=rbind) %do% {
    error_results <- est_error(d, list_classifiers, est, ...)
    cbind(data_set = d$name, error_results)
  }

  sim_summary <- ddply(
    sim_results,
    .(data_set, method), 
    summarize,
    avg_error = mean(error_rate),
    SE = sd(error_rate)
  )
  list_ret <- list(results = sim_results, summary = sim_summary)
  list_ret
}

#' Error rate estimation via a specified method
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
est_error <- function(d, list_classifiers, est, ...) {
  ERROR_EST <- c("split", "cv", "boot", "632", "632+")
  est <- pmatch(est, ERROR_EST)
  if(is.na(est)) stop("Invalid error rate estimator.")
  if(est == -1) stop("Ambiguous error rate estimator. Be more exact.")
  if(est == "split") {
    error_results <- error_split(d, list_classifiers, ...)
  } else {
    stop("Only the random split error rate estimator has been implemented.")
  }
  error_results
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
...) {
    N <- nrow(d$x)
    list_sim <- foreach(i = icount(num_splits)) %dopar% {
      # Partition the data sets.
      train_obs <- sample(seq_len(N), split_pct * N)
      test_obs <- which(!(obs %in% train_obs))
      train_x <- d$x[train_obs,]
      test_x <- d$x[test_obs,]
      train_y <- d$y[train_obs]
      test_y <- d$y[test_obs]

      list_pred <- foreach(cl = list_classifiers) %do% {
        cl_attrib <- get_cl_attrib(cl)

        # Call the classifier and prediction functions.
        cl_out <- cl_attrib$cl_train(x = train_x, y = train_y, ...)
        test_pred <- cl_attrib$cl_predict(object = cl_out, newdata = test_x)
        list(
          method = cl_attrib$cl_method,
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

#' Retrieves the functions for the specified classifier.
#'
#' TODO
#' 
#' Example:
#' cl = list(
#'   method = "RDA",
#'   classifier = "rda",
#'   predict = "predict_rda"
#' )
#' 
#' @param cl TODO
#' @return list TODOs
get_cl_attrib <- function(cl) {
  # Set defaults for classifier attributes.
  cl_method <- cl$classifier
  cl_predict <- "predict"

  # If any these are non-null, replace them with their preferred names.
  if(!is.null(cl$method)) cl_method <- cl$method
  if(!is.null(cl$predict)) cl_predict <- cl$predict

  # Retrieve the classifier and predict functions.
  cl_train <- get(cl$classifier)
  cl_predict <- get(cl_predict)
  list(cl_method = cl_method, cl_train = cl_train, cl_predict = cl_predict)
}
