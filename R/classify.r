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
#' @param error_est TODO
#' @param seed TODO
#'
#' @return list_results list of results
classify <- function(list_data, list_classifiers, error_est = c("split", "cv",
"boot", "632", "632+"), seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)
  # TODO: Return a data.frame with the following columns:
  #   data_set
  #   classifier
  #   error_rate
  #   standard_error
  sim_results <- foreach(d = list_data, .combine=rbind) %do% {
    error_results <- error_split(d, list_classifiers, ...)
    cbind(data_set = d$name, error_results)
  }
  # TODO: Summarize results here
  sim_summary <- ddply(
    sim_results,
    .(data_set, method), 
    summarize,
    avg_error = mean(error_rate),
    SE = sd(error_rate)
  )
  list(results = sim_results, summary = sim_summary)
}

error_split <- function(d, list_classifiers, split_pct = 0.5, num_splits = 50,
...) {
    N <- nrow(d$x)
    list_sim <- foreach(i = icount(num_splits)) %dopar% {
      # Partition the data sets.
      obs <- seq_len(N)
      train_obs <- sort(sample(obs, split_pct * N))
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
