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
#'   model_select = "rda_model_select",
#'   predict = "predict_rda"
#' )
#'
#' @export
#' @param list_data TODO
#' @param list_classifiers TODO
#' @param est TODO
#' @param seed TODO
#'
#' @return list_results list of results
classify <- function(list_data, list_classifiers, est = "split", var_sel = NULL, seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)

  sim_results <- foreach(d = list_data, .combine=rbind) %do% {
    error_results <- est_error(d, list_classifiers, est, var_sel, ...)
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


#' Retrieves the functions for the specified classifier.
#'
#' TODO
#' 
#' Example:
#' cl = list(
#'   method = "RDA",
#'   classifier = "rda",
#'   model_select = "rda_model_select",
#'   predict = "predict_rda"
#' )
#'
#' @export 
#' @param cl TODO
#' @return list TODO
get_cl_attrib <- function(cl) {
  # Set defaults for classifier attributes.
  cl_method <- cl$classifier
  cl_predict <- "predict"

  # If any these are non-null, replace them with their preferred names.
  if(!is.null(cl$method)) cl_method <- cl$method
  if(!is.null(cl$predict)) cl_predict <- cl$predict

  # Retrieve the classifier, model selection, and predict functions.
  # If there is no model selection function specified, it is set as NULL.
  cl_train <- get(cl$classifier)
  cl_model_sel <- ifelse(is.null(cl$model_select), NULL, get(cl$model_select))
  cl_predict <- get(cl_predict)

  list(
    cl_method = cl_method, 
    cl_train = cl_train, 
    cl_model_select = cl_model_sel,
    cl_predict = cl_predict
  )
}
