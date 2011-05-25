#' classify
#'
#' classify is a classification framework intended to streamline the evaluation performance of classifiers in various settings with numerous error rate estimators (e.g. leave-one-out, .632).
#'
#' list_classifiers is a list of classifiers, where each classifier is passed in as a list.
#' Each classifier must specify a 'classifier' option to indicate the function to call
#' to build the classification rule. By default, classify will use the generic function
#' predict. An optional 'predict' can be added to list_classifiers to override the default.
#' Other options???
#'
#' @param list_data TODO
#' @param list_classifiers TODO
#' @param results TODO
#' @param training_pct TODO
#' @param B TODO
#' @param seed TODO
#' @param store_predictions TODO
#'
#' @return list_results list of results
classify <- function(list_data, list_classifiers, results = "list", training_pct = 0.5, B = 50, seed = NULL, store_predictions = TRUE, ...) {
	if(!is.null(seed)) set.seed(seed)

	foreach(d = list_data) %do% {
		N <- nrow(d$x)
		train <- sample(seq_len(N), training_pct * N)

		train_x <- d$x[train,]
		test_x <- d$x[-train,]
		train_y <- d$y[train]
		test_y <- d$y[-train]

		foreach(i=seq_len(B)) %dopar% {
			# TODO: Cycle through a list of classifiers rather than just one
			cl <- get(list_classifiers$classifier)
			cl_predict <- "predict"
			if(!is.null(list_classifiers$predict)) {
				cl_predict <- list_classifiers$predict
			}
			cl_predict <- get(cl_predict)

			cl_out <- cl(x = train_x, y = train_y, ...)
			test_pred <- cl_predict(object = cl_out, newdata = test_x)
			error <- mean(test_pred != test_y)
			# TODO: Organize results
		}
	}
}

list_classifiers = list(
	classifier = "rda",
	predict = "predict_rda"
)
