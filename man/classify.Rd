\name{classify}
\alias{classify}
\title{classify...}
\usage{classify(list_data, list_classifiers, est="split", var_sel, seed, ...)}
\description{classify}
\details{classify is a classification framework intended to streamline the classification performance
of classifiers in various settings with numerous error rate estimators
(e.g. cross-validation, .632, .632+, etc).

list_classifiers is a list of classifiers, where each classifier is passed in as a list.
Each classifier must specify a 'classifier' option to indicate the function to call
to build the classification rule. By default, classify will use the generic function
predict. An optional 'predict' can be added to list_classifiers to override the default.
Other options???

Example:
list_classifiers = list(
classifier = "rda",
model_select = "rda_model_select",
predict = "predict_rda"
)}
\value{list_results list of results}
\arguments{\item{list_data}{TODO}
\item{list_classifiers}{TODO}
\item{est}{TODO}
\item{seed}{TODO}}
