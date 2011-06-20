\name{est_error}
\alias{est_error}
\title{Interface for a variaety of error rate estimators.}
\usage{est_error(d, list_classifiers, est, var_sel, ...)}
\description{Interface for a variaety of error rate estimators.}
\details{list_classifiers is a list of classifiers, where each classifier is passed in as a list.
Each classifier must specify a 'classifier' option to indicate the function to call
to build the classification rule. By default, classify will use the generic function
predict. An optional 'predict' can be added to list_classifiers to override the default.
Other options???}
\value{error_results TODO}
\arguments{\item{d}{TODO}
\item{list_classifiers}{TODO}
\item{est}{TODO}
\item{...}{TODO}}
