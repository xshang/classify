\name{var_sel}
\alias{var_sel}
\title{Interface for a variety of variable selection methods.}
\usage{
  var_sel(x, y, vs_method, ...)
}
\arguments{
  \item{x}{data matrix with N observations and p features.}

  \item{y}{vector of length N with class labels for each of
  the N observations.}

  \item{vs_method}{Variable selection method to use}

  \item{...}{TODO}
}
\value{
  list with variable selection information
}
\description{
  list_classifiers is a list of classifiers, where each
  classifier is passed in as a list. Each classifier must
  specify a 'classifier' option to indicate the function to
  call to build the classification rule. By default,
  classify will use the generic function predict. An
  optional 'predict' can be added to list_classifiers to
  override the default. Other options???
}

