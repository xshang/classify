\name{error_split}
\alias{error_split}
\title{Classification error rate estimation via repeated random splitting of data}
\usage{
  error_split(d, list_classifiers, split_pct = 0.5,
  num_splits = 50, vs_method = NULL, ...)
}
\arguments{
  \item{d}{TODO}

  \item{list_classifiers}{TODO}

  \item{split_pct}{The percentage of observations allocated
  as training data.}

  \item{num_splits}{The number of splits performed for
  error rate estimation.}
}
\value{
  error_results TODO
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

