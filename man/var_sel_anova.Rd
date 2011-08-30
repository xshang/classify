\name{var_sel_anova}
\alias{var_sel_anova}
\title{Variable selection via analysis of variance (ANOVA).}
\usage{
  var_sel_anova(x, y, q = 30)
}
\arguments{
  \item{x}{data matrix with N observations and p features.}

  \item{y}{vector of length N with class labels for each of
  the N observations.}

  \item{q}{The number of variables to select.}
}
\value{
  list with indices of the selected variables, dropped
  variables, and the F statistics for each column.
}
\description{
  For each variable in x, we calculate the F statistic with
  the hypothesis that the means are equal for each class.
  The q variables with largest values of the F statistic
  are kept. The remaining p - q variables are dropped.
}
\details{
  If there is a tie in the F statistics, we rank the first
  column higher.
}

