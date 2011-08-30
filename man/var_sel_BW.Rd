\name{var_sel_BW}
\alias{var_sel_BW}
\title{Variable selection via ratio of Between-group and Within-group (BW) sum of squares.}
\usage{
  var_sel_BW(x, y, q = 30)
}
\arguments{
  \item{x}{data matrix with N observations and p features.}

  \item{y}{vector of length N with class labels for each of
  the N observations.}

  \item{q}{The number of variables to select.}
}
\value{
  list with indices of the selected variables, dropped
  variables, and the BW statistics for each column.
}
\description{
  For each variable in x, we calculate the BW statistic
  from Dudoit, Fridyland, and Speed (2002). The q variables
  with largest values of the BW statistic are kept. The
  remaining p - q variables are dropped.
}
\details{
  If there is a tie in the BW statistics, we rank the first
  column higher.
}

