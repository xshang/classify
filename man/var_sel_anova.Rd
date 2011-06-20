\name{var_sel_anova}
\alias{var_sel_anova}
\title{Variable selection via analysis of variance (ANOVA).}
\usage{var_sel_anova(x, y, q=30)}
\description{Variable selection via analysis of variance (ANOVA).}
\details{For each variable in x, we calculate the F statistic with the hypothesis
that the means are equal for each class.
The q variables with largest values of the F statistic are kept.
The remaining p - q variables are dropped.}
\value{d TODO}
\arguments{\item{x}{TODO}
\item{y}{TODO}
\item{q}{The number of variables to select.}}
