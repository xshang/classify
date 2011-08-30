\name{plot_classify}
\alias{plot_classify}
\title{Plots the classification results for classify simulations.}
\usage{
  plot_classify(obj, ymax = 1, ...)
}
\arguments{
  \item{obj}{Object from 'classify' that contains error
  rate results.}

  \item{ymax}{The maximum error rate to include on the
  error rate plots.}
}
\value{
  list_plots list of plots by data set.
}
\description{
  Generates a ggplot2 graph (boxplots) for the
  classification methods for each data set.
}

