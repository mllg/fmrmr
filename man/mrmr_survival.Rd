% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mrmr.R
\name{mrmr_survival}
\alias{mrmr_survival}
\alias{mrmr_with_relevance}
\title{Minimum Redundancy, maximum relevance

Uses Harrells C index for relevance and the Pearson correlation to quantify redundancy.}
\usage{
mrmr_survival(time, status, x, nselect = ncol(x))

mrmr_with_relevance(relevance, x, nselect = ncol(x))
}
\arguments{
\item{time}{[\code{numeric}]\cr
Vector of survival times.}

\item{status}{[\code{logical}]\cr
Vector of survival event indicators.}

\item{x}{[\code{matrix[double]}]\cr
Matrix of \code{[observations]x[features]}.}

\item{nselect}{[\code{integer(1)}]\cr
Number of features to score. Default is all features.}
}
\value{
[\code{named numeric}]: Vector of scores, named with column
  names of \code{x}, in order of selection.
}

