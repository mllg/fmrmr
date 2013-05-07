\name{aggr.corr}
\alias{aggr.corr}
\title{Calculate the aggregated correlation of matrix columns.}
\usage{
  aggr.corr(x, type = "pearson", aggr = "mean",
    method = "cpp")
}
\arguments{
  \item{x}{[\code{matrix}]\cr An \code{n*p} matrix of
  \code{n} observations and \code{p} features.}

  \item{type}{[\code{character(1)}]\cr Type of correlation.
  Right now, only pearson correlation is supported.}

  \item{aggr}{[\code{character(1)}]\cr Aggregation
  function. Supported are \dQuote{mean} and \dQuote{max}.
  Default is \dQuote{mean}.}

  \item{method}{[character(1)]\cr Possible choices are
  \dQuote{cpp} and \dQuote{R}. Mainly for testing purposes.
  This argument may be removed in a future release.
  Default is \dQuote{cpp}.}
}
\value{
  [\code{numeric}] Numeric vector of length \code{p}
  containing the concordance indices for all features
  provided in \code{x}.
}
\description{
  Calculate the aggregated correlation of matrix columns.
}

