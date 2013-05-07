\name{cindex}
\alias{cindex}
\title{Calculate the concordance index for survival data.}
\usage{
  cindex(surv, x, method = "cpp")
}
\arguments{
  \item{surv}{[\code{Surv}]\cr Survival object of right
  censored survival times. See
  \code{\link[survival]{Surv}}.}

  \item{x}{[\code{matrix(n*p)}]\cr An matrix of \code{n}
  observations and \code{p} features.}

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
  Calculate the concordance index for survival data.
}

