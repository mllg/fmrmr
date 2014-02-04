\name{fmrmr}
\alias{fmrmr}
\title{Calculate mRMR feature scores.}
\usage{
fmrmr(time, status, x, relevance = "cindex", redundance = "mi",
  combine = "difference")
}
\arguments{
  \item{time}{[\code{numeric}]\cr Vector of length \code{n}
  providing (possibly right censored) survival times.}

  \item{status}{[\code{logical}]\cr Vectpr of length
  \code{n} providing censoring informations: \code{FALSE}
  for censored observations, \code{TRUE} for uncensored.}

  \item{x}{[\code{matrix(n*p)}]\cr An matrix of \code{n}
  observations and \code{p} features/scores.}

  \item{relevance}{[character(1)]\cr Measure for relevance.
  Possible values are \dQuote{cindex}.  For
  \dQuote{cindex}, the relevance is determined by twice the
  absolute distance to \code{0.5} (random predictor),
  resulting in a relevance between 0 and 1.  Default is
  \dQuote{cindex}.}

  \item{redundance}{[character(1)]\cr Measure for
  redundance Possible values are \dQuote{pearson} and
  \dQuote{mi}.  Default is \dQuote{mi}.}

  \item{combine}{[character(1)]\cr How to combine
  redundance and relevance. Possible values are
  \dQuote{difference} and \dQuote{quotient}.  Default is
  \dQuote{difference}.}
}
\value{
[\code{numeric}] Numeric vector of length \code{p}
containing the mRMR scores for all features provided in
\code{x}.
}
\description{
Calculate mRMR feature scores.
}

