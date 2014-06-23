#' Calculate mRMR feature scores.
#'
#' @param time [\code{numeric}]\cr
#'   Vector of length \code{n} providing (possibly right censored) survival times.
#' @param status [\code{logical}]\cr
#'   Vector of length \code{n} providing censoring informations: \code{FALSE} for
#'   censored observations, \code{TRUE} for uncensored.
#' @param x [\code{matrix(n*p)}]\cr
#'   An matrix of \code{n} observations and \code{p} features/scores.
#' @param relevance [\code{character(1)}]\cr
#'   Measure for relevance. Possible values are \dQuote{cindex}.
#'   For \dQuote{cindex}, the relevance is determined by twice the absolute distance
#'   to \code{0.5} (random predictor), resulting in a relevance between 0 and 1.
#'   Default is \dQuote{cindex}.
#' @param redundance [\code{character(1)}]\cr
#'   Measure for redundance Possible values are \dQuote{pearson} and \dQuote{mi}.
#'   Default is \dQuote{pearson}.
#' @param combine [\code{character(1)}]\cr
#'   How to combine redundance and relevance. Possible values are \dQuote{difference}
#'   and \dQuote{quotient}.
#'   Default is \dQuote{difference}.
#' @param alpha [\code{numeric(1)}]\cr
#'   Weighting parameter for both combination methods.
#'   Default is 1.
#' @return [\code{data.frame}]. Contains the following columns (rows are named with feature names):
#'   \item{\code{score}}{Combined score}
#'   \item{\code{rel}}{Relevance score}
#'   \item{\code{red}}{Redundancy score}
#' @name fmrmr
NULL
