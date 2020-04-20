#' Computes confidence intervals for one or more parameters in a fitted LORgee
#' model.
#'
#' The (Wald-type) confidence intervals are calculated using either the
#' estimated sandwich (robust) or the model-based (naive) covariance matrix.
#'
#' @title Confidence Intervals for Model Parameters
#'
#' @examples
#' fitmod <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline),
#'   data = arthritis, id = id, LORstr = "uniform", repeated = time)
#' confint(fitmod)
#'
#' @aliases confint
#'
#' @method confint LORgee
#'
#' @param object a fitted model LORgee object.
#' @param parm a specification of which parameters are to be given confidence
#' intervals, either a vector of numbers or a vector of names. If missing, all
#' parameters are considered.
#' @param level the confidence level required.
#' @param method character indicating whether the sandwich (robust) covariance
#' matrix (\code{method = "robust"}) or the model--based (naive) covariance
#' matrix (\code{method = "naive"}) should be used for calculating the
#' confidence intervals.
#' @param ... additional argument(s) for methods.
#'
#' @return A matrix (or vector) with columns giving lower and upper confidence
#' limits for each parameter. These will be labelled as \code{(1-level)/2} and
#' \code{1 - (1-level)/2} in \% (by default 2.5\% and 97.5\%).
#'
#' @export

confint.LORgee <- function(object, parm, level = 0.95, method = "robust", ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm)) {
    parm <- pnames
  } else if (is.numeric(parm)) {
    parm <- pnames[parm]
  }
  a <- (1 - level) / 2
  a <- c(a, 1 - a)
  pct <- format_perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, pct))
  ses <- sqrt(diag(vcov(object, method = method)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}
