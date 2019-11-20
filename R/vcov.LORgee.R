#' Returns the variance-covariance matrix of the main parameters of a fitted
#' model LORgee object.
#'
#'
#' Default is to obtain the robust (sandwich) covariance matrix. However,
#' \code{robust = FALSE} the naive (model-based) covariance matrix is obtained.
#'
#' @title Calculate Variance-Covariance Matrix for a Fitted LORgee Object.
#'
#' @examples
#' fitmod <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline),
#'   data = arthritis, id = id, LORstr = "uniform", repeated = time)
#' vcov(fitmod)
#'
#' @aliases vcov vcov.LORgee
#'
#' @method vcov LORgee
#'
#' @param object a fitted model LORgee object.
#' @param robust logical indicating whether the robust covariance matrix
#' (\code{robust = TRUE}) or the naive covariance matrix should be used
#' (\code{robust = FALSE}) for calculating the confidence intervals.
#' @param ... additional argument(s) for methods.
#'
#' @return A matrix of the estimated covariances between the parameter estimates
#' in the linear predictor of the GEE model. This should have row and column
#' names corresponding to the parameter names given by the coef method.
#'
#' @export

vcov.LORgee <- function(object, robust = TRUE, ...) {
  if (robust) object$robust.variance else object$naive.variance
}
