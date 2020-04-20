#' Returns the variance-covariance matrix of the main parameters of a fitted
#' model LORgee object.
#'
#'
#' Default is to obtain the estimated sandwich (robust) covariance matrix and
#' \code{method = "naive"} obtains the estimated model-based (naive) covariance
#' matrix
#'
#' @title Calculate Variance-Covariance Matrix for a Fitted LORgee Object.
#'
#' @aliases vcov vcov.LORgee
#'
#' @method vcov LORgee
#'
#' @param object a fitted model LORgee object.
#' @param method character indicating whether the sandwich (robust) covariance
#' matrix (\code{method = "robust"}) or the model--based (naive) covariance
#' matrix (\code{method = "naive"}) should be returned.
#' @param ... additional argument(s) for methods.
#'
#' @return A matrix of the estimated covariances between the parameter estimates
#' in the linear predictor of the GEE model. This should have row and column
#' names corresponding to the parameter names given by the coef method.
#'
#' @examples
#' fitmod <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline),
#'   data = arthritis, id = id, repeated = time, LORstr = "uniform")
#' vcov(fitmod, method = "robust")
#' vcov(fitmod, method = "naive")
#'
#' @export

vcov.LORgee <- function(object, method = "robust", ...) {
  icheck <- pmatch(method, c("robust", "naive"), nomatch = 0,
                   duplicates.ok = FALSE)
  if (icheck == 0) stop("unknown method for the covariance matrix")
  if (method == "robust") object$robust.variance else object$naive.variance
}
