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
#' @param method character indicating whether the sandwich covariance matrix
#' (\code{method = "sandwich"}) or the model--based (naive) covariance matrix
#' (\code{method = "naive"}) should be returned.
#' @param ... additional argument(s) for methods.
#'
#' @return A matrix of the estimated covariances between the parameter estimates
#' in the linear predictor of the GEE model. This should have row and column
#' names corresponding to the parameter names given by the coef method.
#'
#' @examples
#' fitmod <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline),
#'   data = arthritis, id = id, repeated = time, LORstr = "uniform")
#' vcov(fitmod, method = "sandwich")
#' vcov(fitmod, method = "naive")
#' 
#' @export

vcov.LORgee <- function(object, method = "sandwich", ...) {
  icheck <- pmatch(method, c("sandwich", "naive"), nomatch = 0,
                   duplicates.ok = FALSE)
  if (icheck == 0) stop("unknown method for the covariance matrix")
  if (method == "sandwich") object$robust.variance else object$naive.variance
}
