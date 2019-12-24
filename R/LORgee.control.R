#' Control For The GEE Solver
#'
#' Control variables for the GEE solver in the \link{nomLORgee} and
#' \link{ordLORgee} functions.
#'
#'
#' @param tolerance positive convergence tolerance. The algorithm converges
#' when the maximum of the absolute relative difference in parameter estimates
#' is less than or equal to \code{tolerance}.
#' @param maxiter positive integer that indicates the maximum number of
#' iterations in the Fisher-scoring iterative algorithm.
#' @param verbose logical that indicates if output should be printed at each
#' iteration.
#' @param TRACE logical that indicates if the parameter estimates and the
#' convergence criterion at each iteration should be saved.
#'
#' @author Anestis Touloumis
#'
#' @seealso \link{nomLORgee} and \link{ordLORgee}.
#'
#' @examples
#' data(arthritis)
#' fitmod <- ordLORgee(y ~ factor(trt) + factor(baseline) + factor(time),
#'   data = arthritis, id = id, repeated = time)
#'
#' ## A one-step GEE estimator
#' fitmod1 <- update(fitmod, control = LORgee.control(maxiter = 1))
#' coef(fitmod)
#' coef(fitmod1)
#'
#' @export LORgee.control
LORgee.control <- function(tolerance = 0.001, maxiter = 15, verbose = FALSE, # nolint
                           TRACE = FALSE) { # nolint
  if (!is.numeric(tolerance) || tolerance <= 0) {
    stop("value of LORgee's 'tolerance' must be > 0")
  } else if (!is.numeric(maxiter) || maxiter <= 0) {
    stop("maximum number of LORgee's iterations must be > 0")
    }
  list(tolerance = tolerance, maxiter = maxiter, verbose = verbose,
       TRACE = TRACE)
}
