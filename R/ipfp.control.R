#' IPFP Control
#' 
#' Control variables for the Iterative Proportion Fitting Procedure function
#' \code{ipfp}.
#' 
#' 
#' @param tol positive convergence tolerance. The algorithm converges when the
#' absolute difference between the observed and the given row or column totals
#' is less than or equal to \code{tol}.
#' @param maxit positive integer that indicates the maximum number of
#' iterations.
#' @note Currently the function \code{ipfp} is internal.
#' @author Anestis Touloumis
#' @seealso \link{nomLORgee} and \link{ordLORgee}.
#' @export
ipfp.control <- function(tol = 1e-06, maxit = 200) {
    if (!is.numeric(tol) || tol <= 0) 
        stop("value of the IPFP 'tol' must be > 0")
    if (!is.numeric(maxit) || maxit <= 0) 
        stop("maximum number of the IPFP iterations must be > 0")
    list(tol = tol, maxit = maxit)
}

