#' Creating A Probability Matrix With Specified Local Odds Ratios
#'
#' Utility function to create a square probability matrix that satisfies the
#' specified local odds ratios structure.
#'
#' This function is designed to ease the construction of the \code{LORterm}
#' argument in the functions \link{nomLORgee} and \link{ordLORgee}.
#'
#' @param x a square matrix with positive entries that describes the desired
#' local odds ratios matrix.
#' @return Returns a square probability matrix that satisfies the local odds
#' ratios structure defined by \code{x}.
#' @section Warning: Caution is needed for local odds ratios close to zero.
#' @author Anestis Touloumis
#' @seealso \link{nomLORgee} and \link{ordLORgee}.
#' @examples
#' ## Illustrating the construction of a "fixed" local odds ratios structure
#' ## using the arthritis dataset. Here, we assume a uniform local odds ratios
#' ## structure equal to 2 for each time pair.
#'
#' ## Create the uniform local odds ratios structure.
#' lorterm <- matrixLOR(matrix(2, 4, 4))
#'
#' ## Create the LORterm argument.
#' lorterm <- c(lorterm)
#' lorterm <- matrix(c(lorterm), 3, 25, TRUE)
#'
#' ## Fit the marginal model.
#' data(arthritis)
#' fitmod.fixed <- ordLORgee(y ~ factor(trt) + factor(time) + factor(baseline),
#'   data = arthritis, id = id, repeated = time, LORstr = "fixed",
#'   LORterm = lorterm)
#' fitmod.fixed
#' @export
matrixLOR <- function(x) {
  if (!is.matrix(x)) {
    stop("'x' must be a matrix")
  }
  if (nrow(x) != ncol(x)) {
    stop("'x' must be a square matrix")
  }
  if (any(x < 0)) {
    stop("all elements of 'x' must be > 0")
  }
  y <- matrix(1, nrow(x) + 1, ncol(x) + 1)
  for (i in seq_len(ncol(x))) {
    for (j in seq_len(ncol(x))) y[i + 1, j + 1] <- prod(x[1:i, 1:j])
  }
  prop.table(y)
}
