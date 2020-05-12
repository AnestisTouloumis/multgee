#' General Linear Hypothesis Test of a GEE Model
#'
#' Performing a general linear hypothesis test for the coefficients of a GEE
#' model based on a Wald-type test. 
#' 
#' @param object A GEE model of the class "\code{LORgee}".
#' @param lambda a matrix where each row corresponds to a linear combination
#' of the coefficients of \code{object}.
#' @param h0vector a vector specifying the values of each linear combination of
#' coefficients under the null hypothesis. 
#'
#' @author Anestis Touloumis
#'
#' @examples
#' data(arthritis)
#' fitmod <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline),
#' data = arthritis, id = id, repeated = time, LORstr = "uniform")
#' lambda <- c(rep(0, 0), c(1, 1, -1))
#' glthgee(fitmod, lambda)
#'
#' @export
glht.gee <- function(object, lambda, h0vector = NULL) 
{
  if (!("LORgee" %in% class(object))) {
    stop("The class of 'object' must be LORgee.")
  }
  beta_hat <- coef(object)
  vcov_mat <- vcov(object, method = "robust")
  if (!is.matrix(lambda))
    lambda <- matrix(lambda, nrow = 1)
  if (ncol(lambda) != length(beta_hat))
  stop("column dimension of 'lambda' is not equal to the number of regression
       parameters")
  if (is.null(h0vector)) {
    h0vector <- rep(0, nrow(lambda))
    } else {
      h0vector <- as.numeric(h0vector)
      if (length(h0vector) != length(beta_hat))
        stop("the length of `h0vector` is not equal the number of regression
             parameters")
      }
  h0vector <- matrix(h0vector, 1, nrow(lambda))
  gamma_hat <- lambda %*% beta_hat - h0vector
  gamma_hat_vcov <- lambda %*% vcov_mat %*% t(lambda)
  statistic <- t(gamma_hat) %*% solve(gamma_hat_vcov) %*% gamma_hat
  df_test <- nrow(lambda)
  pvalue <- 1 - pchisq(statistic, df_test)
  ans <- list(waldstatistic = statistic, df = df_test, pvalue = pvalue,
           lambda = lambda, h0vector = h0vector)
  class(ans) <- "ghltgee"
  ans
}


#' @export
print.ghltgee <- function(x, ...) {
  cat("General Linear Hypothesis Test", "\n")
  cat("General Linear Hypothesis Test", "\n")
  p_value <- round(x$pvalue, 4)
  p_value <- format.pval(p_value, eps = 0.0001, scientific = FALSE)
  statistic <- round(x$waldstatistic, 4)
  cat("\nWald Statistic = ", statistic, ", df = ", x$df,
      paste0(", p-value", ifelse(x$pvalue > 1e-04, " = ", " "), p_value), "\n",
      sep = ""
  )
}