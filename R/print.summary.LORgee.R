#' @export
print.summary.LORgee <- function(x, ...) {
  cat(x$title, "\n")
  cat(x$version, "\n")
  cat("\nLink :", x$link, "\n")
  cat("\nLocal Odds Ratios:")
  cat("\nStructure:        ", x$local.odds.ratios$structure)
  if (!is.null(x$local.odds.ratios$model)) {
    cat("\nModel:            ", x$local.odds.ratios$model)
  }
  if (!is.null(x$local.odds.ratios$homogeneous)) {
    cat("\nHomogenous scores:", x$local.odds.ratios$homogeneous)
  }
  if (!is.null(x$local.odds.ratios$restricted)) {
    cat("\nRestricted scores:", x$local.odds.ratios$restricted)
  }
  cat("\n")
  cat("\ncall:\n")
  print(x$call)
  cat("\nSummary of residuals:\n")
  print(summary(as.vector(x$residuals)))
  cat("\nNumber of Iterations:", x$niter, "\n")
  cat("\nCoefficients:\n")
  printCoefmat(x$coefficients)
  cat("\nLocal Odds Ratios Estimates:\n")
  print(round(x$local.odds.ratios$theta, 3))
  if (!is.null(x$pvalue)) {
    p_value <- format.pval(x$pvalue, eps = 0.0001, scientific = FALSE)
    cat("\np-value of Null model:", p_value, "\n")
  }
}
