#' @export
print.waldts <- function(x, ...) {
  cat("Goodness of Fit based on the Wald test", "\n")
  cat("\nModel under H_0: ")
  print(x$NullModel)
  cat("Model under H_1: ")
  print(x$AlternativeModel)
  p_value <- format.pval(x$pvalue, eps = 0.0001, scientific = FALSE)
  cat("\nWald Statistic = ", x$waldstatistic, ", df = ", x$df,
      paste0(", p-value", ifelse(x$pvalue > 1e-04, " = ", " "), p_value), "\n",
    sep = ""
  )
}
