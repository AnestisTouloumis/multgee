#' @export
print.waldts <- function(x, ...) {
  cat("Goodness of Fit based on the Wald test", "\n")
  cat("\nModel under H_0: ")
  print(x$NullModel)
  cat("Model under H_1: ")
  print(x$AlternativeModel)
  p_value <- round(x$pvalue, digits = 4)
  p_value <- format.pval(p_value, eps = 0.0001, scientific = FALSE)
  statistic <- round(x$waldstatistic, 4)
  cat("\nWald Statistic = ", statistic, ", df = ", x$df,
      paste0(", p-value", ifelse(x$pvalue > 1e-04, " = ", " "), p_value), "\n",
    sep = ""
  )
}
