lht <- function(object, lambda_matrix, null_vector = NULL) 
{
  if (class(object) != "LORgee") {
    stop("object must be a list of LORgee objects fitted with LORgee.")
  }
  betas <- coef(object)
  vcov_mat <- vcov(object)
  if (!is.matrix(lambda_matrix))
    stop("lambda_matrix must be a matrix")
  if (dim(lambda_matrix)[2] != length(betas)) {
    stop("column dimension of lambda matrix does not equal the number of
         regression parameters")
  }
  if (is.null(null_vector)) 
    null_vector <- 0 else{
      null_vector <- as.numeric(null_vector)
      if (length(null_vector) != length(betas))
        stop("the length of null_vector does not equal the number of regression
             parameters")
    }
  null_vector <- matrix(null_vector, 1, dim(lambda_matrix)[1])
  lambda_beta_hat <- lambda_matrix %*% betas - null_vector
  lambda_beta_vcov <- lambda_matrix %*% vcov_mat %*% t(lambda_matrix)
  wald_statistic <- drop(t(lambda_beta_hat) %*% solve(lambda_beta_vcov) %*%
    lambda_beta_hat)
  pvalue <- 1 - pchisq(wald_statistic, dim(lambda_matrix)[1])
  ans <- c(test_statistic = wald_statistic, pvalue = pvalue)
  ans
}
