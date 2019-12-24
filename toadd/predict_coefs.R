#fitted.geem <- function(object, ...){
  #InvLink <- object$FunList$InvLink
#  InvLink <- object$FunList[[if(object$FunList$family == "custom") "InvLink" else "linkinv"]]
#  return(InvLink(object$eta))
#}

predict.LORgee <- function(object, newdata = NULL, ...){
  coefs <- object$coefficients
  if (is.null(newdata)) {
    object$fitted.values
    } else {
      if (dim(newdata)[2] != length(coefficients)) {
        warning("New observations must have the same number of rows as coefficients in the model")
      }
      rownames(fit$linear.predictors) <- seq_len(nrow(fit$linear.predictors))
      colnames(fit$linear.predictors) <- 1:(ncategories - 1)
      fitted.values <- fitmod$fitted.values
      fitted.values.1 <- matrix(fitted.values, ncol = ncategories - 1, byrow = TRUE)
      fitted.values.2 <- 1 - rowSums(fitted.values.1)
      fitted.values <- cbind(fitted.values.1, fitted.values.2)
      rownames(fitted.values) <- seq_len(nrow(fitted.values.1))
      colnames(fitted.values) <- 1:ncategories
      
      return(as.vector(newdata %*% object$beta))
    }
  }

coef.LORgee <- function(object, ...){
  coefs <- object$coefficients
  names(coefs) <- names(object$coefficients)
  return(coefs)
}