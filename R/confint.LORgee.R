#' @method confint LORgee
#' @export
confint.LORgee <- function(object, parm, level = 0.95, robust = TRUE, ...) {
  cf <- coef(object)
  pnames <- names(cf)
  if (missing(parm)) 
    parm <- pnames
  else if (is.numeric(parm)) 
    parm <- pnames[parm]
  a <- (1 - level)/2
  a <- c(a, 1 - a)
  pct <- format.perc(a, 3)
  fac <- qnorm(a)
  ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
                                                             pct))
  ses <- sqrt(diag(vcov(object, robust = robust)))[parm]
  ci[] <- cf[parm] + ses %o% fac
  ci
}
