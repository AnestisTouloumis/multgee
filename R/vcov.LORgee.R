#' @method vcov LORgee
#' @export
vcov.LORgee <- function(object, robust = TRUE, ...)
{
  if (robust)  object$robust.variance else object$naive.variance
}
