#' Model Selection Criteria
#' 
#' add text.
#' 
#' @return Returns a list with the components:
#' \item{qic}{the QIC criterion.} 
#' \item{qic_u}{the QIC_u criterion}
#' \item{cic}{the CIC criterion.}
#' \item{rjc}{the Rotnumber of observations.}
#' \item{quisi_likelihood}{the values of the convergence variables.}
#' 
#' @param object an object of the class "LORgee". 
#' @param digits numeric indicating the number of decimal points in reported 
#' summaries.
#' @author Anestis Touloumis
#' @seealso \link{ordLORgee} and \link{nomLORgee}.
#' @references Pan, W. (2001) Akaike's information criterion in generalized 
#' estimating equations. \emph{Biometrics} \bold{57}, 120--125.
#' 
#' Rotnitzky, A. and Jewell, N. P. (1990) Hypothesis testing of regression 
#' parameters in semiparametric generalized linear models for cluster correlated
#' data. \emph{Biometrika} \bold{77}, 485--497.
#' 
#' @examples
#' data(housing)
#' fitmod <- nomLORgee(y~factor(time)*sec,data=housing,id=id, repeated=time)
#' qic(fitmod)
#' @export

qic <- function(object, digits = 3) {
  independence_model <- update(object, LORstr = "independence")
  independence_naive_covariance <- vcov(independence_model, robust = FALSE)
  robust_covariance <- vcov(object, robust = TRUE)
  fitted_props <- fitted(object)
  y_vector <- object$y
  ncategories <- max(y_vector)
  y_binary <- rep(y_vector, each = ncategories)
  intercept <- rep.int(seq(ncategories), length(object$id))
  y_binary <- as.numeric(y_binary == intercept)
  y_binary <- matrix(y_binary, nrow = nrow(fitted_props), ncol = ncategories,
                     byrow = TRUE)
  quasi_likelihood <- sum(y_binary * log(fitted_props))
  qic_u <- -2 * quasi_likelihood + 2 * length(object$coeff)
  cic <- sum(solve(independence_naive_covariance) * robust_covariance)
  qic <- -2 * quasi_likelihood + 2 * cic
  #q_matrix <- solve(vcov(object, robust = FALSE)) %*% robust_covariance 
  #c1 <-  sum(diag(q_matrix)) / length(object$coeff)
  #c2 <- sum(q_matrix^2) / length(object$coeff)
  #rjc <- sqrt(((1 - c1) ^ 2) + ((1 - c2) ^ 2))
  list(
    qic = round(qic, digits = digits),
    qic_u = round(qic_u, digits = digits),
    cic = round(cic, digits = digits),
    quasi_likelihood = round(quasi_likelihood, digits = digits)
   #rjc = round(rjc, digits = digits)
    )
}
