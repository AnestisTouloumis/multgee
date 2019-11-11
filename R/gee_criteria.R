#' Variable and Covariance Selection Criteria
#' 
#' Reports commonly used within GEE methodology criteria for variable and
#' "working" covariance matrix selection. The Quasi Information Criterion (QIC), 
#' the Correlation Information Criterion (CIC) and the Rotnitzky and Jewell
#' Criterion (RJC) are used for selecting the best association structure. The
#' QICu criterion is used for selecting the best subset of covariates. When
#' choosing among GEE models with different association structures but with the
#' same subset of covariates, the model with the smallest value of QIC, CIC or
#' RJC should be preffered. When choosing between GEE models with different
#' number of covariates, the model with the smallest QICu value should be
#' preferred.
#' 
#' @return Returns a list with components:
#' \item{qic}{the QIC criterion.} 
#' \item{qic_u}{the QIC_u criterion.}
#' \item{cic}{the CIC criterion.}
#' \item{rjc}{the Rotnitzky and Jewell criterion.}
#' 
#' @param object an object of the class "LORgee". 
#' @param digits integer indicating the number of decimal points in reported 
#' summaries.
#' @author Anestis Touloumis
#' @seealso \link{nomLORgee} and \link{ordLORgee}.
#' @references Hin, L.Y. and Wang, Y.G. (2009) Working correlation structure 
#' identification in generalized estimating equations. \emph{Statistics in 
#' Medicine} \bold{28}, 642--658.
#' 
#' Pan, W. (2001) Akaike's information criterion in generalized 
#' estimating equations. \emph{Biometrics} \bold{57}, 120--125.
#' 
#' Rotnitzky, A. and Jewell, N.P. (1990) Hypothesis testing of regression 
#' parameters in semiparametric generalized linear models for cluster correlated
#' data. \emph{Biometrika} \bold{77}, 485--497.
#' 
#' @examples
#' data(housing)
#' fitmod <- nomLORgee(y~factor(time)*sec,data=housing,id=id, repeated=time)
#' gee_criteria(fitmod)
#' @export

gee_criteria <- function(object, digits = 3) {
  independence_model <- update(object, LORstr = "independence")
  independence_naive_covariance <- vcov(independence_model, robust = FALSE)
  robust_covariance <- vcov(object, robust = TRUE)
  naive_covariance <- vcov(object, robust = FALSE)
  fitted_props <- fitted(object)
  y_vector <- object$y
  ncategories <- max(y_vector)
  y_binary <- rep(y_vector, each = ncategories)
  intercept <- rep.int(seq(ncategories), length(object$id))
  y_binary <- as.numeric(y_binary == intercept)
  y_binary <- matrix(y_binary, nrow = nrow(fitted_props), ncol = ncategories,
                     byrow = TRUE)
  quasi_likelihood <- sum(y_binary * log(fitted_props))
  p <- length(object$coeff)
  qic_u <- -2 * quasi_likelihood + 2 * p
  cic <- sum(solve(independence_naive_covariance) * robust_covariance)
  qic <- -2 * quasi_likelihood + 2 * cic
  q_matrix <- solve(naive_covariance) %*% robust_covariance
  c1 <- sum(diag(q_matrix)) / p
  c2 <- sum(q_matrix ^ 2) / p
  rjc <- sqrt(((1 - c1) ^ 2) + ((1 - c2) ^ 2))
  list(
    qic = round(qic, digits = digits),
    qic_u = round(qic_u, digits = digits),
    cic = round(cic, digits = digits),
    rjc = round(rjc, digits = digits)
    )
  cat("QIC =", qic, "\n")
  cat("CIC =", cic, "\n")
  cat("RJC =", rjc, "\n")
  cat("QIC_u =", qic_u, "\n")
}
