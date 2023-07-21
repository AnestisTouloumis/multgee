#' Variable and Covariance Selection Criteria
#' 
#' Reports commonly used criteria for variable selection 
#' and for selecting the "working" association structure for one or several 
#' fitted models from the \code{multgee} package. 
#' 
#' 
#' The Quasi Information Criterion (QIC), the Correlation Information Criterion
#' (CIC) and the Rotnitzky and Jewell Criterion (RJC) are used for selecting the
#' best association structure. The QICu criterion is used for selecting the best 
#' subset of covariates. When choosing among GEE models with different association
#' structures but with the same subset of covariates, the model with the smallest 
#' value of QIC, CIC or RJC should be preffered. When choosing between GEE models
#' with different number of covariates, the model with the smallest QICu value 
#' should be preferred.
#' 
#' @return A vector or matrix with the QIC, QICu, CIC, RJC and the number of 
#' regression parameters (including intercepts).
#' 
#' @param object an object of the class \code{LORgee}. 
#' @param ... optionally more objects of the class \code{LORgee}.
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
#' data(arthritis)
#' fitmod <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline),
#' data = arthritis, id = id, repeated = time, LORstr = "uniform")
#' fitmod1 <- update(fitmod, formula = .~. + age + factor(sex))
#' gee_criteria(fitmod, fitmod1)

#' @export
gee_criteria <- function(object, ...) {
  UseMethod("gee_criteria")
}

#' @aliases gee_criteria gee_criteria.LORgee
#'
#' @method gee_criteria LORgee
#' @export 
gee_criteria.LORgee <- function(object, ...) {
  
  if (!("LORgee" %in% class(object)) ) {
    stop("gee_criteria requires a LORgee object as input")
  }
  
  compute_criteria <- function(object) {
    fitted_props <- fitted(object)
    y_vector <- object$y
    ncategories <- max(y_vector)
    y_binary <- rep(y_vector, 
                    each = ncategories)
    intercept <- rep.int(seq(ncategories), 
                         length(object$id))
    y_binary <- as.numeric(y_binary == intercept)
    y_binary <- matrix(y_binary, 
                       nrow = nrow(fitted_props), 
                       ncol = ncategories,
                       byrow = TRUE)
    y_binary <- as.numeric(y_binary == intercept)
    y_binary <- matrix(y_binary, 
                       nrow = nrow(fitted_props), 
                       ncol = ncategories,
                       byrow = TRUE)
    quasi_likelihood <- sum(y_binary * log(fitted_props))
    independence_model <- update(object, 
                                 LORstr = "independence")
    independence_naive_covariance <- vcov(independence_model, 
                                          robust = FALSE)
    naive_covariance <- vcov(object, 
                             robust = FALSE)
    robust_covariance <- vcov(object, 
                              robust = TRUE)
    
    p <- length(object$coeff)
    qic_u <- round(-2 * quasi_likelihood + 2 * p, 
                   digits = 4)
    cic <- round(sum(solve(independence_naive_covariance) * robust_covariance), 
                 digits = 4)
    qic <- round(-2 * quasi_likelihood + 2 * cic, 
                 digits = 4)
    q_matrix <- naive_covariance %*% robust_covariance
    c1 <- sum(diag(q_matrix)) / p
    c2 <- sum(q_matrix ^ 2) / p
    rjc <- round(sqrt(((1 - c1) ^ 2) + ((1 - c2) ^ 2)),
                 digits = 4)
    ans <- c(qic, qic_u, cic, rjc, p)
    names(ans) <- c("QIC", "CIC", "RJC", "QICu", "Parameters")
    ans
  }
  if (length(list(...))) {
    results <- lapply(list(object, ...), compute_criteria)
    check <- sapply(list(object, ...), function(x) { 
      length(x$y) 
      })
    if (any(check != check[1]))
      warning("models are not all fitted to the same number of observations")
    res <- do.call("rbind", results)
    Call <- match.call()
    Call$k <- NULL
    row.names(res) <- as.character(Call[-1L])
    res
  } else {
    compute_criteria(object)
  }
}