#' Marginal Models For Correlated Nominal Multinomial Responses
#'
#' Solving the generalized estimating equations for correlated nominal
#' multinomial responses assuming a baseline category logit model for the
#' marginal probabilities.
#'
#' The \code{data} must be provided in case level or equivalently in `long'
#' format. See details about the `long' format in the function \link{reshape}.
#'
#' A term of the form \code{offset(expression)} is allowed in the right hand
#' side of \code{formula}.
#'
#' The default set for the response categories is \eqn{\{1,\ldots,J\}}, where
#' \eqn{J>2} is the maximum observed response category. If otherwise, the
#' function recodes the observed response categories onto this set.
#'
#' The \eqn{J}-th response category is treated as baseline.
#'
#' The default set for the \code{id} labels is \eqn{\{1,\ldots,N\}}, where
#' \eqn{N} is the sample size. If otherwise, the function recodes the given
#' labels onto this set.
#'
#' The argument \code{repeated} can be ignored only when \code{data} is written
#' in such a way that the \eqn{t}-th observation in each cluster is recorded at
#' the \eqn{t}-th measurement occasion. If this is not the case, then the user
#' must provide \code{repeated}. The suggested set for the levels of
#' \code{repeated} is \eqn{\{1,\ldots,T\}}, where \eqn{T} is the number of
#' observed levels. If otherwise, the function recodes the given levels onto
#' this set.
#'
#' The variables \code{id} and \code{repeated} do not need to be pre-sorted.
#' Instead the function reshapes \code{data} in an ascending order of \code{id}
#' and \code{repeated}.
#'
#' The fitted marginal baseline category logit model is \deqn{log
#' \frac{Pr(Y_{it}=j |x_{it})}{Pr(Y_{it}=J |x_{it})}=\beta_{j0} +\beta^{'}_j
#' x_{it}} where \eqn{Y_{it}} is the \eqn{t}-th multinomial response for
#' cluster \eqn{i}, \eqn{x_{it}} is the associated covariates vector,
#' \eqn{\beta_{j0}} is the \eqn{j}-th response category specific intercept and
#' \eqn{\beta_{j}} is the \eqn{j}-th response category specific parameter
#' vector.
#'
#' The formula is easier to read from either the Vignette or the Reference
#' Manual (both available
#' \href{https://CRAN.R-project.org/package=multgee}{here}).
#'
#' The \code{LORterm} argument must be an \eqn{L} x \eqn{J^2} matrix, where
#' \eqn{L} is the number of level pairs of \code{repeated}. These are ordered
#' as \eqn{(1,2), (1,3), ...,(1,T), (2,3),...,(T-1,T)} and the rows of
#' \code{LORterm} are supposed to preserve this order. Each row is assumed to
#' contain the vectorized form of a probability table that satisfies the
#' desired local odds ratios structure.
#'
#' @param formula a formula expression as for other regression models for
#' multinomial responses. An intercept term must be included.
#' @param data an optional data frame containing the variables provided in
#' \code{formula}, \code{id} and \code{repeated}.
#' @param id a vector that identifies the clusters.
#' @param repeated an optional vector that identifies the order of the
#' observations within each cluster.
#' @param bstart a vector that includes an initial estimate for the marginal
#' regression parameter vector.
#' @param LORstr a character string that indicates the marginalized local odds
#' ratios structure. Options include \code{"independence"}, \code{"time.exch"},
#' \code{"RC"} or \code{"fixed"}.
#' @param LORem a character string that indicates if the marginalized local
#' odds ratios structure is estimated simultaneously (\code{"3way"}) or
#' independently at each level pair of \code{repeated} (\code{"2way"}).
#' @param LORterm a matrix that satisfies the user-defined local odds ratios
#' structure. It is ignored unless \code{LORstr="fixed"}.
#' @param add a positive constant to be added at each cell of the full
#' marginalized contingency table in the presence of zero observed counts.
#' @param homogeneous a logical that indicates homogeneous score parameters
#' when \code{LORstr="time.exch"} or \code{"RC"}.
#' @param control a vector that specifies the control variables for the GEE
#' solver.
#' @param ipfp.ctrl a vector that specifies the control variables for the
#' function \code{ipfp}.
#' @param IM a character string that indicates the method used for inverting a
#' matrix. Options include \code{"solve"}, \code{"qr.solve"} or
#' \code{"cholesky"}.
#' @return Returns an object of the class \code{"LORgee"}. This has components:
#' \item{call}{the matched call.} \item{title}{title for the GEE model.}
#' \item{version}{the current version of the GEE solver.} \item{link}{the
#' marginal link function.} \item{local.odds.ratios}{the marginalized local
#' odds ratios structure variables.} \item{terms}{the \code{terms} structure
#' describing the marginal model.} \item{contrasts}{the \code{contrasts} used
#' for the factors.} \item{nobs}{the number of observations.}
#' \item{convergence}{the values of the convergence variables.}
#' \item{coefficients}{the estimated regression parameter vector of the
#' marginal model.} \item{linear.pred}{the estimated linear predictor of the
#' marginal regression model. The \eqn{j}-th column corresponds to the
#' \eqn{j}-th response category.} \item{fitted.values}{the estimated fitted
#' values of the marginal regression model. The \eqn{j}-th column corresponds
#' to the \eqn{j}-th response category.} \item{residuals}{the residuals of the
#' marginal regression model based on the binary responses. The \eqn{j}-th
#' column corresponds to the \eqn{j}-th response category.} \item{y}{the
#' multinomial response variables.} \item{id}{the \code{id} variable.}
#' \item{max.id}{the number of clusters.} \item{clusz}{the number of
#' observations within each cluster.} \item{robust.variance}{the estimated
#' "robust" covariance matrix.} \item{naive.variance}{the estimated "naive" or
#' "model-based" covariance matrix.} \item{xnames}{the regression coefficients'
#' symbolic names.} \item{categories}{the number of observed response
#' categories.} \item{occasions}{the levels of the \code{repeated} variable.}
#' \item{LORgee.control}{the control values for the GEE solver.}
#' \item{ipfp.control}{the control values for the function \code{ipfp}.}
#' \item{inverse.method}{the method used for inverting matrices.}
#' \item{adding.constant}{the value used for \code{add}.} \item{pvalue}{the
#' p-value based on a Wald test that no covariates are statistically
#' significant.} Generic \link{coef}, \link{summary}, \link{print},
#' \link{fitted} and \link{residuals} methods are available. The \code{pvalue
#' of the Null model} corresponds to the hypothesis \eqn{H_0:
#' \beta_1=...=\beta_{J-1}=0} based on the Wald test statistic.
#' @author Anestis Touloumis
#' @seealso For an ordinal response scale use the function \link{ordLORgee}.
#' @references Touloumis, A. (2011) \emph{GEE for multinomial responses}. PhD
#' dissertation, University of Florida.
#'
#' Touloumis, A., Agresti, A. and Kateri, M. (2013) GEE for multinomial
#' responses using a local odds ratios parameterization. \emph{Biometrics}
#' \bold{69}, 633--640.
#'
#' Touloumis, A. (2015) R Package multgee: A Generalized Estimating Equations
#' Solver for Multinomial Responses. \emph{Journal of Statistical Software}
#' \bold{64}, 1--14.
#' @examples
#' ## See the interpretation in Touloumis (2011).
#' data(housing)
#' fitmod <- nomLORgee(y ~ factor(time) * sec, data = housing, id = id,
#'                     repeated = time)
#' summary(fitmod)
#' @export
nomLORgee <- function(formula = formula(data), data = parent.frame(), id = id,
                      repeated = NULL, bstart = NULL, LORstr = "time.exch",
                      LORem = "3way", LORterm = NULL, add = 0,
                      homogeneous = TRUE, control = LORgee.control(),
                      ipfp.ctrl = ipfp.control(), IM = "solve") {
  options(contrasts = c("contr.treatment", "contr.poly"))
  restricted <- NULL # nolint
  link <- "bcl"
  call <- match.call()
  mcall <- match.call(expand.dots = FALSE)
  mf <- match(c("formula", "data", "id", "repeated"), names(mcall), 0L)
  m <- mcall[c(1L, mf)]
  if (is.null(m$id)) m$id <- as.name("id")
  m[[1]] <- as.name("model.frame")
  m <- eval(m, envir = parent.frame())
  Terms <- attr(m, "terms")
  if (attr(Terms, "intercept") != 1) stop("an intercept must be included")
  Y <- as.numeric(factor(model.response(m)))
  if (is.null(Y)) stop("response variable not found")
  ncategories <- nlevels(factor(Y))
  if (ncategories <= 2) {
    stop("The response variable should have more than 2 categories")
  }
  id <- model.extract(m, "id")
  if (is.null(id)) stop("'id' variable not found")
  if (length(id) != length(Y)) {
    stop("response variable and 'id' are not of same length")
  }
  repeated <- model.extract(m, "repeated")
  if (is.null(repeated)) {
    index <- order(unlist(split(seq_len(length(id)), id)))
    repeated <- c(unlist(lapply(split(id, id), function(x) seq_len(length(x)))))
    repeated <- repeated[index]
  }
  if (length(repeated) != length(Y)) {
    stop("response variable and 'repeated' are not of same length")
  }
  id <- as.numeric(factor(id))
  repeated <- as.numeric(factor(repeated))
  if (all(id == repeated)) stop("'repeated' and 'id' must not be equal")
  dummy <- split(repeated, id)
  if (any(unlist(lapply(dummy, length)) !=
    unlist(lapply(lapply(dummy, unique), length)))) {
    stop("'repeated' does not have unique values per 'id'")
  }
  offset <- model.extract(m, "offset")
  if (length(offset) <= 1) offset <- rep(0, length(Y))
  if (length(offset) != length(Y)) {
    stop("response variable and 'offset' are not of same length")
  }
  offset <- as.double(offset)
  LORstrs <- c("independence", "time.exch", "RC", "fixed")
  icheck <- as.integer(match(LORstr, LORstrs, -1))
  if (icheck < 1) {
    stop("unknown local odds ratio structure")
  }
  if (LORstr == "independence" | LORstr == "fixed") {
    LORem <- NULL
    add <- NULL
  } else {
    if (!is.numeric(add) | add < 0) stop("'add' must be >=0")
    if (LORstr == "RC") LORem <- "2way"
    if (LORem != "2way" & LORem != "3way") {
      stop("'LORem' must be '2way' or '3way'")
    }
    if (LORstr == "time.exch" | LORstr == "RC") {
      if (!is.logical(homogeneous)) {
        stop("'homogeneous' must be 'TRUE' or 'FALSE'")
      }
    } else {
      homogeneous <- NULL
    }
    data.model <- datacounts(Y, id, repeated, ncategories)
    marpars <- mmpar(LORem, LORstr, max(data.model$tp), homogeneous)
    LORem <- marpars$LORem
    LORstr <- marpars$LORstr
    LORterm <- fitmm(data.model, marpars, homogeneous, NULL, add)
  }
  ipfp.ctrl <- ipfp.ctrl
  control <- control
  verbose <- control$verbose
  IMs <- c("cholesky", "solve", "qr.solve")
  icheck <- as.integer(match(IM, IMs, -1))
  if (icheck < 1) {
    stop("unknown method for inverting a matrix")
  }
  if (is.null(bstart)) {
    family <- VGAM::multinomial(refLevel = ncategories)
    mmodel <- VGAM::vglm(formula = formula, family = family, data = data)
    coeffs <- VGAM::coefficients(mmodel)
    coeffs <- c(matrix(coeffs, ncol = ncategories - 1, byrow = TRUE))
    coeffs <- as.numeric(coeffs)
    if (!is.numeric(coeffs)) {
      stop("Please insert initial values")
    }
    if (verbose) {
      cat("\nGEE FOR NOMINAL MULTINOMIAL RESPONSES\n")
      cat("\nrunning 'vglm' function",
        "to get initial regression estimates\n",
        sep = ""
      )
      print(matrix(coeffs,
        ncol = 1, dimnames =
          list(seq_len(length(coeffs)), "Initial.Values")
      ))
    }
  }
  Y <- rep(Y, each = ncategories - 1)
  Intercept <- rep.int(seq(ncategories - 1), length(id))
  Y <- as.numeric(Y == Intercept)
  id <- rep(id, each = ncategories - 1)
  repeated <- rep(repeated, each = ncategories - 1)
  offset <- rep(offset, each = ncategories - 1)
  Xinit_mat <- model.matrix(Terms, m)
  xxnames <- colnames(Xinit_mat)
  Xinit_mat <- apply(Xinit_mat, 2, function(x) rep(x, each = ncategories - 1))
  X_mat <- model.matrix(~ factor(Intercept) - 1)
  if (ncol(Xinit_mat) > 1) {
    Xinit_mat <- cbind(X_mat, Xinit_mat[, -1])
  } else {
    Xinit_mat <- X_mat
  }
  if (ncol(Xinit_mat) != (ncategories - 1)) {
    X_inter <- X_mat
    for (i in ncategories:ncol(Xinit_mat)) {
      X_mat <- cbind(X_mat, X_inter *
        Xinit_mat[, i])
    }
  }
  X_mat <- matrix(X_mat, ncol = ncol(X_mat), dimnames = NULL)
  X_mat <- X_mat[, c(matrix(seq(ncol(X_mat)),
    ncol = ncategories - 1,
    byrow = TRUE
  ))]
  if (!is.null(bstart)) {
    coeffs <- as.numeric(bstart)
    if (length(coeffs) != ncol(X_mat)) {
      stop("'bstart' and 'beta' differ in length")
    }
    if (verbose) {
      cat("\nGEE FOR NOMINAL MULTINOMIAL RESPONSES\n")
      cat("\nuser's initial regression estimate\n")
      print(matrix(coeffs, ncol = 1, dimnames = list(
        seq_len(length(coeffs)),
        "Initial.Values"
      )))
    }
  }
  ordindex <- order(id, repeated)
  Y <- Y[ordindex]
  X_mat <- X_mat[ordindex, ]
  id <- id[ordindex]
  repeated <- repeated[ordindex]
  offset <- offset[ordindex]
  fitmod <- fitLORgee(Y, X_mat, coeffs, ncategories, id, repeated, offset,
    link, LORterm, marpars, ipfp.ctrl, control, IM,
    LORem = LORem, LORstr = LORstr, add
  )
  fit <- list()
  fit$call <- call
  fit$title <- "GEE FOR NOMINAL MULTINOMIAL RESPONSES"
  fit$version <- "version 1.6.0 modified 2017-07-10"
  fit$link <- c("Baseline Category Logit")
  fit$local.odds.ratios <- list()
  fit$local.odds.ratios$structure <- LORstr
  fit$local.odds.ratios$model <- LORem
  fit$local.odds.ratios$homogeneous <- homogeneous
  fit$local.odds.ratios$theta <- fitmod$theta
  fit$terms <- Terms
  fit$contrasts <- attr(model.matrix(Terms, m), "contrasts")
  fit$convergence <- list()
  fit$convergence$niter <- fitmod$iter
  fit$convergence$criterion <- fitmod$crit[fitmod$iter]
  fit$convergence$conv <- fitmod$conv
  xnames <- paste0("beta", 1:(ncategories - 1), "0")
  if (length(xxnames) > 1) {
    xxnames <- c(xnames, xxnames[-1])
    if (length(xxnames) > length(xnames)) {
      for (i in 1:((ncol(X_mat) - ncategories + 1) / (ncategories - 1))) {
        xnames <- c(xnames, paste(xxnames[i + ncategories - 1],
          1:(ncategories - 1),
          sep = ":"
        ))
      }
    }
    xnames <- xnames[c(matrix(seq(ncol(X_mat)),
      ncol = ncategories - 1,
      byrow = TRUE
    ))]
  }
  fit$coefficients <- fitmod$beta_mat[, fitmod$iter + 1]
  names(fit$coefficients) <- xnames
  fit$linear.predictors <- matrix(fitmod$linear.predictor,
    ncol = ncategories - 1, byrow = TRUE
  )
  rownames(fit$linear.predictors) <- seq_len(nrow(fit$linear.predictors))
  colnames(fit$linear.predictors) <- 1:(ncategories - 1)
  fitted.values <- fitmod$fitted.values
  fitted.values.1 <- matrix(fitted.values, ncol = ncategories - 1, byrow = TRUE)
  fitted.values.2 <- 1 - rowSums(fitted.values.1)
  fitted.values <- cbind(fitted.values.1, fitted.values.2)
  rownames(fitted.values) <- seq_len(nrow(fitted.values.1))
  colnames(fitted.values) <- 1:ncategories
  fit$fitted.values <- fitted.values
  fit$residuals <- matrix(fitmod$residuals,
    ncol = ncategories - 1,
    byrow = TRUE
  )
  rownames(fit$residuals) <- seq_len(nrow(fit$residuals))
  colnames(fit$residuals) <- 1:(ncategories - 1)
  y <- Y
  y <- apply(matrix(y, ncol = ncategories - 1, byrow = TRUE), 1, function(x) {
    which(x == 1)
  })
  y <- as.numeric(y)
  y[is.na(y)] <- ncategories
  fit$y <- y
  fit$nobs <- length(y)
  fit$max.id <- max(unique(id))
  fit$clusz <- unlist(lapply(split(id, id), length)) / (ncategories - 1)
  fit$id <- rep(1:fit$max.id, as.numeric(fit$clusz))
  fit$robust.variance <- fitmod$robust
  dimnames(fit$robust.variance) <- list(xnames, xnames)
  fit$naive.variance <- fitmod$naive
  dimnames(fit$naive.variance) <- list(xnames, xnames)
  fit$xnames <- xnames
  fit$categories <- ncategories
  fit$occasions <- sort(unique(repeated))
  fit$LORgee.control <- control
  fit$ipfp.control <- ipfp.ctrl
  fit$inverse.method <- IM
  fit$adding.constant <- add
  if (control$TRACE) {
    fit$trace <- list()
    fit$trace$coeffs <- fitmod$beta_mat
    fit$trace$crit <- fitmod$crit
  }
  if (length(xxnames) == (ncategories - 1)) {
    fit$pvalue <- NULL
  } else {
    dummy <- seq(1, length(xxnames), ncategories - 1)
    waldts <- fit$coefficients[-dummy] %*%
      solve((fit$robust.variance)[-dummy, -dummy])
    waldts <- waldts %*% fit$coefficients[-dummy]
    fit$pvalue <- 1 - pchisq(waldts, length(xxnames) - length(dummy))
  }
  class(fit) <- "LORgee"
  fit
}
