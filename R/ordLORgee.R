#' Marginal Models For Correlated Ordinal Multinomial Responses
#'
#' Solving the generalized estimating equations for correlated ordinal
#' multinomial responses assuming a cumulative link model or an adjacent
#' categories logit model for the marginal probabilities.
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
#' The \eqn{J}-th response category is omitted.
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
#' The fitted marginal cumulative link model is \deqn{Pr(Y_{it}\le j
#' |x_{it})=F(\beta_{j0} +\beta^{'} x_{it})} where \eqn{Y_{it}} is the
#' \eqn{t}-th multinomial response for cluster \eqn{i}, \eqn{x_{it}} is the
#' associated covariates vector, \eqn{F} is the cumulative distribution
#' function determined by \code{link}, \eqn{\beta_{j0}} is the \eqn{j}-th
#' response category specific intercept and \eqn{\beta} is the marginal
#' regression parameter vector excluding intercepts.
#'
#' The marginal adjacent categories logit model \deqn{log \frac{Pr(Y_{it}=j
#' |x_{it})}{Pr(Y_{it}=j+1 |x_{it})}=\beta_{j0} +\beta^{'} x_{it}} is fitted if
#' and only if \code{link="acl"}. In contrast to a marginal cumulative link
#' model, here the intercepts do not need to be monotone increasing.
#'
#' The formulae are easier to read from either the Vignette or the Reference
#' Manual (both available
#' \href{https://CRAN.R-project.org/package=multgee}{here}).
#'
#' The \code{LORterm} argument must be an \eqn{L} x \eqn{J^2} matrix, where
#' \eqn{L} is the number of level pairs of \code{repeated}. These are ordered
#' as \eqn{(1,2), (1,3),\ldots,(1,T), (2,3),\ldots,(T-1,T)} and the rows of
#' \code{LORterm} are supposed to preserve this order. Each row is assumed to
#' contain the vectorized form of a probability table that satisfies the
#' desired local odds ratios structure.
#'
#' @param formula a formula expression as for other regression models for
#' multinomial responses. An intercept term must be included.
#' @param data an optional data frame containing the variables provided in
#' \code{formula}, \code{id} and \code{repeated}.
#' @param id a vector that identifies the clusters.
#' @param repeated an optional vector that identifies the order of observations
#' within each cluster.
#' @param link a character string that specifies the link function. Options
#' include \code{"logit"}, \code{"probit"}, \code{"cauchit"}, \code{"cloglog"}
#' or \code{"acl"}.
#' @param bstart a vector that includes an initial estimate for the marginal
#' regression parameter vector.
#' @param LORstr a character string that indicates the marginalized local odds
#' ratios structure. Options include \code{"independence"}, \code{"uniform"},
#' \code{"category.exch"}, \code{"time.exch"}, \code{"RC"} or \code{"fixed"}.
#' @param LORem a character string that indicates if the marginalized local
#' odds ratios structure is estimated simultaneously (\code{"3way"}) or
#' independently at each level pair of \code{repeated} (\code{"2way"}).
#' @param LORterm a matrix that satisfies the user-defined local odds ratios
#' structure. It is ignored unless \code{LORstr="fixed"}.
#' @param add a positive constant to be added at each cell of the full
#' marginalized contingency table in the presence of zero observed counts.
#' @param homogeneous a logical that indicates homogeneous score parameters
#' when \code{LORstr="time.exch"} or \code{"RC"}.
#' @param restricted a logical that indicates monotone score parameters when
#' \code{LORstr="time.exch"} or \code{"RC"}.
#' @param control a vector that specifies the control variables for the GEE
#' solver.
#' @param ipfp.ctrl a vector that specifies the control variables for the
#' function \code{ipfp}.
#' @param IM a character string that indicates the method used for inverting a
#' matrix. Options include \code{"solve"}, \code{"qr.solve"} or
#' \code{"cholesky"}.
#'
#' @return Returns an object of the class \code{"LORgee"}. This has components:
#' \item{call}{the matched call.}
#' \item{title}{title for the GEE model.}
#' \item{version}{the current version of the GEE solver.}
#' \item{link}{the marginal link function.}
#' \item{local.odds.ratios}{the marginalized local odds ratios structure
#' variables.}
#' \item{terms}{the \code{terms} structure describing the model.}
#' \item{contrasts}{the \code{contrasts} used for the factors.}
#' \item{nobs}{the number of observations.}
#' \item{convergence}{the values of the convergence variables.}
#' \item{coefficients}{the estimated regression parameter vector of the marginal
#' model.}
#' \item{linear.pred}{the estimated linear predictor of the marginal regression
#' model. The \eqn{j}-th column corresponds to the \eqn{j}-th response
#' category.}
#' \item{fitted.values}{the estimated fitted values of the marginal regression
#' model. The \eqn{j}-th column corresponds to the \eqn{j}-th response
#' category.}
#' \item{residuals}{the residuals of the marginal regression model. The
#' \eqn{j}-th column corresponds to the \eqn{j}-th response category.}
#' \item{y}{the multinomial response variables.}
#' \item{id}{the \code{id} variable.}
#' \item{max.id}{the number of clusters.}
#' \item{clusz}{the number of observations within each cluster.}
#' \item{robust.variance}{the estimated sandwich (robust) covariance matrix.}
#' \item{naive.variance}{the estimated model-based (naive) covariance
#' matrix.}
#' \item{xnames}{the regression coefficients' symbolic names.}
#' \item{categories}{the number of observed response categories.}
#' \item{occasions}{the levels of the \code{repeated} variable.}
#' \item{LORgee_control}{the control values for the GEE solver.}
#' \item{ipfp.control}{the control values for the function \code{ipfp}.}
#' \item{inverse.method}{the method used for inverting matrices.}
#' \item{adding.constant}{the value used for \code{add}.}
#' \item{pvalue}{the p-value based on a Wald test that no covariates are
#' statistically significant.}
#'
#' Generic \link{coef}, \link{summary}, \link{print},
#' \link{fitted} and \link{residuals} methods are available. The \code{pvalue
#' of the Null model} corresponds to the hypothesis \eqn{H_0: \beta=0} based on
#' the Wald test statistic.
#'
#' @author Anestis Touloumis
#'
#' @seealso For a nominal response scale use the function \link{nomLORgee}.
#'
#' @references Touloumis, A., Agresti, A. and Kateri, M. (2013) GEE for
#' multinomial responses using a local odds ratios parameterization.
#' \emph{Biometrics}, \bold{69}, 633-640.
#'
#' Touloumis, A. (2015) R Package multgee: A Generalized Estimating Equations
#' Solver for Multinomial Responses. \emph{Journal of Statistical Software},
#' \bold{64}, 1-14.
#'
#' @examples
#' data(arthritis)
#' intrinsic.pars(y, arthritis, id, time)
#' fitmod <- ordLORgee(formula = y ~ factor(time) + factor(trt) + factor(baseline),
#'   data = arthritis, id = id, repeated = time, LORstr = "uniform")
#' summary(fitmod)
#' @export
ordLORgee <- function(formula = formula(data), data = parent.frame(), id = id,
                      repeated = NULL, link = "logit", bstart = NULL,
                      LORstr = "category.exch", LORem = "3way", LORterm = NULL,
                      add = 0, homogeneous = TRUE, restricted = FALSE,
                      control = LORgee_control(), ipfp.ctrl = ipfp.control(),
                      IM = "solve") {
  options(contrasts = c("contr.treatment", "contr.poly"))
  cl <- match.call()
  mcall <- match.call(expand.dots = FALSE)
  mf <- match(c("formula", "data", "id", "repeated", "offset"), names(mcall),
              0L)
  m <- mcall[c(1L, mf)]
  m$drop.unused.levels <- TRUE
  m[[1]] <- quote(stats::model.frame)
  m <- eval(m, envir = parent.frame())
  Terms <- attr(m, "terms")
  if (attr(Terms, "intercept") != 1) stop("an intercept must be included")
  Y <- as.numeric(factor(model.response(m)))
  if (is.null(Y)) stop("response variable not found")
  ncategories <- nlevels(factor(Y))
  if (ncategories <= 2)
    stop("The response variable should have more than 2 categories")
  id <- model.extract(m, "id")
  if (is.null(id)) stop("'id' variable not found")
  if (length(id) != length(Y))
    stop("response variable and 'id' are not of same length")
  repeated <- model.extract(m, "repeated")
  if (is.null(repeated)) {
    index <- order(unlist(split(seq_len(length(id)), id)))
    repeated <- c(unlist(lapply(split(id, id), function(x) seq_len(length(x)))))
    repeated <- repeated[index]
  }
  if (length(repeated) != length(Y))
    stop("response variable and 'repeated' are not of same length")
  id <- as.numeric(factor(id))
  repeated <- as.numeric(factor(repeated))
  if (all(id == repeated)) {
    stop("'repeated' and 'id' must not be equal")
  }
  dummy <- split(repeated, id)
  if (any(unlist(lapply(dummy, anyDuplicated)) != 0))
    stop("'repeated' does not have unique values per 'id'")
  offset <- model.extract(m, "offset")
  if (length(offset) <= 1) offset <- rep(0, length(Y))
  if (length(offset) != length(Y))
    stop("response variable and 'offset' are not of same length")
  offset <- as.double(offset)
  icheck <- pmatch(LORstr, c(
    "independence", "uniform", "category.exch", "time.exch", "RC", "fixed"
    ),
    nomatch = 0,
    duplicates.ok = FALSE
    )
  if (icheck == 0) stop("unknown local odds ratios structure")
  if (LORstr == "independence" | LORstr == "fixed") {
    LORem <- NULL
    add <- NULL
  } else if (LORstr == "category.exch") {
    LORem <- "3way"
  } else {
    if (LORstr == "RC") LORem <- "2way"
    if (LORem != "2way" & LORem != "3way") {
      stop("'LORem' must be '2way' or '3way'")
    }
  }
  if (LORstr == "time.exch" | LORstr == "RC") {
    if (!is.logical(homogeneous)) {
      stop("'homogeneous' must be 'TRUE' or 'FALSE'")
    }
    if (!is.logical(restricted)) {
      stop("'restricted' must be 'TRUE' or 'FALSE'")
    }
    restricted <- if (!restricted) NULL else TRUE
  } else {
    homogeneous <- restricted <- NULL
  }
  if (LORstr == "independence" | LORstr == "fixed") {
    add <- NULL
  } else {
    if (!is.numeric(add) | add < 0) {
      stop("'add' must be >=0")
    }
  }
  if (LORstr != "independence" & LORstr != "fixed") {
    data.model <- datacounts(Y, id, repeated, ncategories)
    marpars <- mmpar(LORem, LORstr, max(data.model$tp), homogeneous)
    LORem <- marpars$LORem
    LORstr <- marpars$LORstr
    LORterm <- fitmm(data.model, marpars, homogeneous, restricted, add)
  }
  ipfp.ctrl <- ipfp.ctrl
  control <- control
  verbose <- control$verbose
  icheck <- pmatch(IM, c("cholesky", "solve", "qr.solve"), nomatch = 0,
                   duplicates.ok = FALSE)
  if (icheck == 0) stop("unknown method for inverting a matrix")
  link <- as.character(link)
  icheck <- pmatch(link, c(
    "logit", "probit", "cloglog", "cauchit", "acl"
    ),
    nomatch = 0,
    duplicates.ok = TRUE
  )
  if (icheck == 0) stop("'link' must be \"logit\", \"probit\", \"cloglog\",
                        \"cauchit\" or \"acl\"")
  if (is.null(bstart)) {
    if (link == "logit") {
      family <- VGAM::cumulative(link = "logitlink", parallel = TRUE)
    } else if (link == "probit") {
      family <- VGAM::cumulative(link = "probitlink", parallel = TRUE)
    } else if (link == "cloglog") {
      family <- VGAM::cumulative(link = "clogloglink", parallel = TRUE)
    } else if (link == "cauchit") {
      family <- VGAM::cumulative(link = "cauchitlink", parallel = TRUE)
    } else {
      family <- VGAM::acat(reverse = TRUE, parallel = TRUE)
    }
    vglm_model <- VGAM::vglm(formula = formula, family = family, data = data)
    coeffs <- VGAM::coefficients(vglm_model)
    if (any(!is.finite(coeffs))) stop("Please insert initial values")
    if (verbose) {
      cat("\nGEE FOR ORDINAL MULTINOMIAL RESPONSES\n")
      print(matrix(coeffs,
        ncol = 1,
        dimnames = list(seq_len(length(coeffs)), "Initial.Values")
      ))
    }
  }
  Y <- rep(Y, each = ncategories - 1)
  Intercept <- rep.int(seq(ncategories - 1), length(id))
  Y <- as.numeric(Y == Intercept)
  id <- rep(id, each = ncategories - 1)
  repeated <- rep(repeated, each = ncategories - 1)
  offset <- rep(offset, each = ncategories - 1)
  X_mat <- model.matrix(Terms, m)
  if (ncol(X_mat) > 1) {
    xnames <- colnames(X_mat)
    X_mat <- apply(X_mat, 2, function(x) rep(x, each = ncategories - 1))
    X_mat1 <- model.matrix(~ factor(Intercept) - 1)
    X_mat <- cbind(X_mat1, X_mat[, -1])
    X_mat <- matrix(X_mat, ncol = ncol(X_mat), dimnames = NULL)
    xnames <- c(paste0("beta", 1:(ncategories - 1), "0"), xnames[-1])
  } else {
    X_mat <- model.matrix(~ factor(Intercept) - 1)
    xnames <- c(paste0("beta", 1:(ncategories - 1), "0"))
  }
  if (link == "acl") {
    dummy <- ncategories - 1
    dummy.matrix <- diagmod(rep.int(1, dummy))
    dummy.matrix[upper.tri(dummy.matrix)] <- 1
    X_mat[, 1:dummy] <-
      kronecker(rep.int(1, nrow(X_mat) / dummy), dummy.matrix)
    if (dummy != ncol(X_mat)) {
      X_mat[, -c(1:dummy)] <- X_mat[, -c(1:dummy)] * rep(
        dummy:1,
        nrow(X_mat) / dummy
      )
    }
  }
  if (!is.null(bstart)) {
    coeffs <- as.numeric(bstart)
    if (length(coeffs) != ncol(X_mat)) {
      stop("Starting values and parameters vector differ in length")
    }
    if (any(diff(coeffs[1:(ncategories - 1)]) < 0)) {
      stop("cutpoints are not increasing")
    }
    if (verbose) {
      cat("\nGEE FOR ORDINAL MULTINOMIAL RESPONSES\n")
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
  fit$call <- cl
  fit$title <- "GEE FOR ORDINAL MULTINOMIAL RESPONSES"
  fit$version <- "version 1.6.0 modified 2017-07-10"
  fit$link <- if (link == "acl") {
    paste("Adjacent Category Logit")
  } else {
    paste("Cumulative", link, sep = " ")
  }
  fit$local.odds.ratios <- list()
  fit$local.odds.ratios$structure <- LORstr
  fit$local.odds.ratios$model <- LORem
  fit$local.odds.ratios$homogeneous <- homogeneous
  fit$local.odds.ratios$restricted <- restricted
  fit$local.odds.ratios$theta <- fitmod$theta
  fit$terms <- Terms
  fit$contrasts <- attr(model.matrix(Terms, m), "contrasts")
  fit$convergence <- list()
  fit$convergence$niter <- fitmod$iter
  fit$convergence$criterion <- fitmod$crit[fitmod$iter]
  fit$convergence$conv <- fitmod$conv
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
  y <- apply(
    matrix(y, ncol = ncategories - 1, byrow = TRUE), 1,
    function(x) which(x == 1)
  )
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
  fit$LORgee_control <- control
  fit$ipfp.control <- ipfp.ctrl
  fit$inverse.method <- IM
  fit$adding.constant <- add
  if (control$TRACE) {
    fit$trace <- list()
    fit$trace$coeffs <- fitmod$beta_mat
    fit$trace$crit <- fitmod$crit
  }
  if (length(xnames) == (ncategories - 1)) {
    fit$pvalue <- NULL
  } else {
    dummy <- 1:(ncategories - 1)
    waldts <- fit$coefficients[-dummy] %*%
      solve((fit$robust.variance)[-dummy, -dummy])
    waldts <- waldts %*% fit$coefficients[-dummy]
    fit$pvalue <- 1 - pchisq(waldts, length(xnames) - length(dummy))
  }
  class(fit) <- "LORgee"
  fit
}
