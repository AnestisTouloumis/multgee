#' Intrinsic Parameters Estimation
#'
#' Utility function to assess the underlying association pattern.
#'
#' Simulation studies in \cite{Touloumis et al. (2013)} suggested that if the
#' range of the intrinsic parameter estimates is small then simple local odds
#' ratios structures should adequately approximate the association pattern.
#' Otherwise more complicated structures should be employed.
#'
#' The intrinsic parameters are estimated under the heterogeneous
#' linear-by-linear association model (\cite{Agresti, 2013}) for ordinal
#' response categories and under the RC-G(1) model (\cite{Becker and Clogg,
#' 1989}) with homogeneous score parameters for nominal response categories.
#'
#' A detailed description of the arguments \code{id} and \code{repeated} can be
#' found in the Details section of \link{nomLORgee} or \link{ordLORgee}.
#'
#' @param y a vector that identifies the response vector of the desired
#' marginal model.
#' @param data an optional data frame containing the variables provided in
#' \code{y}, \code{id} and \code{repeated}.
#' @param id a vector that identifies the clusters.
#' @param repeated an optional vector that identifies the order of observations
#' within each cluster.
#' @param rscale a character string that indicates the nature of the response
#' scale. Options include "\code{ordinal}" or "\code{nominal}".
#' @return Returns a numerical vector with the estimated intrinsic parameters.
#' @author Anestis Touloumis
#' @seealso \link{nomLORgee} and \link{ordLORgee}.
#' @references Agresti, A. (2013) \emph{Categorical Data Analysis}. New York:
#' John Wiley and Sons, Inc., 3rd Edition.
#'
#' Becker, M. and Clogg, C. (1989) Analysis of sets of two-way contingency
#' tables using association models. \emph{Journal of the American Statistical
#' Association} \bold{84}, 142--151.
#'
#' Touloumis, A., Agresti, A. and Kateri, M. (2013) GEE for multinomial
#' responses using a local odds ratios parameterization. \emph{Biometrics}
#' \bold{69}, 633--640.
#' @examples
#' data(arthritis)
#' intrinsic.pars(y, arthritis, id, time, rscale = "ordinal")
#' ## The intrinsic parameters do not vary much. The 'uniform' local odds ratios
#' ## structure might be a good approximation for the association pattern.
#'
#' set.seed(1)
#' data(housing)
#' intrinsic.pars(y, housing, id, time, rscale = "nominal")
#' ## The intrinsic parameters vary. The 'RC' local odds ratios structure
#' ## might be a good approximation for the association pattern.
#' @export
intrinsic.pars <- function(y = y, data = parent.frame(), id = id,
                           repeated = NULL, rscale = "ordinal") {
  call <- match.call() # nolint
  mcall <- match.call(expand.dots = FALSE)
  mf <- match(c("y", "data", "id", "repeated"), names(mcall), 0L)
  m <- mcall[c(1L, mf)]
  if (is.null(m$id)) {
    m$id <- as.name("id")
  }
  m$formula <- y ~ 1
  m[[1]] <- as.name("model.frame")
  m <- eval(m, envir = parent.frame())
  Terms <- attr(m, "terms")
  if (attr(Terms, "intercept") != 1) {
    stop("an intercept must be included")
  }
  Y <- as.numeric(factor(model.response(m)))
  if (is.null(Y)) {
    stop("response variable not found")
  }
  ncategories <- nlevels(factor(Y))
  if (ncategories <= 2) {
    stop("The response variable should have more than 2 categories")
  }
  id <- model.extract(m, "id")
  if (is.null(id)) {
    stop("'id' variable not found")
  }
  if (length(id) != length(Y)) {
    stop("response variable and 'id' are not of same length")
  }
  repeated <- model.extract(m, "repeated")
  if (is.null(repeated)) {
    index <- order(unlist(split(seq_len(length(id)), id)))
    repeated <- c(unlist(lapply(split(id, id), function(x) {
      seq_len(length(x))
    })))
    repeated <- repeated[index]
  }
  if (length(repeated) != length(Y)) {
    stop("response variable and 'repeated' are not of same length")
  }
  id <- as.numeric(factor(id))
  repeated <- as.numeric(factor(repeated))
  if (all(id == repeated)) {
    stop("'repeated' and 'id' must not be equal")
  }
  dummy <- split(repeated, id)
  if (any(unlist(lapply(dummy, length)) != unlist(lapply(lapply(
    dummy,
    unique
  ), length)))) {
    stop("'repeated' does not have unique values per 'id'")
  }
  cdata <- datacounts(Y, id, repeated, ncategories)
  if (rscale == "ordinal") {
    cmod <- gnm(
      counts ~ (factor(x) + factor(y)) * factor(tp) + factor(tp):x:y,
      family = poisson, data = cdata
    )
    ans <- as.vector(coef(cmod)[pickCoef(cmod, "x:y")])
  } else {
    ans <- rep(0, max(cdata$tp))
    cdata$x <- factor(cdata$x)
    cdata$y <- factor(cdata$y)
    for (i in 1:max(cdata$tp)) {
      cmod <- gnm(counts ~ x + y + MultHomog(x, y),
        family = poisson,
        data = cdata[cdata$tp == i, ]
      )
      cscores <- coef(cmod)[pickCoef(cmod, "MultHomog")]
      cscores <- c(tcrossprod(normscores(cscores)))
      cmod <- gnm(counts ~ factor(x) + factor(y) + cscores,
        family = poisson, data = cdata[cdata$tp == i, ]
      )
      ans[i] <- coef(cmod)["cscores"]
    }
  }
  ans
}
