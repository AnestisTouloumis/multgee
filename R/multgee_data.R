#' Rheumatoid Arthritis Clinical Trial
#'
#' Rheumatoid self-assessment scores for 302 patients, measured on a five-level
#' ordinal response scale at three follow-up times.
#'
#' @format A data frame with 906 observations on the following 7 variables:
#' \describe{ \item{id}{Patient identifier variable.}
#' \item{y}{Self-assessment score of rheumatoid arthritis measured on a
#' five-level ordinal response scale.} \item{sex}{Coded as (1) for
#' female and (2) for male.} \item{age}{Recorded at the baseline.}
#' \item{trt}{Treatment group variable, coded as (1) for the placebo
#' group and (2) for the drug group.} \item{baseline}{Self-assessment
#' score of rheumatoid arthritis at the baseline.}
#' \item{time}{Follow-up time recorded in months.} }
#'
#' @source Lipsitz, S.R. and Kim, K. and Zhao, L. (1994) Analysis of repeated
#' categorical data using generalized estimating equations. \emph{Statistics in
#' Medicine}, \bold{13}, 1149--1163.
#'
#' @keywords datasets
#'
#' @examples
#' data(arthritis)
#' str(arthritis)
"arthritis"

#' Homeless Data
#'
#' Housing status for 362 severely mentally ill homeless subjects measured at
#' baseline and at three follow-up times.
#'
#' @format A data frame with 1448 observations on the following 4 variables:
#' \describe{ \item{id}{Subject identifier variable.}
#' \item{y}{Housing status response, coded as (1) for street living,
#' (2) for community living and (3) for independent housing.}
#' \item{time}{Time recorded in months.} \item{sec}{Section 8
#' rent certificate indicator.} }
#'
#' @source Hulrburt M.S., Wood, P.A. and Hough, R.L. (1996) Providing
#' independent housing for the homeless mentally ill: a novel approach to
#' evaluating longitudinal housing patterns. \emph{Journal of Community
#' Psychology}, \bold{24}, 291--310.
#'
#' @keywords datasets
#'
#' @examples
#' data(housing)
#' str(housing)
"housing"
