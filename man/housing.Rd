% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multgee_data.R
\docType{data}
\name{housing}
\alias{housing}
\title{Homeless Data}
\format{
A data frame with 1448 observations on the following 4 variables:
\describe{ \item{id}{Subject identifier variable.}
\item{y}{Housing status response, coded as (1) for street living,
(2) for community living and (3) for independent housing.}
\item{time}{Time recorded in months.} \item{sec}{Section 8
rent certificate indicator.} }
}
\source{
Hulrburt M.S., Wood, P.A. and Hough, R.L. (1996) Providing
independent housing for the homeless mentally ill: a novel approach to
evaluating longitudinal housing patterns. \emph{Journal of Community
Psychology}, \bold{24}, 291--310.
}
\usage{
housing
}
\description{
Housing status for 362 severely mentally ill homeless subjects measured at
baseline and at three follow-up times.
}
\examples{
data(housing)
str(housing)
}
\keyword{datasets}
