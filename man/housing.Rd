\name{housing}
\alias{housing}
\docType{data}
\title{
Homeless Data
}
\description{
Housing status for 362 severely mentally ill homeless subjects measured at baseline and at three follow-up times.
}
\usage{data(housing)}
\format{
  A data frame with 1448 observations on the following 4 variables:
  \describe{
    \item{\code{id}}{Subject identifier variable.}
    \item{\code{y}}{Housing status response, coded as (1) for street living, (2) for community living and (3) for independent housing.}
    \item{\code{time}}{Time recorded in months.}
    \item{\code{sec}}{Section 8 rent certificate indicator.}
  }
}
\source{
Hulrburt M.S., Wood, P.A. and Hough, R.L. (1996) Providing independent housing for the homeless mentally ill: a novel approach to evaluating longitudinal housing patterns. \emph{Journal of Community Psychology}, \bold{24}, 291--310.
}
\examples{
data(housing)
str(housing)
}
\keyword{datasets}
