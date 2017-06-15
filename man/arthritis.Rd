\name{arthritis}
\alias{arthritis}
\docType{data}
\title{
Rheumatoid Arthritis Clinical Trial
}
\description{
Rheumatoid self-assessment scores for 302 patients, measured on a five-level ordinal response scale at three follow-up times. 
}
\usage{data(arthritis)}
\format{
  A data frame with 906 observations on the following 7 variables:
  \describe{
    \item{\code{id}}{Patient identifier variable.}
    \item{\code{y}}{Self-assessment score of rheumatoid arthritis measured on a five-level ordinal response scale.}
    \item{\code{sex}}{Coded as (1) for female and (2) for male.}
    \item{\code{age}}{Recorded at the baseline.}
    \item{\code{trt}}{Treatment group variable, coded as (1) for the placebo group and (2) for the drug group.}
    \item{\code{baseline}}{Self-assessment score of rheumatoid arthritis at the baseline.}
    \item{\code{time}}{Follow-up time recorded in months.}
  } 
}
\source{
Lipsitz, S.R. and Kim, K. and Zhao, L. (1994) Analysis of repeated categorical data using generalized estimating equations. \emph{Statistics in Medicine}, \bold{13}, 1149--1163.
}
\examples{
data(arthritis)
str(arthritis)
}
\keyword{datasets}
