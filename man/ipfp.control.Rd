\name{ipfp.control}
\alias{ipfp.control}
\title{
IPFP Control
}
\description{
Control variables for the Iterative Proportion Fitting Procedure function \code{ipfp}.
}
\usage{
ipfp.control(tol = 1e-06, maxit = 200)
}
\arguments{
  \item{tol}{
positive convergence tolerance. The algorithm converges when the absolute difference between the observed and the given row or column totals is less than or equal to \code{tol}.
}
  \item{maxit}{
positive integer that indicates the maximum number of iterations.
}
}
\author{
Anestis Touloumis
}
\note{
Currently the function \code{ipfp} is internal.
}
\seealso{
\link{nomLORgee} and \link{ordLORgee}.
}

