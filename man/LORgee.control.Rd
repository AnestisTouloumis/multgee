\name{LORgee.control}
\alias{LORgee.control}
\title{
Control For The GEE Solver
}
\description{
Control variables for the GEE solver in the functions \link{nomLORgee} and \link{ordLORgee}.
}
\usage{
LORgee.control(tolerance = 0.001, maxiter = 15, verbose = FALSE, TRACE = FALSE)
}
\arguments{
  \item{tolerance}{
positive convergence tolerance. The algorithm converges when the maximum of the absolute relative difference in parameter estimates is less than or equal to \code{tolerance}.
}
  \item{maxiter}{
positive integer that indicates the maximum number of iterations in the Fisher-scoring iterative algorithm. 
}
  \item{verbose}{
logical that indicates if output should be printed at each iteration.
}
  \item{TRACE}{
logical that indicates if the parameter estimates and the convergence criterion at each iteration should be saved.
}
}
\author{
Anestis Touloumis
}
\seealso{
\link{nomLORgee} and \link{ordLORgee}.
}
\examples{
data(arthritis)
fitmod <- ordLORgee(y~factor(trt)+factor(baseline)+factor(time), 
                    data = arthritis, id = id, repeated = time)
## A one-step GEE estimator
fitmod1 <- update(fitmod, control = LORgee.control(maxiter=1))
coef(fitmod)
coef(fitmod1)
}
