\name{waldts}
\alias{waldts}
\title{
Wald Test of Nested GEE Models
}
\description{
Comparing two nested GEE models by carrying out a Wald test.
}
\usage{
waldts(object0, object1)
}
\arguments{
  \item{object0}{
A GEE model of the class "\code{LORgee}".
}
  \item{object1}{
A GEE model of the class "\code{LORgee}".
}
}
\details{
The two GEE models implied by \code{object0} and \code{object1} must be nested.
}
\author{
Anestis Touloumis
}
\examples{
data(housing)
set.seed(1)
fitmod1 <- nomLORgee(y~factor(time)*sec,data=housing,id=id,repeated=time)
set.seed(1)
fitmod0 <- update(fitmod1,formula=y~factor(time)+sec)
waldts(fitmod0,fitmod1)
}
