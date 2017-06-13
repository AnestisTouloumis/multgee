\name{matrixLOR}
\alias{matrixLOR}
\title{
Creating A Probability Matrix With Specified Local Odds Ratios
}
\description{
Utility function to create a square probability matrix that satifies the specified local odds ratios structure.
}
\usage{
matrixLOR(x)
}
\arguments{
  \item{x}{
a square matrix with positive entries that describes the desired local odds ratios matrix.
}
}
\value{
Returns a square probability matrix that satisfies the local odds ratios structure defined by \code{x}.
}
\section{Warning}{
Caution is needed for local odds ratios close to zero.
}
\details{
This function is designed to ease the construction of the \code{LORterm} argument in the functions \link{nomLORgee} and \link{ordLORgee}.
}
\author{
Anestis Touloumis
}
\seealso{
\link{nomLORgee} and \link{ordLORgee}.
}
\examples{
## Illustrating the construction of a "fixed" local odds ratios structure 
## using the arthritis dataset. Here, we assume a uniform local odds ratios 
## structure equal to 2 for each time pair.

## Create the uniform local odds ratios structure.
lorterm <- matrixLOR(matrix(2,4,4))

## Create the LORterm argument.
lorterm <- c(lorterm)
lorterm <- matrix(c(lorterm),3,25,TRUE)

## Fit the marginal model.
data(arthritis)
fitmod.fixed <- ordLORgee(y~factor(trt)+factor(time)+factor(baseline),data=arthritis,
                id=id,repeated=time,LORstr="fixed",LORterm=lorterm)
fitmod.fixed
}

