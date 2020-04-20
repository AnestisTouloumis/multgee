\name{NEWS}
\title{NEWS file for the \pkg{multgee} package}


\section{Changes in Version 1.7.0 (2020-04-20)}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item{Added \code{confint} function.}
      \item{Added \code{vcov} function.}
    }
  }
    \subsection{MAJOR CHANGES}{
    \itemize{
      \item{Fixed a bug in \code{matrixLOR}. Now it stops if zero
      entries are provided.}
      \item{Updated vignette to describe the new features.}
    }
  }
  \subsection{MINOR CHANGES}{
    \itemize{
    \item{Added lintr in travis.yml.}
    \item{Moved \pkg{VGAM} to Imports.}
    \item{Updated documentation.}
    \item{Updated README.md.}
    \item{Used JSS template for vignette.}
    }
    }
  }

 
\section{Changes in Version 1.6.0 (2017-07-11)}{
   \subsection{MINOR CHANGES}{
     \itemize{
       \item{Added README.Rmd.}
       \item{Updated DESCRIPTION.}
       \item{Updated documentation.}
       \item{Updated R code.}
       \item{Updated vignette.}
       }
       }
       }

   
\section{Changes in Version 1.5.3 (2016-02-03)}{
   \subsection{MINOR CHANGES}{
     \itemize{
       \item{Updated vignette.}
       }
       }
       }


\section{Changes in Version 1.5.2 (2015-09-08)}{
    \subsection{MAJOR CHANGES}{
      \itemize{
        \item{Updated maintainer's email address.}
        }
        }
    \subsection{MINOR CHANGES}{
      \itemize{
        \item{Imports now functions from \pkg{stats} and \pkg{utils}.}
    }
    }
    }


\section{Changes in Version 1.5.1 (2015-03-11)}{
    \subsection{MAJOR CHANGES}{
      \itemize{
        \item{Added vignette.}
        \item{Argument \code{repeated} in  \code{nomLORgee}, \code{ordLORgee}
        and \code{intrinsic.pars} now accepts variables with names that are derivates of the word
        "repeated" (reported by Arianna Cecchetti).}
    }
    }
    \subsection{MINOR CHANGES}{
      \itemize{
        \item{Updated CITATION.}
        \item{Updated documentation.}
        }
        }
        }


\section{Changes in Version 1.5.0 (2014-11-02)}{
    \subsection{MINOR CHANGES}{
      \itemize{
        \item{Corrected NEWS.}
        \item{Introduced a x.y.z versioning system.}
        \item{Updated CITATION.}
        \item{Updated NAMESPACE.}
        }
        }
        }


\section{Changes in Version 1.4 (2013-12-03)}{
    \subsection{MAJOR CHANGES}{
      \itemize{
        \item{Made optional the argument \code{repeated} in \code{nomLORgee}, \code{ordLORgee} and \code{intrinsic.pars}.}
        \item{Modified the arguments in \code{intrinsic.pars}.}
        \item{Updated maintainer's email address.}
        }
        }
    \subsection{MINOR CHANGES}{
      \itemize{
        \item{Updated CITATION.}
        \item{Updated references in the documentation.}
        }
        }
        }


\section{Changes in Version 1.3 (2013-06-21)}{
    \subsection{MINOR CHANGES}{
      \itemize{
        \item{Changed licence to GLP-2 | GLP-3.}
        }
        }
        }


\section{Changes in Version 1.2 (2013-05-30)}{
    \subsection{NEW FEATURES}{
       \itemize{
         \item{Added \code{waldts} function for testing goodness of fit between nested GEE models.}
         }
         }
    \subsection{MAJOR CHANGES}{
      \itemize{
        \item{Arguments \code{id} and repeated in \code{nomLORgee} and \code{ordLORgee} should not be provided in character string.}
        \item{Made optional the argument \code{data} in \code{nomLORgee} and \code{ordLORgee}.}
        }
        }
    \subsection{MINOR CHANGES}{
      \itemize{
        \item{Created CITATION.}
        \item{Minor changes in the class \code{"LORgee"}.}
        \item{Updated documentation.}
        }
        }
        }


\section{Changes in Version 1.1 (2012-11-07)}{
    \subsection{MAJOR CHANGES}{
      \itemize{
        \item{Added an R(>=2.15.0) dependency.}
        \item{Fixed contrasts in \code{nomLORgee} and \code{ordLORgee}.}
        }
        }
    \subsection{MINOR CHANGES}{
      \itemize{
        \item{Added information regarding the \code{"LORgee"} class.}
        \item{Minor changes in \code{fitLORgee}.}
        }
        }
        }

\section{Version 1.0 (2012-11-07)}{
  \itemize{
    \item{The first version of \pkg{multgee} is released on CRAN.}
    }
    }
    