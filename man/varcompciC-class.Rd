\name{varcompciC-class}
\Rdversion{1.1}
\docType{class}
\alias{varcompciC-class}
\alias{[,varcompciC,character,missing,missing-method}
\alias{show,varcompciC-method}
\alias{summary.varcompciC}

\title{Class "varcompciC"}
\description{
 	\code{varcompciC} is an object containing information for mixed models (maximum 5 factors). 
   You can achive an object of varcompciC using the method \code{\link{varcompci}}
} 	
\section{Slots}{
  \describe{
    \item{\code{EMS}:}{Object of class \code{["matrix"]}: Contains the Expected Mean Square in pretty format. }
    \item{\code{ANOVA}:}{Object of class \code{["ANY"]} : Contains the ANOVA of a mixed model like SAS type III ANOVA (PROC MIXED).}
    \item{\code{Meansq}:}{Object of class \code{["matrix"]} : Contains the Means Squares of all the factors(random or fixe). }
    \item{\code{variance}:}{Object of class \code{["matrix"]} : Contains the variance estimation of the random effects. }
    \item{\code{aic}:}{Object of class \code{["numeric"]} : Contains the aic of the model.}
    \item{\code{bic}:}{Object of class \code{["numeric"]} : Contains the bic of the model.}
    \item{\code{CI}:}{Object of class \code{["ANY"]}: Contains the Confidence Interval of the variance estimation from the random effects.  }
  }
}
       
\references{
VARCOMPCI: A Package for Computation of Confidence Intervals for Variance Components of Mixed Models in R and SAS 
statistical software.
}
\author{
Civit, S., Vilardell, M., Hess, A., Matthew, Z., Ge, Y., Caballe, A.
}
\seealso{
  Classes : \code{\linkS4class{EMSc}} \cr
  Methods : \code{\link{varcompci}}\cr 
}
\examples{
showClass("varcompciC")
}
\keyword{classes}
