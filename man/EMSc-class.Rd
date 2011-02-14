\name{EMSc-class}
\Rdversion{1.1}
\docType{class}
\alias{EMSc-class}
\alias{[,EMSc,character,missing,missing-method}
\alias{show,EMSc-method}



\title{Class "EMSc"}
\description{
\code{EMSc} is an object containing information about the expected Means Square for mixed models (maximum 5 factors).
 You can achive an object of EMSc using the method \link{prettyEMSf}
}

\section{Slots}{
  \describe{
    \item{\code{EMSpretty}:}{Object of class \code{["matrix"]} :  Contains the variance descomposition of the factors in a pretty form.  }
    \item{\code{result_EMS}:}{Object of class \code{["matrix"]} : Contains the numerical values of the decomposition of the variance in
   matrix format. }
    \item{\code{namesdesc}:}{Object of class \code{["matrix"]} : Contains the names of the variables that determine the MS of the variable of the row in question. }
    \item{\code{result_EMSlF}:}{Object of class \code{["matrix"]} : Contains the EMS without the fixed variables.}
    \item{\code{final_EMS}:}{Object of class \code{["matrix"]} : Contains the EMS coefficients associated with each of the variables. }
  }
}

\references{
VARCOMPCI: A Package for Computation of Confidence Intervals for Variance Components of Mixed Models in R.
}
\author{
Civit, S., Vilardell, M., Hess, A., Matthew, Z., Ge, Y., Caballe, A.
}
\seealso{
  Classes : \code{\linkS4class{varcompciC}} \cr
  Methods : \code{\link{prettyEMSf}}\cr 
}
\examples{
showClass("EMSc")
}
\keyword{classes}
