\name{prettyEMSf}
\alias{prettyEMSf}

\title{
  Expected Means Square of mixed model (at most five variables).
}
\description{
  The expected mean squares are the decomposition of the variance in explaining the effects of variances 
  in random effects of higher degree.
  
  That is if we have 3 variable: A, B and C, all of them are random, the first decomposition of the variance according 
  to the EMS would be :
    MS (A) = var (res) + Cj * var (ABC) + Ci * var (AB) + Ck * var (AC) + Ca * var (A)
  
  In this way we can determine the MS of each variable according to the variability explained by all terms that have equal or superior degree 
  from a random effect.

}
\usage{
prettyEMSf(totvar,Matrix, dsn, ...)
}

\arguments{
  \item{totvar}{
  Row vector of factor names (REQUIRED).
}
  \item{Matrix}{
  Matrix that indicate the crossing(0) and nesting(1) relationships between the independent variables and whether
  each variable is fixed(0) or random(1) (REQUIRED).
}
  \item{dsn}{
  Data set name (REQUIRED).
}
  \item{\dots}{
    currently not used.
}
}
\value{
  Returns a class object that contains 5 slots EMSc. 
  They are:\cr
  - PrettyEMS, Matrix of EMS in a nice format for interpretation.\cr
  - EMS, which contains the numerical values of the decomposition of the variance in
   matrix format.\cr
  - Final_EMS, a vector that contain the coefficients associated to the variable to be explained.\cr
  - Result_EMSlF, Matrix of the same format as EMS without the fixed variables.\cr
  - Namesdesc, array containing the names of the  random effects of higher degree. \cr
}
\references{
VARCOMPCI: A Package for Computation of Confidence Intervals for Variance Components of Mixed Models in R.
}
\author{
Civit, S., Vilardell, M., Hess, A., Matthew, Z., Ge, Y., Caballe, A.
}

\seealso{
  Classes : \code{\linkS4class{EMSc}} \cr
}

\examples{
y<-rnorm(12,sd=1) 
X<-data.frame(b=factor(sample(c(1,2), 12, replace=TRUE)), 
	c=factor(sample(c(1,2), 12, replace=TRUE)))
cbind(X,y)
totvar=c("b","c")
Matrix=matrix(cbind(c(1,0),c(1,1)),ncol=2)
response="y"
dsn="X"

ems<-prettyEMSf(totvar,Matrix, dsn,)
show(ems)

}