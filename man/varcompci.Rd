\name{varcompci}
\alias{varcompci}

\title{
Confidence Intervals for Variance Components of Mixed Models
}
\description{
This R package would fit a saturated ANOVA model to
the data and provide an ANOVA table as well as estimates and
confidence intervals for the variance components for any
balanced and unbalanced mixed effects model involving five or
fewer factors.
}
\usage{
varcompci(dsn, response, totvar, Matrix, alpha, report = FALSE, sasnames = FALSE, ...)
}

\arguments{
  \item{dsn}{
  Data Set Name (REQUIRED).
}
  \item{response}{
Name of the \code{response} variable (REQUIRED)
}
  \item{totvar}{
Build confidence intervals on individual components of variance, according to Burdick and Graybill (1992) methodology.
}
 \item{Matrix}{
 Matrix that indicate the crossing(0) and nesting(1) relationships between the independent variables and whether
 each variable is fixed(0) or random(1) (REQUIRED).
}
  \item{alpha}{
\code{alpha=0.025} (default), gives a confidence interval at the 95\% confidence level.
}
  \item{report}{
\code{report= FALSE} (default). 
If your computer have installed \emph{LaTeX}, the option \code{(report=TRUE)} export the results into PDF format.
}
\item{sasnames}{
\code{sasnames= FALSE} (default).
This option is about NESTED DESIGNS notation. Suppose a two-way ANOVA with factor B nested within
factor A, the option \code{sasnames=TRUE}, set up the ANOVA table with \code{B(A)} in the design notation.
}
  \item{\dots}{
 currently not used.
}
}
\details{
Users must pay close attention to \code{Matrix argument}. 
This matrix is a data set (in matrix form) of 0's and 1's that indicates the crossing and 
nesting relationships between the independent variables and whether each variable is \code{fixed}
or \code{random}. 

This data set should correspond to a square matrix whose dimension is the number of 
independent variables, and the rows and columns should be ordered according to order 
used in data set.

In order to indicate that variable \code{i} is \code{fixed}, the [i,i] entry of the matrix should be 0.

In order to indicate that variable \code{j} is \code{random}, the [j,j] entry of the matrix should be 1.

If variable \code{j} is \code{nested} in variable \code{i}, then the [i,j] entry of the matrix should be 1. 
Note that the [j,i] entry should remain as a 0.

If variables \code{i} and \code{j} are \code{crossed}, then [i,j] entry (and the [j,i] entry) of the matrix should
be 0.
}
\value{
 Returns a class object that contains 7 slots \code{\linkS4class{varcompciC}}. 
  They are:\cr
  -EMS, the matrix of Expected Mean Square in a nice format for interpretation.\cr
  -ANOVA, the Anova table of a mixed model like SAS type III ANOVA (PROC MIXED).\cr
  -Meansq, a matrix that contains the Means Squares of all the factors (random or fixed).\cr
  -variance, contains the variance estimation of the random effects.\cr
  -aic, numeric value with the corresponding AIC (Akaike information criterion).\cr
  -bic, a numeric value, just a variation of AIC.\cr
  -CI, the confidence interval of the variance estimation from the random effects according (Burdick and Graybill, 1992).\cr   
}
\references{
VARCOMPCI: A Package for Computation of Confidence Intervals for Variance Components of Mixed Models in R and SAS 
statistical software
}

\author{
Civit, S., Vilardell, M., Hess, A., Matthew, Z., Ge, Y., Caballe, A.
}

\seealso{
Classes : \code{\linkS4class{varcompciC}} \cr
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

x<-varcompci(dsn=dsn,response=response, totvar=totvar, Matrix=Matrix) 
#Applying the varcompci function to get an ANOVA for the nested model;
summary(x)
}