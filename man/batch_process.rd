\name{batch_process}
\alias{batch_process}
\docType{data}
\title{
The influence of dry matter content in buttercream
}
\description{
In batch manufacturing processes, the total process variation is generally decomposed into batch by
batch variation and within-batch variation.
The data provides a generic example for decomposition and estimation of each variance 
component essential to the process improvement; data set taken from Ittzes, I. (2001)
}
\usage{data(batch_process)}
\format{
This data frame contain the following variables: \code{Compound}, \code{Batch} and \code{Measurement}.
For this investigation (r=8) compounds, which can be considered as batches, (m=5) samples 
were taken, on which (n=3) parallel measurements were carried out. 
The data were evaluated by analysis of variance (ANOVA) based on this nested model.
In this model \emph{X_ijk} represents the \emph{kth} measurement of the dry matter 
content of \emph{jth} sample in the \emph{ith} buttercream compound. 
}
\source{
I. Ittzes (2001). Statistical Process Control with Several Variance Components In the Dairy Industry. \emph{Food
Control, 12, 119-125}.
}
\references{
VARCOMPCI: A Package for Computation of Confidence Intervals for Variance Components of Mixed Models in R.
}
