\name{dataAKT_I}
\alias{dataAKT_I}
\docType{data}
\title{
Gene Response Characterization of AKT inhibitors
}
\description{
The report shows the results from two experiments (GSE18232, GSE18005) that could be used to
determine the gene response to AKT inhibitors; data set taken from Krech et al. (2010).
}
\usage{data(dataAKT_I)}
\format{
 This data frame contains:
\emph{kth} measurement of the \emph{gth} gene expression of \emph{jth} chemical compound of
the \emph{ith} group evaluated in the \emph{k-th} cell line. In other words,
5 chemical compounds which act as AKT ihibitors (group 1) were tested versus 4 chemical 
compounds which are not characterized as a direct AKT1 inhibitors (group 2) over three 
different cell lines (HCT116, HT29 and SW480).
It is assumed that the \emph{ith} group evaluated is a fixed effect, whileas the effect of the 
\emph{jth} compound within the \emph{ith} group is a random effect.
The data were evaluated by analysis of variance (ANOVA) based on a nested model.
}
\source{
Krech T, Thiede M, Hilgenberg E, Schadfer R, Jaijrchott K. (2010). Characterization of AKT independent effects of the 
synthetic AKT inhibitors SH-5 and SH-6 using an integrated approach combining transcriptomic profiling and signaling 
pathway perturbations. \emph{BMC Cancer. Jun 14;10:287}.
}
\references{
VARCOMPCI: A Package for Computation of Confidence Intervals for Variance Components of Mixed Models in R.
}
