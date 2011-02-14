setClass("varcompciC",representation(
                 EMS="matrix",
                 ANOVA= "ANY",
                 Meansq="matrix",
                 variance="matrix",
                 aic="numeric",
                 bic="numeric",
                 CI="ANY" ),
       )
       
.varcompciC.show=function(object){
  cat("Confidence Interval of variance components\n")
  print(object["CI"])
  return(invisible())
 }



setMethod(f="show",signature="varcompciC",definition=.varcompciC.show)


summary.varcompciC <- function(object, ...)
 {
  if(!inherits(object, "varcompciC"))
   stop("'object' must be of class 'varcompciC'")
  
   cat("\nExpected Mean Square\n")
   print(object["EMS"])
   cat("\nAnova of mixed model\n")
	 print(as.matrix(object["ANOVA"]), na.print="", quote=FALSE)
	 cat("\nRandom and Fixed Mean Square\n")
	 print(object["Meansq"])
	 cat("\nCovariance Paramater Estimate\n")
	 print(object["variance"])
	 cat("\nFit Statistics\n")
	 print(object["aic"])
	 cat("\n")
	 print(object["bic"])
	 cat("\nConfidence Interval of variance components\n")
   print(object["CI"])
	 return(invisible())
 }
 
 
setMethod(
	f="[",
	signature=c("varcompciC","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			EMS = return(x@EMS),
			ANOVA = return(x@ANOVA),
			Meansq = return(x@Meansq),
			variance = return(x@variance),
			aic = return(x@aic),
			bic = return(x@bic),
			CI = return(x@CI),
			stop("Error:",i,"is not a varcompciC slot")
		)
	}
)

 
 
 varcompci=function(dsn,response,totvar,Matrix,alpha,report=FALSE,sasnames=FALSE,...){
   alpha=initialize(dsn,response,totvar,Matrix,alpha)
   # number of variables
   nv=length(totvar)
   # number of observation
   n=nrow(eval(parse(text=dsn[1])))
   
 ## Obtain the Type 3 ANOVA statistics and the variance-covariance parameters ##
  # make a variable factor
  assign(dsn[1],factorsf(dsn,totvar,nv))
  possibilities=infoNLST(totvar,nv,Matrix,dsn)
  countper=combinposs(nv)
  EMSmat=EMSmatrix(possibilities,Matrix,nv,countper,totvar)
  matrix_EMS=EMSmat[[1]]
  subscripfact=EMSmat[[2]]
  typefact=EMSmat[[3]]
	matrixnameslnw=EMSmat[[4]]   
	# number of columns
  nvari=nrow(matrix_EMS)
  if (sasnames) sasnouns=sasnoun(matrixnameslnw,totvar,nvari,nv) 
 	# combinations posibles in interaccions of two,three,...,nv
  countper=combinposs(nv)
	# Expected Mean Square 
  ems=prettyEMSf(totvar,Matrix,dsn)
	# MS,Df and variance of Random effects
  matrixnamessr=matrixnameslnw[-length(matrixnameslnw)]
	matrixnamessr=t(t(matrixnamessr))
 	 infoaov=recAnova(response,matrixnamessr,dsn,n,totvar,nv)
	 aov=infoaov[[1]]
	 aic=infoaov[[2]]
	 bic=infoaov[[3]]
 	 Meansq=aov$'Mean Sq'
 	 Meansq=t(t(Meansq))
 	 rownames(Meansq)= matrixnameslnw
 	 colnames(Meansq)= " Mean Sq "
  # Variance of Random Effects ##
   varianceRE=covpar(subscripfact,nvari,Meansq,ems@final_EMS,matrixnameslnw,typefact,nv)
	# Fvalues
 	 FPval=fvalues(ems@namesdesc,ems@result_EMS,varianceRE,nvari,Meansq,t(t(aov$Df)))
	 Fval=FPval[[2]]
	 Pval=FPval[[1]]
  # Print Anova
	 anov=printaov(aov,round(Fval,5),matrixnameslnw,round(Meansq,5),nvari,round(Pval,5))

 ###### CI covariance paramaters #######
 
	 CI=ConfInt(aov,nvari,typefact,matrixnameslnw,dsn,n,totvar,nv,matrixnamessr,subscripfact,ems@final_EMS,varianceRE,alpha,Meansq)
   if (sasnames) {
    rownames(ems@EMSpretty)=sasnouns
    rownames(anov)=sasnouns
    rownames(Meansq)=sasnouns
    rownames(varianceRE)=deletfixrow(nvari,typefact,sasnouns)
    rownames(CI)=deletfixrow(nvari,typefact, sasnouns)
    
   } 
     
     varcompciaux=new(Class="varcompciC",EMS=ems@EMSpretty,ANOVA=anov,Meansq=Meansq,variance=varianceRE,aic=aic,bic=bic,CI=CI)
     if (report) crearInforme(object=varcompciaux)
     return(varcompciaux)
}



