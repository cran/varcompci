
 setClass("EMSc",representation(
                 EMSpretty="matrix",
                 result_EMS= "matrix",
                 namesdesc="matrix",
                 result_EMSlF="matrix",
                 final_EMS="matrix"
                  ))
 
 setMethod(
	f="[",
	signature=c("EMSc","character","missing","missing"),
	def = function(x,i,j,drop){
		switch(EXP=i,
			EMSpretty = return(x@EMSpretty ),
			result_EMS = return(x@result_EMS),
			namesdesc = return(x@namesdesc),
			result_EMSlF = return(x@result_EMSlF),
			final_EMS = return(x@final_EMS),
			stop("Error:",i,"is not a EMSc slot")
		)
	}
)
 
 .EMSc.show=function(object){
  cat("Expected Mean Square in nice format\n")
  print(object["EMSpretty"])
 }
 
 setMethod(f="show",signature="EMSc",definition=.EMSc.show)
 
 prettyEMSf<-function(totvar,Matrix,dsn,...){
   nv<-length(totvar)
   possibilities=infoNLST(totvar,nv,Matrix,dsn)
   countper=combinposs(nv)
   EMSmat=EMSmatrix(possibilities,Matrix,nv,countper,totvar)
   matrix_EMS=EMSmat[[1]]
   subscripfact=EMSmat[[2]]
   typefact=EMSmat[[3]]
	 matrixnameslnw=EMSmat[[4]]
	 # number of columns
	 nvari=nrow(matrix_EMS)
	 # Expected Mean Square ("99999" is SAS Q(Vx))
	 rEMS=EMS(subscripfact,typefact,nvari,matrix_EMS,matrixnameslnw,nv)
	 object= new(Class="EMSc")
   object@result_EMS=rEMS[[1]]
	 object@namesdesc=rEMS[[2]]
   object@EMSpretty=EMSwdesc(object@result_EMS,object@namesdesc,nvari,matrixnameslnw)
   object@result_EMSlF=as.matrix(EMSlF(object@result_EMS,typefact))
   object@final_EMS=last_EMS(nvari,object@result_EMS)
  return(object)
 }   
 
