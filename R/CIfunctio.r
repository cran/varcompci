ConfInt=function(aov,nvari,typefact,matrixnameslnw,dsn,n,totvar,nv,matrixnamessr,subscripfact,final_EMS,varianceRE,alpha,Meansq,...){
  # keep source,df and ms
	 df=aov$Df
	 ms=Meansq
	 source=deletfixrow(nvari,typefact,matrixnameslnw)
	 df=deletfixrow(nvari,typefact,df)
	 ms=deletfixrow(nvari,typefact,ms)

	# count nrm effects
	 nrm=countnrm(nvari,typefact)

	# Creating &nrm "Fake" datasets in order to compute the L matrix
	 for (j in 1:nrm){
 	  seed=1525 + j
 	  set.seed(seed)
 	  M_random=matrix(0,nrow=n,ncol=1)
 	  M_random=runif(n)
 	  auxil=cbind(eval(parse(text=dsn)),M_random)
 	  assign(sprintf('random%d',j),cbind(eval(parse(text=dsn)),M_random))

 	  responseaux="M_random"
 	  dsnaux='auxil'
	  aovaux=recAnovaII(responseaux,matrixnamessr,auxil,totvar,nv)
	  Meansqaux=aovaux$'Mean Sq'
 	  Meansqaux2=deletfixrow(nvari,typefact,Meansqaux)
 	  assign(sprintf('ms%d',j),Meansqaux2)
 	  assign(sprintf('cp%d',j),t(covpar(subscripfact,nvari,Meansqaux,final_EMS,matrixnameslnw,typefact,nv)))
	 }

	# Source is the vector of names corresponding to df,ms and cp
 	# achive Bj
	 auxcount=1
	 while ( auxcount<(nrm+1)){
	  assign(sprintf('B%d',auxcount), matrix(-99,nrow=auxcount,ncol=auxcount))
	  aux=matrix(-99,nrow=auxcount,ncol=auxcount)
	  k=1
	  while(k<(auxcount+1)){
	   x=eval(parse(text=sprintf('cp%d',k)))
	   x=x[1,(nrm-auxcount+1):nrm]
	   aux[k,]=x
	   assign(sprintf('B%d',auxcount),aux)
	   k=eval(k+1)
	  }
	  auxcount=auxcount+1
	 }

	# achive Vj
	 auxcount=1
	 while ( auxcount<(nrm+1)){
	  assign(sprintf('V%d',auxcount), matrix(-99,nrow=auxcount,ncol=1))
	  aux=matrix(-99,nrow=auxcount,ncol=1)
	  k=1
	  while(k<(auxcount+1)){
		x=eval(parse(text=sprintf('ms%d',k)))
		x=x[(nrm+1-auxcount)]
		aux[k]=x
	 	assign(sprintf('V%d',auxcount),aux)
		k=k+1
	  }
	  auxcount=auxcount+1
	 }

	# dimension
	 dime=(nrm)*(nrm+1)/2

 	# achive B and V
 	 B=matrix(0,ncol=dime,nrow=dime)
 	 V=matrix(0,ncol=1,nrow=dime)

 	 count=1
	 for (j in nrm:1){
	  auxB=eval(parse(text=sprintf('B%d',j)))
	  auxV=eval(parse(text=sprintf('V%d',j)))
	  B[count:(count+j-1),count:(count+j-1)]=auxB
	  V[count:(count+j-1),1]=auxV
	  count=count+j
	 }
  	 X= solve(B)%*%V

  	 # ones
 	 ones=matrix(1,nrow=nrm,ncol=1)
 	 L=matrix(0,nrow=nrm,ncol=nrm)
 	 count=1
 	 for (j in 1:nrm){
 	  for (k in j:nrm){
   	   L[j,k]=X[count,1]
   	   count=count+1
	  }
	 }
 	 L[,nrm]=ones

	# matrix Linv
 	 Linv=solve(L)

  	# Zeroes
	 Zeroes=array(0,dim=c(nrm,1))
	 for (j in 1:nrm){
 	  if( df[j]==0){
 	  Linv[,j]=Zeroes
	  }
	 }

	 countN=array(-99,dim=c(nrm,1))
	 countP=array(-99,dim=c(nrm,1))
	 for (i in 1:nrm){
 	  sumN=0
 	  sumP=0
 	  for (j in 1:nrm){
  	   if (abs(Linv[i,j])<0.000001){
     		Linv[i,j]=0
	   }
  	   if (abs(L[i,j])<0.000001){
    		L[i,j]=0
	   }
  	   if (Linv[i,j]<0){
    	      sumN=sumN+1
	   }
  	   if (Linv[i,j]>0){
    		sumP=sumP+1
	   }
	  }
     	  countN[i]=sumN
	  countP[i]=sumP
	 }

 	# creating dataset that contains Linv
	 varnames=array('',dim=c(nrm))
	 for (i in 1:nrm){
 	  varnames[i]=sprintf('C%d', i)
	 }
	 Linvmat=Linv
	 colnames(Linvmat)=varnames

	 Sq4=ms^2
	 Cq2=Linv^2
	 Est2=varianceRE^2

 	# method indicates the methode used to compute the CI
	 Method=array("XXXXX",nrm,1)
	 for (i in 1:nrm){
  	  if (countN[i,1]>0){
   	   Method[i]="TBGJL"
	  }
  	  if (countN[i,1]==0){
   	   Method[i]="GWMLS"
	  }
  	  if ((countN[i,1]==0)&&(countP[i]==1)){
   	   Method[i]="Exact"
	  }
  	  if ((countN[i,1]==0)&&(countP[i]==0)){
  	   Method[i]="N/A"
	  }
	 }

	# computing CI using GWMLS and TBGJL method
	 GWL=array(-99,dim=c(nrm,1))
	 GWU=GWL

	 CL1=GWL
	 CU1=GWL

	 CL2=GWL
	 CU2=GWL

	 CL3=GWL
	 CU3=GWL

	 CL4=GWL
	 CU4=GWL

	 for (i in 1:nrm){
 	  sumLA=0
 	  sumUA=0

	  sumLB=0
 	  sumUB=0

 	  sumLC=0
 	  sumUC=0
 	  for (j in 1:nrm){
  	   dfj=df[j]
  	   Sq4j=Sq4[j]
  	   Cq2ij=Cq2[i,j]

   	   if (Linv[i,j]>0){
   	    chil=qchisq((1-alpha),dfj)
   	    sumLA=sumLA+Cq2ij*Sq4j*(1-(1/(chil/dfj)))^2
   	    chiu=qchisq(alpha,dfj)
   	    sumUA=sumUA+Cq2ij*Sq4j*(1/(chiu/dfj)-1)^2

   	    sumLB=sumLB+Cq2ij*Sq4j*(1-(1/(chil/dfj)))^2
   	    sumUB=sumUB+Cq2ij*Sq4j*(1/(chiu/dfj)-1)^2
	   }

    	  if (Linv[i,j]<0){
   	   chil=qchisq(alpha,dfj)
   	   sumLC=sumLC+Cq2ij*Sq4j*(1/(chil/dfj)-1)^2
   	   chiu=qchisq((1-alpha),dfj)
   	   sumUC=sumUC+Cq2ij*Sq4j*(1-(dfj/chiu))^2
	  }
	 }
   	 GWL[i]=sqrt(sumLA)
   	 GWU[i]=sqrt(sumUA)

   	 CL1[i]=sumLB
   	 CU1[i]=sumUB

   	 CL2[i]=sumLC
    	 CU2[i]=sumUC
	}

 	for (k in 1:nrm){
		sumL=0
		sumU=0
		for (q in 1:nrm){
		   dfq=df[q]
		   Linvkq=Linv[k,q]
		   for (r in q:nrm){
			dfr=df[r]
			Linvkr=Linv[k,r]

			if ((Linvkq>0)&&(Linvkr<0)){
				Gq2=(1-(dfq/qchisq(1-alpha,dfq)))^2
				Hr2=((dfr/qchisq(alpha,dfr))-1)^2
				FL=qf(1-alpha,dfq,dfr)
				Gqr=((FL-1)^2-Gq2*FL^2-Hr2)/FL
				sumL=sumL+Gqr*Linvkq*abs(Linvkq)*ms[q]*ms[r]
				Gr2=(1-(dfr/qchisq(1-alpha,dfr)))^2
				Hq2=((dfq/qchisq(alpha,dfq))-1)^2
				FU= qf(alpha,dfq,dfr)
				Hqr=((FU-1)^2-Hq2*FU^2-Gr2)/FU
				sumU=sumU+Hqr*Linvkq*abs(Linvkr)*ms[q]*ms[r]
			}
		   }
		  }
		  CL3[k,1]=sumL
		  CU3[k,1]=sumU
	 }

   	 if (nrm>1){
   	  for ( k in 1:nrm){
	   sumL4=0
	   sumU4=0
	   for (q in 1:(nrm-1)){
	    dfq=df[q]
	    Linvkq=Linv[k,q]
	    for ( t in (q+1):nrm){
	     dft= df[t]
	     dfqpdft=dfq+dft
	     Linvkt=Linv[k,t]

	     if ((Linvkq>0)&& (Linvkt>0)){
		F= (qchisq(1-alpha,dfqpdft))/dfqpdft
		Gq2=(1-(dfq/qchisq(1-alpha,dfq)))^2
		Gt2=(1-(dft/qchisq(1-alpha,dft)))^2
		if (countP[k,1]>1){
		  MultL4=((((1- 1/F)^2)*(dfqpdft)^2)/(dfq*dft)-Gq2*dfq/dft - Gt2*dft/dfq)/(countP[k,1]-1)
		}
		sumL4=sumL4+MultL4*Linvkq*Linvkt*ms[q]*ms[t]
	     }

	     if ((Linvkq<0)&& (Linvkt<0)){
		F= (qchisq(1-alpha,dfqpdft))/dfqpdft
		Gr2=(1-dfq/qchisq(1-alpha,dfq))^2
		Gu2=(1-dft/qchisq(1-alpha,dft))^2
		if (countN[k,1]>1){
		  MultU4=(((1- 1/F)^2)*(dfqpdft)^2/(dfq*dft)-Gr2*dfq/dft - Gu2*dft/dfq)/(countN[k,1]-1)
		}
	      sumU4=sumU4+MultU4*Linvkq*Linvkt*ms[q]*ms[t]
	     }
	    }
	   }
	   CL4[k,1]=sumL4
	   CU4[k,1]=sumU4
	  }

	  VL=CL1+CL2+CL3+CL4
	  VU=CU1+CU2+CU3+CU4
	  SqrtVL=sqrt(VL)
	  SqrtVU=sqrt(VU)}

  	# computes LB and UB based on the Method Used
 	 LB=array(-99,dim=c(nrm,1))
	 UB=LB

 	 for (i in 1:nrm){
  	  Cpi=varianceRE[i]
  	  dfi=df[i]
  	  if (Method[i]=="GWMLS"){
   	   LB[i,1]=Cpi-GWL[i,1]
   	   UB[i,1]=Est2[i,1]+GWU[i,1]
	  }

  	  if (Method[i]=="Exact"){
    	   chi=qchisq(1-alpha,dfi)
    	   LB[i,1]=Cpi/(chi/dfi)
  	   chi=qchisq(alpha,dfi)
  	   UB[i,1]=Cpi/(chi/dfi)
	  }

  	  if (Method[i]=="TBGJL"){
  	   LB[i,1]=Cpi-SqrtVL[i,1]
  	   UB[i,1]=Cpi+SqrtVU[i,1]
	  }
       }

	CI=printCI(Method,LB,varianceRE,UB,source,nrm)
	CI= data.frame(CI)
  return(CI)
  }