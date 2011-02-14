initialize=function(dsn,response,totvar,Matrix,alpha){
               	# check to see if the name of of the input SAS data set has been specified.
	# If not, end the macro.
		if (missing(dsn)){
			stop("not specificate a data set name.This macro will now end")
    }
    
	# check to see if the name of the response variable has been specified.
 	# If not, end the macro.
		if (missing(response)){
			stop("not specificate a response name. This macro will now end")
 		}

	# Count and create macro variables for the classification variable(s)
      # in the model.  At least one is required.
    if (!missing(totvar)){
     if (!is.character(totvar)) 	stop("totvar must be a character vector.This macro will now end")
     else{
     nv=0
		 varlist=totvar
		 while ( nv<length(varlist)){
			nv=eval(nv+1)
			assign(sprintf('cv%d', nv), varlist[nv])
     }
		 }
		}
		else{
			stop("Not variables in data set. This macro will now end")
		}

	# Check to see if the statistical significance level has been specified.
  	# If not, use 0.05 as this level.
	 	if (missing(alpha)){
		 alpha=0.025
		}
		if (!is.numeric(alpha)) 	stop("alpha must be a number.This macro will now end")
		if ((alpha < 0) || (alpha>1)){
			stop("Is not betwen 0 and 1. This macro will now end")
		}

    if (!is.character(dsn)) 	stop("dsn is not a data set name (must be a character).This macro will now end")
    if (!is.character(response)) 	stop("response is not a character name(must be a character).This macro will now end")


	# check to see if the nesteds in the Matrix are corrects.
	# If not, end the macro;
   if(!missing(Matrix)){
            if (!is.matrix(Matrix)) 	stop("Matrix must be a matrix.This macro will now end")
            if (ncol(Matrix)!=nrow(Matrix)) stop(" Matrix must be quadratic")
                  	   apply(as.matrix(1:nv),1,function(j){
                  		  if (Matrix[j,j]==1){
                  		   apply(as.matrix(1:nv),1,function(i){
                  		    if((i!=j)&&(Matrix[i,i]==0)&&(Matrix[j,i]==1)){
                  			   stop("The nested matrix are not correct. This macro will now end")
                  		    }
                  	    })
                       }
                      })
                     }
   else stop("The nested matrix are not indicate. This macro will now end")
                                        return(alpha)           }
       
                                        

# make a variable factor
  factorsf=function(dsn,totvar,nv){
	assign(paste(dsn[1]),within(eval(parse(text=dsn[1])),{
					for (i in 1:nv){
					 assign(totvar[i],factor(eval(parse(text=totvar[i]))))
					}
				   }
			   )
	)
	assign(paste(dsn[1]),eval(parse(text=dsn[1]))[,-ncol(eval(parse(text=dsn[1])))])
	return(eval(parse(text=dsn[1])))
  }

# effect type
effectype=function(Matrix,nv){
 type=array('',dim=(nv+1))
 i=1
 while ((i-1)<nv){
	for (j in 1:nv){
		if(Matrix[i,i]==1){
			type[i]='R'}

		if(Matrix[i,i]==0){
			type[i]='F'}	 }
	i=i+1
		   }
  type[i]='R' ##subscript for error
  return(type)
}


#subscrips
subscriptss=function(nv){
 word=c('i','j','l','m','n')
 subscripts=array('',dim=(nv+1))
 i=1
 while ((i-1)<nv){
	subscripts[i]=word[i]
		i=i+1
	}
 subscripts[i]='k'
 return(subscripts)
 }


#levels
levelsf=function(dsn,nv,totvar,...){
 assign(dsn[1],factorsf(dsn,totvar,nv))
 i=1
 level=array(0,dim=(nv+1))
 while (i<(nv+1)){
	level[i]=length(levels(with(eval(parse(text=dsn[1])),eval(parse(text=totvar[i])))))
	i=i+1
		     }
 j=1
 multilevel=1
 while(j<(nv+1)){
  multilevel= multilevel*level[j]
  j=j+1
 }
 level[i]=nrow(eval(parse(text=dsn[1])))/multilevel
 return(level)
}


#levels,names,subscripts and type

infoNLST=function(totvar,nv,Matrix,dsn,...){
 possibilities=array('',dim=c(4,(nv+1)))

 #effect types
 type=effectype(Matrix,nv)

 #subcripts
 subscripts=subscriptss(nv)

 #levels
 level=levelsf(dsn,nv,totvar)

 for (i in 1:nv){
	possibilities[1,i]=totvar[i]
	possibilities[2,i]=subscripts[i]
	possibilities[3,i]=level[i]
	possibilities[4,i]=type[i]
 }
 possibilities[1,i+1]= 'Resid'
 possibilities[2,i+1]=subscripts[i+1]
 possibilities[3,i+1]=level[i+1]
 possibilities[4,i+1]=type[i+1]

 return(possibilities)
}


#combinations
combinposs=function(nv){
 countper=0
 for(i in 1:nv){
	countper=countper+factorial(nv)/(factorial(i)*factorial(nv-i))
 }
 return(countper)
}


########### compute matrix_EMS, types factors, subscripts and nouns

EMSmatrix=function(possibilities,Matrix,nv,countper,totvar,...){
 matrix_EMS=matrix("",ncol=nv+1,nrow=countper+1)
 subscripfact=matrix('',nrow=(countper+1),ncol=nv)
 typefact=matrix('',nrow=(countper+1),ncol=1)
 matrixnames=matrix("",nrow=countper+1)
 EMSmat=list(matrix_EMS,subscripfact,typefact,matrixnames)

 # add Random or Fixe in matrix_EMS
	i=1
	while (i<(nv+1)){
			matrix_EMS=addRoFinmEMS(i,possibilities,matrix_EMS)
			subscripfact[i,i]=possibilities[2,i]
			typefact[i]=possibilities[4,i]
			i=i+1
      }

 #add levels in EMS
  matrix_EMS=addlev1inmEMS(nv,possibilities,matrix_EMS)

 # row names
  matrixnames[1:nv]=possibilities[1,1:nv]

 #interaccions(2)
  ini=nv
  if (nv>1){
   k=2
   s=1
   inter=2
   fin=nv+factorial(nv)/(factorial(inter)*factorial(nv-inter))
   for (i in (ini+1):fin){
	matrix_EMS=addRoF2inmEMS(i,nv,s,k,possibilities,matrix_EMS)
	subscripfact[i,s]=possibilities[2,s]
	subscripfact[i,k]=possibilities[2,k]
	if((possibilities[4,s]=='F')&&(possibilities[4,k]=='F')){
		typefact[i]='F'
	}
	else{typefact[i]='R'}
	if (k<nv){k=k+1}
      else {s=s+1;k=s+1}
   }
  matrix_EMS=addlevinmEMS(nv,ini,fin,possibilities,matrix_EMS)

 # name of rows
  matrixnames=names2(matrixnames,nv,possibilities,fin)
 }

 #interaccions (3)
  if (nv>2){
   q=3
   k=2
   s=1
   inter=3
   ini= fin
   fin= ini+factorial(nv)/(factorial(inter)*factorial(nv-inter))
   for (i in (ini+1):fin){
	matrix_EMS=addRoF3inmEMS(i,s,k,q,nv,possibilities,matrix_EMS)
	subscripfact[i,s]=possibilities[2,s]
	subscripfact[i,k]=possibilities[2,k]
	subscripfact[i,q]=possibilities[2,q]
	if((possibilities[4,s]=='F')&&(possibilities[4,k]=='F')&&(possibilities[4,q]=='F')){
		typefact[i]='F'
      }
	else{typefact[i]='R'}
	if (q<nv){q=q+1}
	else {
	  if(s+2<nv){
		if(k+1<nv){k=k+1;q=k+1}
		else {s=s+1;k=s+1;q=k+1}
 	  }
	}
   }
   matrix_EMS=addlevinmEMS(nv,ini,fin,possibilities,matrix_EMS)

  # name of rows
 	matrixnames=names3(matrixnames,ini,fin,nv,possibilities)
  }

 #interaccions (4)
  if (nv>3){
   t=4
   q=3
   k=2
   s=1
   inter=4
   ini= fin
   fin= ini+factorial(nv)/(factorial(inter)*factorial(nv-inter))
   for (i in (ini+1):fin){
	matrix_EMS=addRoF4inmEMS(i,s,k,q,t,nv,possibilities,matrix_EMS)
	subscripfact[i,s]=possibilities[2,s]
	subscripfact[i,k]=possibilities[2,k]
	subscripfact[i,q]=possibilities[2,q]
	subscripfact[i,t]=possibilities[2,t]
	if((possibilities[4,s]=='F')&&(possibilities[4,k]=='F')&&(possibilities[4,q]=='F')&&(possibilities[4,t]=='F')){
		typefact[i]='F'
      }
	else{typefact[i]='R'}
	if (t<nv){t=t+1}
	else {
	  if(s+3<nv){
	   if(k+2<nv){
		if(q+1<nv){q=q+1;t=q+1}
		else {k=k+1;q=k+1;t=q+1}
	   }
	   else{s=s+1;k=s+1;q=k+1;t=q+1}
        }
	    }
      }
	matrix_EMS=addlevinmEMS(nv,ini,fin,possibilities,matrix_EMS)

  # name of rows
	matrixnames=names4(matrixnames,ini,fin,nv,possibilities)
  }

 #interaccions (5)
  if (nv>4){
  p=5
  t=4
  q=3
  k=2
  s=1
  inter=5
  ini= fin
  fin= ini+factorial(nv)/(factorial(inter)*factorial(nv-inter))
  for (i in (ini+1):fin){
   for (j in 1:nv){
	if (possibilities[4,j]=='R'){
   	 matrix_EMS[i,j]= 1
      }
      else{ matrix_EMS[i,j]= 0}
   }
   subscripfact[i,1]=possibilities[2,s]
   subscripfact[i,2]=possibilities[2,k]
   subscripfact[i,3]=possibilities[2,q]
   subscripfact[i,4]=possibilities[2,t]
   subscripfact[i,5]=possibilities[2,p]
   if((possibilities[4,s]=='F')&&(possibilities[4,k]=='F')&&(possibilities[4,q]=='F')&&(possibilities[4,t]=='F')&&(possibilities[4,p]=='F')){
		typefact[i]='F'
   }
   else{typefact[i]='R'}
  }

 # name of rows
  i=1
  k=ini+1
  t=i+2
  j=i+1
  l=i+3
  h=i+4
  matrixnames[k]=paste(paste(possibilities[1,i]),paste(possibilities[1,j]),paste(possibilities[1,t]),paste(possibilities[1,l]),paste(possibilities[1,h]),sep=":")
 }

 # cas error
  for (j in 1:(nv+1)){
   matrix_EMS[(fin+1),j]=1
  }

  for (i in 1:countper){
   matrix_EMS[i,(nv+1)]=possibilities[3,(nv+1)]
  }

  for (j in 1:nv){
   subscripfact[fin+1,j]=possibilities[2,j]
  }
  typefact[countper+1]='R'

 # rows name
   matrixnames[countper+1]="resid"
   rownames(typefact)=matrixnames
   rownames(subscripfact)=matrixnames

 # final matrix
  matrix_EMS=apply(matrix_EMS, 2, as.numeric)
  rownames(matrix_EMS)=matrixnames

 # chose only the needed
  EMSout=matrix("",nrow=countper)
  EMSout=selectEMSout(Matrix,countper,totvar,nv)

 # indica les rows to delete
  rowstodelete=deletrows(EMSout,countper,matrixnames)

 #delete rows from typefact,indextype and matrix_EMS
  matrixnameslnw=matrixnames
  if(length(rowstodelete)>0){
   for (i in length(rowstodelete):1){
	matrixnameslnw=matrixnameslnw[-rowstodelete[i]]
  	typefact=typefact[-rowstodelete[i]]
	subscripfact=subscripfact[-rowstodelete[i],]
	matrix_EMS=matrix_EMS[-rowstodelete[i],]
   }
   matrixnameslnw=t(t(matrixnameslnw))
   typefact=t(t(typefact))
   rownames(typefact)=matrixnameslnw
  }
  EMSmat=list(matrix_EMS,subscripfact,typefact,matrixnameslnw)
  return(EMSmat)
 }

 # add Random or Fixe in EMS
  addRoFinmEMS=function(i,possibilities,matrix_EMS){
		if (possibilities[4,i]=='R'){
			matrix_EMS[i,i]=1
	      }
		if (possibilities[4,i]== 'F'){
			matrix_EMS[i,i]=0
	      }
   return(matrix_EMS)
  }

 # add levels in EMS
  addlev1inmEMS=function(nv,possibilities,matrix_EMS){
   i=1
   j=1
   for (i in 1:nv){
	for (j in 1:nv){
		if (possibilities[2,i]!=possibilities[2,j]){
			matrix_EMS[j,i]= possibilities[3,i]
		}
      }
   }
   return(matrix_EMS)
  }


 # matrix_EMS (interaccions 2)
 addRoF2inmEMS=function(i,nv,s,k,possibilities,matrix_EMS){
  for (j in 1:nv) {
	 for (l in 1:nv) {
       	  if (j!=l) {
		   if((possibilities[2,s]==possibilities[2,j])&
	   	    (possibilities[2,k]==possibilities[2,l])){
			if (possibilities[4,j]=='R'){
			 matrix_EMS[i,j]= 1
  			}
		      else{ matrix_EMS[i,j]= 0}
			if (possibilities[4,l]=='R'){
			 matrix_EMS[i,l]= 1
  			}
		      else{ matrix_EMS[i,l]= 0}
		   }
		  }
	 }
   }
   return(matrix_EMS)
 }


 addlevinmEMS=function(nv,ini,fin,possibilities,matrix_EMS){
  for (i in (ini+1):fin){
	for (j in 1:nv) {
	 if (matrix_EMS[i,j]==''){
		 matrix_EMS[i,j]=possibilities[3,j]
	 }
	}
  }
  return(matrix_EMS)
 }


 # name of rows(2)
  names2=function(matrixnames,nv,possibilities,fin){
   i=1
   k=nv+1
   while (k<fin+1){
    for (j in (i+1):nv){
     matrixnames[k]=paste(paste(possibilities[1,i]),paste(possibilities[1,j]),sep=":")
     k=k+1
    }
    i=i+1
   }
   return(matrixnames)
  }


 # matrix_EMS (interaccions 3)
  addRoF3inmEMS=function(i,s,k,q,nv,possibilities,matrix_EMS){
	for (j in 1:nv){
	 for (l in 1:nv){
	  for (m in 1:nv){
	   if ((j!=l)&&(j!=m)&&(l!=m)){
	    if((possibilities[2,s]==possibilities[2,j])&&
	   	 (possibilities[2,k]==possibilities[2,l])&&
		 (possibilities[2,q]==possibilities[2,m])){
			if (possibilities[4,j]=='R'){
			 matrix_EMS[i,j]= 1
 			}
		      else{ matrix_EMS[i,j]= 0}
			if (possibilities[4,l]=='R'){
			 matrix_EMS[i,l]= 1
			}
		      else{ matrix_EMS[i,l]= 0}
			if (possibilities[4,m]=='R'){
			 matrix_EMS[i,m]= 1
			}
		      else{ matrix_EMS[i,m]= 0}
	    }
	   }
        }
	 }
	}
	return(matrix_EMS)
    }


 ## name of rows(3)
  names3=function(matrixnames,ini,fin,nv,possibilities){
  i=1
  k=ini+1
  t=i+2
  j=i+1
  while (k<fin+1){
    for (i in 1:(nv-2)){
     for (j in (i+1):(nv-1)){
      for (t in (j+1):nv){
	 matrixnames[k]=paste(paste(possibilities[1,i]),paste(possibilities[1,j]),paste(possibilities[1,t]),sep=":")
	 	k=k+1
      }
     }
    }
   }
   return(matrixnames)
  }


 # matrix_EMS (interaccions 4)
  addRoF4inmEMS=function(i,s,k,q,t,nv,possibilities,matrix_EMS){
   for (j in 1:nv){
    for (l in 1:nv){
     for (m in 1:nv){
      for (n in 1:nv){
       if ((j!=l)&&(j!=m)&&(j!=n)&&(l!=m)&&(l!=n)&&(m!=n)){
        if((possibilities[2,s]==possibilities[2,j])&&
	   	  (possibilities[2,k]==possibilities[2,l])&&
		  (possibilities[2,q]==possibilities[2,m])&&
		  (possibilities[2,t]==possibilities[2,n])){

    			if (possibilities[4,j]=='R'){
			 matrix_EMS[i,j]= 1
 			}
		      else{ matrix_EMS[i,j]= 0}
			if (possibilities[4,l]=='R'){
			 matrix_EMS[i,l]= 1
			}
		      else{ matrix_EMS[i,l]= 0}
			if (possibilities[4,m]=='R'){
			 matrix_EMS[i,m]= 1
			}
		      else{ matrix_EMS[i,m]= 0}
			if (possibilities[4,n]=='R'){
			 matrix_EMS[i,n]= 1
			}
		      else{ matrix_EMS[i,n]= 0}

	   }
	  }
	 }
	}
     }
    }
    return(matrix_EMS)
   }


 ## name of rows(4)
  names4=function(matrixnames,ini,fin,nv,possibilities){
  i=1
  k=ini+1
  t=i+2
  j=i+1
  l=i+3
  while (k<fin+1){
   for (i in 1:(nv-3)){
    for (j in (i+1):(nv-2)){
     for (t in (j+1):(nv-1)){
      for (l in (t+1):nv){
	 matrixnames[k]=paste(paste(possibilities[1,i]),paste(possibilities[1,j]),paste(possibilities[1,t]),paste(possibilities[1,l]),sep=":")
	 	k=k+1
      }
     }
    }
   }
  }
  return(matrixnames)
  }


 ### chose only the needed
  selectEMSout=function(Matrix,countper,totvar,nv){
   k=1
   q=1
  EMSout=matrix("",nrow=countper)
  for (i in 1:nv){
   for (j in 1:nv){
    if (i!= j){
     if (Matrix[i,j]==1){
      out=FALSE
      for (r in 1:countper){
	 if (EMSout[r]==totvar[j]){
	  out=TRUE
       }
	}
      if (!out){
       EMSout[k]=totvar[j]
	 k=k+1
      }
      if (nv>2){
       aux=1
       while( aux<(nv+1)){
       out=FALSE
       for (r in 1:countper){
	  if ((aux!=j)&&(aux!=i)){
	   expreaux=paste(totvar[min(j,aux)],totvar[max(aux,j)],sep=":")
	   if (EMSout[r]==expreaux){
		out=TRUE
         }
        }
	 }
	 if (!out){
	  if ((aux!=j)&&(aux!=i)){
	   EMSout[k]= paste(totvar[min(j,aux)],totvar[max(aux,j)],sep=":")
	    k=k+1
	  }
	 }
	 aux=aux+1
	}
     }
     if(nv>3){
	for (aux2 in 1:nv){
	 for (aux22 in 1:nv){
	  if ( (aux2!=j) && (aux22!=j)&&(aux2!=i) && (aux22!=i)&& (aux2!=aux22) ){
	    out=FALSE
	    expreaux=paste(totvar[min(j,aux2,aux22)],totvar[median(c(j,aux2,aux22))],totvar[max(j,aux2,aux22)],sep=":")
	    for (r in 1:countper){
	     	if (EMSout[r]==expreaux){
		 out=TRUE
            }
          }
	    if (!out){
	 	 EMSout[k]=expreaux
 		 k=k+1
  	    }
	  }
	 }
	}
     }

     if(nv>4){
	for (aux3 in 1:nv){
	 for (aux33 in 1:nv){
	  for (aux333 in 1:nv){
	   if ( (aux3!=j)&&(aux33!=j)&&(aux3!=i)&&(aux33!=i)&&(aux3!=aux33)&&
              (aux333!=j)&&(aux333!=i)&&(aux3!=aux333)&&(aux33!=aux333) ){
		 	out=FALSE
	    		for (r in 1:countper){
			 sort_aux=c(j,aux3,aux33,aux333)
                   sort_aux=sort(sort_aux)
	     		 expreaux=paste(totvar[sort_aux[1]],totvar[sort_aux[2]],totvar[sort_aux[3]],totvar[sort_aux[4]],sep=":")
		        if (EMSout[r]==expreaux){
		 	  out=TRUE
			  }
			 }
	    		 if (!out){
	 	 	  EMSout[k]=expreaux
 			  k=k+1
  			 }
	   }
	  }
	 }
	}
     }
    }
   }
  }
 }

  # delete white
   i=countper
   while (i > 0){
    if (EMSout[i]==""){
     EMSout=EMSout[-i]
    }
    i=i-1
   }
  return(EMSout)
  }


 # select rows to delete
  deletrows=function(EMSout,countper,matrixnames){
  rowstodelete=array(0,dim=length(EMSout))
  if (length(EMSout)>0){
   j=1
   for (i in 1:length(EMSout)){
    for (k in 1:countper){
     if(EMSout[i]==matrixnames[k]){
      rowstodelete[j]=k; j=j+1
     }
    }
   }
   rowstodelete=sort(rowstodelete)
  }
  return(rowstodelete)
  }


 # Result EMS
EMS=function(subscripfact,typefact,nvari,matrix_EMS,matrixnameslnw,nv){
   result_EMS=matrix(0,nrow=nvari,ncol=nvari)
   namesdesc=matrix("",nrow=nvari,ncol=nvari)
  
   for (i in 1:(nvari-1)){
	for (j in nvari:1)
       result_EMS[i,(nvari-j+1)] = prod(matrix_EMS[j,-which(subscripfact[i,]!="")])*
                             as.numeric(all(subscripfact[i,subscripfact[i,]!=""]==
                        subscripfact[j,subscripfact[i,]!=""])) 
   }
    result_EMS[nvari,]=c(1,rep(0,nvari-1))
    
    for (i in 1:nvari){
      if(typefact[i]=="F")
 	  result_EMS[i,(nvari-i+1)]=99999
    }
    for (i in 1:nvari){
      for (j in 1:nvari){
          if (result_EMS[i,j]!=0)
               (namesdesc[i,j]=matrixnameslnw[(nvari-j+1)])
          else namesdesc[i,j]=""  
    }}
    rownames(result_EMS)=matrixnameslnw
    rEMS=list(result_EMS,namesdesc)
    return(rEMS)
   }



 # EMS less fixe
EMSlF=function(result_EMS,typefact,nvari){
   result_EMSlF=result_EMS[-which(typefact=="F"),-(nvari-which(typefact=="F")+1)]  
   return(result_EMSlF)
 }



 # the last EMS for all effects
  last_EMS=function(nvari,result_EMS){
  
   final_EMS<-numeric(nvari)
   for (i in 1:nvari)
          final_EMS[i] <-result_EMS[i,(nvari-i+1)]
   return(as.matrix(final_EMS))
  }



 # covariance paramaters
  covpar=function(subscripfact,nvari,Meansq,final_EMS,matrixnameslnw,typefact,nv,...){
  varianceRE=matrix(0,nrow=nvari)
  varsxems=matrix(0,nrow=nvari)
  varianceRE[nvari]=Meansq[nvari]
  for (i in (nvari-1):1){
   if(typefact[i]=="R"){
    for (j in (i+1):nvari){
     decident=FALSE
     k=1
     while (k<(nv+1)){
      if ((subscripfact[i,k]==subscripfact[j,k])||(subscripfact[i,k]=='')){
	 k=k+1
	 decidint=TRUE
      }
	else{k=6;decidint=FALSE}
     }
     if (decidint==TRUE){
 	varsxems[i]= varsxems[i]+varianceRE[j]*final_EMS[j]
     }
    }
    varianceRE[i]=(Meansq[i]-varsxems[i])/final_EMS[i]
   }
  }
  rownameslF=matrixnameslnw
  # delete Fixe Rows and count fixed and random effects
   varianceRE=deletfixrow(nvari,typefact,varianceRE)
   rownameslF=deletfixrow(nvari,typefact, rownameslF)

  # final Variance
   rownameslF=t(t(rownameslF))
   varianceRE=t(t(varianceRE))
   rownames(varianceRE)=rownameslF
   colnames(varianceRE)="Covariance paramater"

   return(varianceRE)
  }


 # Fvalues
   fvalues=function(namesdesc,result_EMS,varianceRE,nvari,Meansq,df){
   Errorterm=array(0,dim=c(nvari,1))
   for (i in 1:nvari){
     for (j in 1:nvari){
      if((namesdesc[i,j]!="")&&(i!=nvari-j+1)){
    	   aux=namesdesc[i,j]
	     if(result_EMS[i,j]!=99999)
	       Errorterm[i,1]= Errorterm[i,1]+ result_EMS[i,j]*varianceRE[aux,1]
      }
    }
   }

   Fval=array(0,dim=c(nvari,1))
   Fval=Meansq/Errorterm
   Fval[nvari,1]=0
   Fval=t(t(Fval))
   colnames(Fval)="F-value"
   ddf=matrix(0,ncol=1,nrow=nvari)
   rownames(df)=rownames(Meansq)
   for (i in 1:(nvari-1)){
    
     if ( Errorterm[i]==Meansq[namesdesc[i,(nvari-i+1)],1]){
              ddf[i]=df[namesdesc[i,(nvari-i+1)],1]
      }
      else{
          denom=0
            for (j in 1:nvari){
              if(namesdesc[i,j]!="")
                denom=denom+(Meansq[namesdesc[i,j],1])^2/df[namesdesc[i,j],1]
            }
            ddf[i]=Fval[i,1]*df[i]/(Meansq[i,1])*Errorterm[i]
      }
   }
   pval=matrix(0,ncol=1,nrow=nvari)
   for (i in 1:(nvari-1)) pval[i]=(1-pf(Fval[i],df[i,1],ddf[i]))
   rownames(pval)=rownames(Fval)
   colnames(pval)="P-valors"
   aux=list(pval,Fval)
   return(aux)
  }


  ## si l'error term és el mateix que el mean square de grau superior=> ddf= df del de grau sup
  ## sinó, cas III (suma del Ms de grau sup + suma de segon grau negatius..)^2 /suma de tots partits per els df de cada un .



 #delete fixed rows of matrix
  deletfixrow=function(nvari,typefact,x){
  for (i in (nvari-1):1){
   if (typefact[i]=='F'){
	x=x[-i]
   }
  }
  return(x)
  }


 #count nrm effects
  countnrm=function(nvari,typefact){
  nrm=0
  for (i in (nvari):1){
   if (typefact[i]=='R'){
	nrm=nrm+1
   }
  }
  return(nrm)
  }


 # return anova
  recAnova=function(response,matrixnamessr,dsn,n,totvar,nv,...){
  assign(dsn[1],factorsf(dsn,totvar,nv))
  (formu <- as.formula(paste(paste(response),"~ ", paste(matrixnamessr,collapse="+"))))
  (fm1Y=lm(formu,eval(parse(text=dsn[1]))))
   aic=AIC(fm1Y)
   bic=AIC(fm1Y,k=log(n))
   aov=anova(fm1Y)
   names(aic)="AIC (smaller is better) "
	 names(bic)="BIC (smaller is better) "
	infoaov=list(aov,aic,bic)
   return(infoaov)
  }


 # return anova (partII CI)
  recAnovaII=function(response,matrixnamessr,dsn,totvar,nv,...){
   	dsn=within(dsn,{
  		for (i in 1:nv){
	       assign(totvar[i],factor(eval(parse(text=totvar[i]))))
		}
	     }
	    )

    dsn=dsn[,-ncol(dsn)]
    formu <- as.formula(paste(paste(response),"~ ", paste(matrixnamessr,collapse="+")))
    fm1Y=lm(formu,eval(dsn))
     aov=anova(fm1Y)
     return(aov)
  }


 # return print anova
  printaov=function(aov,Fval,matrixnameslnw,Meansq,nvari,Pval){
   df=t(t(aov$Df))
   SS=t(t(aov$'Sum Sq'))
   Fval["resid",1]<-NA
   Pval["resid",1]<-NA
   anov=matrix(c(df,round(SS,5),Meansq,Fval,Pval),ncol=5,nrow=nvari)
   rownames(anov)=matrixnameslnw
   colnamesanov=c("df","SS","MS","F","Pval")
   colnames(anov)=colnamesanov
   return(anov)
  }


 # return print CI
  printCI=function(Method,LB,varianceRE,UB,source,nrm){
   CI=matrix(c(Method,round(LB,digits=5),round(varianceRE,digits=5),round(UB,digits=5)),ncol=4,nrow=nrm)
   column=c("Method","LB","Estimate","UB")
   colnames(CI)=column
   rownames(CI)=source
   return(CI)
  }


 # return pretty EMS
 EMSwdesc=function(result_EMS,namesdesc,nvari,matrixnameslnw){
   prettyEMS=matrix("",ncol=1,nrow=nvari)
   for (i in 1:(nvari-1)){
    j=1
    aux=paste("var(Resid)")
    for (j in 2:nvari){
      if (i!=(nvari-j+1)){
       if(namesdesc[i,j]!="")
           aux=paste(aux,paste(result_EMS[i,j],"var(",namesdesc[i,j],")",sep=""),sep=" + ")
       }}
     if (result_EMS[i,(nvari-i+1)]==99999)
           prettyEMS[i,1]=paste(aux,paste("Q(",matrixnameslnw[i],")", sep=""),sep=" + ")
       
     if (result_EMS[i,(nvari-i+1)]!=99999)
       prettyEMS[i,1]=paste(aux,paste(result_EMS[i,(nvari-i+1)],"var(",matrixnameslnw[i],")",sep=""),sep=" + ")
    
    }
   prettyEMS[nvari,1]=paste("var(Resid)")
   rownames(prettyEMS)=matrixnameslnw
   colnames(prettyEMS)="EMS"
   return(prettyEMS)
  }


 # prints
  printvariance=function(varianceRE){
    	 cat("\nCovariance Paramater Estimate\n")
	 print(varianceRE)
  }
  printEMS=function(EMSpretty){
	 cat("\nExpected Mean Squaare\n")
	 print(EMSpretty)
  }
  printMeansq=function(Meansq){
	 cat("\nRandom and Fixed Mean Square\n")
	 print(Meansq)
  }
  printanov=function(anov){
	 cat("\nAnalysis of variance\n")
	 print(anov)
   }
   printaic=function(aic,bic){
	 cat("\nFit Statistics\n")
	 print(aic)
	 print(bic)
	  cat("\n")
   }
   printci=function(CI,alpha){
	cat(paste("\nThe following are approximate ",(1-2*alpha),"% confidence interval \n",sep=""))
	print(CI)
    }

