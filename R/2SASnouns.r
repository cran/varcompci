sasnoun=function(matrixnameslnw,totvar,nvari,nv,...){
  k=1
  count1=0
  sasnouns=matrixnameslnw
  for (i in 1:length(totvar)){
   aux=paste(totvar[i])
   for (j in 1:length(totvar)){
    if (matrixnameslnw[j]==aux){
      sasnouns[k]=aux
      k=k+1
      count1=count1+1
    }
   }
  }
  count2=count1
  if (nv>1){
   for ( i in 1:(nv-1)){
    for (j in (i+1):nv){
     decident1=FALSE
     decident2=FALSE
     aux=paste(totvar[i],totvar[j],sep=":")
     for (r in (count2+1):nvari){
      if (aux==matrixnameslnw[r]){
       for (s in 1:count1){
         if(sasnouns[s]==totvar[i]){
          decident1=TRUE
         }
         if(sasnouns[s]==totvar[j]){
          decident2=TRUE
         }
       }
      }
     }
      if ((decident1) && (decident2)){
       sasnouns[k]= aux
       k=k+1
       count2=count2+1
      }
      else if (decident1){
       sasnouns[k]= paste(totvar[j],"(",totvar[i],")",sep="")
       k=k+1
       count2=count2+1
      }
      else if (decident2){
       sasnouns[k]= paste(totvar[i],"(",totvar[j],")",sep="")
       k=k+1
       count2=count2+1
      }
    }
   }
  }

   if (nv>2){
   count3=count2
   for ( i in 1:(nv-2)){
    for (j in (i+1):nv-1){
     for (s in (j+1):nv){
     decident1=FALSE
     decident2=FALSE
     decident3=FALSE
     aux=paste(totvar[i],totvar[j],totvar[s],sep=":")
      for (t in (count3+1):nvari){
       if (aux==matrixnameslnw[t]){
        for (d in 1:count1){
         if(sasnouns[d]==totvar[i]){
          decident1=TRUE
         }
         if(sasnouns[d]==totvar[j]){
          decident2=TRUE
         }
         if (sasnouns[d]==totvar[s]){
          decident3=TRUE
         }
       }
      }
     }
     if ((decident1) && (decident2) && (decident3)){
       sasnouns[k]= aux
       k=k+1
       count3=count3+1
      }
      else if ((decident1)&&(decident2)){
        sasnouns=sasauxnouns3b(totvar,count1,count2,s,i,j,matrixnameslnw,sasnouns,k)
        k=k+1
        count3=count3+1
      }
      else if ((decident1)&&(decident3)){
       sasnouns= sasauxnouns3b(totvar,count1,count2,j,i,s,matrixnameslnw,sasnouns,k)
       k=k+1
       count3=count3+1
      }
      else if ((decident2)&&(decident3)){
       sasnouns= sasauxnouns3b(totvar,count1,count2,i,j,s,matrixnameslnw,sasnouns,k)
       k=k+1
       count3=count3+1
      }
      else if (decident1){
        sasnouns=sasauxnouns3(totvar,count1,count2,j,s,i,matrixnameslnw,sasnouns,k)
        k=k+1
        count3=count3+1
      }
      else if (decident2){
        sasnouns=sasauxnouns3(totvar,count1,count2,i,s,j,matrixnameslnw,sasnouns,k)
        k=k+1
        count3=count3+1
      }
      else if (decident3){
        sasnouns=sasauxnouns3(totvar,count1,count2,i,j,s,matrixnameslnw,sasnouns,k)
        k=k+1
        count3=count3+1
      }
     }
    }
   }
  }


   if (nv>3){
   count4=count3
   for ( i in 1:(nv-3)){
    for (j in (i+1):nv-2){
     for (s in (j+1):nv-1){
      for (t in (s+1):nv){
       decident1=FALSE
       decident2=FALSE
       decident3=FALSE
       decident4=FALSE
       aux=paste(totvar[i],totvar[j],totvar[s],totvar[t],sep=":")

        if (any(aux==matrixnameslnw)){
         for (d in 1:count1){
          if(any(sasnouns==totvar[i]))  decident1=TRUE
          if(any(sasnouns==totvar[j]))   decident2=TRUE
          if (any(sasnouns==totvar[s])) decident3=TRUE
          if (any(sasnouns==totvar[t]))  decident4=TRUE
         }
        }
      if ((decident1) && (decident2) && (decident3) && (decident4)){
       sasnouns[k]= aux
       k=k+1
      }
      else if ((decident1)&&(decident2)&&(decident3)){
       sasnouns=sasauxnouns4(totvar,t,i,j,s,matrixnameslnw,sasnouns,k)
       k=k+1
      }
      else if ((decident1)&&(decident2)&&(decident4)){
       sasnouns=sasauxnouns4(totvar,s,i,j,t,matrixnameslnw,sasnouns,k)
       k=k+1
      }
      else if ((decident1)&&(decident3)&&(decident4)){
       sasnouns=sasauxnouns4(totvar,j,i,s,t,matrixnameslnw,sasnouns,k)
       k=k+1
      }
      else if ((decident2)&&(decident3)&&(decident4)){
       sasnouns=sasauxnouns4(totvar,i,j,s,t,matrixnameslnw,sasnouns,k)
       k=k+1
      }
      else if ((decident1)&&(decident2)){
        sasnouns=sasauxnouns4a(totvar,s,t,i,j,matrixnameslnw,sasnouns,k)
        k=k+1
      }
      else if ((decident1)&&(decident3)){
        sasnouns=sasauxnouns4a(totvar,j,t,i,s,matrixnameslnw,sasnouns,k)
        k=k+1
      }
      else if ((decident1)&&(decident4)){
        sasnouns=sasauxnouns4a(totvar,j,s,i,t,matrixnameslnw,sasnouns,k)
        k=k+1
      }
      else if ((decident2)&&(decident3)){
        sasnouns=sasauxnouns4a(totvar,i,t,j,s,matrixnameslnw,sasnouns,k)
        k=k+1
     }
     else if ((decident2)&&(decident4)){
        sasnouns=sasauxnouns4a(totvar,i,s,j,t,matrixnameslnw,sasnouns,k)
        k=k+1
     }
     else if ((decident3)&&(decident4)){
        sasnouns=sasauxnouns4a(totvar,i,j,s,t,matrixnameslnw,sasnouns,k)
        k=k+1
     }
     else if (decident1){
        sasnouns=sasauxnouns4b(totvar,j,s,t,i,matrixnameslnw,sasnouns,k)
        k=k+1
     }
     else if (decident2){
        sasnouns=sasauxnouns4b(totvar,i,s,t,j,matrixnameslnw,sasnouns,k)
        k=k+1
     }
     else if (decident3){
        sasnouns=sasauxnouns4b(totvar,i,j,t,s,matrixnameslnw,sasnouns,k)
        k=k+1
     }
     else if (decident4){
      sasnouns=sasauxnouns4b(totvar,i,j,s,t,matrixnameslnw,sasnouns,k)
      k=k+1
     }
    }
   }
  }
 }
 }

    if (nv>4){
       count5=count4
       decident1=FALSE
       decident2=FALSE
       decident3=FALSE
       decident4=FALSE
       decident5=FALSE
       aux=paste(totvar[1],totvar[2],totvar[3],totvar[4],totvar[5],sep=":")
       if(any(sasnouns==totvar[1]))   decident1=TRUE
       if(any(sasnouns==totvar[2]))  decident2=TRUE
       if(any(sasnouns==totvar[3]))   decident3=TRUE
       if(any(sasnouns==totvar[4]))   decident4=TRUE
       if(any(sasnouns==totvar[5]))   decident5=TRUE
          
     if ((decident1) && (decident2) && (decident3) && (decident4) && (decident5))  sasnouns[k]=aux
     else if ((decident1)&&(decident2)&&(decident3)&&(decident4))  sasnouns=sasauxnouns5(totvar,5,1,2,3,4,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident2)&&(decident3)&&(decident5))  sasnouns=sasauxnouns5(totvar,4,1,2,3,5,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident2)&&(decident4)&&(decident5))  sasnouns=sasauxnouns5(totvar,3,1,2,4,5,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident3)&&(decident4)&&(decident5))  sasnouns=sasauxnouns5(totvar,2,1,3,4,5,matrixnameslnw,sasnouns,k)
     else if ((decident2)&&(decident3)&&(decident4)&&(decident5))  sasnouns=sasauxnouns5(totvar,1,2,3,4,5,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident2)&&(decident3))  sasnouns=sasauxnouns5a(totvar,4,5,1,2,3,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident2)&&(decident4))  sasnouns=sasauxnouns5a(totvar,3,5,1,2,4,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident2)&&(decident5))  sasnouns=sasauxnouns5a(totvar,3,4,1,2,5,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident3)&&(decident4))  sasnouns=sasauxnouns5a(totvar,2,5,1,3,4,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident3)&&(decident5))  sasnouns=sasauxnouns5a(totvar,2,4,1,3,5,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident4)&&(decident5))  sasnouns=sasauxnouns5a(totvar,2,3,1,4,5,matrixnameslnw,sasnouns,k)
     else if ((decident2)&&(decident3)&&(decident4))  sasnouns=sasauxnouns5a(totvar,1,5,2,3,4,matrixnameslnw,sasnouns,k)
     else if ((decident2)&&(decident3)&&(decident5))  sasnouns=sasauxnouns5a(totvar,1,4,2,3,4,matrixnameslnw,sasnouns,k)
     else if ((decident2)&&(decident4)&&(decident5))  sasnouns=sasauxnouns5a(totvar,1,3,2,4,5,matrixnameslnw,sasnouns,k)
     else if ((decident3)&&(decident4)&&(decident5))  sasnouns=sasauxnouns5a(totvar,1,2,3,4,5,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident2))  sasnouns=sasauxnouns5b(totvar,3,4,5,1,2,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident3))  sasnouns=sasauxnouns5b(totvar,2,4,5,1,3,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident4))  sasnouns=sasauxnouns5b(totvar,2,3,5,1,4,matrixnameslnw,sasnouns,k)
     else if ((decident1)&&(decident5))  sasnouns=sasauxnouns5b(totvar,2,3,4,1,5,matrixnameslnw,sasnouns,k)
     else if ((decident2)&&(decident3))  sasnouns=sasauxnouns5b(totvar,1,4,5,2,3,matrixnameslnw,sasnouns,k)
     else if ((decident2)&&(decident4))  sasnouns=sasauxnouns5b(totvar,1,3,5,2,4,matrixnameslnw,sasnouns,k)
     else if ((decident2)&&(decident5))  sasnouns=sasauxnouns5b(totvar,1,3,4,2,5,matrixnameslnw,sasnouns,k)
     else if ((decident3)&&(decident4))  sasnouns=sasauxnouns5b(totvar,1,2,5,3,4,matrixnameslnw,sasnouns,k)
     else if ((decident3)&&(decident5))  sasnouns=sasauxnouns5b(totvar,1,2,4,3,5,matrixnameslnw,sasnouns,k)
     else if ((decident4)&&(decident5))  sasnouns=sasauxnouns5b(totvar,1,2,3,4,5,matrixnameslnw,sasnouns,k)
     else if ((decident1))  sasnouns=sasauxnouns5c(totvar,2,3,4,5,1,matrixnameslnw,sasnouns,k)
     else if ((decident2))  sasnouns=sasauxnouns5c(totvar,1,3,4,5,2,matrixnameslnw,sasnouns,k)
     else if ((decident3))  sasnouns=sasauxnouns5c(totvar,1,2,4,5,3,matrixnameslnw,sasnouns,k)
     else if ((decident4))  sasnouns=sasauxnouns5c(totvar,1,2,3,5,4,matrixnameslnw,sasnouns,k)
     else if ((decident5))  sasnouns=sasauxnouns5c(totvar,1,2,3,4,5,matrixnameslnw,sasnouns,k)
}

    return (sasnouns)
    }



  sasauxnouns3=function(totvar,counti,countf,p1,p2,p3,matrixnameslnw,sasnouns,k){
       aux2=paste(totvar[min(p1,p3)],totvar[max(p1,p3)],sep=":")
       aux3=paste(totvar[min(p2,p3)],totvar[max(p2,p3)],sep=":")
       auxdec=F
       for ( cd in (counti+1):countf){
        if (matrixnameslnw[cd]==aux2){
         sasnouns[k]= paste(paste(totvar[p2]),"(",paste(sasnouns[cd]),")",sep="")
         k=k+1
         auxdec=T
        }
        if (matrixnameslnw[cd]==aux3){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(sasnouns[cd]),")",sep="")
         k=k+1
         auxdec=T
        }
       }
       if (!auxdec){
         sasnouns[k]= paste(paste(totvar[min(p1,p2)],totvar[max(p1,p2)],sep=":"),"(",paste(totvar[p3]),")",sep="")
         k=k+1
       }
       return(sasnouns)
  }
  
  sasauxnouns3b=function(totvar,counti,countf,p1,p2,p3,matrixnameslnw,sasnouns,k){
       decaux1=FALSE
       for (cd2 in (counti+1):countf){
         if (matrixnameslnw[cd2]== paste(totvar[min(p1,p2)],totvar[max(p1,p2)],sep=":")){
          decaux1=TRUE
          decaux2=FALSE
          for (cd3 in (counti+1):countf){
            if (matrixnameslnw[cd3]== paste(totvar[min(p1,p3)],totvar[max(p1,p3)],sep=":")){
             decaux2=TRUE
             sasnouns[k]= paste(totvar[p1],"(",paste(totvar[p2],totvar[p3],sep=":"),")",sep="")
            }
          }
          if (!decaux2){
             sasnouns[k]=paste(totvar[p1],"(",paste(totvar[p2]),"):",totvar[p3],sep="")
          }
         }
        }
        if(!decaux1){
           sasnouns[k]= paste(totvar[p1],"(",paste(totvar[p3]),"):",totvar[p2],sep="")
        }
        return(sasnouns)
    }          
         
    sasauxnouns4=function(totvar,p1,p2,p3,p4,matrixnameslnw,sasnouns,k){
        if (sasauxnouns2aux(totvar,p1,p2,matrixnameslnw)){
          sasnouns[k]= paste(totvar[p1],"(",totvar[p2],"):",paste(totvar[min(p3,p4)],totvar[max(p3,p4)],sep=":"),sep="")
          k=k+1
        }
        else if (sasauxnouns2aux(totvar,p1,p3,matrixnameslnw)){
          sasnouns[k]= paste(totvar[p1],"(",totvar[p3],"):",paste(totvar[min(p2,p4)],totvar[max(p2,p4)],sep=":"),sep="")
          k=k+1
        }
        else if (sasauxnouns2aux(totvar,p1,p4,matrixnameslnw)){
          sasnouns[k]= paste(totvar[p1],"(",totvar[p4],"):",paste(totvar[min(p2,p3)],totvar[max(p2,p3)],sep=":"),sep="")
          k=k+1
        }

      return (sasnouns)
    }

   sasauxnouns4a=function(totvar,p1,p2,p3,p4,matrixnameslnw,sasnouns,k){
       aux2=paste(totvar[min(p1,p2,p3)],totvar[sort(c(p1,p2,p3))[2]],totvar[max(p1,p2,p3)],sep=":")
       aux3=paste(totvar[min(p1,p2,p4)],totvar[sort(c(p1,p2,p4))[2]],totvar[max(p1,p2,p4)],sep=":")
       if (any(matrixnameslnw==aux2)){
         sasnouns[k]= paste(paste(totvar[p4]),":",paste(sasnouns[which(matrixnameslnw==aux2)[1]],sep=""))
         k=k+1
       }
       else if (any(matrixnameslnw==aux3)){
         sasnouns[k]= paste(paste(totvar[p3]),":",paste(sasnouns[which(matrixnameslnw==aux3)[1]],sep=""))
         k=k+1
       }
       else {
        if (any(matrixnameslnw==paste(totvar(p1),totvar(p3)))&&any(matrixnameslnw==paste(totvar(p2),totvar(p4)))){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(totvar[p3]),"):",totvar[p2],"(",paste(totvar[p4]),")",sep="")
         k=k+1
       }
       else if(any(matrixnameslnw==paste(totvar(p1),totvar(p4)))&&any(matrixnameslnw==paste(totvar(p2),totvar(p3)))){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(totvar[p4]),"):",totvar[p2],"(",paste(totvar[p3]),")",sep="")
         k=k+1
       }
       else{}
      }
      return(sasnouns)
  }
  
  
      sasauxnouns2aux=function(totvar,p1,p2,matrixnameslnw){
             aux=paste(totvar[min(p1,p2)],totvar[max(p1,p2)],sep=":")
             return (any(matrixnameslnw==aux))
      }
       #sasauxnouns3aux=function(totvar,p1,p2,p3,matrixnameslnw){
        #      aux=paste(totvar[min(p1,p2,p3)],totvar[sort(c(p1,p2,p3))[2]],totvar[max(p1,p2,p3)],sep=":")
         #     return any(matrixnameslnw==aux)
       #}
       
      
      sasauxnouns4b1=function(totvar,p1,p2,p3,p4,aux2,aux3,aux4,matrixnameslnw,sasnouns,k){
           aux12=paste(totvar[min(p1,p3,p4)],totvar[sort(c(p1,p3,p4))[2]],totvar[max(p1,p3,p4)],sep=":")
           aux13=paste(totvar[min(p2,p3,p4)],totvar[sort(c(p2,p3,p4))[2]],totvar[max(p2,p3,p4)],sep=":")
           decident11=F
           decident12=F
           
           if(any(matrixnameslnw==aux12)) decident11=T
           if(any(matrixnameslnw==aux13)) decident12=T
           
           if(decident11&&decident12){
                aux112= paste(totvar[min(p1,p2,p4)],totvar[sort(c(p1,p2,p4))[2]],totvar[max(p1,p2,p4)],sep=":")
                sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux2)[1]],sasnouns[which(matrixnameslnw==aux3)[1]],paste(totvar[p3],"(",aux112,")",sep=""),sep=":")
                k=k+1
           }
           else if(decident11){
                sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux3)[1]],paste(totvar[p3],"(",sasnouns[which(matrixnameslnw==aux2)[1]],")",sep=""),sep=":")
                k=k+1
           }
           else if(decident12){
                sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux2)[1]],paste(totvar[p3],"(",sasnouns[which(matrixnameslnw==aux3)[1]],")",sep=""),sep=":")
                k=k+1
           }
           return (sasnouns)
      }
       
      sasauxnouns4b2=function(totvar,p1,p2,p3,p4,sasnouns,matrixnameslnw,k){
           aux12=paste(totvar[min(p1,p2,p4)],totvar[sort(c(p1,p2,p4))[2]],totvar[max(p1,p2,p4)],sep=":")
           aux13=paste(totvar[min(p1,p3,p4)],totvar[sort(c(p1,p3,p4))[2]],totvar[max(p1,p3,p4)],sep=":")             
           decident11=F
           decident12=F
           
           if(any(matrixnameslnw==aux12)) decident11=T
           if(any(matrixnameslnw==aux13)) decident12=T
           
           if(decident11&&decident12){
                sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],sasnouns[which(matrixnameslnw==aux13)[1]],sep=":")
                k=k+1
           }
           else if(decident11){
                sasnouns[k]=paste(totvar[p3],"(",sasnouns[which(matrixnameslnw==aux12)[1]],")",sep="")
                k=k+1
           }
           else if(decident12){
                sasnouns[k]=paste(totvar[p2],"(",sasnouns[which(matrixnameslnw==aux13)[1]],")",sep="")
                k=k+1
           }        
           return (sasnouns)
        }
       
      sasauxnouns4b=function(totvar,p1,p2,p3,p4,matrixnameslnw,sasnouns,k,...){
       
       aux2=paste(totvar[min(p1,p4)],totvar[max(p1,p4)],sep=":")
       aux3=paste(totvar[min(p2,p4)],totvar[max(p2,p4)],sep=":")
       aux4=paste(totvar[min(p3,p4)],totvar[max(p3,p4)],sep=":")
       decident1=F
       decident2=F
       decident3=F
       if(any(matrixnameslnw==aux2)) decident1=T
       if(any(matrixnameslnw==aux3)) decident2=T
       if(any(matrixnameslnw==aux4)) decident3=T
      
       if(decident1&&decident2&&decident3){
         sasnouns[k]=paste(sasnouns[which( matrixnameslnw==aux2)[1]],sasnouns[which( matrixnameslnw==aux3)[1]],sasnouns[which( matrixnameslnw==aux4)[1]],sep=":")
         k=k+1
       }
       else if(decident1&&decident2) sasnouns=sasauxnouns4b1(totvar,p1,p2,p3,p4,aux2,aux3,aux4,matrixnameslnw,sasnouns,k)      
       else if(decident1&&decident3) sasnouns=sasauxnouns4b1(totvar,p1,p3,p2,p4,aux2,aux4,aux3,matrixnameslnw,sasnouns,k) 
       else if(decident2&&decident3) sasnouns=sasauxnouns4b1(totvar,p2,p3,p1,p4,aux3,aux4,aux2,matrixnameslnw,sasnouns,k)     
       else if(decident1)            sasnouns=sasauxnouns4b2(totvar,p1,p2,p3,p4,sasnouns,matrixnameslnw,k)
       else if(decident2)            sasnouns=sasauxnouns4b2(totvar,p2,p1,p3,p4,sasnouns,matrixnameslnw,k)  
       else if(decident3)            sasnouns=sasauxnouns4b2(totvar,p3,p1,p2,p4,sasnouns,matrixnameslnw,k) 
     
       return (sasnouns)
     }                                                                       
         
     
 sasauxnouns5=function(totvar,p1,p2,p3,p4,p5,matrixnameslnw,sasnouns,k){
        if (sasauxnouns2aux(totvar,p1,p2,matrixnameslnw)){
          sasnouns[k]= paste(totvar[p1],"(",totvar[p2],"):",paste(totvar[min(p3,p4,p5)],totvar[sort(c(p3,p4,p5))[2]],totvar[max(p3,p4,p5)],sep=":"),sep="")
          k=k+1
        }
        else if (sasauxnouns2aux(totvar,p1,p3,matrixnameslnw)){
          sasnouns[k]= paste(totvar[p1],"(",totvar[p3],"):",paste(totvar[min(p2,p4,p5)],totvar[sort(c(p2,p4,p5))[2]],totvar[max(p2,p4,p5)],sep=":"),sep="")
          k=k+1
        }
        else if (sasauxnouns2aux(totvar,p1,p4,matrixnameslnw)){
          sasnouns[k]= paste(totvar[p1],"(",totvar[p4],"):",paste(totvar[min(p2,p3,p5)],totvar[sort(c(p2,p3,p5))[2]],totvar[max(p2,p3,p5)],sep=":"),sep="")
          k=k+1
        }
        else if (sasauxnouns2aux(totvar,p1,p5,matrixnameslnw)){
          sasnouns[k]= paste(totvar[p1],"(",totvar[p5],"):",paste(totvar[min(p2,p3,p4)],totvar[sort(c(p2,p3,p4))[2]],totvar[max(p2,p3,p4)],sep=":"),sep="")
          k=k+1
        }

      return (sasnouns)
    }


    sasauxnouns5a=function(totvar,p1,p2,p3,p4,p5,matrixnameslnw,sasnouns,k){
       aux2=paste(totvar[min(p1,p2,p3)],totvar[sort(c(p1,p2,p3))[2]],totvar[max(p1,p2,p3)],sep=":")
       aux3=paste(totvar[min(p1,p2,p4)],totvar[sort(c(p1,p2,p4))[2]],totvar[max(p1,p2,p4)],sep=":")
       aux4=paste(totvar[min(p1,p2,p5)],totvar[sort(c(p1,p2,p5))[2]],totvar[max(p1,p2,p5)],sep=":")
       if (any(matrixnameslnw==aux2)){
         sasnouns[k]= paste(paste(totvar[p4],totvar[p5],sep=":"),":",paste(sasnouns[which(matrixnameslnw==aux2)[1]],sep=""))
       }
       else if (any(matrixnameslnw==aux3)){
         sasnouns[k]= paste(paste(totvar[p3],totvar[p5],sep=":"),":",paste(sasnouns[which(matrixnameslnw==aux3)[1]],sep=""))
       }
       else if (any(matrixnameslnw==aux4)){
         sasnouns[k]= paste(paste(totvar[p3],totvar[p4],sep=":"),":",paste(sasnouns[which(matrixnameslnw==aux4)[1]],sep=""))
       }

       else {
        if (any(matrixnameslnw==paste(totvar(p1),totvar(p3)))&&any(matrixnameslnw==paste(totvar(p2),totvar(p4)))){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(totvar[p3]),"):",totvar[p2],"(",paste(totvar[p4]),")",sep="")
       }
       else if(any(matrixnameslnw==paste(totvar(p1),totvar(p4)))&&any(matrixnameslnw==paste(totvar(p2),totvar(p3)))){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(totvar[p4]),"):",totvar[p2],"(",paste(totvar[p3]),")",sep="")
       }
       else if (any(matrixnameslnw==paste(totvar(p1),totvar(p3)))&&any(matrixnameslnw==paste(totvar(p2),totvar(p5)))){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(totvar[p3]),"):",totvar[p2],"(",paste(totvar[p5]),")",sep="")
       }
       else if(any(matrixnameslnw==paste(totvar(p1),totvar(p5)))&&any(matrixnameslnw==paste(totvar(p2),totvar(p3)))){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(totvar[p5]),"):",totvar[p2],"(",paste(totvar[p3]),")",sep="")
       }
       else if (any(matrixnameslnw==paste(totvar(p1),totvar(p4)))&&any(matrixnameslnw==paste(totvar(p2),totvar(p5)))){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(totvar[p4]),"):",totvar[p2],"(",paste(totvar[p5]),")",sep="")
       }
       else if(any(matrixnameslnw==paste(totvar(p1),totvar(p5)))&&any(matrixnameslnw==paste(totvar(p2),totvar(p4)))){
         sasnouns[k]= paste(paste(totvar[p1]),"(",paste(totvar[p5]),"):",totvar[p2],"(",paste(totvar[p4]),")",sep="")
       }
       else{}
      }
      return(sasnouns)
  }
  

      sasauxnouns5b1=function(totvar,p5,aux2,aux3,aux4,dostip,sasnouns,matrixnameslnw,k){
          if (!dostip)  sasnouns[k]=paste(totvar[p5],":",sasnouns[which(matrixnameslnw==aux2)[1]],sasnouns[which(matrixnameslnw==aux3)[1]],sasnouns[which( matrixnameslnw==aux4)[1]],sep=":")
          else          sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux2)[1]],sasnouns[which(matrixnameslnw==aux3)[1]],sasnouns[which( matrixnameslnw==aux4)[1]],sep=":")
          return (sasnouns)
      }

      sasauxnouns5b2=function(totvar,p1,p2,p3,p4,p5,p6=NULL,aux2,aux3,sasnouns,matrixnameslnw,k){
           aux12=paste(totvar[min(p1,p2,p4)],totvar[sort(c(p1,p2,p4))[2]],totvar[max(p1,p2,p4)],sep=":")
           aux13=paste(totvar[min(p1,p3,p5)],totvar[sort(c(p1,p3,p5))[2]],totvar[max(p1,p3,p5)],sep=":")
           decident11=F
           decident12=F

           if(any(matrixnameslnw==aux12)) decident11=T
           if(any(matrixnameslnw==aux13)) decident12=T

           if(decident11&&decident12) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],sasnouns[which(matrixnameslnw==aux13)[1]],sep=":")
           else if(decident11){
            if (p6==NULL) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux3)[1]],sep="")
            else          sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux3)[1]],":",totvar[p6],sep="")
           }
           else if(decident12){
            if (p6==NULL) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux2)[1]],sep="")
            else          sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux2)[1]],":",totvar[p6],sep="")
           }
           return (sasnouns)
        }

      sasauxnouns5b3=function(totvar,p1,p2,p3,p4,p5,aux2,sasnouns,matrixnameslnw,k){
           aux12=paste(totvar[min(p1,p2,p4)],totvar[sort(c(p1,p2,p4))[2]],totvar[max(p1,p2,p4)],sep=":")
           aux13=paste(totvar[min(p1,p3,p4)],totvar[sort(c(p1,p3,p4))[2]],totvar[max(p1,p3,p4)],sep=":")
           decident11=F
           decident12=F

           if(any(matrixnameslnw==aux12)) decident11=T
           if(any(matrixnameslnw==aux13)) decident12=T

           if(decident11&&decident12) sasnouns[k]=paste(totvar[p5],sasnouns[which(matrixnameslnw==aux12)[1]],sasnouns[which(matrixnameslnw==aux13)[1]],sep=":")
           else {
                  aux121=paste(totvar[min(p1,p2,p3,p4)],totvar[sort(c(p1,p2,p3,p4))[2]],totvar[sort(c(p1,p2,p3,p4))[3]],totvar[max(p1,p2,p3,p4)],sep=":")
                  sasnouns[k]=paste(totvar[p5],sasnouns[which(matrixnameslnw==aux121)[1]],sep=":")
           }
           return (sasnouns)
        }


      sasauxnouns5b=function(totvar,p1,p2,p3,p4,p5,matrixnameslnw,sasnouns,k,...){

       aux2=paste(totvar[min(p1,p4)],totvar[max(p1,p4)],sep=":")
       aux3=paste(totvar[min(p2,p4)],totvar[max(p2,p4)],sep=":")
       aux4=paste(totvar[min(p3,p4)],totvar[max(p3,p4)],sep=":")
       aux5=paste(totvar[min(p1,p5)],totvar[max(p1,p5)],sep=":")
       aux6=paste(totvar[min(p2,p5)],totvar[max(p2,p5)],sep=":")
       aux7=paste(totvar[min(p3,p5)],totvar[max(p3,p5)],sep=":")
       decident1=F
       decident2=F
       decident3=F
       decident4=F
       decident5=F
       decident6=F
       
       if(any(matrixnameslnw==aux2)) decident1=T
       if(any(matrixnameslnw==aux3)) decident2=T
       if(any(matrixnameslnw==aux4)) decident3=T
       if(any(matrixnameslnw==aux5)) decident4=T
       if(any(matrixnameslnw==aux6)) decident5=T
       if(any(matrixnameslnw==aux7)) decident6=T

       if(decident1&&decident2&&decident3) sasnouns=sasauxnouns5b1(totvar,p5,aux2,aux3,aux4,FALSE,sasnouns,k)
       else if(decident1&&decident2&&decident6) sasnouns=sasauxnouns5b1(totvar,NULL,aux2,aux3,aux7,TRUE,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident5&&decident3) sasnouns=sasauxnouns5b1(totvar,NULL,aux2,aux6,aux4,TRUE,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident5&&decident6) sasnouns=sasauxnouns5b1(totvar,NULL,aux2,aux6,aux7,TRUE,sasnouns,matrixnameslnw,k)
       else if(decident4&&decident2&&decident3) sasnouns=sasauxnouns5b1(totvar,NULL,aux5,aux3,aux4,TRUE,sasnouns,matrixnameslnw,k)
       else if(decident4&&decident2&&decident6) sasnouns=sasauxnouns5b1(totvar,NULL,aux5,aux3,aux7,TRUE,sasnouns,matrixnameslnw,k)
       else if(decident4&&decident5&&decident3) sasnouns=sasauxnouns5b1(totvar,NULL,aux5,aux6,aux4,TRUE,sasnouns,matrixnameslnw,k)
       else if(decident4&&decident5&&decident6) sasnouns=sasauxnouns5b1(totvar,p4,aux5,aux6,aux7,TRUE,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident2) sasnouns=sasauxnouns5b2(totvar,p3,p1,p2,p4,p4,p6=p5,aux2,aux3,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident3) sasnouns=sasauxnouns5b2(totvar,p2,p1,p3,p4,p4,p6=p5,aux2,aux4,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident5) sasnouns=sasauxnouns5b2(totvar,p3,p1,p2,p4,p5,p6=NULL,aux2,aux6,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident6) sasnouns=sasauxnouns5b2(totvar,p2,p1,p3,p4,p5,p6=NULL,aux2,aux7,sasnouns,matrixnameslnw,k)
       else if(decident2&&decident3) sasnouns=sasauxnouns5b2(totvar,p1,p2,p3,p4,p4,p6=p5,aux3,aux4,sasnouns,matrixnameslnw,k)
       else if(decident2&&decident6) sasnouns=sasauxnouns5b2(totvar,p1,p2,p3,p4,p5,p6=NULL,aux3,aux7,sasnouns,matrixnameslnw,k)
       else if(decident4&&decident2) sasnouns=sasauxnouns5b2(totvar,p3,p1,p2,p5,p5,p6=NULL,aux5,aux3,sasnouns,matrixnameslnw,k)
       else if(decident4&&decident3) sasnouns=sasauxnouns5b2(totvar,p2,p1,p3,p5,p4,p6=NULL,aux5,aux4,sasnouns,matrixnameslnw,k)
       else if(decident4&&decident5) sasnouns=sasauxnouns5b2(totvar,p3,p1,p2,p5,p5,p6=p4,aux5,aux6,sasnouns,matrixnameslnw,k)
       else if(decident4&&decident6) sasnouns=sasauxnouns5b2(totvar,p2,p1,p3,p5,p5,p6=p4,aux5,aux7,sasnouns,matrixnameslnw,k)
       else if(decident5&&decident3) sasnouns=sasauxnouns5b2(totvar,p1,p2,p3,p5,p4,p6=NULL,aux6,aux4,sasnouns,matrixnameslnw,k)
       else if(decident5&&decident6) sasnouns=sasauxnouns5b2(totvar,p1,p2,p3,p5,p5,p6=p4,aux6,aux7,sasnouns,matrixnameslnw,k)
       else if(decident1)            sasnouns=sasauxnouns5b3(totvar,p1,p2,p3,p4,p5,aux2,sasnouns,matrixnameslnw,k)
       else if(decident2)            sasnouns=sasauxnouns5b3(totvar,p2,p1,p3,p4,p5,aux3,sasnouns,matrixnameslnw,k)
       else if(decident3)            sasnouns=sasauxnouns5b3(totvar,p3,p1,p2,p4,p5,aux4,sasnouns,matrixnameslnw,k)
       else if(decident4)            sasnouns=sasauxnouns5b3(totvar,p1,p2,p3,p5,p4,aux5,sasnouns,matrixnameslnw,k)
       else if(decident5)            sasnouns=sasauxnouns5b3(totvar,p2,p1,p3,p5,p4,aux6,sasnouns,matrixnameslnw,k)
       else if(decident6)            sasnouns=sasauxnouns5b3(totvar,p3,p1,p2,p5,p4,aux7,sasnouns,matrixnameslnw,k)

       return (sasnouns)
     }
     
     
       sasauxnouns5c1=function(totvar,p1,p2,p3,p4,p5,aux2,aux3,aux4,sasnouns,matrixnameslnw,k){
           aux12=paste(totvar[min(p1,p2,p5)],totvar[sort(c(p1,p2,p5))[2]],totvar[max(p1,p2,p5)],sep=":")
           aux13=paste(totvar[min(p1,p3,p5)],totvar[sort(c(p1,p3,p5))[2]],totvar[max(p1,p3,p5)],sep=":")
           aux14=paste(totvar[min(p1,p4,p5)],totvar[sort(c(p1,p4,p5))[2]],totvar[max(p1,p4,p5)],sep=":")
           
           decident11=F
           decident12=F
           decident13=F
           
           if(any(matrixnameslnw==aux12)) decident11=T
           if(any(matrixnameslnw==aux13)) decident13=T
           if(any(matrixnameslnw==aux14)) decident12=T
           
           
           if(decident11&&decident12&&decident13) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],sasnouns[which(matrixnameslnw==aux13)[1]],sasnouns[which(matrixnameslnw==aux14)[1]],sep=":")
           else if(decident11&&decident12) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux4)[1]],sep="")
           else if(decident11&&decident13) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],":",sasnouns[which(matrixnameslnw==aux3)[1]],sep="")          
           else if(decident12&&decident13) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],":",sasnouns[which(matrixnameslnw==aux2)[1]],sep="")          
           else if(decident11)  sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux3)[1]],":",sasnouns[which(matrixnameslnw==aux4)[1]],":",sasnouns[which(matrixnameslnw==aux12)[1]],sep="")          
           else if(decident12)  sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux2)[1]],":",sasnouns[which(matrixnameslnw==aux4)[1]],":",sasnouns[which(matrixnameslnw==aux13)[1]],sep="")          
           else if(decident13)  sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux2)[1]],":",sasnouns[which(matrixnameslnw==aux3)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],sep="")          
           return (sasnouns)
        }


       sasauxnouns5c2=function(totvar,p1,p2,p3,p4,p5,aux2,aux3,sasnouns,matrixnameslnw,k){
           aux12=paste(totvar[min(p1,p3,p5)],totvar[sort(c(p1,p2,p5))[2]],totvar[max(p1,p2,p5)],sep=":")
           aux13=paste(totvar[min(p1,p4,p5)],totvar[sort(c(p1,p4,p5))[2]],totvar[max(p1,p4,p5)],sep=":")
           aux14=paste(totvar[min(p2,p3,p5)],totvar[sort(c(p2,p3,p5))[2]],totvar[max(p2,p3,p5)],sep=":")
           aux15=paste(totvar[min(p2,p4,p5)],totvar[sort(c(p2,p4,p5))[2]],totvar[max(p2,p4,p5)],sep=":")
           
           decident11=F
           decident12=F
           decident13=F
           decident14=F
           
           if(any(matrixnameslnw==aux12)) decident11=T
           if(any(matrixnameslnw==aux13)) decident12=T
           if(any(matrixnameslnw==aux14)) decident13=T
           if(any(matrixnameslnw==aux15)) decident14=T
           
           
           if(decident11&&decident12&&decident13&decident14) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],sasnouns[which(matrixnameslnw==aux13)[1]],sasnouns[which(matrixnameslnw==aux14)[1]],sasnouns[which(matrixnameslnw==aux15)[1]],sep=":")
           else if(decident11&&decident12&&decident13) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],sep="")
           else if(decident11&&decident12&&decident14) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux15)[1]],sep="")
           else if(decident11&&decident13&&decident14) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],":",sasnouns[which(matrixnameslnw==aux15)[1]],sep="")
           else if(decident12&&decident13&&decident14) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],":",sasnouns[which(matrixnameslnw==aux15)[1]],sep="")
           else if(decident11&&decident12) {
             aux121=paste(totvar[min(p1,p2,p3,p5)],totvar[sort(c(p1,p2,p3,p5))[2]],totvar[sort(c(p1,p2,p3,p5))[3]],totvar[max(p1,p2,p3,p5)],sep=":")
             aux122=paste(totvar[min(p1,p2,p4,p5)],totvar[sort(c(p1,p2,p3,p5))[2]],totvar[sort(c(p1,p2,p4,p5))[3]],totvar[max(p1,p2,p3,p5)],sep=":")
             decident111=F
             decident112=F
             if(any(matrixnameslnw==aux121)) decident111=T
             if(any(matrixnameslnw==aux122)) decident112=T
             if (decident111&&decident112) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux121)[1]],":",sasnouns[which(matrixnameslnw==aux122)[1]],sep="")
             if (decident111) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux121)[1]],":",sasnouns[which(matrixnameslnw==aux13)[1]],sep="")
             if (decident112) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux122)[1]],":",sasnouns[which(matrixnameslnw==aux12)[1]],sep="")
           }
           else if(decident11&&decident13)  paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],sep="")
           else if(decident11&&decident14)  paste(sasnouns[which(matrixnameslnw==aux12)[1]],":",sasnouns[which(matrixnameslnw==aux15)[1]],sep="")
           else if(decident12&&decident13)  paste(sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],sep="")
           else if(decident12&&decident14)  paste(sasnouns[which(matrixnameslnw==aux13)[1]],":",sasnouns[which(matrixnameslnw==aux15)[1]],sep="")
           else if(decident13&&decident14) {
             aux121=paste(totvar[min(p1,p2,p3,p5)],totvar[sort(c(p1,p2,p3,p5))[2]],totvar[sort(c(p1,p2,p3,p5))[3]],totvar[max(p1,p2,p3,p5)],sep=":")
             aux122=paste(totvar[min(p1,p2,p4,p5)],totvar[sort(c(p1,p2,p3,p5))[2]],totvar[sort(c(p1,p2,p4,p5))[3]],totvar[max(p1,p2,p3,p5)],sep=":")
             decident111=F
             decident112=F
             if(any(matrixnameslnw==aux121)) decident111=T
             if(any(matrixnameslnw==aux122)) decident112=T
             if (decident111&&decident112) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux121)[1]],":",sasnouns[which(matrixnameslnw==aux122)[1]],sep="")
             if (decident111) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux121)[1]],":",sasnouns[which(matrixnameslnw==aux15)[1]],sep="")
             if (decident112) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux122)[1]],":",sasnouns[which(matrixnameslnw==aux14)[1]],sep="")
           }
           else if(decident11||decident13){
             aux121=paste(totvar[min(p1,p2,p3,p5)],totvar[sort(c(p1,p2,p3,p5))[2]],totvar[sort(c(p1,p2,p3,p5))[3]],totvar[max(p1,p2,p3,p5)],sep=":")         
             paste(sasnouns[which(matrixnameslnw==aux121)[1]],":",sasnouns[which(matrixnameslnw==aux3)[1]],sep="")
           }   
           else if(decident12||decident14){
             aux121=paste(totvar[min(p1,p2,p4,p5)],totvar[sort(c(p1,p2,p4,p5))[2]],totvar[sort(c(p1,p2,p4,p5))[3]],totvar[max(p1,p2,p4,p5)],sep=":")         
             paste(sasnouns[which(matrixnameslnw==aux121)[1]],":",sasnouns[which(matrixnameslnw==aux2)[1]],sep="")
           }            
        
          return (sasnouns)
        }       
     
      sasauxnouns5c31=function(totvar,p1,p2,p3,p4,p5,aux12,aux13,sasnouns,matrixnameslnw,k){
             aux112=paste(totvar[min(p1,p3,p4,p5)],totvar[sort(c(p1,p3,p4,p5))[2]],totvar[sort(c(p1,p3,p4,p5))[3]],totvar[max(p1,p3,p4,p5)],sep=":")         
             aux113=paste(totvar[min(p2,p3,p4,p5)],totvar[sort(c(p2,p3,p4,p5))[2]],totvar[sort(c(p2,p3,p4,p5))[3]],totvar[max(p2,p3,p4,p5)],sep=":")         
             decident111=F
             decident112=F
                        
             if(any(matrixnameslnw==aux112)) decident111=T
             if(any(matrixnameslnw==aux113)) decident112=T
           
             if (decident111&&decident112)  sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux112)[1]],":",sasnouns[which(matrixnameslnw==aux113)[1]],sep="")
             if (decident111)  sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux112)[1]],":",sasnouns[which(matrixnameslnw==aux13)[1]],sep="")
             if (decident112)  sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux113)[1]],":",sasnouns[which(matrixnameslnw==aux12)[1]],sep="")
             return (sasnouns)
      }
     
      sasauxnouns5c32=function(totvar,p1,p2,p3,p4,p5,sasnouns,matrixnameslnw,k){
             aux112=paste(totvar[min(p1,p2,p4,p5)],totvar[sort(c(p1,p2,p4,p5))[2]],totvar[sort(c(p1,p2,p4,p5))[3]],totvar[max(p1,p2,p4,p5)],sep=":")         
             aux113=paste(totvar[min(p1,p3,p4,p5)],totvar[sort(c(p1,p3,p4,p5))[2]],totvar[sort(c(p1,p3,p4,p5))[3]],totvar[max(p1,p3,p4,p5)],sep=":")         
             decident111=F
             decident112=F
             
             if(any(matrixnameslnw==aux112)) decident111=T
             if(any(matrixnameslnw==aux113)) decident112=T
          
             if(decident111&&decident112) sasnouns[k]= paste(sasnouns[which(matrixnameslnw==aux112)[1]],":",sasnouns[which(matrixnameslnw==aux113)[1]],sep="")
             else if(decident111)  sasnouns[k]= paste(totvar[p3],"(",sasnouns[which(matrixnameslnw==aux112)[1]],")",sep="")
             else if(decident112)  sasnouns[k]= paste(totvar[p2],"(",sasnouns[which(matrixnameslnw==aux113)[1]],")",sep="")
             return (sasnouns)       
      }
      
      sasauxnouns5c3=function(totvar,p1,p2,p3,p4,p5,aux2,sasnouns,matrixnameslnw,k,...){
           aux12=paste(totvar[min(p1,p4,p5)],totvar[sort(c(p1,p4,p5))[2]],totvar[max(p1,p4,p5)],sep=":")
           aux13=paste(totvar[min(p2,p4,p5)],totvar[sort(c(p2,p4,p5))[2]],totvar[max(p2,p4,p5)],sep=":")
           aux14=paste(totvar[min(p3,p4,p5)],totvar[sort(c(p3,p4,p5))[2]],totvar[max(p3,p4,p5)],sep=":")
          
           decident11=F
           decident12=F
           decident13=F
           
           if(any(matrixnameslnw==aux12)) decident11=T
           if(any(matrixnameslnw==aux13)) decident12=T
           if(any(matrixnameslnw==aux14)) decident13=T           
           
           if(decident11&&decident12&&decident13) sasnouns[k]=paste(sasnouns[which(matrixnameslnw==aux12)[1]],sasnouns[which(matrixnameslnw==aux13)[1]],sasnouns[which(matrixnameslnw==aux14)[1]],sep=":")
           else if(decident11&&decident12) sasnouns= sasauxnouns5c31(totvar,p1,p2,p3,p4,p5,aux12,aux13,sasnouns,matrixnameslnw,k)
           else if(decident11&&decident13) sasnouns= sasauxnouns5c31(totvar,p1,p3,p2,p4,p5,aux12,aux14,sasnouns,matrixnameslnw,k)
           else if(decident12&&decident13) sasnouns= sasauxnouns5c31(totvar,p2,p3,p1,p4,p5,aux13,aux14,sasnouns,matrixnameslnw,k)
           else if(decident11) sasnouns=sasauxnouns5c32(totvar,p1,p2,p3,p4,p5,sasnouns,matrixnameslnw,k)
           else if(decident12) sasnouns=sasauxnouns5c32(totvar,p2,p1,p3,p4,p5,sasnouns,matrixnameslnw,k)
           else if(decident13) sasnouns=sasauxnouns5c32(totvar,p3,p1,p2,p4,p5,sasnouns,matrixnameslnw,k)
           return(sasnouns)
      }     
           
     
     sasauxnouns5c=function(totvar,p1,p2,p3,p4,p5,matrixnameslnw,sasnouns,k,...){

       aux2=paste(totvar[min(p1,p5)],totvar[max(p1,p5)],sep=":")
       aux3=paste(totvar[min(p2,p5)],totvar[max(p2,p5)],sep=":")
       aux4=paste(totvar[min(p3,p5)],totvar[max(p3,p5)],sep=":")
       aux5=paste(totvar[min(p4,p5)],totvar[max(p4,p5)],sep=":")

       decident1=F
       decident2=F
       decident3=F
       decident4=F

       
       if(any(matrixnameslnw==aux2)) decident1=T
       if(any(matrixnameslnw==aux3)) decident2=T
       if(any(matrixnameslnw==aux4)) decident3=T
       if(any(matrixnameslnw==aux5)) decident4=T


       else if(decident1&&decident2&&decident3&&decident4) {
            sasnouns[k]= paste(sasnouns[which(matrixnameslnw==aux2)[1]],sasnouns[which(matrixnameslnw==aux3)[1]],sasnouns[which(matrixnameslnw==aux3)[1]],sasnouns[which(matrixnameslnw==aux4)[1]],sep=":")
       }
       else if(decident1&&decident2&&decident3) sasnouns=sasauxnouns5c1(totvar,p4,p1,p2,p3,p5,aux2,aux3,aux4,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident2&&decident4) sasnouns=sasauxnouns5c1(totvar,p3,p1,p2,p4,p5,aux2,aux3,aux5,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident3&&decident4) sasnouns=sasauxnouns5c1(totvar,p2,p1,p3,p4,p5,aux2,aux4,aux5,sasnouns,matrixnameslnw,k)      
       else if(decident2&&decident3&&decident4) sasnouns=sasauxnouns5c1(totvar,p1,p2,p3,p4,p5,aux3,aux4,aux5,sasnouns,matrixnameslnw,k)  
       else if(decident1&&decident2) sasnouns=sasauxnouns5c2(totvar,p3,p4,p1,p2,p5,aux2,aux3,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident3) sasnouns=sasauxnouns5c2(totvar,p2,p4,p1,p3,p5,aux2,aux4,sasnouns,matrixnameslnw,k)
       else if(decident1&&decident4) sasnouns=sasauxnouns5c2(totvar,p2,p3,p1,p4,p5,aux2,aux5,sasnouns,matrixnameslnw,k)
       else if(decident2&&decident3) sasnouns=sasauxnouns5c2(totvar,p1,p4,p2,p3,p5,aux3,aux4,sasnouns,matrixnameslnw,k)
       else if(decident2&&decident4) sasnouns=sasauxnouns5c2(totvar,p1,p3,p2,p4,p5,aux3,aux5,sasnouns,matrixnameslnw,k)
       else if(decident3&&decident4) sasnouns=sasauxnouns5c2(totvar,p1,p2,p3,p4,p5,aux4,aux5,sasnouns,matrixnameslnw,k)
       else if(decident1) sasnouns=sasauxnouns5c3(totvar,p2,p3,p4,p1,p5,aux2,sasnouns,matrixnameslnw,k)
       else if(decident2) sasnouns=sasauxnouns5c3(totvar,p1,p3,p4,p2,p5,aux3,sasnouns,matrixnameslnw,k)
       else if(decident3) sasnouns=sasauxnouns5c3(totvar,p1,p2,p4,p3,p5,aux4,sasnouns,matrixnameslnw,k)
       else if(decident4) sasnouns=sasauxnouns5c3(totvar,p1,p2,p3,p4,p5,aux5,sasnouns,matrixnameslnw,k)

       return (sasnouns)
     }