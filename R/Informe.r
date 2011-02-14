
crearInforme=function(object){
       name="REPORT"
  #Crear Sweave.sty
        	for(i in 1:length(.libPaths())){
          	file.copy(from=paste(.libPaths()[i],"/varcompci/doc/Sweave.sty",sep=""),to=paste("Sweave.sty"))
					}
	    
					
  ##Crear informe.Rnw
					sink(paste(name,".Rnw",sep=""))
					cat("\\documentclass{article}\n")
					cat("\n\\usepackage[colorlinks=true,urlcolor=blue]{hyperref}\n")
					cat("\n\\usepackage{color}\n")
					cat("\n\\usepackage{Sweave}\n")
					cat("\n\\usepackage[cp1252]{inputenc}\n")
					cat("\n\\usepackage{amscd}\n")
					cat("\n\\begin{document}\n")
					cat(paste("\n\\title{",name,"}\n",sep=""))
#					cat(paste("\n\\author{"}\n",sep=""))
					cat("\n\\maketitle\n")
					cat("\n\\SweaveOpts{echo=FALSE}\n")
					
				#Va salida de R
				  cat("\\begin{Schunk}\n\\begin{Soutput}\n")
    	       printEMS(object@EMS)
        		 printanov(object@ANOVA)
        		 printMeansq(object@Meansq)
        		 printvariance(object@variance)
        		 printaic(object@aic,object@bic)
        		 show(object)
          cat("\\end{Soutput}\n\\end{Schunk}\n\n")
					
					cat("\n\\end{document}\n")
					sink()
					suppressWarnings(sink())
					
  #Crear .tex
					capture.output(Sweave(paste(name,".Rnw",sep="")))
					
  #Crear .pdf
			   	b=shell(cmd=paste("pdfLatex",name,sep=" "),intern=T)
	
  #Abre el .pdf
          shell.exec(paste(getwd(),"/",name,".pdf",sep=""))		   	
  #tancar fitxers 
 
  unlink(paste(name,".","SAS.OutputLog.701",sep=""))
  unlink(paste(name,".","Rnw",sep=""))  
  unlink(paste(name,".","aux",sep=""))
  unlink(paste(name,".","out",sep=""))
  unlink(paste(name,".","TeX",sep=""))

}