
# Plot 1: Concentrations:----   (concentration plots are similar whether FFQ data or not)
# generate results based on inputs from ui.R:  Concentrations
## ---- distPlot1_1 --------
distPlot1_1 <- function(input_lim, unit_concen, hazard_concen, input_upper, input_lower, n_sim, input_selectdist, input_selectscale, foodnamesused,
                        nfused, foodindex, hazardnamesused, hazardtypesused, nhused, 
                        hazardnamesK, hazardnamesM, hazardnamesusedK, hazardnamesusedM,
                        nhusedK, nhusedM, hazardindex, hazardindexK, hazardindexM,
                        nexactK,nexactM,
                        nhK, nf, mucK, sigcK, pK, nhM, mucM, sigcM, pM,
                        logcK, logLOQK, logLODK, logLOQLimK, logLODLimK, logcM, logLOQM, logLODM, logLOQLimM, logLODLimM
){
  
  
  # generate results based on inputs from ui.R:  
  # Concentrations
  
  par(oma = c(4, 1, 0, 1),cex.lab=1.3,cex.main=1.3) # Outer margins for legend
  
  # Chemical concentrations:----
  
  if((nhusedK>0)&(nfused>0)){
    
    for(h in 1:nhusedK){
      Unit <- unit_concen[hazard_concen == hazardnamesusedK[h]] # the measurement unit used for hazard concentration
      Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
      Unit2 <- sub(".*p.", "", Unit) # Extract characters after pattern
      
      for(i in 1:nfused){
        if(nexactK[hazardindexK[h],foodindex[i]]>0){ # this hazard-food is modeled
          if(input_selectdist=="Density"){ ## density----
            if(input_selectscale=="Absolute"){ 
              cmeanK <- exp(mucK[,hazardindexK[h],foodindex[i]]+0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)
              cmedianK <- exp(mucK[,hazardindexK[h],foodindex[i]])
              
              maxx <- quantile(
                qlnorm(input_lim,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]), 
                0.9,names=FALSE)
              
              plot(density(cmedianK,from=0,to=maxx,n=2048),lwd=3,main=paste(hazardnamesusedK[h],"in",foodnamesused[i]),
                   xlab=paste("Concentration+ (", Unit1, "per", Unit2,")"),ylab="Probability density",xlim=c(0,maxx))
              xvalues <- seq(0,maxx,length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                                   mucK[,hazardindexK[h],foodindex[i]],
                                                   sigcK[,hazardindexK[h],foodindex[i]]),
                                            input_upper,names=FALSE) 
                lowervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                                   mucK[,hazardindexK[h],foodindex[i]],
                                                   sigcK[,hazardindexK[h],foodindex[i]]),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              
              lines(density(cmedianK,from=0,to=maxx,n=2048),lwd=3)
              lines(density(cmeanK,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3,main=paste(hazardnamesusedK[h],"in",foodnamesused[i]),xlab="Concentration+",ylab="",xlim=c(0,maxx))
              # mark data points and possible LOD and LOQ values for censored data:
              rug(exp(logcK[hazardindexK[h],foodindex[i],]),lwd=2.5,col="#D0006F",quiet=TRUE)
              rug(exp(logLOQK[hazardindexK[h],foodindex[i],]),lwd=4.5,col="green",quiet=TRUE)
              rug(exp(logLODK[hazardindexK[h],foodindex[i],]),lwd=4.5,col="#004F71",quiet=TRUE)
            }
            if(input_selectscale=="Logarithmic"){
              
              maxx <- quantile(qnorm(input_lim,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),
                               0.9,names=FALSE)
              minn <- quantile(qnorm(0.05,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),
                               0.5,names=FALSE)
              plot(density(mucK[,hazardindexK[h],foodindex[i]]/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(hazardnamesusedK[h],"in",foodnamesused[i]),
                   xlab=paste("log Concentration+ (", Unit1, "per", Unit2,")"),ylab="Probability density",xlim=c(minn/log(10),maxx/log(10))) 
              xvalues <- seq(minn/log(10),maxx/log(10),length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (mucK[,hazardindexK[h],foodindex[i]])/log(10),
                                                  (sigcK[,hazardindexK[h],foodindex[i]])/log(10) ),
                                            input_upper,names=FALSE) 
                lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (mucK[,hazardindexK[h],foodindex[i]])/log(10),
                                                  (sigcK[,hazardindexK[h],foodindex[i]])/log(10) ),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(density(mucK[,hazardindexK[h],foodindex[i]]/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(hazardnamesusedK[h],"in",foodnamesused[i]),xlab="log Concentration+",ylab="",xlim=c(minn/log(10),maxx/log(10))) 
              
              # mark data points and possible LOD and LOQ values for censored data:
              rug(logcK[hazardindexK[h],foodindex[i],]/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
              rug(logLOQK[hazardindexK[h],foodindex[i],]/log(10),lwd=4.5,col="green",quiet=TRUE)
              rug(logLODK[hazardindexK[h],foodindex[i],]/log(10),lwd=4.5,col="#004F71",quiet=TRUE)
            }
          } # end of if density
          
          if(input_selectdist=="Cumulative"){ ## cumulative ----
            par(yaxt="s")
            cump <- seq(1,n_sim)
            cump <- cump/length(cump)
            if(input_selectscale=="Absolute"){
            
              maxx <- quantile(
                qlnorm(input_lim,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]), 
                0.9,names=FALSE)
              
              cmedianK <- sort(exp(mucK[,hazardindexK[h],foodindex[i]]))
              plot(cmedianK[cmedianK<maxx],cump[cmedianK<maxx],col="#F7CE3C",lwd=3,main=paste(hazardnamesusedK[h],"in",foodnamesused[i]),
                   xlab=paste("Concentration+ (", Unit1, "per", Unit2,")"),ylab="Cumulative probability",xlim=c(0,maxx),ylim=c(0,1),type="l")
              xvalues <- seq(0,maxx*1.1,length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(plnorm(xvalues[xv],
                                                   mucK[,hazardindexK[h],foodindex[i]],
                                                   sigcK[,hazardindexK[h],foodindex[i]]),
                                            input_upper,names=FALSE) #0.975,names=FALSE)
                lowervalues[xv] <- quantile(plnorm(xvalues[xv],
                                                   mucK[,hazardindexK[h],foodindex[i]],
                                                   sigcK[,hazardindexK[h],foodindex[i]]),
                                            input_lower,names=FALSE) #0.025,names=FALSE)
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              cmeanK <- sort(exp(mucK[,hazardindexK[h],foodindex[i]]+0.5*sigcK[,hazardindexK[h],foodindex[i]]^2))
              cmedianK <- sort(exp(mucK[,hazardindexK[h],foodindex[i]]))
              lines(cmeanK,cump,col="#F7CE3C",lwd=3,main=paste(hazardnamesusedK[h],"in",foodnamesused[i]),xlab="Concentration+",ylab="",xlim=c(0,maxx),type="l") 
              lines(cmedianK,cump,lwd=3)
              # mark data points and possible LOD and LOQ values for censored data:
              rug(exp(logcK[hazardindexK[h],foodindex[i],]),lwd=2.5,col="#D0006F",quiet=TRUE)
              rug(exp(logLOQK[hazardindexK[h],foodindex[i],]),lwd=4.5,col="green",quiet=TRUE)
              rug(exp(logLODK[hazardindexK[h],foodindex[i],]),lwd=4.5,col="#004F71",quiet=TRUE)
              lines(ecdf(
                c(exp(logcK[hazardindexK[h],foodindex[i],]),
                  exp(logLOQK[hazardindexK[h],foodindex[i],]),
                  exp(logLODK[hazardindexK[h],foodindex[i],])
                )),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
              lines(ecdf(
                c(exp(logcK[hazardindexK[h],foodindex[i],]),
                  exp(logLOQLimK[hazardindexK[h],foodindex[i],]),
                  exp(logLODLimK[hazardindexK[h],foodindex[i],]-20)
                )),verticals=TRUE,do.points=FALSE,lwd=2,col="#004F71")
            }
            if(input_selectscale=="Logarithmic"){
              
              maxx <- quantile(qnorm(input_lim,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),
                               0.9,names=FALSE)
              minn <- quantile(qnorm(0.05,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),
                               0.5,names=FALSE)
              
              
              plot(sort(mucK[,hazardindexK[h],foodindex[i]]/log(10)),cump,lwd=3,main=paste(hazardnamesusedK[h],"in",foodnamesused[i]),
                   xlab=paste("log Concentration+ (", Unit1, "per", Unit2,")"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),type="l") 
              xvalues <- seq(minn/log(10),maxx/log(10),length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (mucK[,hazardindexK[h],foodindex[i]])/log(10),
                                                  (sigcK[,hazardindexK[h],foodindex[i]])/log(10) ),
                                            input_upper,names=FALSE) 
                lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (mucK[,hazardindexK[h],foodindex[i]])/log(10),
                                                  (sigcK[,hazardindexK[h],foodindex[i]])/log(10) ),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(sort(mucK[,hazardindexK[h],foodindex[i]]/log(10)),cump,lwd=3,main=paste(hazardnamesusedK[h],"in",foodnamesused[i]),xlab="log Concentration+",ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
              # mark data points and possible LOD and LOQ values for censored data:
              rug(logcK[hazardindexK[h],foodindex[i],]/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
              rug(logLOQK[hazardindexK[h],foodindex[i],]/log(10),lwd=4.5,col="green",quiet=TRUE)
              rug(logLODK[hazardindexK[h],foodindex[i],]/log(10),lwd=4.5,col="#004F71",quiet=TRUE)
              lines(ecdf(
                c(logcK[hazardindexK[h],foodindex[i],]/log(10),
                  logLOQK[hazardindexK[h],foodindex[i],]/log(10),
                  logLODK[hazardindexK[h],foodindex[i],]/log(10) 
                )),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
              lines(ecdf(
                c(logcK[hazardindexK[h],foodindex[i],]/log(10),
                  logLOQLimK[hazardindexK[h],foodindex[i],]/log(10),
                  logLODLimK[hazardindexK[h],foodindex[i],]/log(10)-20)
              ),verticals=TRUE,do.points=FALSE,lwd=2,col="#004F71")
              
            }  
          } # end of if cumulative
          
          
          # legend----
          mtext(paste("Prevalence of", hazardnamesusedK[h],"in",foodnamesused[i], ": ",round(quantile(100*pK[,hazardindexK[h],foodindex[i]],0.5,names=FALSE),1),
                      "% (posterior median). 95% uncertainty interval:",round(quantile(100*pK[,hazardindexK[h],foodindex[i]],0.025,names=FALSE),1),"% - ",round(quantile(100*pK[,hazardindexK[h],foodindex[i]],0.975,names=FALSE),1),"%"),
                side = 1, adj = 0,line=1, cex = 1,
                outer = TRUE)
          if(input_selectscale=="Absolute"){
          mtext(paste("95% uncertainty interval for the Q50% concentration+:", 
                      round(quantile(qlnorm(0.5,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),0.025,names=FALSE),2),"-", round(quantile(qlnorm(0.5,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),0.975,names=FALSE),2),", and for Q95%:",
                      round(quantile(qlnorm(0.95,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),0.025,names=FALSE),2),"-", round(quantile(qlnorm(0.95,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),0.975,names=FALSE),2)),
                side = 1, adj = 0,line=2, cex = 1,
                outer = TRUE)}
          if(input_selectscale=="Logarithmic"){
            mtext(paste("95% uncertainty interval for the Q50% log(concentration+):", 
                        round(quantile(qnorm(0.5,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]])/log(10),0.025,names=FALSE),2),"-", round(quantile(qnorm(0.5,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]])/log(10),0.975,names=FALSE),2),", and for Q95%:",
                        round(quantile(qnorm(0.95,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]])/log(10),0.025,names=FALSE),2),"-", round(quantile(qnorm(0.95,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]])/log(10),0.975,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)}
          
          
        } else # end of if this hazard-food was modeled  
        {
          par(mar = c(0,0,0,0))
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.8, paste("The data for this food-hazard combination is not sufficient for modeling\n",
                                       "(e.g., concentration measurements (>LOQ) about the food-hazard combination are missing)."), 
               cex = 1.6, col = "#D0006F")
          par(mar = c(5, 4, 4, 2) + 0.1)
        }
        
        
      }} # end of for nhusedK nfused
  }  # end of if nhusedK >0 nfused >0
  
  
  # Microbiological concentrations:----
  
  if((nhusedM>0)&(nfused>0)){
    
    for(h in 1:nhusedM){
      Unit <- unit_concen[hazard_concen == hazardnamesusedM[h]] # the measurement unit used for hazard concentration
      Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
      Unit2 <- sub(".*p.", "", Unit) # Extract characters after pattern
      
      for(i in 1:nfused){
        if(nexactM[hazardindexM[h],foodindex[i]]>0){ # this hazard-food is modeled
          if(input_selectdist=="Density"){ ## density----
            if(input_selectscale=="Absolute"){
              cmeanM <- exp(mucM[,hazardindexM[h],foodindex[i]]+0.5*sigcM[,hazardindexM[h],foodindex[i]]^2)
              cmedianM <- exp(mucM[,hazardindexM[h],foodindex[i]])
              
              maxx <- quantile(qlnorm(input_lim,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),
                               0.9,names=FALSE)
              
              plot(density(cmedianM,from=0,to=maxx,n=2048),lwd=3,main=paste(hazardnamesusedM[h],"in",foodnamesused[i]),
                   xlab=paste("Concentration+ (", Unit1, "per", Unit2,")"),ylab="Probability density",xlim=c(0,maxx)) 
              xvalues <- seq(0,maxx,length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                                   mucM[,hazardindexM[h],foodindex[i]],
                                                   sigcM[,hazardindexM[h],foodindex[i]]),
                                            input_upper,names=FALSE) #0.975,names=FALSE)
                lowervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                                   mucM[,hazardindexM[h],foodindex[i]],
                                                   sigcM[,hazardindexM[h],foodindex[i]]),
                                            input_lower,names=FALSE) #0.025,names=FALSE)
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(density(cmeanM,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3,main=paste(hazardnamesusedM[h],"in",foodnamesused[i]),xlab="Concentration+",ylab="",xlim=c(0,maxx)) 
              lines(density(cmedianM,from=0,to=maxx,n=2048),lwd=3)
              
              # mark data points and possible LOD and LOQ values for censored data:
              rug(exp(logcM[hazardindexM[h],foodindex[i],]),lwd=2.5,col="#D0006F",quiet=TRUE)
              rug(exp(logLOQM[hazardindexM[h],foodindex[i],]),lwd=4.5,col="green",quiet=TRUE)
              rug(exp(logLODM[hazardindexM[h],foodindex[i],]),lwd=4.5,col="#004F71",quiet=TRUE)
            }
            
            if(input_selectscale=="Logarithmic"){
              
              maxx <- quantile(qnorm(input_lim,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),
                               0.9,names=FALSE)
              minn <- quantile(qnorm(0.05,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),
                               0.5,names=FALSE)
              
              plot(density(mucM[,hazardindexM[h],foodindex[i]]/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(hazardnamesusedM[h],"in",foodnamesused[i]),
                   xlab=paste("log Concentration+ (", Unit1, "per", Unit2,")"),ylab="Probability density",xlim=c(minn/log(10),maxx/log(10))) 
              
              xvalues <- seq(minn/log(10),maxx/log(10),length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (mucM[,hazardindexM[h],foodindex[i]])/log(10),
                                                  (sigcM[,hazardindexM[h],foodindex[i]])/log(10) ),
                                            input_upper,names=FALSE)
                lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (mucM[,hazardindexM[h],foodindex[i]])/log(10), 
                                                  (sigcM[,hazardindexM[h],foodindex[i]])/log(10) ),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(density(mucM[,hazardindexM[h],foodindex[i]]/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(hazardnamesusedM[h],"in",foodnamesused[i]),xlab="log Concentration+",ylab="",xlim=c(minn/log(10),maxx/log(10))) 
              
              # mark data points and possible LOD and LOQ values for censored data:
              rug(logcM[hazardindexM[h],foodindex[i],]/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
              rug(logLOQM[hazardindexM[h],foodindex[i],]/log(10),lwd=4.5,col="green",quiet=TRUE)
              rug(logLODM[hazardindexM[h],foodindex[i],]/log(10),lwd=4.5,col="#004F71",quiet=TRUE)
            } # end of if logarithmic
            
          } # end of if density
          
          if(input_selectdist=="Cumulative"){ ## cumulative ----
            par(yaxt="s")
            cump <- seq(1,n_sim)
            cump <- cump/length(cump)
            
            if(input_selectscale=="Absolute"){
              cmeanM <- sort(exp(mucM[,hazardindexM[h],foodindex[i]]+0.5*sigcM[,hazardindexM[h],foodindex[i]]^2))
              cmedianM <- sort(exp(mucM[,hazardindexM[h],foodindex[i]]))
              
              maxx <- quantile(qlnorm(input_lim,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),
                               0.9,names=FALSE)
              plot(cmedianM[cmedianM<maxx],cump[cmedianM<maxx],lwd=3,main=paste(hazardnamesusedM[h],"in",foodnamesused[i]),
                   xlab=paste("Concentration+ (", Unit1, "per", Unit2,")"),ylab="Cumulative probability",xlim=c(0,maxx),ylim=c(0,1),type="l") 
              xvalues <- seq(0,maxx*1.1,length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(plnorm(xvalues[xv],
                                                   mucM[,hazardindexM[h],foodindex[i]],
                                                   sigcM[,hazardindexM[h],foodindex[i]]),
                                            input_upper,names=FALSE)
                lowervalues[xv] <- quantile(plnorm(xvalues[xv],
                                                   mucM[,hazardindexM[h],foodindex[i]],
                                                   sigcM[,hazardindexM[h],foodindex[i]]),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(cmedianM[cmedianM<maxx],cump[cmedianM<maxx],lwd=3) 
              lines(cmeanM[cmeanM<maxx],cump[cmeanM<maxx],lwd=3,col="#F7CE3C") 
              # mark data points and possible LOD and LOQ values for censored data:
              rug(exp(logcM[hazardindexM[h],foodindex[i],]),lwd=2.5,col="#D0006F",quiet=TRUE)
              rug(exp(logLOQM[hazardindexM[h],foodindex[i],]),lwd=4.5,col="green",quiet=TRUE)
              rug(exp(logLODM[hazardindexM[h],foodindex[i],]),lwd=4.5,col="#004F71",quiet=TRUE)
              lines(ecdf(
                c(exp(logcM[hazardindexM[h],foodindex[i],]),
                  exp(logLOQM[hazardindexM[h],foodindex[i],]),
                  exp(logLODM[hazardindexM[h],foodindex[i],]))
              ),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
              lines(ecdf(
                c(exp(logcM[hazardindexM[h],foodindex[i],]),
                  exp(logLOQLimM[hazardindexM[h],foodindex[i],]),
                  exp(logLODLimM[hazardindexM[h],foodindex[i],]-20))
              ),verticals=TRUE,do.points=FALSE,lwd=2,col="#004F71")
              
              
            } # end of if absolute
            
            if(input_selectscale=="Logarithmic"){
              
              maxx <- quantile(qnorm(input_lim,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),
                               0.9,names=FALSE)
              minn <- quantile(qnorm(0.05,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),
                               0.5,names=FALSE)
              
              plot(sort(mucM[,hazardindexM[h],foodindex[i]]/log(10)),cump,lwd=3,main=paste(hazardnamesusedM[h],"in",foodnamesused[i]),
                   xlab=paste("log Concentration+ (", Unit1, "per", Unit2,")"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),type="l") 
              xvalues <- seq(minn/log(10),maxx/log(10),length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (mucM[,hazardindexM[h],foodindex[i]])/log(10),
                                                  (sigcM[,hazardindexM[h],foodindex[i]])/log(10) ),
                                            input_upper,names=FALSE) 
                lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (mucM[,hazardindexM[h],foodindex[i]])/log(10),
                                                  (sigcM[,hazardindexM[h],foodindex[i]])/log(10) ),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(sort(mucM[,hazardindexM[h],foodindex[i]]/log(10)),cump,lwd=3,main=paste(hazardnamesusedM[h],"in",foodnamesused[i]),xlab="log Concentration+",ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l") 
              # mark data points and possible LOD and LOQ values for censored data:
              rug(logcM[hazardindexM[h],foodindex[i],]/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
              rug(logLOQM[hazardindexM[h],foodindex[i],]/log(10),lwd=4.5,col="green",quiet=TRUE)
              rug(logLODM[hazardindexM[h],foodindex[i],]/log(10),lwd=4.5,col="#004F71",quiet=TRUE)
              lines(ecdf(
                c(logcM[hazardindexM[h],foodindex[i],]/log(10),
                  logLOQM[hazardindexM[h],foodindex[i],]/log(10),
                  logLODM[hazardindexM[h],foodindex[i],]/log(10))
              ),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
              lines(ecdf(
                c(logcM[hazardindexM[h],foodindex[i],]/log(10),
                  logLOQLimM[hazardindexM[h],foodindex[i],]/log(10),
                  logLODLimM[hazardindexM[h],foodindex[i],]/log(10)-20)
              ),verticals=TRUE,do.points=FALSE,lwd=2,col="#004F71")
            } # end of if logarithmic
          } # end of if cumulative
          
          
          # legend ----
          # outside the figure, but onto the current plot, so it is part of the png file:
          mtext(paste("Prevalence of", hazardnamesusedM[h],"in",foodnamesused[i], ": ",round(quantile(100*pM[,hazardindexM[h],foodindex[i]],0.5,names=FALSE),1),
                      "% (posterior median). 95% uncertainty interval:",round(quantile(100*pM[,hazardindexM[h],foodindex[i]],0.025,names=FALSE),1),"% - ", round(quantile(100*pM[,hazardindexM[h],foodindex[i]],0.975,names=FALSE),1),"%"),
                side = 1, adj = 0,line=1, cex = 1,
                outer = TRUE)
          if(input_selectscale=="Absolute"){
            mtext(paste("95% uncertainty interval for the Q50% concentration+:", 
                        round(quantile(qlnorm(0.5,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),0.025,names=FALSE),2),"-", round(quantile(qlnorm(0.5,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),0.975,names=FALSE),2),", and for Q95%:",
                        round(quantile(qlnorm(0.95,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),0.025,names=FALSE),2),"-", round(quantile(qlnorm(0.95,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),0.975,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)}
          if(input_selectscale=="Logarithmic"){
            mtext(paste("95% uncertainty interval for the Q50% log(concentration+):", 
                        round(quantile(qnorm(0.5,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]])/log(10),0.025,names=FALSE),2),"-", round(quantile(qnorm(0.5,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]])/log(10),0.975,names=FALSE),2),", and for Q95%:",
                        round(quantile(qnorm(0.95,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]])/log(10),0.025,names=FALSE),2),"-", round(quantile(qnorm(0.95,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]])/log(10),0.975,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)}
          
          
        } else # end of if hazard-food modeled  
        {
          par(mar = c(0,0,0,0))
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.8, paste("The data for this food-hazard combination is not sufficient for modeling\n",
                                       "(e.g., concentration measurements (>LOQ) about the food-hazard combination are missing)."), 
               cex = 1.6, col = "#D0006F")
          par(mar = c(5, 4, 4, 2) + 0.1)
        }   
      }} # end of for nhusedM nfused
  } # end of if nhusedM nfused >0  
  
}

# Plot 2: Consumptions:---- 
## ---- distPlot2_1 -------- 
distPlot2_1 <- function(input_lim, food_consum, unit_consum, input_upper, input_lower, n_sim, input_selectdist, input_selectscale, foodnames, foodnamesused, nfused, foodindex,
                        nf, nr, nd,logs, logsw,
                        mus0,muw,logitp0,sigw,Ss,Ss0,constant.consum,Weight,osdlogsw1,osdlogsw2,input_modelchoice
) {
  # generate results based on inputs from ui.R:  
  # Consumption amounts
  
  par(oma = c(4, 1, 0, 1)) # Outer margins for legend
  par(mfrow=c(1,2),cex.lab=1.3,cex.main=1.3,yaxt="n")
  
  if(constant.consum=="TRUE"){ # plot constant consumption amounts in pie chart
  swcons<-numeric(nf)
  scons<-numeric(nf)
  labw <- character(nf)
  lab <- character(nf)
  for(i in 1:nf){ # through all the foods:
    # Set the constant value for consumptions, over all days:
    swconstant <- exp(logsw[,1:nd,i]) # per bodyweight
    sconstant <-  exp(logs[,1:nd,i])   # absolute
    swconstant[is.na(swconstant)==TRUE]<-0  # days when not consumed in data
    sconstant[is.na(sconstant)==TRUE]<-0   # days when not consumed in data
    # consumptions assumed to be constants over all days:
    swcons[i] <- round(mean(swconstant),2)
    scons[i] <- round(mean(sconstant),2)
    labw[i] <- paste(foodnames[i],swcons[i])
    lab[i]  <- paste(foodnames[i],scons[i])
  }
  
  pie(swcons[1:nf],labels=labw[1:nf],main="Consumption/bw",radius=0.5)
  text(0,1.2,paste("Consumption as constant /",as.character(mean(Weight))))  
  pie(scons[1:nf],labels=lab[1:nf],main="Absolute consumption",radius=0.5)
  text(0,1.2,"Consumption as constant")  
  }
  
  if(constant.consum=="FALSE"){ 
    
  OIM <- numeric() # observed individual mean consumptions
  
  p0 <- exp(logitp0)/(1+exp(logitp0)) # consumption frequencies in population  
  for(i in 1:nfused){
    Unit <- unit_consum[food_consum == foodnamesused[i]] # the measurement unit used for food consumptions
    Unit3 <- sub(".*p.", "", Unit) # Extract characters after pattern
    
    Vs <- numeric() # variances
    Vs0 <- numeric() # variances
    for(u in 1:n_sim){
      if(input_modelchoice=="Fixed variance"){
        Vs[u] <- osdlogsw2[foodindex[i]]^2  # observed value from data  
        Vs0[u] <- osdlogsw1[foodindex[i]]^2 # observed value from data
      } else {
        Vs[u] <- Ss[u,foodindex[i],foodindex[i]] 
        Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]]
      }
    }
    
    if(input_selectdist=="Density"){
      if(input_selectscale=="Absolute"){
        # distributions of chronic consumptions, on consumption days (absolute per bodyweight)
        meansmean <- exp(mus0[,foodindex[i]]+0.5*Vs+0.5*Vs0  )
        meansmedian <- exp(mus0[,foodindex[i]]+0.5*Vs )
        
        maxx <- quantile(qlnorm(input_lim,mus0[,foodindex[i]]+0.5*Vs,sqrt(Vs0) ),
                         0.9,names=FALSE)
        
        plot(density(meansmedian,from=0,to=maxx,n=2048),lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("C.consumption/bw+ (", Unit3,"per kg)"),ylab="Probability density",xlim=c(0,maxx)) 
        lines(density(meansmean,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3)
        
        xvalues <- seq(0,maxx,length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+0.5*Vs,sqrt(Vs0) ),
                                      input_upper,names=FALSE)
          
          lowervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+0.5*Vs,sqrt(Vs0) ),
                                      input_lower,names=FALSE) 
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(density(meansmedian,from=0,to=maxx,n=2048),lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="C.consumption/bw+",ylab="",xlim=c(0,maxx),type="l")
        lines(density(meansmean,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3)
        
        for(r in 1:nr){
          OIM[r]<- mean(exp(logsw[r,1:nd,foodindex[i]]),na.rm=TRUE) 
        } 
        OIM<-OIM[!is.na(OIM)]
        # mark data points: (observed individual means)
        rug(OIM,lwd=2.5,col="#D0006F",quiet=TRUE)
        
        
        # distribution of acute consumptions, on consumption days (absolute):
        smean <- exp(mus0[,foodindex[i]]+0.5*Vs0+0.5*Vs+muw+0.5*sigw^2)
        smedian <- exp(mus0[,foodindex[i]]+muw)
        
        maxx <- quantile(qlnorm(input_lim,mus0[,foodindex[i]]+muw,
                                sqrt(Vs0+Vs+sigw^2)),
                         0.9,names=FALSE)
        
        plot(density(smedian,from=0,to=maxx,n=2048),lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("A.consumption+ (", Unit3,")"),ylab="",xlim=c(0,maxx)) 
        lines(density(smean,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3)
        
        xvalues <- seq(0,maxx,length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+muw,
                                             sqrt(Vs0+Vs+sigw^2)),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+muw,
                                             sqrt(Vs0+Vs+sigw^2)),
                                      input_lower,names=FALSE) 
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(density(smedian,from=0,to=maxx,n=2048),lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="A.consumption+",ylab="",xlim=c(0,maxx),type="l") 
        lines(density(smean,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3)
        
        # mark data points: (individual acute consumptions)
        rug(exp(logs[1:nr,1:nd,foodindex[i]]),lwd=2.5,col="#D0006F",quiet=TRUE)
        
      } # end of if absolute
      
      if(input_selectscale=="Logarithmic"){
        
        # distributions of chronic consumptions, on consumption days (log per bodyweight)
        musmean <- mus0[,foodindex[i]]+0.5*Vs
        
        maxx <- quantile(qnorm(input_lim,mus0[,foodindex[i]]+0.5*Vs,sqrt(Vs0) ),
                         0.9,names=FALSE)
        minn <- quantile(qnorm(0.05,mus0[,foodindex[i]]+0.5*Vs,sqrt(Vs0) ),
                         0.5,names=FALSE)
        
        plot(density(musmean/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("log C.consumption/bw+ (", Unit3,"per kg)"),ylab="Probability density",xlim=c(minn/log(10),maxx/log(10))) 
        
        xvalues <- seq(minn/log(10),maxx/log(10),length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(dnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+0.5*Vs)/log(10),
                                            sqrt(Vs0)/log(10) ),
                                      input_upper,names=FALSE)
          lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+0.5*Vs)/log(10),
                                            sqrt(Vs0)/log(10) ),
                                      input_lower,names=FALSE) 
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(density(musmean/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="log C.consumption/bw+",ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        
        for(r in 1:nr){
          OIM[r]<- log(mean(exp(logsw[r,1:nd,foodindex[i]]),na.rm=TRUE)) 
        } 
        OIM<-OIM[!is.na(OIM)]
        # mark data points: (observed individual means, in log-scale)
        rug(OIM/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
        
        
        # distribution of acute consumptions, on consumption days (log):
        logsmean <- mus0[,foodindex[i]]+muw
        
        maxx <- quantile(qnorm(input_lim,mus0[,foodindex[i]]+muw,
                               sqrt(Vs0+Vs+sigw^2)),
                         0.9,names=FALSE)
        minn <- quantile(qnorm(0.05,mus0[,foodindex[i]]+muw,
                               sqrt(Vs0+Vs+sigw^2)),
                         0.5,names=FALSE)
        
        plot(density(logsmean/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("log A.consumption+ (", Unit3,")"),ylab="",xlim=c(minn/log(10),maxx/log(10))) 
        
        # mark data points: (individual acute consumptions, in log-scale)
        rug(logs[1:nr,1:nd,foodindex[i]]/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
        
        xvalues <- seq(minn/log(10),maxx/log(10),length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(dnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+muw)/log(10),
                                            (sqrt(Vs0+Vs+sigw^2))/log(10) ),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+muw)/log(10),
                                            (sqrt(Vs0+Vs+sigw^2))/log(10) ),
                                      input_lower,names=FALSE) 
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(density(logsmean/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="log A.consumption+",ylab="",xlim=c(minn/log(10),maxx/log(10))) 

        
      } # end of if logarithmic
    } # end of if density
    
    if(input_selectdist=="Cumulative"){
      par(yaxt="s")
      cump <- seq(1,n_sim)
      cump <- cump/length(cump)
      if(input_selectscale=="Absolute"){
        
        # distributions of chronic consumptions (absolute per bodyweight)
        meansmean <- sort(exp(mus0[,foodindex[i]]+0.5*Vs +0.5*Vs0 ))
        meansmedian <- sort(exp(mus0[,foodindex[i]]+0.5*Vs ))
        
        maxx <- quantile(qlnorm(input_lim,mus0[,foodindex[i]]+0.5*Vs,sqrt(Vs0) ),
                         0.9,names=FALSE)
        
        plot(meansmean,cump,col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("C.consumption/bw+ (", Unit3,"per kg)"),ylab="Cumulative probability",xlim=c(0,maxx),type="l")
        lines(meansmedian,cump,lwd=3)
        
        xvalues <- seq(0,maxx,length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(plnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+0.5*Vs,sqrt(Vs0) ),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(plnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+0.5*Vs,sqrt(Vs0) ),
                                      input_lower,names=FALSE) 
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(meansmean,cump,col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="C.consumption/bw+",ylab="",xlim=c(0,maxx),type="l")
        lines(meansmedian,cump,lwd=3)
        
        for(r in 1:nr){
          OIM[r]<- mean(exp(logsw[r,1:nd,foodindex[i]]),na.rm=TRUE) 
        } 
        OIM<-OIM[!is.na(OIM)]
        # mark data points: (observed individual means)
        rug(OIM,lwd=2.5,col="#D0006F",quiet=TRUE)
        lines(ecdf(OIM),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
        
        
        # distribution of acute consumptions (absolute):
        smean <- sort(exp(mus0[,foodindex[i]]+0.5*Vs0+0.5*Vs+muw+0.5*sigw^2))
        smedian <- sort(exp(mus0[,foodindex[i]]+muw))
        
        maxx <- quantile(qlnorm(input_lim,mus0[,foodindex[i]]+muw,
                                sqrt(Vs0+Vs+sigw^2)),
                         0.9,names=FALSE)
        
        
        plot(smean,cump,col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("A.consumption+ (", Unit3,")"),ylab="",xlim=c(0,maxx),type="l") 
        lines(smedian,cump,lwd=3)
        
        xvalues <- seq(0,maxx,length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(plnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+muw,
                                             sqrt(Vs0+Vs+sigw^2)),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(plnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+muw,
                                             sqrt(Vs0+Vs+sigw^2)),
                                      input_lower,names=FALSE)
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(smean,cump,col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="A.consumption+",ylab="",xlim=c(0,maxx),type="l") 
        lines(smedian,cump,lwd=3)
        
        # mark data points: (individual acute consumptions)
        rug(exp(logs[1:nr,1:nd,foodindex[i]]),lwd=2.5,col="#D0006F",quiet=TRUE)
        lines(ecdf(exp(logs[1:nr,1:nd,foodindex[i]])),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
        
      } # end of if absolute
      
      if(input_selectscale=="Logarithmic"){
        # distributions of chronic consumptions (log per bodyweight)
        musmean <- sort(mus0[,foodindex[i]]+0.5*Vs)
        
        maxx <- quantile(qnorm(input_lim,mus0[,foodindex[i]]+0.5*Vs, sqrt(Vs0) ),
                         0.9,names=FALSE)
        minn <- quantile(qnorm(0.05,mus0[,foodindex[i]]+0.5*Vs, sqrt(Vs0) ),
                         0.5,names=FALSE)
        
        plot(musmean/log(10),cump,lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("log C.consumption/bw+ (", Unit3,"per kg)"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        xvalues <- seq(minn/log(10),maxx/log(10),length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+0.5*Vs)/log(10),
                                            sqrt(Vs0)/log(10) ),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+0.5*Vs)/log(10),
                                            sqrt(Vs0)/log(10) ),
                                      input_lower,names=FALSE) 
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(musmean/log(10),cump,lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="log C.consumption/bw+",ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        
        for(r in 1:nr){
          OIM[r]<- log(mean(exp(logsw[r,1:nd,foodindex[i]]),na.rm=TRUE)) 
        } 
        OIM<-OIM[!is.na(OIM)]
        # mark data points: (observed individual means, in log-scale)
        rug(OIM/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
        lines(ecdf(OIM/log(10)),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
        
        
        # distribution of acute consumptions (log):
        logsmean <- sort(mus0[,foodindex[i]]+muw)
        maxx <- quantile(qnorm(input_lim,mus0[,foodindex[i]]+muw,
                               sqrt(Vs0+Vs+sigw^2)),
                         0.9,names=FALSE)
        minn <- quantile(qnorm(0.05,mus0[,foodindex[i]]+muw,
                               sqrt(Vs0+Vs+sigw^2)),
                         0.5,names=FALSE)
        plot(logsmean/log(10),cump,lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("log A.consumption+ (", Unit3,")"),ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        xvalues <- seq(minn/log(10),maxx/log(10),length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+muw)/log(10),
                                            (sqrt(Vs0+Vs+sigw^2))/log(10) ),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+muw)/log(10),
                                            (sqrt(Vs0+Vs+sigw^2))/log(10) ),
                                      input_lower,names=FALSE)
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(logsmean/log(10),cump,lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="log A.consumption+",ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        
        # mark data points: (individual acute consumptions, in log-scale)
        rug(logs[1:nr,1:nd,foodindex[i]]/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
        lines(ecdf(logs[1:nr,1:nd,foodindex[i]]/log(10)),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
        
      } # end of if logarithmic
    } # end of if cumulative
    
     # legend ----
  # outside the figure, but onto the current plot, so it is part of the png file:
  mtext(paste("Consumption frequency of", foodnamesused[i], ": ", round(quantile(100*p0[,foodindex[i]],0.5,names=FALSE),1),
              "% (posterior median)."),
        side = 1, adj = 0,line=0, cex = 1,
        outer = TRUE)
  mtext(paste("95% uncertainty interval for the consumption frequency: ", 
              round(quantile(100*p0[,foodindex[i]],0.025,names=FALSE),1),"%-", round(quantile(100*p0[,foodindex[i]],0.975,names=FALSE),1),"%"),
        side = 1, adj = 0,line=1, cex = 1,
        outer = TRUE)
  if(input_selectscale=="Absolute"){
  mtext(paste("Q50% C.consumption/bw+ for", foodnamesused[i], ": ", round(quantile(exp(mus0[,foodindex[i]]+0.5*Vs),0.5,names=FALSE),2),
              "(posterior median)."),
        side = 1, adj = 0,line=2, cex = 1,
        outer = TRUE)
  mtext(paste("Q50% A.consumption+ for", foodnamesused[i], ": ", round(quantile(exp(mus0[,foodindex[i]]+muw),0.5,names=FALSE),2),
              "(posterior median)."),
        side = 1, adj = 0,line=3, cex = 1,
        outer = TRUE) 
  }
  if(input_selectscale=="Logarithmic"){
    mtext(paste("Q50% log(C.consumption/bw+) for", foodnamesused[i], ": ", round(quantile((mus0[,foodindex[i]]+0.5*Vs)/log(10),0.5,names=FALSE),2),
                "(posterior median)."),
          side = 1, adj = 0,line=2, cex = 1,
          outer = TRUE)
    mtext(paste("Q50% log(A.consumption+) for", foodnamesused[i], ": ", round(quantile((mus0[,foodindex[i]]+muw)/log(10),0.5,names=FALSE),2),
                "(posterior median)."),
          side = 1, adj = 0,line=3, cex = 1,
          outer = TRUE) 
  }
  } # end of for nfused
  
  
 
  } # end of constant.consum == FALSE
  #} # end of if selectresults == "Consumptions"
}


# Plot 3: Exposures:----
## ---- distPlot3_1 --------
distPlot3_1 <- function(input_lim, unit_concen, hazard_concen, input_upper, input_lower, n_sim, 
                        input_selectdist, input_selectscale, input_modelchoice, input_modelchoice2, 
                        foodnamesused, nfused, foodindex, hazardnames,
                        nhused,  hazardnamesusedK, hazardnamesusedM,
                        nhusedK, nhusedM, hazardindexK, hazardindexM, Rall, Pall,nhK,nhM,nf,nr,nd,
                        nexactK, nexactM, 
                        logs,logsw,logcK,logLOQK,logLODK,logLOQLimK,logLODLimK, logcM,logLOQM,logLODM,logLOQLimM,logLODLimM,
                        logitp0,mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                        Ss,Ss0,Sp,Weight,constant.consum,limitexpoK,limitexpoM,osdlogsw1,osdlogsw2
) {
  # generate results based on inputs from ui.R: 
  # Exposures----
  
  par(oma = c(5, 1, 0, 1),cex.lab=1.3,cex.main=1.3) # Outer margins for legend
  
  # CHEMICAL EXPOSURES: ----
  
  if((nhusedK>0)&(nfused>0)){
    RK = matrix(NA,nf,nhK) # factors for concentrations
    RK[1:nf,1:nhK] = Rall[1:nf,is.element(hazardnames,hazardnamesusedK)]
    logRK = log(RK)
    PK = matrix(NA,nf,nhK) # factors for prevalence
    PK[1:nf,1:nhK] = Pall[1:nf,is.element(hazardnames,hazardnamesusedK)]
    
    for(h in 1:nhusedK){
      Unit <- unit_concen[hazard_concen == hazardnamesusedK[h]] # the measurement unit used for hazard concentration
      Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
      
      for(i in 1:nfused){
        if(nexactK[hazardindexK[h],foodindex[i]]>0){ # this hazard-food is modeled
          ##############################################################################
          if(constant.consum==FALSE){
          p0 <- exp(logitp0)/(1+exp(logitp0))
          Vs <- numeric() # variances between days (servings), over actual positives
          Vs0 <- numeric() # variances between individual means, over actual positives 
          for(u in 1:n_sim){
            if(input_modelchoice=="Fixed variance"){
              Vs[u] <- osdlogsw2[foodindex[i]]^2  # observed value from data
              Vs0[u] <- osdlogsw1[foodindex[i]]^2 # observed value from data
              } else { 
              Vs[u] <- Ss[u,foodindex[i],foodindex[i]] 
              Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]]
              }
          }  
          
          ################################
          # absolute value:
          if(input_selectscale=="Absolute"){
          # positive chronic exposures, variability 95% quantile:
          chronicqlnormpos95K <- qlnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                                +mus0[,foodindex[i]]
                                +mucK[,hazardindexK[h],foodindex[i]]
                                +0.5*Vs
                                +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                sqrt(Vs0))
          # posterior quantiles of chronic variability 95% quantile:
          chronicq95_50 <- round(quantile(chronicqlnormpos95K,0.5,names=FALSE),3)
          chronicq95_95 <- round(quantile(chronicqlnormpos95K,0.95,names=FALSE),3)
          chronicq95_05 <- round(quantile(chronicqlnormpos95K,0.05,names=FALSE),3)
          
          # positive acute exposures, variability 95% quantile:
          acuteqlnormpos95K <- qlnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                                      +mus0[,foodindex[i]]
                                      +mucK[,hazardindexK[h],foodindex[i]],
                                      sqrt(Vs0+Vs+sigcK[,hazardindexK[h],foodindex[i]]^2))
          # posterior quantiles of acute variability 95% quantile:
          acuteq95_50 <- round(quantile(acuteqlnormpos95K,0.5,names=FALSE),3)
          acuteq95_95 <- round(quantile(acuteqlnormpos95K,0.95,names=FALSE),3)
          acuteq95_05 <- round(quantile(acuteqlnormpos95K,0.05,names=FALSE),3)
          }
          # logarithmic value:
          if(input_selectscale=="Logarithmic"){
          # positive chronic exposures, variability 95% quantile:
          chronicqnormpos95K <- qnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                                 +mus0[,foodindex[i]]
                                 +mucK[,hazardindexK[h],foodindex[i]]
                                 +0.5*Vs
                                 +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                 sqrt(Vs0))/log(10)
          # posterior quantiles of chronic variability 95% quantile:
          chronicql95_50 <- round(quantile(chronicqnormpos95K,0.5,names=FALSE),3)
          chronicql95_95 <- round(quantile(chronicqnormpos95K,0.95,names=FALSE),3)
          chronicql95_05 <- round(quantile(chronicqnormpos95K,0.05,names=FALSE),3)
          
          # positive acute exposures, variability 95% quantile:
          acuteqnormpos95K <- qnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                                    +mus0[,foodindex[i]]
                                    +mucK[,hazardindexK[h],foodindex[i]],
                                    sqrt(Vs0+Vs+sigcK[,hazardindexK[h],foodindex[i]]^2))/log(10)
          # posterior quantiles of acute variability 95% quantile:
          acuteql95_50 <- round(quantile(acuteqnormpos95K,0.5,names=FALSE),3)
          acuteql95_95 <- round(quantile(acuteqnormpos95K,0.95,names=FALSE),3)
          acuteql95_05 <- round(quantile(acuteqnormpos95K,0.05,names=FALSE),3)
          }
          
          ##############################################
  
          # chronic exposure (hazard i, food j) over all days, all servings (including zeros):
          V <- 2000 # variability simulations i.e individual chronic exposures
          logitpconsume <- numeric()
          pconsume <- numeric()
          cmeanpos <- numeric()
          cpos <- numeric()
          chronicqtotal95 <- numeric()
          chronicqltotal95 <- numeric()
          acuteqtotal95 <- numeric()
          acuteqltotal95 <- numeric()
          qpos95 <- numeric()
          qlpos95 <- numeric()
          
          
          for(u in 1:n_sim){ 
            # simulate variability for V individuals, 
            # per each uncertain parameter:
            if((input_modelchoice=="Independent days")|(input_modelchoice=="Fixed variance") ){   
              if(input_modelchoice2 =="Yes"){ 
                logitpconsume[1:V] <-  rnorm(V,logitp0[u,foodindex[i]],sqrt(Sp[u,foodindex[i],foodindex[i]]))
              }
              if(input_modelchoice2 == "No"){
                logitpconsume[1:V] <- rep(logitp0[u,foodindex[i]],V)   
              }
              pconsume[1:V] <- exp(logitpconsume[1:V])/(1+exp(logitpconsume[1:V]))  
            }
            if(input_modelchoice=="Dependent days"){
              pconsume[1:V] <- rep(p0[u,foodindex[i]],V)
            }
            
            
            # evaluate the 95% quantile of the exposure distribution 
            # (over V individuals for chronic exposure, over V individual days for acute exposure) including all days 
            # (not only positively contaminated consumptions)
            
            # variability of mean positive exposures:
            cmeanpos[1:V] <- rlnorm(V,logRK[foodindex[i],hazardindexK[h]]
                                    +mus0[u,foodindex[i]]
                                    +mucK[u,hazardindexK[h],foodindex[i]]
                                    +0.5*Vs[u]
                                    +0.5*sigcK[u,hazardindexK[h],foodindex[i]]^2,
                                    sqrt(Vs0[u]))
            # variability of individual (acute) positive exposures ("servings"):
            cpos[1:V] <- rlnorm(V,logRK[foodindex[i],hazardindexK[h]]
                                    +mus0[u,foodindex[i]]
                                    +mucK[u,hazardindexK[h],foodindex[i]],
                                    sqrt(Vs0[u]+Vs[u]+sigcK[u,hazardindexK[h],foodindex[i]]^2))
            
            # absolute, over all days:
            if(input_selectscale=="Absolute"){
            chronicqtotal95[u]<-quantile(
              pconsume[1:V]*
                pK[u,hazardindexK[h],foodindex[i]]*
                PK[foodindex[i],hazardindexK[h]]*
                cmeanpos[1:V],0.95,names=FALSE)
            acuteqtotal95[u]<-quantile(
              rbinom(V,1,pconsume[1:V]*pK[u,hazardindexK[h],foodindex[i]]*PK[foodindex[i],hazardindexK[h]])*
                cpos[1:V],0.95,names=FALSE)
            }
            # logarithmic, over all days:
            if(input_selectscale=="Logarithmic"){
            chronicqltotal95[u]<-quantile(
              log10(pconsume[1:V]*
                      pK[u,hazardindexK[h],foodindex[i]]*
                      PK[foodindex[i],hazardindexK[h]]*
                      cmeanpos[1:V]),0.95,names=FALSE)
            acuteqltotal95[u]<-quantile(
              log10(rbinom(V,1,pconsume[1:V]*pK[u,hazardindexK[h],foodindex[i]]*PK[foodindex[i],hazardindexK[h]])*
                      cpos[1:V]),0.95,names=FALSE)
            }
            
            # positive chronic exposure variability quantile:
            qpos95[u] <- qlnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                                +mus0[u,foodindex[i]]
                                +mucK[u,hazardindexK[h],foodindex[i]]
                                +0.5*Vs[u]
                                +0.5*+sigcK[u,hazardindexK[h],foodindex[i]]^2,
                                sqrt(Vs0[u]))
            qlpos95[u] <- qnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                                 +mus0[u,foodindex[i]]
                                 +mucK[u,hazardindexK[h],foodindex[i]]
                                 +0.5*Vs[u]
                                 +0.5*+sigcK[u,hazardindexK[h],foodindex[i]]^2,
                                 sqrt(Vs0[u]))/log(10)
          } # end of for u
          
          # positive chronic exposures, 
          # posterior quantiles of 95% variability quantile:
          chronicq95_05 <- round(quantile(qpos95,0.05,names=FALSE,na.rm=TRUE),3)
          chronicq95_50 <- round(quantile(qpos95,0.5,names=FALSE,na.rm=TRUE),3)
          chronicq95_95 <- round(quantile(qpos95,0.95,names=FALSE,na.rm=TRUE),3)
          chronicql95_05 <- round(quantile(qlpos95,0.05,names=FALSE,na.rm=TRUE),3)
          chronicql95_50 <- round(quantile(qlpos95,0.5,names=FALSE,na.rm=TRUE),3)
          chronicql95_95 <- round(quantile(qlpos95,0.95,names=FALSE,na.rm=TRUE),3)
          
          ##Density----
          if(input_selectdist=="Density"){ 
            ############## exposure.chronicKbw
            # plot posterior of the mean & median exposure/bw 
            # (expected chronic exposure for anyone)
            ###Absolute----
            if(input_selectscale=="Absolute"){
              meanexposurechronic <- exp(logRK[foodindex[i],hazardindexK[h]]+
                                    mus0[,foodindex[i]]+
                                    mucK[,hazardindexK[h],foodindex[i]]+  
                                    0.5*Vs +
                                    0.5*sigcK[,hazardindexK[h],foodindex[i]]^2+
                                    0.5*Vs0 )
              maxxchronic <- quantile(qlnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                      +mus0[,foodindex[i]]
                                      +mucK[,hazardindexK[h],foodindex[i]]
                                      +0.5*Vs
                                      +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                      sqrt(Vs0) ),
                               0.99,names=FALSE)
              medianexposurechronic <- exp(logRK[foodindex[i],hazardindexK[h]]+
                                      mus0[,foodindex[i]]+
                                      mucK[,hazardindexK[h],foodindex[i]]+  
                                      0.5*Vs+
                                      0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)
              
              plot(density(medianexposurechronic,from=0,to=maxxchronic,n=2048),main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("C.exposure/bw+  (", Unit1,"per kg)"),ylab="Probability density",xlim=c(0,maxxchronic),lwd=3) 
              
              xvalueschronic <- seq(0,maxxchronic,length=100)
              chronicuppervalues <- numeric()
              chroniclowervalues <- numeric()
              for(xv in 1:100){
                chronicuppervalues[xv] <- quantile(dlnorm(xvalueschronic[xv],logRK[foodindex[i],hazardindexK[h]]
                                                          +mus0[,foodindex[i]]
                                                          +mucK[,hazardindexK[h],foodindex[i]]
                                                          +0.5*Vs
                                                          +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                                          sqrt(Vs0)),
                                            input_upper,names=FALSE) 
                chroniclowervalues[xv] <- quantile(dlnorm(xvalueschronic[xv],logRK[foodindex[i],hazardindexK[h]]
                                                          +mus0[,foodindex[i]]
                                                          +mucK[,hazardindexK[h],foodindex[i]]
                                                          +0.5*Vs
                                                          +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                                          sqrt(Vs0)),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalueschronic,xvalueschronic[100:1]),c(chronicuppervalues,chroniclowervalues[100:1]),col="#CEB888")
              
              lines(density(meanexposurechronic,from=0,to=maxxchronic,n=2048),col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="C.exposure/bw+",ylab="",xlim=c(0,maxxchronic),lwd=3)
              lines(density(medianexposurechronic,from=0,to=maxxchronic,n=2048),lwd=3)
            
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Q95% for C.exposure+: ",chronicq95_50,  
                          "(posterior median). 90% uncertainty interval for the Q95%:", chronicq95_05,"-", chronicq95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Q95% for C.exposure: ",round(quantile(chronicqtotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q95%: ", round(quantile(chronicqtotal95,0.05,names=FALSE),3),"-",round(quantile(chronicqtotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if absolute
            
            ##Logarithmic----
            if(input_selectscale=="Logarithmic"){
              meanlogexposurechronic <- logRK[foodindex[i],hazardindexK[h]]+
                                        mus0[,foodindex[i]]+
                                        mucK[,hazardindexK[h],foodindex[i]]+
                                        0.5*Vs+
                                        0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
              
              maxxchronic <- quantile(qnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*Vs
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0)),
                               0.99,names=FALSE)
              minnchronic <- quantile(qnorm(0.01,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*Vs
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0)),
                               0.05,names=FALSE)
              
              plot(density(meanlogexposurechronic/log(10),from=minnchronic/log(10),to=maxxchronic/log(10),n=2048),col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("log(C.exposure/bw+  (", Unit1,"per kg))"),ylab="Probability density",xlim=c(minnchronic/log(10),maxxchronic/log(10)),lwd=3) 
                           
              
              xvalueschronic <- seq(minnchronic/log(10),maxxchronic/log(10),length=100)
              chronicuppervalues <- numeric()
              chroniclowervalues <- numeric()
              for(xv in 1:100){
                chronicuppervalues[xv] <- quantile(dnorm(xvalueschronic[xv],(logRK[foodindex[i],hazardindexK[h]]
                                                   +mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*Vs
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_upper,names=FALSE) 
                chroniclowervalues[xv] <- quantile(dnorm(xvalueschronic[xv],(logRK[foodindex[i],hazardindexK[h]]
                                                   +mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*Vs
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_lower,names=FALSE)
              }
              polygon(c(xvalueschronic,xvalueschronic[100:1]),c(chronicuppervalues,chroniclowervalues[100:1]),col="#CEB888")
              lines(density(meanlogexposurechronic/log(10),from=minnchronic/log(10),to=maxxchronic/log(10),n=2048),col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="log (C.exposure/bw+)",ylab="",xlim=c(minnchronic/log(10),maxxchronic/log(10)),lwd=3)
            
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Q95% for log(C.exposure+): ",chronicql95_50,  
                          "(posterior median). 90% uncertainty interval for the Q95%: ", chronicql95_05,"-", chronicql95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Q95% for log(C.exposure): ",round(quantile(chronicqltotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q95%: ", round(quantile(chronicqltotal95,0.05,names=FALSE),3),"-",round(quantile(chronicqltotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
              } # end of if logarithmic
          } # end of if density
          
          #Cumulative----
          if(input_selectdist=="Cumulative"){
            par(mfrow=c(2,1))
            par(yaxt="s")
            cump <- seq(1,n_sim)
            cump <- cump/length(cump)
            ############## exposure.chronicKbw
            # plot posterior of the mean & median exposure/bw 
            # (expected chronic exposure for anyone)
            ##Absolute----
            if(input_selectscale=="Absolute"){
              
              # mean of chronic exposures
              meanexposurechronic <- sort(
                exp(logRK[foodindex[i],hazardindexK[h]]
                    +mus0[,foodindex[i]]
                    +mucK[,hazardindexK[h],foodindex[i]]
                    +0.5*Vs
                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                    +0.5*Vs0) )
              medianexposurechronic <- sort(
                exp(logRK[foodindex[i],hazardindexK[h]]
                    +mus0[,foodindex[i]]
                    +mucK[,hazardindexK[h],foodindex[i]]
                    +0.5*Vs
                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2))
              # mean of acute exposures
              meanexposureacute <- sort(exp(logRK[foodindex[i],hazardindexK[h]]
                                            +mus0[,foodindex[i]]
                                            +mucK[,hazardindexK[h],foodindex[i]]
                                            +0.5*Vs
                                            +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                            +0.5*Vs0 ))
              medianexposureacute <- sort(exp(logRK[foodindex[i],hazardindexK[h]]+
                                                mus0[,foodindex[i]]+
                                                mucK[,hazardindexK[h],foodindex[i]]))
              # plot range max for chronic:
              maxxchronic <- quantile(qlnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                      +mus0[,foodindex[i]]
                                      +mucK[,hazardindexK[h],foodindex[i]]
                                      +0.5*Vs
                                      +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                      sqrt(Vs0) ),
                               0.95,names=FALSE)
              # plot range max for acute:
              maxxacute <- quantile(qlnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                      +mus0[,foodindex[i]]
                                      +mucK[,hazardindexK[h],foodindex[i]],
                                      sqrt(Vs0+Vs+sigcK[,hazardindexK[h],foodindex[i]]^2) ),
                               0.95,names=FALSE)
              xvalueschronic <- seq(0,maxxchronic,length=100) # plot range for chronic
              xvaluesacute <- seq(0,maxxacute,length=100) # plot range for acute
              
              # uncertainty for mean and median-chronic exposure:
              plot(meanexposurechronic[meanexposurechronic<maxxchronic],cump[meanexposurechronic<maxxchronic],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("C.exposure/bw+  (", Unit1,"per kg)"),ylab="Cumulative probability",xlim=c(0,maxxchronic),ylim=c(0,1),lwd=3,type="l")
              lines(medianexposurechronic[medianexposurechronic<maxxchronic],cump[medianexposurechronic<maxxchronic],lwd=3)   
              
             
              # uncertainty bounds for chronic and acute exposure distributions:
              chronicuppervalues <- numeric()
              chroniclowervalues <- numeric()
              acuteuppervalues <- numeric()
              acutelowervalues <- numeric()
            
              for(xv in 1:100){
                # uncertainty bounds for acute exposure distribution:
                acuteuppervalues[xv] <- quantile(plnorm(xvaluesacute[xv],logRK[foodindex[i],hazardindexK[h]]
                                        +mus0[,foodindex[i]]
                                        +mucK[,hazardindexK[h],foodindex[i]],
                                        sqrt(Vs+Vs0+sigcK[,hazardindexK[h],foodindex[i]]^2) ),input_upper,names=FALSE)
                acutelowervalues[xv] <- quantile(plnorm(xvaluesacute[xv],logRK[foodindex[i],hazardindexK[h]]
                                        +mus0[,foodindex[i]]
                                        +mucK[,hazardindexK[h],foodindex[i]],
                                        sqrt(Vs+Vs0+sigcK[,hazardindexK[h],foodindex[i]]^2) ),input_lower,names=FALSE) 
                
                # uncertainty bounds for chronic exposure distribution:
                chronicuppervalues[xv] <- quantile(plnorm(xvalueschronic[xv],
                                                   logRK[foodindex[i],hazardindexK[h]]
                                                   +mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*Vs
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                                   sqrt(Vs0)),input_upper,names=FALSE) 
                chroniclowervalues[xv] <- quantile(plnorm(xvalueschronic[xv],logRK[foodindex[i],hazardindexK[h]]
                                                   +mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*Vs
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                                   sqrt(Vs0)),input_lower,names=FALSE) 
              }
              # uncertainty bounds for chronic exposure distribution:
              polygon(c(xvalueschronic,xvalueschronic[100:1]),c(chronicuppervalues,chroniclowervalues[100:1]),col="#CEB888")
              
              # plot empirically generated cumulative chronic exposure distributions
              OIM <- numeric() # Observed Individual Means
              for(r in 1:nr){
                OIM[r]<- mean(exp(logsw[r,1:nd,foodindex[i]]),na.rm=TRUE) 
              } 
              OIM<-OIM[!is.na(OIM)]
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                        logLOQK[hazardindexK[h],foodindex[i],],
                                        logLODK[hazardindexK[h],foodindex[i],]))
              # and using lower bounds:
              concentrationsLB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                        logLOQLimK[hazardindexK[h],foodindex[i],],
                                        logLODLimK[hazardindexK[h],foodindex[i],]-20))
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              
              for(resample in 1:40){   
                # create 40 replicate ('bootstrap') data with original nsample:
                sampleOIM <- sample(OIM,length(OIM),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 2000 simulations from each replicated data:
                sampleOIM <- sample(sampleOIM,2000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
                lines(ecdf(sampleOIM*mean(sampleconUB)*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxchronic),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(sampleOIM*mean(sampleconLB)*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxchronic),lwd=1,lty=3,col="#004F71")
              }
              # uncertainty for mean and median-chronic exposure:
              lines(meanexposurechronic[meanexposurechronic<maxxchronic],cump[meanexposurechronic<maxxchronic],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="C.exposure/bw+",ylab="",xlim=c(0,maxxchronic),lwd=3) 
              lines(medianexposurechronic[medianexposurechronic<maxxchronic],cump[medianexposurechronic<maxxchronic],xlim=c(0,maxxchronic),lwd=3)
              
              ##################
              # plot a new frame for acute exposures:
              plot(meanexposureacute[meanexposureacute<maxxacute],cump[meanexposureacute<maxxacute],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(acute)"),
                   xlab=paste("A.exposure/bw+  (", Unit1,"per kg)"),ylab="Cumulative probability",xlim=c(0,maxxacute),ylim=c(0,1),lwd=3,type="l")
              lines(medianexposureacute[medianexposureacute<maxxacute],cump[medianexposureacute<maxxacute],lwd=3)  
              # uncertainty bounds for acute exposure distributions:
              polygon(c(xvaluesacute,xvaluesacute[100:1]),c(acuteuppervalues,acutelowervalues[100:1]),col="#CEB888")
              
              # plot empirically generated cumulative acute exposure distributions
              servings <- exp(logsw[1:nr,1:nd,foodindex[i]])
              servings <- servings[!is.na(servings)] # consumption days 
              
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                        logLOQK[hazardindexK[h],foodindex[i],],
                                        logLODK[hazardindexK[h],foodindex[i],]))
              # and using lower bounds:
              concentrationsLB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                        logLOQLimK[hazardindexK[h],foodindex[i],],
                                        logLODLimK[hazardindexK[h],foodindex[i],]-20))
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              
              for(resample in 1:40){   
                # create 40 replicate ('bootstrap') data with original nsample:
                sampleser <- sample(servings,length(servings),replace=TRUE) 
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 2000 simulations from each replicated data:
                sampleser <- sample(sampleser,2000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
                lines(ecdf(sampleser*sampleconUB*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxacute),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(sampleser*sampleconLB*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxacute),lwd=1,lty=3,col="#004F71")
              }
              # uncertainty for mean and median exposure:
              lines(meanexposureacute[meanexposureacute<maxxacute],cump[meanexposureacute<maxxacute],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="C.exposure/bw+",ylab="",xlim=c(0,maxxacute),lwd=3) 
              lines(medianexposureacute[medianexposureacute<maxxacute],cump[medianexposureacute<maxxacute],xlim=c(0,maxxacute),lwd=3)
              
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Q95% for C.exposure+: ",chronicq95_50,  
                          "(posterior median). 90% uncertainty interval for the Q95%: ", chronicq95_05,"-", chronicq95_95),
                    side = 1, adj = 0,line=1, cex = 1,
                    outer = TRUE)
              mtext(paste("Q95% for C.exposure: ",round(quantile(chronicqtotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q95%: ", round(quantile(chronicqtotal95,0.05,names=FALSE),3),"-",round(quantile(chronicqtotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Q95% for A.exposure+: ",acuteq95_50,  
                          "(posterior median). 90% uncertainty interval for the Q95%: ", acuteq95_05,"-", acuteq95_95),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              mtext(paste("Q95% for A.exposure: ",round(quantile(acuteqtotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q95%: ", round(quantile(acuteqtotal95,0.05,names=FALSE),3),"-",round(quantile(acuteqtotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=4, cex = 1,
                    outer = TRUE)
              
              
            } # end of if absolute
            
            ##Logarithmic----
            if(input_selectscale=="Logarithmic"){
              # mean of the consumer specific means, in log-scale:
              meanlogexposurechronic <- sort(logRK[foodindex[i],hazardindexK[h]]
                +mus0[,foodindex[i]]
                +mucK[,hazardindexK[h],foodindex[i]]
                +0.5*Vs
                +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)
              # overall means, in log-scale:
              meanlogexposureacute <- sort(logRK[foodindex[i],hazardindexK[h]]
                +mus0[,foodindex[i]]
                +mucK[,hazardindexK[h],foodindex[i]])
              
              maxxchronic <- quantile(qnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*Vs
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0)),
                               0.95,names=FALSE)
              minnchronic <- quantile(qnorm(0.01,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*Vs
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0)), 
                               0.05,names=FALSE)
              maxxacute <- quantile(qnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]],
                                     sqrt(Vs0+Vs+sigcK[,hazardindexK[h],foodindex[i]]^2)),
                               0.95,names=FALSE)
              minnacute <- quantile(qnorm(0.01,logRK[foodindex[i],hazardindexK[h]]
                                          +mus0[,foodindex[i]]
                                          +mucK[,hazardindexK[h],foodindex[i]],
                                          sqrt(Vs0+Vs+sigcK[,hazardindexK[h],foodindex[i]]^2)),
                                    0.5,names=FALSE)
              chronicxvalues <- seq(minnchronic/log(10),maxxchronic/log(10),length=100) # plot range for chronic
              acutexvalues  <- seq(minnacute/log(10),maxxacute/log(10),length=100) # plot range for acute
             
              # uncertainty for logarithmic chronic (mean) exposure
              plot(meanlogexposurechronic/log(10),cump,main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("log(C.exposure/bw+  (", Unit1,"per kg))"),ylab="Cumulative probability",xlim=c(minnchronic/log(10),maxxchronic/log(10)),lwd=3,type="l") 
              
              
              chronicuppervalues <- numeric()
              chroniclowervalues <- numeric()
              acuteuppervalues <- numeric()
              acutelowervalues <- numeric()
              
              for(xv in 1:100){
                # distribution of chronic logarithmic exposures:
                chronicuppervalues[xv] <- quantile(pnorm(chronicxvalues[xv],(logRK[foodindex[i],hazardindexK[h]]
                                                   +mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*Vs
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_upper,names=FALSE)
                chroniclowervalues[xv] <- quantile(pnorm(chronicxvalues[xv],(logRK[foodindex[i],hazardindexK[h]]
                                                   +mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*Vs
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_lower,names=FALSE)
                # distribution of acute logarithmic exposures:
                acuteuppervalues[xv] <- quantile(pnorm(acutexvalues[xv],(logRK[foodindex[i],hazardindexK[h]]
                                                 +mus0[,foodindex[i]]
                                                 +mucK[,hazardindexK[h],foodindex[i]] )/log(10),
                                                 sqrt(Vs0+Vs+sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10) ),
                                            input_upper,names=FALSE) 
                acutelowervalues[xv] <- quantile(pnorm(acutexvalues[xv],(logRK[foodindex[i],hazardindexK[h]]
                                                 +mus0[,foodindex[i]]
                                                 +mucK[,hazardindexK[h],foodindex[i]]  )/log(10),
                                                 sqrt(Vs0+Vs+sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10) ),
                                            input_lower,names=FALSE)
              }
              # uncertainty bounds for logarithmic chronic (mean) exposure distribution
              polygon(c(chronicxvalues,chronicxvalues[100:1]),c(chronicuppervalues,chroniclowervalues[100:1]),col="#CEB888")
              
              
              # plot empirically generated cumulative logarithmic mean exposure distributions
              OIM <- numeric() # Observed Individual Means
              for(r in 1:nr){
                OIM[r]<- mean(exp(logsw[r,1:nd,foodindex[i]]),na.rm=TRUE) 
              } 
              OIM<-OIM[!is.na(OIM)]
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                      logLOQK[hazardindexK[h],foodindex[i],],
                                      logLODK[hazardindexK[h],foodindex[i],]))
              # and using lower bounds:
              concentrationsLB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                       logLOQLimK[hazardindexK[h],foodindex[i],],
                                       logLODLimK[hazardindexK[h],foodindex[i],]-20))
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              
              for(resample in 1:40){
                # create 40 replicate ('bootstrap') data with original nsample:
                sampleOIM <- sample(OIM,length(OIM),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 2000 simulations from each replicated data:
                sampleOIM <- sample(sampleOIM,2000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
                lines(ecdf(log(sampleOIM*mean(sampleconUB)*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minnchronic/log(10),maxxchronic/log(10)),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(log(sampleOIM*mean(sampleconLB)*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minnchronic/log(10),maxxchronic/log(10)),lwd=1,lty=3,col="#004F71")
              }
              # uncertainty for mean log-chronic exposure  E(log E(e^+)) 
              lines(meanlogexposurechronic/log(10),cump,lwd=3)
              # uncertainty for mean log-acute exposure  E(log e^+)
              lines(meanlogexposureacute/log(10),cump,lwd=3,lty="dashed")
              
              
              
              ##################
              # plot a new frame for acute exposures:
              
              # uncertainty for variability distribution of logarithmic acute exposures
              plot(meanlogexposureacute/log(10),cump,main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(acute)"),
                   xlab=paste("log(A.exposure/bw+  (", Unit1,"per kg))"),ylab="Cumulative probability",xlim=c(minnacute/log(10),maxxacute/log(10)),lwd=3,type="l") 
              # uncertainty bounds for logarithmic acute distribution
              polygon(c(acutexvalues,acutexvalues[100:1]),c(acuteuppervalues,acutelowervalues[100:1]),col="#CEB888")
              
              servings <- exp(logsw[1:nr,1:nd,foodindex[i]])
              servings <- servings[!is.na(servings)] # consumption days 
              
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                        logLOQK[hazardindexK[h],foodindex[i],],
                                        logLODK[hazardindexK[h],foodindex[i],]))
              # and using lower bounds:
              concentrationsLB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                        logLOQLimK[hazardindexK[h],foodindex[i],],
                                        logLODLimK[hazardindexK[h],foodindex[i],]-20))
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              
              for(resample in 1:40){
                # create 40 replicate ('bootstrap') data with original nsample:
                sampleser <- sample(servings,length(servings),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 2000 simulations from each replicated data:
                sampleser <- sample(sampleser,2000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
                lines(ecdf(log(sampleser*sampleconUB*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minnacute/log(10),maxxacute/log(10)),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(log(sampleser*sampleconLB*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minnacute/log(10),maxxacute/log(10)),lwd=1,lty=3,col="#004F71")
              }
              # uncertainty for mean log-acute exposure  E(log e^+)
              lines(meanlogexposureacute/log(10),cump,lwd=3)
              # uncertainty for mean log-chronic exposure  E(log E(e^+)) 
              lines(meanlogexposurechronic/log(10),cump,lwd=3,lty="dashed")  
              
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Q95% for log(C.exposure+): ",chronicql95_50,  
                          "(posterior median). 90% uncertainty interval for the Q95%: ", chronicql95_05,"-", chronicql95_95),
                    side = 1, adj = 0,line=1, cex = 1,
                    outer = TRUE)
              mtext(paste("Q95% for log(C.exposure): ",round(quantile(chronicqltotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q95%: ", round(quantile(chronicqltotal95,0.05,names=FALSE),3),"-",round(quantile(chronicqltotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Q95% for log(A.exposure+): ",acuteql95_50,  
                          "(posterior median). 90% uncertainty interval for the Q95%: ", acuteql95_05,"-", acuteql95_95),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              mtext(paste("Q95% for log(A.exposure): ",round(quantile(acuteqltotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q95%: ", round(quantile(acuteqltotal95,0.05,names=FALSE),3),"-",round(quantile(acuteqltotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=4, cex = 1,
                    outer = TRUE)
              
            } # end of if logarithmic    
            
          } # end of if cumulative
          
          # legend outside the figure, but onto the current plot, so it is part of the png file:
          mtext(paste("Population frequency of exposure from", hazardnamesusedK[h],"from",foodnamesused[i], ": ",
                      round(quantile(100*PK[foodindex[i],hazardindexK[h]]*pK[,hazardindexK[h],foodindex[i]]*p0[,foodindex[i]],0.5,names=FALSE),1),
                      "% (posterior median). 95% uncertainty interval:", round(quantile(100*PK[foodindex[i],hazardindexK[h]]*pK[,hazardindexK[h],foodindex[i]]*p0[,foodindex[i]],0.025,names=FALSE),1),"% -", 
                      round(quantile(100*PK[foodindex[i],hazardindexK[h]]*pK[,hazardindexK[h],foodindex[i]]*p0[,foodindex[i]],0.975,names=FALSE),1),"%"),
                side = 1, adj = 0,line=0, cex = 1,
                outer = TRUE)
          
        ############################################################################################ 
        } # end of if constant.consum FALSE  #########################################################
          else {  # consumption is constant. --> Chronic exposure =E(c)*consum, only uncertainty distribution of chronic exposure.
            # but uncertainty for variability distribution of acute exposures.
             source("constantconsumK.R",local=TRUE)
            } # end of if constant consumption 
          
        } else # end of if hazard-food modeled 
          
          ##Empty plot----
        {
          par(mar = c(0,0,0,0))
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.8, paste("The data for this food-hazard combination is not sufficient for modeling\n",
                                       "(e.g., concentration measurements (>LOQ) about the food-hazard combination are missing)."), 
               cex = 1.6, col = "#D0006F")
          par(mar = c(5, 4, 4, 2) + 0.1)
        }
      }} # end of for nhused nfused
  } 
  
  # MICROBIAL EXPOSURES:  ----
  
  if((nhusedM>0)&(nfused>0)){
    RM = matrix(NA,nf,nhM) # factors for concentration
    RM[1:nf,1:nhM] = Rall[1:nf,is.element(hazardnames,hazardnamesusedM)]
    logRM = log(RM)
    PM = matrix(NA,nf,nhM) # factors for prevalence
    PM[1:nf,1:nhM] = Pall[1:nf,is.element(hazardnames,hazardnamesusedM)]
    
    for(h in 1:nhusedM){
      Unit <- unit_concen[hazard_concen == hazardnamesusedM[h]] # the measurement unit used for hazard concentration
      Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
      
      for(i in 1:nfused){
        if(nexactM[hazardindexM[h],foodindex[i]]>0){ # this hazard-food is modeled
          ######################################################################################## 
          if(constant.consum==FALSE){ 
          p0 <- exp(logitp0)/(1+exp(logitp0))    
          Vs <- numeric() # variances between days (servings), over actual positives
          Vs0 <- numeric() # variances between individual means, over actual positives
          for(u in 1:n_sim){
            if(input_modelchoice=="Fixed variance"){
              Vs[u] <- osdlogsw2[foodindex[i]]^2  # observed value from data
              Vs0[u] <- osdlogsw1[foodindex[i]]^2 # observed value from data
            } else { 
              Vs[u] <- Ss[u,foodindex[i],foodindex[i]] 
              Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]]
            }
          }  
          
          ##########################################
          # absolute value:
          if(input_selectscale=="Absolute"){
            # positive acute exposures, variability 90% quantile:
            acuteqlnormpos90M <- qlnorm(0.90,logRM[foodindex[i],hazardindexM[h]]
                                   +mus0[,foodindex[i]]
                                   +muw
                                   +mucM[,hazardindexM[h],foodindex[i]],
                                   sqrt(Vs0+Vs+sigcM[,hazardindexM[h],foodindex[i]]^2+sigw^2 ))
            # posterior quantiles of acute variability 95%quantile:
            acuteq90_50 <- round(quantile(acuteqlnormpos90M,0.5,names=FALSE),3)
            acuteq90_95 <- round(quantile(acuteqlnormpos90M,0.95,names=FALSE),3)
            acuteq90_05 <- round(quantile(acuteqlnormpos90M,0.05,names=FALSE),3)
            
            # positive chronic exposures, variability 90% quantile:  
            chronicqlnormpos90M <- qlnorm(0.90,logRM[foodindex[i],hazardindexM[h]]
                                   +muw     
                                   +mus0[,foodindex[i]]
                                   +mucM[,hazardindexM[h],foodindex[i]]
                                   +0.5*Vs
                                   +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                   +0.5*sigw^2,
                                   sqrt(Vs0) )
            # posterior quantiles of chronic variability 95%quantile:
            chronicq90_50 <- round(quantile(chronicqlnormpos90M,0.5,names=FALSE),3)
            chronicq90_95 <- round(quantile(chronicqlnormpos90M,0.95,names=FALSE),3)
            chronicq90_05 <- round(quantile(chronicqlnormpos90M,0.05,names=FALSE),3)
          } 
          # logarithmic value:
          if(input_selectscale=="Logarithmic"){
            # positive acute exposures, variability 90% quantile:
            acuteqnormpos90M <- qnorm(0.90,logRM[foodindex[i],hazardindexM[h]]
                                 +mus0[,foodindex[i]]
                                 +muw
                                 +mucM[,hazardindexM[h],foodindex[i]],
                                 sqrt(Vs0+Vs+sigcM[,hazardindexM[h],foodindex[i]]^2)+sigw^2)/log(10)
            # posterior quantiles of acute variability 90% quantile:
            acuteql90_50 <- round(quantile(acuteqnormpos90M,0.5,names=FALSE),3)
            acuteql90_95 <- round(quantile(acuteqnormpos90M,0.95,names=FALSE),3)
            acuteql90_05 <- round(quantile(acuteqnormpos90M,0.05,names=FALSE),3)
            # positive chronic (mean) exposures, variability 90% quantile: 
            chronicqnormpos90M <- qnorm(0.90,logRM[foodindex[i],hazardindexM[h]]
                                 +muw
                                 +mus0[,foodindex[i]]
                                 +mucM[,hazardindexM[h],foodindex[i]]
                                 +0.5*Vs
                                 +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                 +0.5*sigw^2 ,
                                 sqrt(Vs0))/log(10)
            # posterior quantiles of chronic variability 90% quantile:
            chronicql90_50 <- round(quantile(chronicqnormpos90M,0.5,names=FALSE),3)
            chronicql90_95 <- round(quantile(chronicqnormpos90M,0.95,names=FALSE),3)
            chronicql90_05 <- round(quantile(chronicqnormpos90M,0.05,names=FALSE),3)
            
          }
          
          ##########################################
          
          # acute exposure (hazard i, food j) over all days, all servings (including zeros):
          V <- 2000 # variability simulations (individual exposure events)
          logitpconsume <- numeric()
          pconsume <- numeric()
          cmeanpos <- numeric()
          poissonmeans <- numeric()
          acuteqtotal90 <- numeric()
          acuteqltotal90 <- numeric()
          chronicqtotal90 <- numeric()
          chronicqltotal90 <- numeric()
          qpos90 <- numeric()
          qlpos90 <- numeric()
          
          
          for(u in 1:n_sim){ 
            # simulate variability for V individuals, per each uncertain parameter: 
            if((input_modelchoice == "Independent days")|(input_modelchoice == "Fixed variance") ){ 
              if(input_modelchoice2 =="Yes"){
                logitpconsume[1:V] <- rnorm(V,logitp0[u,foodindex[i]],sqrt(Sp[u,foodindex[i],foodindex[i]]))
              }
              if(input_modelchoice2 == "No"){
                logitpconsume[1:V] <- rep(logitp0[u,foodindex[i]],V)   
              }
              pconsume[1:V] <- exp(logitpconsume[1:V])/(1+exp(logitpconsume[1:V]))  
            }
            if(input_modelchoice=="Dependent days"){
              pconsume[1:V] <- rep(p0[u,foodindex[i]],V)
            } 
            
            # evaluate the 90% quantile of the exposure distribution 
            # (over V individuals for chronic exposure, over V individual days for acute exposure) including all days 
            # (not only positively contaminated consumptions)
            
            # variability of mean positive exposures:    ######################################
            cmeanpos[1:V] <- rlnorm(V,logRM[foodindex[i],hazardindexM[h]]
                                    +mus0[u,foodindex[i]]
                                    +muw[u]
                                    +mucM[u,hazardindexM[h],foodindex[i]]
                                    +0.5*Vs[u]
                                    +0.5*sigcM[u,hazardindexM[h],foodindex[i]]^2
                                    +0.5*sigw[u]^2,
                                    sqrt(Vs0[u]))
            # variability of individual (acute) positive exposures (expected cell counts in "servings"):
            poissonmeans[1:V] <- rlnorm(V,logRM[foodindex[i],hazardindexM[h]]
                                   +mus0[u,foodindex[i]]
                                   +mucM[u,hazardindexM[h],foodindex[i]]
                                   +muw[u],
                                   sqrt(Vs[u]
                                        +Vs0[u]
                                        +sigcM[u,hazardindexM[h],foodindex[i]]^2
                                        +sigw[u]^2))
            # absolute, over all days:
            if(input_selectscale=="Absolute"){
            acuteqtotal90[u]<-quantile(
              rbinom(V,1,pconsume[1:V]*
                       pM[u,hazardindexM[h],foodindex[i]]*
                       PM[foodindex[i],hazardindexM[h]])*poissonmeans[1:V],0.90,names=FALSE)
            chronicqtotal90[u]<-quantile(
                pconsume[1:V]*
                pM[u,hazardindexM[h],foodindex[i]]*
                PM[foodindex[i],hazardindexM[h]]*
                cmeanpos[1:V],0.90,names=FALSE)
            }
            # logarithmic, over all days:
            if(input_selectscale=="Logarithmic"){
            acuteqltotal90[u]<-quantile(
              log10(rbinom(V,1,pconsume[1:V]*
                       pM[u,hazardindexM[h],foodindex[i]]*
                       PM[foodindex[i],hazardindexM[h]])*poissonmeans[1:V])     
              ,0.90,names=FALSE)
            chronicqltotal90[u]<-quantile(
              log10(pconsume[1:V]*
                      pM[u,hazardindexM[h],foodindex[i]]*
                      PM[foodindex[i],hazardindexM[h]]*
                      cmeanpos[1:V]),0.90,names=FALSE)
            }
            # positive acute exposure (poisson mean) variability quantile:
            qpos90[u] <- qlnorm(0.90,logRM[foodindex[i],hazardindexM[h]]
                                +mus0[u,foodindex[i]]
                                +mucM[u,hazardindexM[h],foodindex[i]]
                                +muw[u],
                                sqrt(Vs[u]
                                     +Vs0[u]
                                     +sigcM[u,hazardindexM[h],foodindex[i]]^2
                                     +sigw[u]^2))
            qlpos90[u] <- qnorm(0.90,logRM[foodindex[i],hazardindexM[h]]
                                +mus0[u,foodindex[i]]
                                +mucM[u,hazardindexM[h],foodindex[i]]
                                +muw[u],
                                sqrt(Vs[u]
                                     +Vs0[u]
                                     +sigcM[u,hazardindexM[h],foodindex[i]]^2
                                     +sigw[u]^2))/log(10)
            
          } # end of for u
          
          # positive acute exposures (poisson means), 
          # posterior quantiles of 90% variability quantile:
          acuteq90_05 <- round(quantile(qpos90,0.05,names=FALSE,na.rm=TRUE),3)
          acuteq90_50 <- round(quantile(qpos90,0.5,names=FALSE,na.rm=TRUE),3)
          acuteq90_95 <- round(quantile(qpos90,0.95,names=FALSE,na.rm=TRUE),3)
          acuteql90_05 <- round(quantile(qlpos90,0.05,names=FALSE,na.rm=TRUE),3)
          acuteql90_50 <- round(quantile(qlpos90,0.5,names=FALSE,na.rm=TRUE),3)
          acuteql90_95 <- round(quantile(qlpos90,0.95,names=FALSE,na.rm=TRUE),3)
          
          ##Density----
          if(input_selectdist=="Density"){
            ############## exposure.acuteM
            # plot posterior of the mean & median exposure 
            # (expected acute exposure for anyone)
            ###Absolute----
            if(input_selectscale=="Absolute"){   
              meanexposureacute <- exp(logRM[foodindex[i],hazardindexM[h]]
                                  +mus0[,foodindex[i]]
                                  +mucM[,hazardindexM[h],foodindex[i]]
                                  +muw
                                  +0.5*Vs0
                                  +0.5*Vs
                                  +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2 
                                  +0.5*sigw^2 )
              maxxacute <- quantile(qlnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]+
                                        mus0[,foodindex[i]]+
                                        mucM[,hazardindexM[h],foodindex[i]]+
                                        muw,   
                                      sqrt( Vs0+
                                              Vs+
                                              sigcM[,hazardindexM[h],foodindex[i]]^2+
                                              sigw^2) ),
                               0.99,names=FALSE)
              medianexposureacute <- exp(logRM[foodindex[i],hazardindexM[h]]+
                                      mus0[,foodindex[i]]+
                                      mucM[,hazardindexM[h],foodindex[i]]+muw)
              
              plot(density(medianexposureacute,from=0,to=maxxacute,n=2048),main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),
                   xlab=paste("A.exposure+  ( E(",Unit1,") per day)"),ylab="Probability density",xlim=c(0,maxxacute),lwd=3) 
              lines(density(meanexposureacute,from=0,to=maxxacute,n=2048),lwd=3,col="#F7CE3C")
              
              xvaluesacute <- seq(0,maxxacute,length=100)
              acuteuppervalues <- numeric()
              acutelowervalues <- numeric()
              for(xv in 1:100){
                acuteuppervalues[xv] <- quantile(dlnorm(xvaluesacute[xv],
                                                   logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     muw,
                                                   sqrt( Vs0+
                                                           Vs+
                                                           sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                           sigw^2)),
                                            input_upper,names=FALSE)
                acutelowervalues[xv] <- quantile(dlnorm(xvaluesacute[xv],
                                                   logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     muw,
                                                   sqrt( Vs0+
                                                           Vs+
                                                           sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                           sigw^2)),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvaluesacute,xvaluesacute[100:1]),c(acuteuppervalues,acutelowervalues[100:1]),col="#CEB888")
              lines(density(meanexposureacute,from=0,to=maxxacute,n=2048),col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),xlab="A.exposure+",ylab="",xlim=c(0,maxxacute),lwd=3) 
              lines(density(medianexposureacute,from=0,to=maxxacute,n=2048),lwd=3)
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Q90% for A.exposure+: ",acuteq90_50,  
                          "(posterior median). 90% uncertainty interval for the Q90%: ", acuteq90_05,"-", acuteq90_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Q90% for A.exposure: ",round(quantile(acuteqtotal90,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(quantile(acuteqtotal90,0.05,names=FALSE),3),"-",round(quantile(acuteqtotal90,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if absolute
            
            ##Logarithmic----
            if(input_selectscale=="Logarithmic"){
              meanlogexposureacute <- logRM[foodindex[i],hazardindexM[h]]+
                mus0[,foodindex[i]]+mucM[,hazardindexM[h],foodindex[i]]+muw
              # plot range max (acute):
              maxxacute <- quantile(qnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +muw,
                                     sqrt(Vs0
                                          +Vs
                                          +sigcM[,hazardindexM[h],foodindex[i]]^2
                                          +sigw^2)),
                               0.99,names=FALSE)
              # plot range min (acute):
              minnacute <- quantile(qnorm(0.01,logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +muw,
                                     sqrt(Vs0
                                          +Vs
                                          +sigcM[,hazardindexM[h],foodindex[i]]^2
                                          +sigw^2)),
                               0.05,names=FALSE)
              plot(density(meanlogexposureacute/log(10),from=minnacute/log(10),to=maxxacute/log(10),n=2048),col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),
                   xlab=paste("log(A.exposure+  ( E(",Unit1,") per day))"),ylab="Probability density",xlim=c(minnacute/log(10),maxxacute/log(10)),lwd=3) 
              
              xvaluesacute <- seq(minnacute/log(10),maxxacute/log(10),length=100)
              acuteuppervalues <- numeric()
              acutelowervalues <- numeric()
              for(xv in 1:100){
                acuteuppervalues[xv] <- quantile(dnorm(xvaluesacute[xv],
                                                  (logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     muw)/log(10),
                                                  (sqrt( Vs0+
                                                           Vs+
                                                           sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                           sigw^2))/log(10) ),
                                            input_upper,names=FALSE) 
                acutelowervalues[xv] <- quantile(dnorm(xvaluesacute[xv],
                                                  (logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     muw)/log(10),
                                                  (sqrt( Vs0+
                                                           Vs+
                                                           sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                           sigw^2))/log(10) ),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvaluesacute,xvaluesacute[100:1]),c(acuteuppervalues,acutelowervalues[100:1]),col="#CEB888")
              lines(density(meanlogexposureacute/log(10),from=minnacute/log(10),to=maxxacute/log(10),n=2048),col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),xlab="log (A.exposure+)",ylab="",xlim=c(minnacute/log(10),maxxacute/log(10)),lwd=3)
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Q90% for log(A.exposure+): ",acuteql90_50,  
                          "(posterior median). 90% uncertainty interval for the Q90%: ", acuteql90_05,"-", acuteql90_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Q90% for log(A.exposure): ",round(quantile(acuteqltotal90,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(quantile(acuteqltotal90,0.05,names=FALSE),3),"-",round(quantile(acuteqltotal90,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if logarithmic
          } # end of if density
          
          #Cumulative----
          if(input_selectdist=="Cumulative"){
            par(mfrow=c(2,1))
            par(yaxt="s")
            cump=seq(1,n_sim)
            cump=cump/length(cump)
            ############## exposure.acuteM
            # plot posterior of the mean & median exposure 
            # (expected acute exposure for anyone)
            ##Absolute----
            if(input_selectscale=="Absolute"){
            
              # mean of chronic exposures
              meanexposurechronic <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                +mus0[,foodindex[i]]
                +mucM[,hazardindexM[h],foodindex[i]]
                +muw
                +0.5*Vs0
                +0.5*Vs
                +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                +0.5*sigw^2))
              
              medianexposurechronic <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                +mus0[,foodindex[i]]
                +mucM[,hazardindexM[h],foodindex[i]]
                +muw
                +0.5*Vs
                +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                +0.5*sigw^2))
              
              # mean of acute exposures
              meanexposureacute <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                                       +mus0[,foodindex[i]]
                                       +mucM[,hazardindexM[h],foodindex[i]]
                                       +muw
                                       +0.5*Vs0
                                       +0.5*Vs
                                       +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2 
                                       +0.5*sigw^2 ))
              medianexposureacute <- sort(exp(logRM[foodindex[i],hazardindexM[h]]+
                                           mus0[,foodindex[i]]+
                                           mucM[,hazardindexM[h],foodindex[i]]+
                                           muw))
              # plot range max (acute):
              maxxacute <- quantile(qlnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]+
                                        mus0[,foodindex[i]]+
                                        mucM[,hazardindexM[h],foodindex[i]]+
                                        muw,   
                                      sqrt(Vs0+
                                           Vs+
                                           sigcM[,hazardindexM[h],foodindex[i]]^2+
                                           sigw^2) ),
                               0.99,names=FALSE)
              # plot range max (chronic):
              maxxchronic <- quantile(qlnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]
                                      +mus0[,foodindex[i]]         
                                      +muw
                                      +mucM[,hazardindexM[h],foodindex[i]]
                                      +0.5*Vs+
                                      +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                      +0.5*sigw^2,
                                      sqrt(Vs0) ),
                               0.99,names=FALSE)
              xvaluesacute <- seq(0,maxxacute,length=100) # plot range for acute
              xvalueschronic <- seq(0,maxxchronic,length=100) # plot range for chronic
              
              # uncertainty for mean and median-acute exposure
              plot(meanexposureacute[meanexposureacute<maxxacute],cump[meanexposureacute<maxxacute],col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),
                   xlab=paste("A.exposure+  ( E(",Unit1,") per day)"),ylab="Cumulative probability",xlim=c(0,maxxacute),ylim=c(0,1),type="l",lwd=3) 
              lines(medianexposureacute[medianexposureacute<maxxacute],cump[medianexposureacute<maxxacute],lwd=3)   
              
              # uncertainty bounds for chronic and acute exposure distributions:
              acuteuppervalues <- numeric()
              acutelowervalues <- numeric()
              chronicuppervalues <- numeric()
              chroniclowervalues <- numeric()
              for(xv in 1:100){
                # uncertainty bounds for chronic (mean) exposure distribution
                chronicuppervalues[xv] <- quantile(plnorm(xvalueschronic[xv],logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +muw
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +0.5*Vs
                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                     +0.5*sigw^2,
                                     sqrt(Vs0)),input_upper,names=FALSE)
                chroniclowervalues[xv] <- quantile(plnorm(xvalueschronic[xv],logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]                      
                                     +muw 
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +0.5*Vs
                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                     +0.5*sigw^2,
                                     sqrt(Vs0)),input_lower,names=FALSE)
                # uncertainty bounds for acute exposure distribution
                acuteuppervalues[xv] <- quantile(plnorm(xvaluesacute[xv],logRM[foodindex[i],hazardindexM[h]]
                                                     +mus0[,foodindex[i]]
                                                     +mucM[,hazardindexM[h],foodindex[i]]
                                                     +muw,
                                                   sqrt( Vs0
                                                         +Vs
                                                         +sigcM[,hazardindexM[h],foodindex[i]]^2
                                                         +sigw^2)),
                                            input_upper,names=FALSE) 
                acutelowervalues[xv] <- quantile(plnorm(xvaluesacute[xv],logRM[foodindex[i],hazardindexM[h]]
                                                     +mus0[,foodindex[i]]
                                                     +mucM[,hazardindexM[h],foodindex[i]]
                                                     +muw,
                                                   sqrt(Vs0
                                                        +Vs
                                                        +sigcM[,hazardindexM[h],foodindex[i]]^2
                                                        +sigw^2)),
                                            input_lower,names=FALSE)
              }
              # uncertainty bounds for acute exposure distribution
              polygon(c(xvaluesacute,xvaluesacute[100:1]),c(acuteuppervalues,acutelowervalues[100:1]),col="#CEB888")
              
              
              # plot empirically generated cumulative acute exposure distributions
              W <- matrix(sample(Weight),nr,nd) # randomize bodyweights of individuals 
              servings <- exp(logsw[1:nr,1:nd,foodindex[i]])*W[1:nr,1:nd]  # absolute consumptions (because for microbial exposure)
              servings <- servings[!is.na(servings)]
                              
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                      logLOQM[hazardindexM[h],foodindex[i],],
                                      logLODM[hazardindexM[h],foodindex[i],]))
              # and using lower bounds:
              concentrationsLB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                       logLOQLimM[hazardindexM[h],foodindex[i],], 
                                       logLODLimM[hazardindexM[h],foodindex[i],]-20))
              
              servings <- servings[!is.na(servings)]
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              for(resample in 1:40){
                # create 40 replicate ('bootstrap') data with original nsample:
                W <- matrix(sample(Weight),nr,nd) # randomize bodyweights of individuals 
                servings <- exp(logsw[1:nr,1:nd,foodindex[i]])*W[1:nr,1:nd]  # absolute consumptions (because for microbial exposure)
                servings <- servings[!is.na(servings)]
                sampleser <- sample(servings,length(servings),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 2000 simulations from each replicated data:
                sampleser <- sample(sampleser,2000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
                lines(ecdf(sampleser*sampleconUB*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxacute),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(sampleser*sampleconLB*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxacute),lwd=1,lty=3,col="#004F71")
              }
              # uncertainty for mean and median acute exposure:
              lines(meanexposureacute[meanexposureacute<maxxacute],cump[meanexposureacute<maxxacute],col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),xlab="A.exposure+",ylab="",xlim=c(0,maxxacute),lwd=3) 
              lines(medianexposureacute[medianexposureacute<maxxacute],cump[medianexposureacute<maxxacute],xlim=c(0,maxxacute),lwd=3)
              
              ######################################
              # plot a new frame for chronic exposures:
              plot(meanexposurechronic[meanexposurechronic<maxxchronic],cump[meanexposurechronic<maxxchronic],col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("C.exposure+  ( E(",Unit1,") per day)"),ylab="Cumulative probability",xlim=c(0,maxxchronic),ylim=c(0,1),type="l",lwd=3) 
              lines(medianexposurechronic[medianexposurechronic<maxxchronic],cump[medianexposurechronic<maxxchronic],lwd=3)  
              # uncertainty bounds for chronic (mean) exposure distributions:
              polygon(c(xvalueschronic,xvalueschronic[100:1]),c(chronicuppervalues,chroniclowervalues[100:1]),col="#CEB888")
              
              # plot empirically generated cumulative chronic (mean) exposure distributions
              W <- matrix(sample(Weight),nr,nd) # randomize bodyweights of individuals 
              OIM <- numeric()  # Observed Individual Means
              for(r in 1:nr){
                OIM[r]<- mean(exp(logsw[r,1:nd,foodindex[i]])*W[r,1:nd],na.rm=TRUE) # absolute consumptions (because for microbial exposure) 
              } 
              OIM<-OIM[!is.na(OIM)]
              
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                        logLOQM[hazardindexM[h],foodindex[i],],
                                        logLODM[hazardindexM[h],foodindex[i],]))
              # and using lower bounds:
              concentrationsLB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                        logLOQLimM[hazardindexM[h],foodindex[i],],
                                        logLODLimM[hazardindexM[h],foodindex[i],]-20))
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              
              for(resample in 1:40){   
                # create 40 replicate ('bootstrap') data with original nsample:
                W <- matrix(sample(Weight),nr,nd) # randomize bodyweights of individuals 
                OIM <- numeric()  # Observed Individual Means
                for(r in 1:nr){
                  OIM[r]<- mean(exp(logsw[r,1:nd,foodindex[i]])*W[r,1:nd],na.rm=TRUE) # absolute consumptions (because for microbial exposure) 
                } 
                OIM<-OIM[!is.na(OIM)]
                sampleOIM <- sample(OIM,length(OIM),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 2000 simulations from each replicated data:
                sampleOIM <- sample(sampleOIM,2000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
                lines(ecdf(sampleOIM*mean(sampleconUB)*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxchronic),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(sampleOIM*mean(sampleconLB)*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxchronic),lwd=1,lty=3,col="#004F71")
              }
              # uncertainty for mean and median chronic exposure:
              lines(meanexposurechronic[meanexposurechronic<maxxchronic],cump[meanexposurechronic<maxxchronic],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="C.exposure/bw+",ylab="",xlim=c(0,maxxchronic),lwd=3) 
              lines(medianexposurechronic[medianexposurechronic<maxxchronic],cump[medianexposurechronic<maxxchronic],xlim=c(0,maxxchronic),lwd=3)
              
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Q90% for A.exposure+: ",round(acuteq90_50,3),  
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(acuteq90_05,3),"-", round(acuteq90_95,3)),
                    side = 1, adj = 0,line=1, cex = 1,
                    outer = TRUE)
              mtext(paste("Q90% for A.exposure: ",round(quantile(acuteqtotal90,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(quantile(acuteqtotal90,0.05,names=FALSE),3),"-",round(quantile(acuteqtotal90,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Q90% for C.exposure+: ",round(chronicq90_50,3),  
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(chronicq90_05,3),"-", round(chronicq90_95,3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              mtext(paste("Q90% for C.exposure: ",round(quantile(chronicqtotal90,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(quantile(chronicqtotal90,0.05,names=FALSE),3),"-",round(quantile(chronicqtotal90,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=4, cex = 1,
                    outer = TRUE)
              
              
            } # end of if absolute
            
            ##Logarithmic----
            if(input_selectscale=="Logarithmic"){
              # mean of acute log exposures
              meanlogexposureacute <- sort(logRM[foodindex[i],hazardindexM[h]]
                                      +mus0[,foodindex[i]]
                                      +mucM[,hazardindexM[h],foodindex[i]]
                                      +muw )
              
              # mean of chronic log exposures
              meanlogexposurechronic <- sort(logRM[foodindex[i],hazardindexM[h]]
                +mus0[,foodindex[i]]
                +muw
                +mucM[,hazardindexM[h],foodindex[i]]
                +0.5*Vs
                +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                +0.5*sigw^2)
              
              # plot range max for acute:
              maxxacute <- quantile(qnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +muw
                                     +mucM[,hazardindexM[h],foodindex[i]],
                                     sqrt(Vs0
                                          +Vs
                                          +sigcM[,hazardindexM[h],foodindex[i]]^2
                                          +sigw^2)),
                               0.99,names=FALSE)
              # plot range min for acute:
              minnacute <- quantile(qnorm(0.01,logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +muw
                                     +mucM[,hazardindexM[h],foodindex[i]],
                                     sqrt(Vs0
                                          +Vs
                                          +sigcM[,hazardindexM[h],foodindex[i]]^2
                                          +sigw^2)),
                               0.05,names=FALSE)
              # plot range max for chronic:
              maxxchronic <- quantile(qnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]
                                          +mus0[,foodindex[i]]
                                          +muw
                                          +mucM[,hazardindexM[h],foodindex[i]]
                                          +0.5*Vs
                                          +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                          +0.5*sigw^2,
                                          sqrt(Vs0)),
                                    0.99,names=FALSE)
              # plot range min for chronic:
              minnchronic <- quantile(qnorm(0.01,logRM[foodindex[i],hazardindexM[h]]
                                            +mus0[,foodindex[i]]
                                            +muw
                                            +mucM[,hazardindexM[h],foodindex[i]]
                                            +0.5*Vs
                                            +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                            +0.5*sigw^2,
                                            sqrt(Vs0)),
                                    0.05,names=FALSE)
              
              acutexvalues <- seq(minnacute/log(10),maxxacute/log(10),length=100) # plot range for acute
              chronicxvalues  <- seq(minnchronic/log(10),maxxchronic/log(10),length=100) # plot range for chronic
              
              # uncertainty for logarithmic mean-acute exposure
              plot(meanlogexposureacute/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),
                   xlab=paste("log(A.exposure+  ( E(",Unit1,") per day))"),ylab="Cumulative probability",xlim=c(minnacute/log(10),maxxacute/log(10) ),lwd=3,type="l") 
              
              
              acuteuppervalues <- numeric()
              acutelowervalues <- numeric()
              chronicuppervalues <- numeric()
              chroniclowervalues <- numeric()
              
              for(xv in 1:100){
                # uncertainty bounds for acute:
                acuteuppervalues[xv] <- quantile(pnorm(acutexvalues[xv],(logRM[foodindex[i],hazardindexM[h]]
                                                     +mus0[,foodindex[i]]
                                                     +mucM[,hazardindexM[h],foodindex[i]]
                                                     +muw)/log(10),
                                                  sqrt(Vs0 
                                                       +Vs
                                                       +sigcM[,hazardindexM[h],foodindex[i]]^2
                                                       +sigw^2)/log(10)),
                                            input_upper,names=FALSE) 
                # uncertainty bounds for acute:
                acutelowervalues[xv] <- quantile(pnorm(acutexvalues[xv],(logRM[foodindex[i],hazardindexM[h]]
                                                     +mus0[,foodindex[i]]
                                                     +muw
                                                     +mucM[,hazardindexM[h],foodindex[i]])/log(10),
                                                  sqrt( Vs0
                                                        +Vs
                                                        +sigcM[,hazardindexM[h],foodindex[i]]^2
                                                        +sigw^2)/log(10)),
                                            input_lower,names=FALSE)
                # uncertainty bounds for chronic:
                chronicuppervalues[xv] <- quantile(pnorm(chronicxvalues[xv],(logRM[foodindex[i],hazardindexM[h]]
                                                   +mus0[,foodindex[i]]
                                                   +muw
                                                   +mucM[,hazardindexM[h],foodindex[i]]
                                                   +0.5*Vs+
                                                   +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                                   +0.5*sigw^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_upper,names=FALSE)
                # uncertainty bounds for chronic:
                chroniclowervalues[xv] <- quantile(pnorm(chronicxvalues[xv],(logRM[foodindex[i],hazardindexM[h]]
                                                   +mus0[,foodindex[i]]
                                                   +muw
                                                   +mucM[,hazardindexM[h],foodindex[i]]
                                                   +0.5*Vs+
                                                   +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                                   +0.5*sigw^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_lower,names=FALSE)
              }
              # uncertainty bounds for logarithmic acute exposure distribution
              polygon(c(acutexvalues,acutexvalues[100:1]),c(acuteuppervalues,acutelowervalues[100:1]),col="#CEB888")
              # uncertainty for mean log-acute exposure  E(log e^+)
              lines(meanlogexposureacute/log(10),cump,lwd=3)
              # uncertainty for mean log-chronic exposure  E(log E(e^+)) 
              lines(meanlogexposurechronic/log(10),cump,lwd=3,lty="dashed")  
              
              
              # plot empirically generated cumulative acute exposure distributions
              W <- matrix(sample(Weight),nr,nd) # randomize bodyweights of individuals 
              servings <- exp(logsw[1:nr,1:nd,foodindex[i]])*W[1:nr,1:nd] # absolute consumptions (because for microbial exposure) 
              servings <- servings[!is.na(servings)]
            
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                      logLOQM[hazardindexM[h],foodindex[i],],
                                      logLODM[hazardindexM[h],foodindex[i],]))
              # and using lower bounds
              concentrationsLB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                       logLOQLimM[hazardindexM[h],foodindex[i],],
                                       logLODLimM[hazardindexM[h],foodindex[i],]-20))
              servings <- servings[!is.na(servings)]
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              
              for(resample in 1:40){
                # create 40 replicate ('bootstrap') data with original nsample:
                W <- matrix(sample(Weight),nr,nd) # randomize bodyweights of individuals 
                servings <- exp(logsw[1:nr,1:nd,foodindex[i]])*W[1:nr,1:nd] # absolute consumptions (because for microbial exposure) 
                servings <- servings[!is.na(servings)]
                sampleser <- sample(servings,length(servings),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 2000 simulations from each replicated data:
                sampleser <- sample(sampleser,2000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
                lines(ecdf(log10(sampleser*sampleconUB*RM[foodindex[i],hazardindexM[h]])),verticals=TRUE,do.points=FALSE,xlim=c(minnacute/log(10),maxxacute/log(10)),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(log10(sampleser*sampleconLB*RM[foodindex[i],hazardindexM[h]])),verticals=TRUE,do.points=FALSE,xlim=c(minnacute/log(10),maxxacute/log(10)),lwd=1,lty=3,col="#004F71")
              }  
              # uncertainty for log mean exposure
              lines(meanlogexposureacute/log(10),cump,lwd=3) 
              
              ################################################
              # plot a new frame for chronic exposures:
              
              # uncertainty for mean log-chronic exposure
              plot(meanlogexposurechronic/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("log(C.exposure+  ( E(",Unit1,") per day))"),ylab="Cumulative probability",xlim=c(minnchronic/log(10),maxxchronic/log(10) ),lwd=3,type="l") 
              # uncertainty bounds for logarithmic chronic (mean) variability distribution
              polygon(c(chronicxvalues,chronicxvalues[100:1]),c(chronicuppervalues,chroniclowervalues[100:1]),col="#CEB888")
              # uncertainty for mean log-chronic exposure
              lines(meanlogexposurechronic/log(10),cump,lwd=3) 
    
              
              # plot empirically generated cumulative chronic exposure distributions
              W <- matrix(sample(Weight),nr,nd) # randomize bodyweights of individuals 
              OIM <- numeric() # Observed Individual Means
              for(r in 1:nr){
                OIM[r]<- mean(exp(logsw[r,1:nd,foodindex[i]])*W[r,1:nd],na.rm=TRUE) # absolute values (because microbial exposure) 
              } 
              OIM<-OIM[!is.na(OIM)]
    
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                        logLOQM[hazardindexM[h],foodindex[i],],
                                        logLODM[hazardindexM[h],foodindex[i],]))
              # and using lower bounds:
              concentrationsLB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                        logLOQLimM[hazardindexM[h],foodindex[i],],
                                        logLODLimM[hazardindexM[h],foodindex[i],]-20))
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              
              for(resample in 1:40){   
                # create 40 replicate ('bootstrap') data with original nsample:
                W <- matrix(sample(Weight),nr,nd) # randomize bodyweights of individuals 
                OIM <- numeric() # Observed Individual Means
                for(r in 1:nr){
                  OIM[r]<- mean(exp(logsw[r,1:nd,foodindex[i]])*W[r,1:nd],na.rm=TRUE) # absolute values (because microbial exposure) 
                } 
                OIM<-OIM[!is.na(OIM)]
                sampleOIM <- sample(OIM,length(OIM),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 2000 simulations from each replicated data:
                sampleOIM <- sample(sampleOIM,2000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
                lines(ecdf(log10(sampleOIM*mean(sampleconUB)*RM[foodindex[i],hazardindexM[h]])),verticals=TRUE,do.points=FALSE,xlim=c(minnchronic/log(10),maxxchronic/log(10)),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(log10(sampleOIM*mean(sampleconLB)*RM[foodindex[i],hazardindexM[h]])),verticals=TRUE,do.points=FALSE,xlim=c(minnchronic/log(10),maxxchronic/log(10)),lwd=1,lty=3,col="#004F71")
              }
              # uncertainty for mean log-chronic exposure  E(log E(e^+)) 
              lines(meanlogexposurechronic/log(10),cump,lwd=3)   
              # uncertainty for mean log-acute exposure  E(log e^+)
              lines(meanlogexposureacute/log(10),cump,lwd=3,lty="dashed")
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Q90% for log(A.exposure+): ",round(acuteql90_50,3),  
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(acuteql90_05,3),"-", round(acuteql90_95,3)),
                    side = 1, adj = 0,line=1, cex = 1,
                    outer = TRUE)
              mtext(paste("Q90% for log(A.exposure): ",round(quantile(acuteqltotal90,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(quantile(acuteqltotal90,0.05,names=FALSE),3),"-",round(quantile(acuteqltotal90,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Q90% for log(C.exposure+): ",round(chronicql90_50,3),
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(chronicql90_05,3),"-", round(chronicql90_95,3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              mtext(paste("Q90% for log(C.exposure): ",round(quantile(chronicqltotal90,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q90%: ", round(quantile(chronicqltotal90,0.05,names=FALSE),3),"-",round(quantile(chronicqltotal90,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=4, cex = 1,
                    outer = TRUE)
              
              
            } # end of if logarithmic
          } # end of if cumulative
          
          # legend outside the figure, but onto the current plot, so it is part of the png file:
          mtext(paste("Population frequency of exposure from", hazardnamesusedM[h],"from",foodnamesused[i], ": ",
                      round(quantile(100*PM[foodindex[i],hazardindexM[h]]*pM[,hazardindexM[h],foodindex[i]]*p0[,foodindex[i]],0.5,names=FALSE),1),
                      "% (posterior median). 95% uncertainty interval:", round(quantile(100*PM[foodindex[i],hazardindexM[h]]*pM[,hazardindexM[h],foodindex[i]]*p0[,foodindex[i]],0.025,names=FALSE),1),"% -", 
                      round(quantile(100*PM[foodindex[i],hazardindexM[h]]*pM[,hazardindexM[h],foodindex[i]]*p0[,foodindex[i]],0.975,names=FALSE),1),"%"),
                side = 1, adj = 0,line=0, cex = 1,
                outer = TRUE)
         
        ############################################################################################ 
        } # end of if constant.consum FALSE  #########################################################
        else {  # consumption is constant. --> Chronic exposure =E(c)*consum, only uncertainty distribution of chronic exposure.
                # but uncertainty for variability distribution of acute exposures.
           source("constantconsumM.R",local=TRUE)
          } # end of if constant consumption  
          
        } else # end of if hazard-food modeled  
            
          ##Empty plot----
        {
          par(mar = c(0,0,0,0))
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.8, paste("The data for this food-hazard combination is not sufficient for modeling\n",
                                       "(e.g., concentration measurements (>LOQ) about the food-hazard combination are missing)."), 
               cex = 1.6, col = "#D0006F")
          par(mar = c(5, 4, 4, 2) + 0.1)
        }     
      }} # end of for nhusedM nfused
  } # end of if nhusedM nfused >0
  
}


# Plot 4: Quantiles:----

## ---- distPlot4_1 -------- 
distPlot4_1 <- function(unit_concen, hazard_concen, n_sim, input_selectscale, input_selectQ, nV,
                        nU, Rall, Pall, input_modelchoice, input_modelchoice2,
                        nfused, foodindex, 
                        nexactK, nexactM, 
                        nhused, hazardnames, hazardnamesusedK, hazardnamesusedM,
                        nhusedK, nhusedM, hazardindexK, hazardindexM, nhK,nhM,nf,
                        mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                        Ss,Ss0,Sp,
                        logitp0,constant.consum,osdlogsw1,osdlogsw2){
  # generate results based on inputs from ui.R:  
  # uncertainties of variability quantiles
  if(constant.consum==FALSE){
  if (!is.element("None", input_selectQ)) {
    # which percentile is selected?: 
    if(is.element("Q5% Exposure",input_selectQ)){theQ=5}
    if(is.element("Q10% Exposure",input_selectQ)){theQ=10}
    if(is.element("Q25% Exposure",input_selectQ)){theQ=25}
    if(is.element("Q50% Exposure",input_selectQ)){theQ=50}
    if(is.element("Q75% Exposure",input_selectQ)){theQ=75}
    if(is.element("Q90% Exposure",input_selectQ)){theQ=90}
    if(is.element("Q95% Exposure",input_selectQ)){theQ=95}
    
    par(oma = c(4, 1, 0, 1),cex.lab=1.3,cex.main=1.3) # Outer margins for legend
    
    
    # generate nU variability distributions (each with nV variability simulations), 
    # then evaluate quantiles for each of those variability distributions:
    nU <- min(nU,n_sim) # number of uncertainty samples nU cannot be larger than total n_sim from MCMC.
    mc <- round(seq(1,n_sim,length=nU),0) 
    
    p0 <- exp(logitp0)/(1+exp(logitp0))
    # Chemical exposure quantiles----
    
    if((nhusedK>0)&(nfused>0)){ 
      RK = matrix(NA,nf,nhK) # concentration factors
      RK[1:nf,1:nhK] = Rall[1:nf,is.element(hazardnames,hazardnamesusedK)]
      logRK = log(RK)
      PK = matrix(NA,nf,nhK) # prevalence factors
      PK[1:nf,1:nhK] = Pall[1:nf,is.element(hazardnames,hazardnamesusedK)]
      
      #Independent days----
      if((input_modelchoice == "Independent days")|(input_modelchoice=="Fixed variance")){  
        
        logitpmc <- matrix(NA,nV,nf); pmc<-logitpmc; musmc <- pmc
        Eemc <- array(NA,dim=c(nV,nhusedK,nfused))  # for all days
        Eemcconuse <- array(NA,dim=c(nV,nhusedK,nfused)) # for contaminated consumption days
        Eetotmc <- matrix(NA,nV,nhusedK)
        Eetotmcconuse <- matrix(NA,nV,nhusedK)
        Q <- matrix(NA,nU,nhusedK) # for chronic exposure all days
        Qplus <- matrix(NA,nU,nhusedK) # for chronic exposure from consumption days
        thin <- 0 # for indexing a thinned sample of (simulated) variability distributions
        exposurevarsample <- matrix(NA,ceiling(nU/5),nV) # for thinned uncertainty sample
        
        # 2D simulation of uncertainty & variability:
        Vs <- numeric() # variances
        for(u in 1:nU){ # for nU parameter sets
          if(nf>1){ # if many foods
            if(input_modelchoice2=="Yes" ){ # variability between users' frequencies
              logitpmc[1:nV,1:nf] <- rmvnorm(nV,logitp0[mc[u],1:nf],Sp[mc[u],1:nf,1:nf])
            }
            if(input_modelchoice2=="No" ){ # no variability between users' frequencies   
              logitpmc[1:nV,1:nf] <- t(matrix(logitp0[mc[u],1:nf],nf,nV))  
            }
            pmc[1:nV,1:nf] <- exp(logitpmc[1:nV,1:nf])/(1+exp(logitpmc[1:nV,1:nf])) # individual probability of consuming foods
            if(input_modelchoice=="Fixed variance"){
            musmc[1:nV,1:nf] <- rmvnorm(nV,mus0[mc[u],1:nf], diag(osdlogsw1^2) ) # individual (>0) mean amount consumed   
            }else
            musmc[1:nV,1:nf] <- rmvnorm(nV,mus0[mc[u],1:nf],Ss0[mc[u],1:nf,1:nf]) # individual (>0) mean amount consumed
          }
          if(nf==1){ # if only one food
            if(input_modelchoice2=="Yes" ){ # variability between users' frequencies
              logitpmc[1:nV,1] <- rnorm(nV,logitp0[mc[u],1],sqrt(Sp[mc[u],1,1]))
            }
            if(input_modelchoice2=="No" ){ # no variability between users' frequencies  
              logitpmc[1:nV,1] <- rep(logitp0[mc[u],1],nV)
            }
            pmc[1:nV,1] <- exp(logitpmc[1:nV,1])/(1+exp(logitpmc[1:nV,1])) # individual probability of consuming food
            if(input_modelchoice=="Fixed variance"){    
            musmc[1:nv,1] <- rnorm(nV,mus0[mc[u],1], osdlogsw1[1] ) # individual (>0) mean amount consumedosdlogsw1,osdlogsw2
            }else
            musmc[1:nV,1] <- rnorm(nV,mus0[mc[u],1],sqrt(Ss0[mc[u],1,1])) # individual (>0) mean amount consumed
          }
          
          for(v in 1:nV){ # for nV variable values per each parameter set
            
            h<- nhusedK # =1, one selected hazard for quantile analysis
            
              for(i in 1:nfused){
                if(nexactK[hazardindexK[h],foodindex[i]]==0){ # hazard-food not modeled
                  Eemc[v,h,i]<- 0
                  Eemcconuse[v,h,i] <- 0
                }
                if(nexactK[hazardindexK[h],foodindex[i]]>0){
                  
                  # evaluate mean exposure of hazard h in food i, variable individual v,
                  # for all days (consumed or not, contaminated or not)
                  if(input_modelchoice=="Fixed variance"){
                    Vs[u] <- osdlogsw2[foodindex[i]]^2   
                  }else
                    Vs[u] <- Ss[mc[u],foodindex[i],foodindex[i]]
                  
                  Eemc[v,h,i]<-pK[mc[u],hazardindexK[h],foodindex[i]]*
                    PK[foodindex[i],hazardindexK[h]]*
                    pmc[v,foodindex[i]]*exp(logRK[foodindex[i],hazardindexK[h]]
                                            +musmc[v,foodindex[i]]
                                            +0.5*Vs[u]
                                            +mucK[mc[u],hazardindexK[h],foodindex[i]]
                                            +0.5*sigcK[mc[u],hazardindexK[h],foodindex[i]]^2)  
                  
                  # evaluate mean exposure of hazard h in food i, variable individual v,
                  # for actual consumption days for actual contaminated occurrences
                  Eemcconuse[v,h,i] <- exp(logRK[foodindex[i],hazardindexK[h]]
                                           +musmc[v,foodindex[i]]
                                           +0.5*Vs[u]
                                           +mucK[mc[u],hazardindexK[h],foodindex[i]]
                                           +0.5*sigcK[mc[u],hazardindexK[h],foodindex[i]]^2)
                } # end of if nexactK>0
              } # end of i
              # simulated total chronic exposure for individual, hazard h, all foods:
              Eetotmc[v,h] <- sum(Eemc[v,h,1:nfused]) 
              # simulated total chronic exposure for individual,
              # for contaminated consumption days, hazard h, all foods 
              Eetotmcconuse[v,h] <- sum(Eemcconuse[v,h,1:nfused])   
             
          } # end of v (variability) 
    
            # variability quantile of total exposure, hazard h
            Q[u,h]<-quantile(Eetotmc[,h],theQ/100,names=FALSE)
            # variability quantile of total exposure, hazard h, 
            # for contaminated consumption days
            Qplus[u,h]<-quantile(Eetotmcconuse[,h],theQ/100,names=FALSE)
          
          #######################################################
          # pick out thinned sample:
          if(ceiling(u/5)==floor(u/5)){ thin<-thin+1; exposurevarsample[thin,1:nV]<- t(Eetotmcconuse[1:nV,h]) }
          #######################################################
        } # end of u (uncertainty)
        
          Unit <- unit_concen[hazard_concen == hazardnamesusedK[h]] # the measurement unit used for hazard concentration
          Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
          
          ##Absolute----
          if(input_selectscale=="Absolute"){
            #########################################
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotK <- sum(nexactK[hazardindexK[h],foodindex]>0)
            xmin <- min(exposurevarsample[1:thin,1:nV],na.rm=TRUE)
            xmax <- max(quantile(exposurevarsample[1:thin,1:nV],0.95,na.rm=TRUE,names=FALSE),
                    quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE) ) 
            plot(ecdf(exposurevarsample[1,1:nV]),verticals=TRUE,do.points=FALSE,yaxt="s",
                 xlim=c(xmin,xmax),ylim=c(0,1),
                 lwd=1,lty=3,col=rgb(0.816,0.004,0.435),      
                 xlab=paste("C.exposure/bw+ (", Unit1, "per kg)"),ylab="Cumulative probability",
                 main=paste("Exposure:",hazardnamesusedK[h],"total from",nftotK,"foods (chronic)"))
          
            for(a in 2:thin){
              lines(ecdf(exposurevarsample[a,1:nV]),verticals=TRUE,do.points=FALSE,
                    xlim=c(xmin,xmax),
                    lwd=1,lty=3,col="#D0006F")     
            }
            quplim <- quantile(Qplus[,h],0.95,names=FALSE)
            qlolim <- quantile(Qplus[,h],0.05,names=FALSE)
            lines(density(Qplus[,h],from=qlolim,to=quplim)$x,density(Qplus[,h],from=qlolim,to=quplim)$y/max(density(Qplus[,h],from=qlolim,to=quplim)$y),lwd=3)   
            lines(quantile(Qplus[,h],c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(Qplus[,h],c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste0("Q",theQ,"% for the positive days mean exposures: ",round(quantile(Qplus[,h],0.5,names=FALSE,na.rm=TRUE),2),  
                        " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(Qplus[,h],0.05,names=FALSE,na.rm=TRUE),2),"-", round(quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste0("Q",theQ,"% for all days mean exposures: ",round(quantile(Q[,h],0.5,names=FALSE),2),  
                        " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(Q[,h],0.05,names=FALSE),2),"-", round(quantile(Q[,h],0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          } # end of absolute
          
          ##Logarithmic----
          if(input_selectscale=="Logarithmic"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotK <- sum(nexactK[hazardindexK[h],foodindex]>0) 
            xmin <- log10(min(exposurevarsample[1:thin,1:nV],na.rm=TRUE))
            xmax <- max(log10(quantile(exposurevarsample[1:thin,1:nV],0.95,na.rm=TRUE,names=FALSE)),
                    quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE))
            plot(ecdf(log(exposurevarsample[1,1:nV])/log(10)),verticals=TRUE,do.points=FALSE,yaxt="s",
                 xlim=c(xmin,xmax),ylim=c(0,1),
                 lwd=1,lty=3,col="#D0006F",ylab="Cumulative probability",
                 xlab=paste("log( C.exposure/bw+)(", Unit1,"per kg)"),
                 main=paste("Exposure:",hazardnamesusedK[h],"total from",nftotK,"foods (chronic)"))
            for(a in 2:thin){
              lines(ecdf(log(exposurevarsample[a,1:nV])/log(10)),verticals=TRUE,do.points=FALSE,
                    xlim=c(xmin,xmax),
                    lwd=1,lty=3,col="#D0006F")     
            }
            quplim <- quantile(log10(Qplus[,h]),0.95,names=FALSE)
            qlolim <- quantile(log10(Qplus[,h]),0.05,names=FALSE)
            lines(density(log10(Qplus[,h]),from=qlolim,to=quplim)$x,density(log10(Qplus[,h]),from=qlolim,to=quplim)$y/max(density(log10(Qplus[,h]),from=qlolim,to=quplim)$y),lwd=3)   
            lines(quantile(log10(Qplus[,h]),c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste0("Q",theQ,"% for the positive days mean log-exposures: ",round(quantile(log10(Qplus[,h]),0.5,names=FALSE,na.rm=TRUE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(log10(Qplus[,h]),0.05,names=FALSE,na.rm=TRUE),2),"-", round(quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste0("Q",theQ,"% for all days mean log-exposures: ",round(quantile(log10(Q[,h]),0.5,names=FALSE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(log10(Q[,h]),0.05,names=FALSE),2),"-", round(quantile(log10(Q[,h]),0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          } # end of logarithmic    
        
      } # end of if independent days | Fixed variance
      
      ########################################
      ##Dependent days----
      if(input_modelchoice == "Dependent days"){
        musmc <- matrix(NA,nV,nf)
        Eemc <- array(NA,dim=c(nV,nhusedK,nfused)) # for all days
        Eemcconuse <- array(NA,dim=c(nV,nhusedK,nfused)) # for contaminated consumption days
        Eetotmc <- matrix(NA,nV,nhusedK)
        Eetotmcconuse <- matrix(NA,nV,nhusedK)
        Q <- matrix(NA,nU,nhusedK) # for chronic exposure all days
        Qplus <- matrix(NA,nU,nhusedK) # for chronic exposure from consumption days
        thin <- 0 # for indexing a thinned sample of (simulated) variability distributions
        exposurevarsample <- matrix(NA,ceiling(nU/5),nV) # for thinned uncertainty sample
        
        Vs <- numeric() # variances
        for(u in 1:nU){ # for nU parameter sets
            if(nf>1){ # if several foods
              musmc[1:nV,1:nf] <- rmvnorm(nV,mus0[mc[u],1:nf],Ss0[mc[u],1:nf,1:nf]) # individual mean amount
            }
          if(nf==1){ # if only one food
            musmc[1:nV,1] <- rnorm(nV,mus0[mc[u],1],sqrt(Ss0[mc[u],1,1])) # individual mean amount
          }
          
          for(v in 1:nV){ # for nV variable values per each parameter set
            
            h<- nhusedK # =1, one selected hazard for quantile analysis
            
              Unit <- unit_concen[hazard_concen == hazardnamesusedK[h]] # the measurement unit used for hazard concentration
              Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
              
              for(i in 1:nfused){
                if(nexactK[hazardindexK[h],foodindex[i]]==0){ # hazard-food not modeled
                  Eemc[v,h,i] <- 0
                  Eemcconuse[v,h,i] <- 0
                }
                if(nexactK[hazardindexK[h],foodindex[i]]>0){
                  # variance for consumptions:  
                  Vs[u] <- Ss[mc[u],foodindex[i],foodindex[i]]
                  # evaluate mean exposure of hazard h in food i, variable individual v,
                  # for all days (consumed or not, contaminated or not)
                  Eemc[v,h,i] <- pK[mc[u],hazardindexK[h],foodindex[i]]*
                    PK[foodindex[i],hazardindexK[h]]*
                    p0[mc[u],foodindex[i]]*exp(logRK[foodindex[i],hazardindexK[h]]
                                                  +musmc[v,foodindex[i]]
                                                  +0.5*Vs[u]
                                                  +mucK[mc[u],hazardindexK[h],foodindex[i]]
                                                  +0.5*sigcK[mc[u],hazardindexK[h],foodindex[i]]^2)  
                  
                  # evaluate mean exposure of hazard h in food i, variable individual v,
                  # for actual consumption days for actual contaminated occurrences
                  Eemcconuse[v,h,i] <- exp(logRK[foodindex[i],hazardindexK[h]]
                                           +musmc[v,foodindex[i]]
                                           +0.5*Vs[u]
                                           +mucK[mc[u],hazardindexK[h],foodindex[i]]
                                           +0.5*sigcK[mc[u],hazardindexK[h],foodindex[i]]^2)
                  
                } # end of if nexactK >0
              } # end of i
              # simulated total chronic exposure for individual: 
              Eetotmc[v,h] <- sum(Eemc[v,h,1:nfused])    
              # simulated total chronic exposure for individual for contaminated consumption days:
              Eetotmcconuse[v,h] <- sum(Eemcconuse[v,h,1:nfused]) 
          } # end of v (variability)   
    
            # variability quantile of total exposure, hazard h
            Q[u,h]<-quantile(Eetotmc[,h],theQ/100,names=FALSE)
            # variability quantile of total exposure, hazard h, 
            # for contaminated consumption days
            Qplus[u,h]<-quantile(Eetotmcconuse[,h],theQ/100,names=FALSE)
          
          #######################################################
          # pick out thinned sample:
          if(ceiling(u/5)==floor(u/5)){ thin<-thin+1; exposurevarsample[thin,1:nV]<- t(Eetotmcconuse[1:nV,h]) }
          #######################################################
        } # end of u (uncertainty)
        
        # plot the simulated variability distributions, for each uncertain parameter set
          
          ###Absolute----
          if(input_selectscale=="Absolute"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotK <- sum(nexactK[hazardindexK[h],foodindex]>0)
            xmin <- min(exposurevarsample[1:thin,1:nV],na.rm=TRUE)
            xmax <- max(quantile(exposurevarsample[1:thin,1:nV],0.95,na.rm=TRUE,names=FALSE),
                    quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE)) 
            plot(ecdf(exposurevarsample[1,1:nV]),verticals=TRUE,do.points=FALSE,yaxt="s",
                 xlim=c(xmin,xmax),ylim=c(0,1),
                 lwd=1,lty=3,col="#D0006F",
                 xlab=paste("C.exposure/bw+ (", Unit1, "per kg)"),ylab="Cumulative probability",
                 main=paste("Uncertainty of distribution:",hazardnamesusedK[h],"total from",nftotK,"foods (chronic)"))
            for(a in 2:thin){
              lines(ecdf(exposurevarsample[a,1:nV]),verticals=TRUE,do.points=FALSE,
                    xlim=c(xmin,xmax),
                    lwd=1,lty=3,col="#D0006F")     
            }
            quplim <- quantile(Qplus[,h],0.95,names=FALSE)
            qlolim <- quantile(Qplus[,h],0.05,names=FALSE)
            lines(density(Qplus[,h],from=qlolim,to=quplim)$x,density(Qplus[,h],from=qlolim,to=quplim)$y/max(density(Qplus[,h],from=qlolim,to=quplim)$y),lwd=3)   
            lines(quantile(Qplus[,h],c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(Qplus[,h],c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)

            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste0("Q",theQ,"% for the positive days mean exposures: ",round(quantile(Qplus[,h],0.5,names=FALSE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(Qplus[,h],0.05,names=FALSE),2),"-", round(quantile(Qplus[,h],0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste0("Q",theQ,"% for all days mean exposures: ",round(quantile(Q[,h],0.5,names=FALSE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(Q[,h],0.05,names=FALSE),2),"-", round(quantile(Q[,h],0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          } # end of absolute
          
          ###Logarithmic----
          if(input_selectscale=="Logarithmic"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotK <- sum(nexactK[hazardindexK[h],foodindex]>0)
            xmin <- log10(min(exposurevarsample[1:thin,1:nV],na.rm=TRUE))
            xmax <- max(log10(quantile(exposurevarsample[1:thin,1:nV],0.95,na.rm=TRUE,names=TRUE)),
                    quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE))
            plot(ecdf(log(exposurevarsample[1,1:nV])/log(10)),verticals=TRUE,
                 do.points=FALSE,yaxt="s",
                 xlim=c(xmin,xmax),ylim=c(0,1),
                 lwd=1,lty=3,col="#D0006F",ylab="Cumulative probability",
                 xlab=paste("log( C.exposure/bw+)(", Unit1,"per kg)"),
                 main=paste("Uncertainty of distribution:",hazardnamesusedK[h],"total from",nftotK,"foods (chronic)"))
            for(a in 2:thin){
              lines(ecdf(log(exposurevarsample[a,1:nV])/log(10)),verticals=TRUE,do.points=FALSE,
                    xlim=c(xmin,xmax),
                    lwd=1,lty=3,col="#D0006F")     
            }    
            quplim <- quantile(log10(Qplus[,h]),0.95,names=FALSE)
            qlolim <- quantile(log10(Qplus[,h]),0.05,names=FALSE)
            lines(density(log10(Qplus[,h]),from=qlolim,to=quplim)$x,density(log10(Qplus[,h]),from=qlolim,to=quplim)$y/max(density(log10(Qplus[,h]),from=qlolim,to=quplim)$y),lwd=3)  
            lines(quantile(log10(Qplus[,h]),c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste0("Q",theQ,"% for the positive days mean log-exposures: ",round(quantile(log10(Qplus[,h]),0.5,names=FALSE,na.rm=TRUE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(log10(Qplus[,h]),0.05,names=FALSE,na.rm=TRUE),2),"-", round(quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste0("Q",theQ,"% for all days mean log-exposures: ",round(quantile(log10(Q[,h]),0.5,names=FALSE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(log10(Q[,h]),0.05,names=FALSE),2),"-", round(quantile(log10(Q[,h]),0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          } # end of logarithmic
        
      } # end of if dependent days
    } # end of if nhusedK>0 nfused>0
    
    #######################################################################
    
    # Microbial exposure quantiles----
    if((nhusedM>0)&(nfused>0)){  
      
      RM = matrix(NA,nf,nhM) # factors for concentrations
      RM[1:nf,1:nhM] = Rall[1:nf,is.element(hazardnames,hazardnamesusedM)]
      logRM = log(RM)
      PM = matrix(NA,nf,nhM) # factors for prevalence
      PM[1:nf,1:nhM] = Pall[1:nf,is.element(hazardnames,hazardnamesusedM)]
      
      ##Independent days----
      if((input_modelchoice == "Independent days")|(input_modelchoice=="Fixed variance")){ 
         
        wmc <- numeric()
        logitpmc <- matrix(NA,nV,nf); pmc<-logitpmc; 
        musmc <- matrix(NA,nV,nf)
        Umc<-musmc; smc<-musmc
        Imc <- array(NA,dim=c(nV,nhM,nf)); cmc<-Imc
        nplus<-matrix(NA,nU,nhusedM)
        poissonmeansall <- array(NA,dim=c(nV,nhusedM,nfused))
        poissonmeanspos <- array(NA,dim=c(nV,nhusedM,nfused))
        acuteexpoall <- matrix(NA,nV,nhusedM)
        acuteexpopos <- matrix(NA,nV,nhusedM) 
        Q <- matrix(NA,nU,nhusedM); Qplus <- matrix(NA,nU,nhusedM) 
        thin <- 0 # for indexing a thinned sample of (simulated) variability distributions
        exposurevarsample <- matrix(NA,ceiling(nU/5),nV) # for thinned uncertainty sample
        
        h<- nhusedM # =1, one selected hazard for quantile analysis
        
        # 2D simulation of uncertainty & variability:
        for(u in 1:nU){ # for nU parameter sets
          
          wmc[1:nV] <- rlnorm(nV,muw[mc[u]],sigw[mc[u]]) # bodyweight for v:th individual
            if(nf>1){ # if many foods
              if(input_modelchoice2=="Yes" ){ # variability between users' frequencies
                logitpmc[1:nV,1:nf] <- rmvnorm(nV,logitp0[mc[u],1:nf],Sp[mc[u],1:nf,1:nf])
              }
              if(input_modelchoice2=="No" ){ # no variability between users' frequencies   
                logitpmc[1:nV,1:nf] <- t(matrix(logitp0[mc[u],1:nf],nf,nV))
              } 
              pmc[1:nV,1:nf] <- exp(logitpmc[1:nV,1:nf])/(1+exp(logitpmc[1:nV,1:nf]))
              for(f in 1:nf){
                Umc[1:nV,f] <- rbinom(nV,1,pmc[1:nV,f])  # actual use  
              }
              if(input_modelchoice=="Fixed variance"){
              musmc[1:nV,1:nf] <- rmvnorm(nV,mus0[mc[u],1:nf], diag(osdlogsw1^2) ) 
              }else
              musmc[1:nV,1:nf] <- rmvnorm(nV,mus0[mc[u],1:nf],Ss0[mc[u],1:nf,1:nf])
            for(v in 1:nV){  
              if(input_modelchoice=="Fixed variance"){
              smc[v,1:nf] <- exp(rmvnorm(1,musmc[v,1:nf], diag(osdlogsw2^2) ))  # actual amount   
              }else
              smc[v,1:nf] <- exp(rmvnorm(1,musmc[v,1:nf],Ss[mc[u],1:nf,1:nf]))  # actual amount  
            }
            }
          if(nf==1){ # if only one food
            if(input_modelchoice2=="Yes" ){ # variability between users' frequencies 
              logitpmc[1:nV,1] <- rnorm(nV,logitp0[mc[u],1],sqrt(Sp[mc[u],1,1]))
            }
            if(input_modelchoice2=="No" ){ # no variability between users' frequencies  
              logitpmc[1:nV,1] <- rep(logitp0[mc[u],1],nV)  
            } 
            pmc[1:nV,1] <- exp(logitpmc[1:nV,1])/(1+exp(logitpmc[1:nV,1]))
            Umc[1:nV,1] <- rbinom(nV,rep(1,1),pmc[1:nV,1]) # actual use
            if(input_modelchoice=="Fixed variance"){
            musmc[1:nV,1] <- rnorm(nV,mus0[mc[u],1],osdlogsw1[1]) 
            smc[1:nV,1] <- exp(rnorm(nV,musmc[1:nV,1],osdlogsw2[1]))  # actual amount   
            }else{
            musmc[1:nV,1] <- rnorm(nV,mus0[mc[u],1],sqrt(Ss0[mc[u],1,1]))
            smc[1:nV,1] <- exp(rnorm(nV,musmc[1:nV,1],sqrt(Ss[mc[u],1,1])))  # actual amount 
            }
          }
            for(f in 1:nf){  
            # actual contamination yes/no:
            Imc[1:nV,hazardindexM[h],f] <- rbinom(nV,1,pM[mc[u],hazardindexM[h],f]*PM[f,hazardindexM[h]])
            # actual concentration level:
            cmc[1:nV,hazardindexM[h],f] <- rlnorm(nV,mucM[mc[u],hazardindexM[h],f],sigcM[mc[u],hazardindexM[h],f])
            }
          
            for(i in 1:nfused){
              
              if(nexactM[hazardindexM[h],foodindex[i]]==0){ # hazard-food not modeled
                poissonmeansall[1:nV,h,i] <- rep(0,nV)
                poissonmeanspos[1:nV,h,i] <- rep(0,nV)
              }
              if(nexactM[hazardindexM[h],foodindex[i]]>0){ 
                # (poisson)mean exposure for day serving when contaminated & consumed:
                poissonmeanspos[1:nV,h,i] <- smc[1:nV,foodindex[i]]*
                  RM[foodindex[i],hazardindexM[h]]*
                  cmc[1:nV,hazardindexM[h],foodindex[i]]*wmc[1:nV]
                # (poisson)mean exposure for any day incl. zeros:
                poissonmeansall[1:nV,h,i] <- Imc[1:nV,hazardindexM[h],foodindex[i]]*
                  Umc[1:nV,foodindex[i]]*
                  smc[1:nV,foodindex[i]]*
                  RM[foodindex[i],hazardindexM[h]]*
                  cmc[1:nV,hazardindexM[h],foodindex[i]]*wmc[1:nV]
                
              } # end of if nexactM>0
            } # end of for i
          
          
          for(v in 1:nV){ # for nV variable values per each parameter set
            
              # microbiological exposure from Poisson distribution, 
              # but approximately from normal if large mean:  
              if(sum(poissonmeansall[v,h,1:nfused])<=5000){
                # sum of all food serving exposures incl. zeros
                acuteexpoall[v,h] <- rpois(1,sum(poissonmeansall[v,h,1:nfused]))
              }   
              if(sum(poissonmeansall[v,h,1:nfused])>5000){
                # sum of all food serving exposures, incl. zeros: 
                acuteexpoall[v,h] <- round(rnorm(1,sum(poissonmeansall[v,h,1:nfused]),sqrt(sum(poissonmeansall[v,h,1:nfused])))) 
              }   
              if(sum(poissonmeanspos[v,h,1:nfused])<=5000){
                # sum of all food serving exposures when all foods used and contaminated: 
                acuteexpopos[v,h] <- rpois(1,sum(poissonmeanspos[v,h,1:nfused]))
              } 
              if(sum(poissonmeanspos[v,h,1:nfused])>5000){
                # sum of all food serving exposures when all foods used and contaminated:
                acuteexpopos[v,h] <- round(rnorm(1,sum(poissonmeanspos[v,h,1:nfused]),sqrt(sum(poissonmeanspos[v,h,1:nfused]))))
              }     
          } # end of v (variability)
          
            nplus[u,h] <- sum(acuteexpopos[1:nV,h]>0) # number of actually positive exposures simulated for hazard h 
            if(nplus[u,h]<=19){  # not enough to estimate quantiles of the non-zeros
              # quantile from positive servings =NA, when Poisson outcomes were all zero:
              Qplus[u,h] <- NA  
            }
            if(nplus[u,h]>19){
              # quantile from actually positive servings, 
              # among those where Poisson outcomes (bacteria counts) were truly positive:
              Qplus[u,h]<-quantile(acuteexpopos[acuteexpopos[1:nV,h]>0,h],theQ/100,names=FALSE) 
            }
            # quantile from ALL servings, incl. zero contaminations (due to not consuming the food the day, or food not contaminated the day):
            Q[u,h] <- quantile(acuteexpoall[,h],theQ/100,names=FALSE) 
          #######################################################
          # pick out thinned sample of positive acute exposures:
          if(ceiling(u/5)==floor(u/5)){ 
            thin<-thin+1 
            #if(nplus[u,h]<=19){    # not needed, initiated as NAs
            #  exposurevarsample[thin,1]<-NA  
            #}
            if(nplus[u,h]>19){
              exposurevarsample[thin,1:nplus[u,h]] <- t(acuteexpopos[acuteexpopos[1:nV,h]>0,h]) 
            }
          }
          #######################################################
          
        } # end of u (uncertainty)
        
          Unit <- unit_concen[hazard_concen == hazardnamesusedM[h]] # the measurement unit used for hazard concentration
          Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
          
          ###Absolute----
          if(input_selectscale=="Absolute"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotM <- sum(nexactM[hazardindexM[h],foodindex]>0)
            
            if(sum(nplus[,h]<=19)==0){ # all simulations had at least 20 positive exposures
            if(sum(!is.na(exposurevarsample[1,]))>0){
              
              xmin <- min(exposurevarsample[1:thin,1:max(nplus)],na.rm=TRUE)
              xmax <- max(quantile(exposurevarsample[1:thin,1:max(nplus)],0.95,na.rm=TRUE,names=FALSE),
                      quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE))
              plot(ecdf(exposurevarsample[1,!is.na(exposurevarsample[1,])]),verticals=TRUE,
                   do.points=FALSE,yaxt="s",
                   xlim=c(xmin,xmax),ylim=c(0,1),
                   lwd=1,lty=3,col="#D0006F",
                   xlab=paste("A.dose+ (", Unit1,"per day)"),ylab="Cumulative probability",
                   main=paste("Exposures:",hazardnamesusedM[h],"total from",nftotM,"foods (acute).\n Each curve >=",min(nplus[,h]),"positive values"))
            }
            for(a in 2:thin){
              if(sum(!is.na(exposurevarsample[a,]))>0){   
                lines(ecdf(exposurevarsample[a,!is.na(exposurevarsample[a,])]),
                      verticals=TRUE,do.points=FALSE,
                      xlim=c(xmin,xmax),
                      ylim=c(0,1),
                      lwd=1,lty=3,col="#D0006F")     
              }
            }
            quplim <- quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE)
            qlolim <- quantile(Qplus[,h],0.05,names=FALSE,na.rm=TRUE)
            lines(density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$x,density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$y/max(density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$y),lwd=3)   
            lines(quantile(Qplus[,h],c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(Qplus[,h],c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste0("Q",theQ,"% for the positive days single exposures: ",round(quantile(Qplus[,h],0.5,names=FALSE,na.rm=TRUE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(Qplus[,h],0.05,names=FALSE,na.rm=TRUE),2),"-", round(quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste0("Q",theQ,"% for all days single exposures: ",round(quantile(Q[,h],0.5,names=FALSE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(Q[,h],0.05,names=FALSE),2),"-", round(quantile(Q[,h],0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            }
            
            if(sum(nplus[,h]<=19)>=1){ # at least some simulations had less than 20 positive exposures  
              par(mar = c(0,0,0,0))
              plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
              text(x = 0.5, y = 0.8, paste("Less than 20 positive exposure values \n occurred in some simulations. \n",
                                           "Not reliable for quantile analysis. \n Increase variability sample size."), 
                   cex = 1.6, col = "#D0006F")
              par(mar = c(5, 4, 4, 2) + 0.1)
            }
            
            
          }
          
          ###Logarithmic----
          if(input_selectscale=="Logarithmic"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotM <- sum(nexactM[hazardindexM[h],foodindex]>0)
            
            if(sum(nplus[,h]<=19)==0){ # all simulations had at least 20 positive exposures
            if(sum(!is.na(exposurevarsample[1,]))>0){
              
              xmin <- log10(min(exposurevarsample[1:thin,1:max(nplus)],na.rm=TRUE))
              xmax <- max(log10(quantile(exposurevarsample[1:thin,1:max(nplus)],0.95,na.rm=TRUE,names=FALSE)),
                      quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE))
              plot(ecdf(log10(exposurevarsample[1,!is.na(exposurevarsample[1,])])),
                   verticals=TRUE,do.points=FALSE,yaxt="s",
                   xlim=c(xmin,xmax),ylim=c(0,1),
                   lwd=1,lty=3,col="#D0006F",
                   xlab=paste("log( A.dose+ (", Unit1,"per day))"),ylab="Cumulative probability",
                   main=paste("Exposures:",hazardnamesusedM[h],"total from",nftotM,"foods (acute).\n Each curve >=",min(nplus[,h]),"positive values"))
            }
            for(a in 2:thin){
              if(sum(!is.na(exposurevarsample[a,]))>0){   
                lines(ecdf(log10(exposurevarsample[a,!is.na(exposurevarsample[a,])])),verticals=TRUE,do.points=FALSE,
                      xlim=c(xmin,xmax),
                      lwd=1,lty=3,col="#D0006F")     
              }
            }
            quplim <- quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE)
            qlolim <- quantile(log10(Qplus[,h]),0.05,names=FALSE,na.rm=TRUE)
            lines(density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$x,density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$y/max(density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$y),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste0("Q",theQ,"% for the positive days single log-exposures: ",round(quantile(log10(Qplus[,h]),0.5,names=FALSE,na.rm=TRUE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(log10(Qplus[,h]),0.05,names=FALSE,na.rm=TRUE),2),"-", round(quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste0("Q",theQ,"% for all days single log-exposures: ",round(quantile(log10(Q[,h]),0.5,names=FALSE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(log10(Q[,h]),0.05,names=FALSE),2),"-", round(quantile(log10(Q[,h]),0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            }
            
            if(sum(nplus[,h]<=19)>=1){ # at least some simulations had less than 20 positive exposures  
              par(mar = c(0,0,0,0))
              plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
              text(x = 0.5, y = 0.8, paste("Less than 20 positive exposure values \n occurred in some simulations. \n",
                                           "Not reliable for quantile analysis. \n Increase variability sample size."), 
                   cex = 1.6, col = "#D0006F")
              par(mar = c(5, 4, 4, 2) + 0.1)
            }
            
            
            
          }
      } # end of if independent days
      
      ########################################
      
      ##Dependent days----
      if(input_modelchoice == "Dependent days"){
        wmc <- numeric()
        pmc <- matrix(NA,nV,nf) 
        musmc <- matrix(NA,nV,nf)
        Umc<-musmc; smc<-musmc
        Imc <- array(NA,dim=c(nV,nhM,nf)); cmc<-Imc
        nplus<-matrix(NA,nU,nhusedM)
        poissonmeansall <- array(NA,dim=c(nV,nhusedM,nfused))
        poissonmeanspos <- array(NA,dim=c(nV,nhusedM,nfused))
        acuteexpoall <- matrix(NA,nV,nhusedM)
        acuteexpopos <- matrix(NA,nV,nhusedM) 
        Q <- matrix(NA,nU,nhusedM); Qplus <- matrix(NA,nU,nhusedM) 
        thin <- 0 # for indexing a thinned sample of (simulated) variability distributions
        exposurevarsample <- matrix(NA,ceiling(nU/5),nV) # for thinned uncertainty sample
        
        h<- nhusedM # =1, one selected hazard for quantile analysis
        
        for(u in 1:nU){ # for nU parameter sets
            wmc[1:nV] <- rlnorm(nV,muw[mc[u]],sigw[mc[u]]) # bodyweight
            
            if(nf>1){ # if many foods
            for(f in 1:nf){
              Umc[1:nV,f] <- rbinom(nV,1,p0[mc[u],f])  # actual use  
            }
            
            musmc[1:nV,1:nf] <- rmvnorm(nV,mus0[mc[u],1:nf],Ss0[mc[u],1:nf,1:nf])
            for(v in 1:nV){  
              smc[v,1:nf] <- exp(rmvnorm(1,musmc[v,1:nf],Ss[mc[u],1:nf,1:nf]))  # actual amount  
            }
            }
            if(nf==1){ # if only one food
              musmc[1:nV,1] <- rnorm(nV,mus0[mc[u],1],sqrt(Ss0[mc[u],1,1]))
              smc[1:nV,1] <- rlnorm(nV,musmc[1:nV,1],sqrt(Ss[mc[u],1,1])) # actual amount  
              Umc[1:nV,1] <- rbinom(nV,1,p0[mc[u],1])  # actual use  
            }
              for(f in 1:nfused){  
                # actual contamination yes/no:
                Imc[1:nV,hazardindexM[h],f] <- rbinom(nV,1,pM[mc[u],hazardindexM[h],f]*PM[f,hazardindexM[h]])
                # actual concentration level:
                cmc[1:nV,hazardindexM[h],f] <- rlnorm(nV,mucM[mc[u],hazardindexM[h],f],sigcM[mc[u],hazardindexM[h],f])
              }
              for(i in 1:nfused){
                if(nexactM[hazardindexM[h],foodindex[i]]==0){ # hazard-food not modeled
                  poissonmeansall[1:nV,h,i] <- rep(0,nV)
                  poissonmeanspos[1:nV,h,i] <- rep(0,nV)
                }
                if(nexactM[hazardindexM[h],foodindex[i]]>0){
                  poissonmeansall[1:nV,h,i] <- Imc[1:nV,hazardindexM[h],foodindex[i]]*
                    Umc[1:nV,foodindex[i]]*
                    smc[1:nV,foodindex[i]]*
                    RM[foodindex[i],hazardindexM[h]]*
                    cmc[1:nV,hazardindexM[h],foodindex[i]]*wmc[1:nV]
                  poissonmeanspos[1:nV,h,i] <- smc[1:nV,foodindex[i]]*
                    RM[foodindex[i],hazardindexM[h]]*
                    cmc[1:nV,hazardindexM[h],foodindex[i]]*wmc[1:nV]
                } # end of if nexactM >0
              } # end of for i
              
          for(v in 1:nV){ # for nV variable values per each parameter set
            
              if(sum(poissonmeansall[v,h,1:nfused])<=5000){
                acuteexpoall[v,h] <- rpois(1,sum(poissonmeansall[v,h,1:nfused]))}  # sum of all serving exposures, incl. zeros 
              if(sum(poissonmeansall[v,h,1:nfused])>5000){
                acuteexpoall[v,h] <- rnorm(1,sum(poissonmeansall[v,h,1:nfused]),sqrt(sum(poissonmeansall[v,h,1:nfused])))}
              if(sum(poissonmeanspos[v,h,1:nfused])<=5000){
                acuteexpopos[v,h] <- rpois(1,sum(poissonmeanspos[v,h,1:nfused]))} # simulated total acute exposure for individual when used and contaminated 
              if(sum(poissonmeanspos[v,h,1:nfused])>5000){
                acuteexpopos[v,h] <- round(rnorm(1,sum(poissonmeanspos[v,h,1:nfused]),sqrt(sum(poissonmeanspos[v,h,1:nfused]))))} # simulated total acute exposure for individual when used and contaminated
          } # end of v (variability)  
            
            nplus[u,h] <- sum(acuteexpopos[1:nV,h]>0)
            if(nplus[u,h]<=19){  # not enough for determining quantiles  
              Qplus[u,h] <- NA  # quantile from positive servings
            }
            if(nplus[u,h]>19){
              Qplus[u,h]<-quantile(acuteexpopos[acuteexpopos[1:nV,h]>0,h],theQ/100,names=FALSE) # quantile from pos servings
            }
            Q[u,h] <- quantile(acuteexpoall[,h],theQ/100,names=FALSE) # quantile from ALL servings
          #######################################################
          # pick out thinned sample of positive acute exposures:
          if(ceiling(u/5)==floor(u/5)){ 
            thin<-thin+1 
            if(nplus[u,h]>19){
              exposurevarsample[thin,1:nplus[u,h]] <- t(acuteexpopos[acuteexpopos[1:nV,h]>0,h]) 
            }
          }
          #######################################################
        } # end of u (uncertainty)
  
          Unit <- unit_concen[hazard_concen == hazardnamesusedM[h]] # the measurement unit used for hazard concentration
          Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
          
          ###Absolute----
          if(input_selectscale=="Absolute"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotM <- sum(nexactM[hazardindexM[h],foodindex]>0)
            
            if(sum(nplus[,h]<=19)==0){ # all simulations had at least 20 positive exposures
            if(sum(!is.na(exposurevarsample[1,]))>0){
              xmin <- min(exposurevarsample[1:thin,1:max(nplus)],na.rm=TRUE)
              xmax <- max(quantile(exposurevarsample[1:thin,1:max(nplus)],0.95,na.rm=TRUE,names=FALSE),
                          quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE))      
              
              plot(ecdf(exposurevarsample[1,!is.na(exposurevarsample[1,])]),verticals=TRUE,
                   do.points=FALSE,yaxt="s",
                   xlim=c(xmin,xmax),
                   lwd=1,lty=3,col="#D0006F",
                   xlab=paste("A.dose+ (", Unit1,"per day)"),ylab="Cumulative probability",
                   main=paste("Exposures:",hazardnamesusedM[h],"total from",nftotM,"foods (acute).\n Each curve >=",min(nplus[,h]),"positive values"))
            }
            for(a in 2:thin){
              if(sum(!is.na(exposurevarsample[a,]))>0){   
                lines(ecdf(exposurevarsample[a,!is.na(exposurevarsample[a,])]),verticals=TRUE,do.points=FALSE,
                      xlim=c(xmin,xmax),
                      lwd=1,lty=3,col="#D0006F")     
              }
            }
            quplim <- quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE)
            qlolim <- quantile(Qplus[,h],0.05,names=FALSE,na.rm=TRUE)
            lines(density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$x,density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$y/max(density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$y),lwd=3)   
            lines(quantile(Qplus[,h],c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(Qplus[,h],c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            }
            
            if(sum(nplus[,h]<=19)>=1){ # at least some simulations had less than 20 positive exposures  
              par(mar = c(0,0,0,0))
              plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
              text(x = 0.5, y = 0.8, paste("Less than 20 positive exposure values \n occurred in some simulations. \n",
                                           "Not reliable for quantile analysis. \n Increase variability sample size."), 
                   cex = 1.6, col = "#D0006F")
              par(mar = c(5, 4, 4, 2) + 0.1)
            }
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste0("Q",theQ,"% for the positive single exposures: ",round(quantile(Qplus[,h],0.5,names=FALSE,na.rm=TRUE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(Qplus[,h],0.05,names=FALSE,na.rm=TRUE),2),"-", round(quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste0("Q",theQ,"% for all days single exposures: ",round(quantile(Q[,h],0.5,names=FALSE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(Q[,h],0.05,names=FALSE),2),"-", round(quantile(Q[,h],0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          }
          
          ###Logarithmic----
          if(input_selectscale=="Logarithmic"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotM <- sum(nexactM[hazardindexM[h],foodindex]>0)
            
            if(sum(nplus[,h]<=19)==0){ # all simulations had at least 20 positive exposures
            if(sum(!is.na(exposurevarsample[1,]))>0){
              xmin <- log10(min(exposurevarsample[1:thin,1:max(nplus)],na.rm=TRUE))
              xmax <- max(log10(quantile(exposurevarsample[1:thin,1:max(nplus)],0.95,na.rm=TRUE,names=FALSE)),
                          quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE))
              
              plot(ecdf(log10(exposurevarsample[1,!is.na(exposurevarsample[1,])])),
                   verticals=TRUE,do.points=FALSE,yaxt="s",
                   xlim=c(xmin,xmax),
                   lwd=1,lty=3,col="#D0006F",
                   xlab=paste("log( A.dose+ (", Unit1,"per day))"),ylab="Cumulative probability",
                   main=paste("Exposures:",hazardnamesusedM[h],"total from",nftotM,"foods (acute).\n Each curve >=",min(nplus[,h]),"positive values"))
            }
            for(a in 2:thin){
              if(sum(!is.na(exposurevarsample[a,]))>0){   
                lines(ecdf(log10(exposurevarsample[a,!is.na(exposurevarsample[a,])])),verticals=TRUE,do.points=FALSE,
                      xlim=c(xmin,xmax),
                      lwd=1,lty=3,col="#D0006F")    
              }
            }
            quplim <- quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE)
            qlolim <- quantile(log10(Qplus[,h]),0.05,names=FALSE,na.rm=TRUE)
            lines(density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$x,density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$y/max(density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$y),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            }
            
            if(sum(nplus[,h]<=19)>=1){ # at least some simulations had less than 20 positive exposures  
              par(mar = c(0,0,0,0))
              plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
              text(x = 0.5, y = 0.8, paste("Less than 20 positive exposure values \n occurred in some simulations. \n",
                                           "Not reliable for quantile analysis. \n Increase variability sample size."), 
                   cex = 1.6, col = "#D0006F")
              par(mar = c(5, 4, 4, 2) + 0.1)
            }
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste0("Q",theQ,"% for the positive days single log-exposures: ",round(quantile(log10(Qplus[,h]),0.5,names=FALSE,na.rm=TRUE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(log10(Qplus[,h]),0.05,names=FALSE,na.rm=TRUE),2),"-", round(quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste0("Q",theQ,"% for all days single log-exposures: ",round(quantile(log10(Q[,h]),0.5,names=FALSE),2),  
                         " (posterior median). 90% uncertainty interval for the Q",theQ,"%: ", round(quantile(log10(Q[,h]),0.05,names=FALSE),2),"-", round(quantile(log10(Q[,h]),0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          }
      } # end of if dependent days   
      
    } # end of if nhusedM>0 nfused>0
    
  } # end of if theresults
  } else{  # constant.consum TRUE
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.8, paste("Consumption was constant \n",
                                 "This feature not available"), 
         cex = 1.6, col = "#D0006F")
    par(mar = c(5, 4, 4, 2) + 0.1)
  }  
}  # end of renderPlot



# Plot 5.1: MCMC diagnostic plots, Concentration parameters----
## ---- distPlot5_1 --------
distPlot5_1 <- function(n_sim, foodnamesused, nfused, foodindex, 
                        hazardnamesusedK, hazardnamesusedM, nhusedK, nhusedM,
                        hazardindexK, hazardindexM,nf,nhK,nhM,
                        nexactK, nexactM,
                        mucK,mucM,pK,pM,sigcK,sigcM
) {
  
  par(oma = c(4, 3, 3, 0)) # Outer margins for legend
  par(mar=rep(2,4),cex.lab=1.3,cex.main=1.3) #one hazard at a time -> required for the download option
  # mfrow=c(3,1),
  layout(mat = matrix(c(2, 1, 4, 3, 6, 5), 
                      nrow = 2, 
                      ncol = 3),
         heights = c(2, 4),    # Heights of the two rows
         widths = c(1, 1, 1))     # Widths of the two columns
  
  
  #Chemical---- 
  
  if((nhusedK>0)&(nfused>0)){
    for(h in 1:nhusedK){
      for(i in 1:nfused){
        
        if(nexactK[hazardindexK[h],foodindex[i]]>0){ # hazard-food was modeled
          plot(mucK[,hazardindexK[h],foodindex[i]]/log(10),y= 1:length(mucK[,hazardindexK[h],foodindex[i]]),pch=16,cex=0.5,col="#D0006F")
          plot(density(mucK[,hazardindexK[h],foodindex[i]]/log(10))$x,0.3*n_sim/max(density(mucK[,hazardindexK[h],foodindex[i]]/log(10))$y)*density(mucK[,hazardindexK[h],foodindex[i]]/log(10))$y,main=bquote(.(hazardnamesusedK[h])~"in"~.(foodnamesused[i])~":"~mu),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
          plot(sigcK[,hazardindexK[h],foodindex[i]]/log(10),y= 1:length(sigcK[,hazardindexK[h],foodindex[i]]),pch=16,cex=0.5,col="#D0006F") 
          plot(density(sigcK[,hazardindexK[h],foodindex[i]]/log(10))$x,0.3*n_sim/max(density(sigcK[,hazardindexK[h],foodindex[i]]/log(10))$y)*density(sigcK[,hazardindexK[h],foodindex[i]]/log(10))$y,main=bquote(.(hazardnamesusedK[h])~"in"~.(foodnamesused[i])~":"~sigma),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
          plot(pK[,hazardindexK[h],foodindex[i]],y= 1:length(pK[,hazardindexK[h],foodindex[i]]),pch=16,cex=0.5,col="#D0006F") 
          plot(density(pK[,hazardindexK[h],foodindex[i]])$x,0.3*n_sim/max(density(pK[,hazardindexK[h],foodindex[i]])$y)*density(pK[,hazardindexK[h],foodindex[i]])$y,main=bquote(.(hazardnamesusedK[h])~"in"~.(foodnamesused[i])~":"~q),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
          mtext("Parameter value",
                side = 1, adj = 0.5,line=2, cex = 1.3,
                outer = TRUE)
          mtext("MCMC samples",
                side = 2, adj = 0.25,line=1, cex = 1.3,
                outer = TRUE)
          mtext("Approximated marginal probability density",
                side = 3, adj = 0.5,line=1, cex = 1.3,
                outer = TRUE)
        } else
          
          #Empty plot
        {
          par(mar = c(0,0,0,0))
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n', ylab = "Marginal posterior distribution")
          text(x = 0.5, y = 0.8, paste("The data for this food-hazard combination is not sufficient for modeling\n",
                                       "(e.g., concentration measurements (>LOQ) about the food-hazard combination are missing)."), 
               cex = 1.6, col = "#D0006F")
          par(mar = c(5, 4, 4, 2) + 0.1)
        }   
      }} # for, for
  } # if
  
  
  #Microbiological----
  
  if( (nhusedM>0)&(nfused>0) ){
    
    for(h in 1:nhusedM){
      for(i in 1:nfused){
        if(nexactM[hazardindexM[h],foodindex[i]]>0){  # hazard-food was modeled
          plot(mucM[,hazardindexM[h],foodindex[i]]/log(10),y= 1:length(mucM[,hazardindexM[h],foodindex[i]]), pch=16,cex=0.5,col="#D0006F") 
          plot(density(mucM[,hazardindexM[h],foodindex[i]]/log(10))$x,0.3*n_sim/max(density(mucM[,hazardindexM[h],foodindex[i]]/log(10))$y)*density(mucM[,hazardindexM[h],foodindex[i]]/log(10))$y,main=bquote(.(hazardnamesusedM[h])~"in"~.(foodnamesused[i])~":"~mu),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
          plot(sigcM[,hazardindexM[h],foodindex[i]]/log(10),y= 1:length(sigcM[,hazardindexM[h],foodindex[i]]),pch=16,cex=0.5,col="#D0006F") 
          plot(density(sigcM[,hazardindexM[h],foodindex[i]]/log(10))$x,0.3*n_sim/max(density(sigcM[,hazardindexM[h],foodindex[i]]/log(10))$y)*density(sigcM[,hazardindexM[h],foodindex[i]]/log(10))$y,main=bquote(.(hazardnamesusedM[h])~"in"~.(foodnamesused[i])~":"~sigma),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
          plot(pM[,hazardindexM[h],foodindex[i]],y= 1:length(pM[,hazardindexM[h],foodindex[i]]),pch=16,cex=0.5,col="#D0006F") 
          plot(density(pM[,hazardindexM[h],foodindex[i]])$x,0.3*n_sim/max(density(pM[,hazardindexM[h],foodindex[i]])$y)*density(pM[,hazardindexM[h],foodindex[i]])$y,main=bquote(.(hazardnamesusedM[h])~"in"~.(foodnamesused[i])~":"~q),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
          mtext("Parameter value",
                side = 1, adj = 0.5,line=2, cex = 1.3,
                outer = TRUE)
          mtext("MCMC samples",
                side = 2, adj = 0.25,line=1, cex = 1.3,
                outer = TRUE)
          mtext("Approximated marginal probability density",
                side = 3, adj = 0.5,line=1, cex = 1.3,
                outer = TRUE)
        } # end of if hazard-food modeled  
        else
          
          #Empty plot
        {
          par(mar = c(0,0,0,0))
          plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
          text(x = 0.5, y = 0.8, paste("The data for this food-hazard combination is not sufficient for modeling\n",
                                       "(e.g., concentration measurements (>LOQ) about the food-hazard combination are missing)."), 
               cex = 1.6, col = "#D0006F")
          par(mar = c(5, 4, 4, 2) + 0.1)
        }   
      }} # for, for
  } # if
  
}



# Plot 5.2: MCMC diagnostic plots, Consumption parameters----
## ---- distPlot5_2 --------
distPlot5_2 <- function(n_sim,foodnamesused, nfused, foodindex,
                        nf,
                        mus0,logitp0,
                        Ss,constant.consum,input_modelchoice
) {
  if(constant.consum==TRUE){
    ##Empty plot----
    {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.8, paste("Consumption was constant. \n",
                                   "Consumption model was not applied. \n",
                                    "Therefore, parameter samples not available"), 
           cex = 1.6, col = "#D0006F")
      par(mar = c(5, 4, 4, 2) + 0.1)
    } 
    
  } else {
    
  par(oma = c(4, 3, 3, 0)) # Outer margins for legend
  par(mar=rep(2,4),cex.lab=1.3,cex.main=1.3) #one food at a time -> required for the download option
  #mfrow=c(3,2),
  layout(mat = matrix(c(2, 1, 4, 3, 6, 5), 
                      nrow = 2, 
                      ncol = 3),
         heights = c(2, 4),    # Heights of the two rows
         widths = c(1, 1, 1))     # Widths of the two columns

  p0 <- exp(logitp0)/(1+exp(logitp0)) 
  for(i in 1:nfused){
    if(input_modelchoice!="Fixed variance"){
    Vs <- numeric() # variances, day-to-day (serving) variation
    for(u in 1:n_sim){
      Vs[u] <- Ss[u,foodindex[i],foodindex[i]]  
    }
    }
    plot(mus0[,foodindex[i]]/log(10),y= 1:length(mus0[,foodindex[i]]), pch=16,cex=0.5,col="#D0006F") 
    plot(density(mus0[,foodindex[i]]/log(10))$x, 0.3*n_sim/max(density(mus0[,foodindex[i]]/log(10))$y)*density(mus0[,foodindex[i]]/log(10))$y,main=bquote(.(foodnamesused[i])~":"~mu),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
    
    if(input_modelchoice!="Fixed variance"){
    plot(Vs/log(10),y= 1:length(Vs),pch=16,cex=0.5,col="#D0006F") 
    plot(density(Vs/log(10))$x,0.3*n_sim/max(density(Vs/log(10))$y)*density(Vs/log(10))$y,main=bquote(.(foodnamesused[i])~":"~sigma),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
    }
    
    plot(p0[,foodindex[i]],y= 1:length(p0[,foodindex[i]]),pch=16,cex=0.5,col="#D0006F") 
    plot(density(p0[,foodindex[i]])$x,0.3*n_sim/max(density(p0[,foodindex[i]])$y)*density(p0[,foodindex[i]])$y,main=bquote(.(foodnamesused[i])~":"~p),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
    #lines(0.3*n_sim/max(density(p0[,foodindex[i]])$y)*density(p0[,foodindex[i]])$y,density(p0[,foodindex[i]])$x,lwd=3)
    
    mtext("Parameter value",
          side = 1, adj = 0.5,line=2, cex = 1.3,
          outer = TRUE)
    mtext("MCMC samples",
          side = 2, adj = 0.25,line=1, cex = 1.3,
          outer = TRUE)
    mtext("Approximated marginal probability density",
          side = 3, adj = 0.5,line=1, cex = 1.3,
          outer = TRUE)
  } # for
  } # constant.consum == FALSE  
}


# Plot 6: Serving correlation----
## ---- distPlot6_1 --------
distPlot6_1 <- function(food_consum, unit_consum, n_sim, foodnamesused,
                        nfused, foodindex,nr,nd,nf,logsw,
                        mus0,
                        Ss,Ss0,constant.consum,input_modelchoice,
                        osdlogsw1,osdlogsw2
) {
  
  
  if(constant.consum==TRUE){
    ##Empty plot----
    {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.8, paste("Consumption was constant. \n",
                                   "Consumption model was not applied. \n",
                                   "Therefore, correlation model results not available"), 
           cex = 1.6, col = "#D0006F")
      par(mar = c(5, 4, 4, 2) + 0.1)
    }   
  } else{
  # generate results based on inputs from ui.R: 
  # Correlation plots for consumptions
  Unit <- unit_consum[food_consum == foodnamesused] # the measurement unit used for food consumptions
  Unit3 <- sub(".*p.", "", Unit) # Extract characters after pattern
  
  
  if((nfused>1) ){  
    # generate a model predicted sample of positive consumptions, 
    # and plot these in pairs (with data points)
    nsample <- 1000 # number of samples to generate 
    sampledmus <- matrix(NA,nsample,nf)
    sampledsw <- matrix(NA,nsample,nf)
    mc <- round(seq(1,n_sim,length=nsample))
    
    Ssconst1 <- diag(osdlogsw1[1:nf]^2) # correlation matrix with constant variances taken from data (no correlations) 
    Ssconst2 <- diag(osdlogsw2[1:nf]^2) # correlation matrix with constant variances taken from data (no correlations) 
    
    for(i in 1:nsample){
      if(input_modelchoice=="Fixed variance"){
        sampledmus[i,1:nf] <- rmvnorm(1,mus0[mc[i],1:nf],Ssconst1[1:nf,1:nf])
        sampledsw[i,1:nf] <- exp(rmvnorm(1,sampledmus[i,1:nf],Ssconst2[1:nf,1:nf])) 
      }else{
        sampledmus[i,1:nf] <- rmvnorm(1,mus0[mc[i],1:nf],Ss0[mc[i],1:nf,1:nf])
        sampledsw[i,1:nf] <- exp(rmvnorm(1,sampledmus[i,1:nf],Ss[mc[i],1:nf,1:nf]))
      }
    }
    
    datasw <- matrix(NA,nr*nd,nf)
    rowindex <- 0
    for(r in 1:nr){  # consumers
      for(t in 1:nd){ # days
        rowindex <- rowindex+1
        datasw[rowindex,1:nf]<-exp(logsw[r,t,1:nf])
      }
    }
    group <- c(rep(1,nr*nd),rep(2,nsample)) # groups for data values and simulated values
    DF1 <- data.frame(log10(datasw[1:(nr*nd),foodindex]))
    DF2 <- data.frame(log10(sampledsw[1:nsample,foodindex]))
    colnames(DF1) <- foodnamesused
    colnames(DF2) <- foodnamesused
    par(xpd=TRUE)
    pairs(rbind(DF1,DF2),
          main=paste("Pairwise scatterplots of log (consumption/bw+(", Unit3,"per kg))"),
          upper.panel=NULL,omd=c(1,1,15,1),
          cex=c(1,0.4)[group],pch=c(16,16)[group],col=c("#004F71","#D0006F")[group])
  } # nfused >1 
}
}

# Plot 7: Mean serving correlations----
## ---- distPlot7_1 --------
distPlot7_1 <- function(food_consum, unit_consum, n_sim, foodnamesused, nfused, foodindex,
                        nf,nr,nd,logsw,
                        mus0,
                        Ss,Ss0,constant.consum,input_modelchoice,
                        osdlogsw1,osdlogsw2
) { 
  
  if(constant.consum==TRUE){
    ##Empty plot----
    {
      par(mar = c(0,0,0,0))
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.8, paste("Consumption was constant. \n",
                                   "Consumption model was not applied. \n",
                                   "Therefore, correlation model results not available"), 
           cex = 1.6, col = "#D0006F")
      par(mar = c(5, 4, 4, 2) + 0.1)
    }   
  } else{
  # generate results based on inputs from ui.R: 
  # Correlation plots for mean consumptions
  
  Unit <- unit_consum[food_consum == foodnamesused] # the measurement unit used for food consumptions
  Unit3 <- sub(".*p.", "", Unit) # Extract characters after pattern
  
  
  if(nfused>1){  
    # generate a model predicted sample of positive mean consumptions, 
    # and plot these in pairs (with data points)
    nsample <- 1000 # number of samples to generate
    sampledmus <- matrix(NA,nsample,nf) # for the means in log-scale
    sampledmeans <- matrix(NA,nsample,nf) # for the means in absolute scale
    mc <- round(seq(1,n_sim,length=nsample))
    
    Ssconst1 <- diag(osdlogsw1[1:nf]^2) # correlation matrix with constant variances taken from data (no correlations) 
    Ssconst2 <- diag(osdlogsw2[1:nf]^2) # correlation matrix with constant variances taken from data (no correlations) 
    
    for(i in 1:nsample){
      if(input_modelchoice=="Fixed variance"){
      sampledmus[i,1:nf] <- rmvnorm(1,mus0[mc[i],1:nf],Ssconst1[1:nf,1:nf])
      sampledmeans[i,1:nf] <- exp(sampledmus[i,1:nf]+0.5*diag(Ssconst2[1:nf,1:nf])) 
      }else{
      sampledmus[i,1:nf] <- rmvnorm(1,mus0[mc[i],1:nf],Ss0[mc[i],1:nf,1:nf])
      sampledmeans[i,1:nf] <- exp(sampledmus[i,1:nf]+0.5*diag(Ss[mc[i],1:nf,1:nf]))
      }
    }
    
    datameansw <- matrix(NA,nr,nf)
    for(r in 1:nr){ # consumers
      for(i in 1:nf){ # data based individual mean consumptions:
        datameansw[r,i]<- mean(exp(logsw[r,1:nd,i]),na.rm=TRUE)
      }
    }
    group <- c(rep(1,nr),rep(2,nsample)) # groups for data values and simulated values
    DF1 <- data.frame(log10(datameansw[1:nr,foodindex]))
    DF2 <- data.frame(log10(sampledmeans[1:nsample,foodindex]))
    colnames(DF1) <- foodnamesused
    colnames(DF2) <- foodnamesused
    par(xpd=TRUE)
    pairs(rbind(DF1,DF2),
          main=paste("Pairwise scatterplots of log (E(consumption/bw+(", Unit3,"per kg))"),
          # main="Pairwise scatterplots of log (E(consumption/bw+))",
          upper.panel=NULL,omd=c(1,1,15,1),
          cex=c(1,0.4)[group],pch=c(16,16)[group],col=c("#004F71","#D0006F")[group])
  }
  }
  
}


