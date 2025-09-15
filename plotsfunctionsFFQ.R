

# Plot functions when FFQ data is modelled

# FFQ data represents reported mean daily consumptions of each food for each respondent 
# (Reported zero mean consumption is assumed to be a truly non-consumer). 
# Hence, the models have no day-to-day variation. Only variation of mean consumptions between individuals.
# Therefore, acute exposures cannot be estimated, nor consumption frequencies (day-to-day). 
# Only mean exposures for both chemical and microbiological hazards are estimated.
# Chemical mean exposures are evaluated as per bodyweight, microbial as absolute mean exposures.


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
                                            input_upper,names=FALSE) #0.975,names=FALSE)
                lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (mucK[,hazardindexK[h],foodindex[i]])/log(10),
                                                  (sigcK[,hazardindexK[h],foodindex[i]])/log(10) ),
                                            input_lower,names=FALSE) #0.025,names=FALSE)
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
                                            input_upper,names=FALSE) #0.975,names=FALSE)
                lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (mucK[,hazardindexK[h],foodindex[i]])/log(10),
                                                  (sigcK[,hazardindexK[h],foodindex[i]])/log(10) ),
                                            input_lower,names=FALSE) #0.025,names=FALSE)
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
          mtext(paste("Estimated prevalence for", hazardnamesusedK[h],"in",foodnamesused[i], ": ",round(quantile(100*pK[,hazardindexK[h],foodindex[i]],0.5,names=FALSE),1),
                      "% (posterior median)."),
                side = 1, adj = 0,line=1, cex = 1,
                outer = TRUE)
          mtext(paste("95% uncertainty interval for the prevalence: ", 
                      round(quantile(100*pK[,hazardindexK[h],foodindex[i]],0.025,names=FALSE),1),"%-", round(quantile(100*pK[,hazardindexK[h],foodindex[i]],0.975,names=FALSE),1),"%"),
                side = 1, adj = 0,line=2, cex = 1,
                outer = TRUE)
          if(input_selectscale=="Absolute"){
            mtext(paste("95% uncertainty interval for the Q50% concentration:", 
                        round(quantile(qlnorm(0.5,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),0.025,names=FALSE),2),"-", round(quantile(qlnorm(0.5,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),0.975,names=FALSE),2),"and Q95%:",
                        round(quantile(qlnorm(0.95,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),0.025,names=FALSE),2),"-", round(quantile(qlnorm(0.95,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]]),0.975,names=FALSE),2)),
                  side = 1, adj = 0,line=3, cex = 1,
                  outer = TRUE)}
          if(input_selectscale=="Logarithmic"){
            mtext(paste("95% uncertainty interval for the Q50% log-concentration:", 
                        round(quantile(qnorm(0.5,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]])/log(10),0.025,names=FALSE),2),"-", round(quantile(qnorm(0.5,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]])/log(10),0.975,names=FALSE),2),"and Q95%:",
                        round(quantile(qnorm(0.95,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]])/log(10),0.025,names=FALSE),2),"-", round(quantile(qnorm(0.95,mucK[,hazardindexK[h],foodindex[i]],sigcK[,hazardindexK[h],foodindex[i]])/log(10),0.975,names=FALSE),2)),
                  side = 1, adj = 0,line=3, cex = 1,
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
                                            input_upper,names=FALSE) #0.975,names=FALSE)
                lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (mucM[,hazardindexM[h],foodindex[i]])/log(10), 
                                                  (sigcM[,hazardindexM[h],foodindex[i]])/log(10) ),
                                            input_lower,names=FALSE) #0.025,names=FALSE)
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
                                            input_upper,names=FALSE) #0.975,names=FALSE)
                lowervalues[xv] <- quantile(plnorm(xvalues[xv],
                                                   mucM[,hazardindexM[h],foodindex[i]],
                                                   sigcM[,hazardindexM[h],foodindex[i]]),
                                            input_lower,names=FALSE) #0.025,names=FALSE)
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
                                            input_upper,names=FALSE) #0.975,names=FALSE)
                lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (mucM[,hazardindexM[h],foodindex[i]])/log(10),
                                                  (sigcM[,hazardindexM[h],foodindex[i]])/log(10) ),
                                            input_lower,names=FALSE) #0.025,names=FALSE)
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
          mtext(paste("Estimated prevalence for", hazardnamesusedM[h],"in",foodnamesused[i], ": ",round(quantile(100*pM[,hazardindexM[h],foodindex[i]],0.5,names=FALSE),1),
                      "% (posterior median)"),
                side = 1, adj = 0,line=1, cex = 1,
                outer = TRUE)
          mtext(paste("95% uncertainty interval for the prevalence: ", 
                      round(quantile(100*pM[,hazardindexM[h],foodindex[i]],0.025,names=FALSE),1),"%-", round(quantile(100*pM[,hazardindexM[h],foodindex[i]],0.975,names=FALSE),1),"%"),
                side = 1, adj = 0,line=2, cex = 1,
                outer = TRUE)
          if(input_selectscale=="Absolute"){
          mtext(paste("95% uncertainty interval for the Q50% concentration:", 
                      round(quantile(qlnorm(0.5,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),0.025,names=FALSE),2),"-", round(quantile(qlnorm(0.5,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),0.975,names=FALSE),2),"and Q95%:",
                      round(quantile(qlnorm(0.95,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),0.025,names=FALSE),2),"-", round(quantile(qlnorm(0.95,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]]),0.975,names=FALSE),2)),
                side = 1, adj = 0,line=3, cex = 1,
                outer = TRUE)}
          if(input_selectscale=="Logarithmic"){
            mtext(paste("95% uncertainty interval for the Q50% concentration:", 
                        round(quantile(qnorm(0.5,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]])/log(10),0.025,names=FALSE),2),"-", round(quantile(qnorm(0.5,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]])/log(10),0.975,names=FALSE),2),"and Q95%:",
                        round(quantile(qnorm(0.95,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]])/log(10),0.025,names=FALSE),2),"-", round(quantile(qnorm(0.95,mucM[,hazardindexM[h],foodindex[i]],sigcM[,hazardindexM[h],foodindex[i]])/log(10),0.975,names=FALSE),2)),
                  side = 1, adj = 0,line=3, cex = 1,
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

# Plot 2: Consumptions from FFQ data:---- 
## ---- distPlot2_1 -------- 
distPlot2_1FFQ <- function(input_lim, food_consum, unit_consum, input_upper, input_lower, n_sim, input_selectdist, input_selectscale, foodnamesused, nfused, foodindex,
                        nf, nr,logs, logsw,
                        mus0,muw,logitp0,sigw,Ss0
) {
  # generate results based on inputs from ui.R:  
  # Consumption amounts
  
  par(oma = c(4, 1, 0, 1)) # Outer margins for legend
  par(mfrow=c(1,2),cex.lab=1.3,cex.main=1.3,yaxt="n")
  
  OIM <- numeric() # observed individual mean consumptions
  
  p0 <- exp(logitp0)/(1+exp(logitp0)) # probability to consume at all each food type  
  for(i in 1:nfused){
    Unit <- unit_consum[food_consum == foodnamesused[i]] # the measurement unit used for food consumptions
    Unit3 <- sub(".*p.", "", Unit) # Extract characters after pattern
    
    Vs0 <- numeric() # variances
    for(u in 1:n_sim){
      Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]]
    }
    
    if(input_selectdist=="Density"){
      if(input_selectscale=="Absolute"){
        # distributions of chronic consumptions, on consumption days (absolute per bodyweight)
        meansmean <- exp(mus0[,foodindex[i]]+0.5*Vs0  )
        meansmedian <- exp(mus0[,foodindex[i]])
        
        maxx <- quantile(qlnorm(input_lim,mus0[,foodindex[i]],sqrt(Vs0) ),
                         0.9,names=FALSE)
        
        plot(density(meansmedian,from=0,to=maxx,n=2048),lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("C.consumption/bw+ (", Unit3,"per kg)"),ylab="Probability density",xlim=c(0,maxx)) 
        lines(density(meansmean,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3)
        
        xvalues <- seq(0,maxx,length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                             mus0[,foodindex[i]],sqrt(Vs0) ),
                                      input_upper,names=FALSE) #0.975,names=FALSE)
          
          lowervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                             mus0[,foodindex[i]],sqrt(Vs0) ),
                                      input_lower,names=FALSE) #0.025,names=FALSE)
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(density(meansmedian,from=0,to=maxx,n=2048),lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="C.consumption/bw+",ylab="",xlim=c(0,maxx),type="l")
        lines(density(meansmean,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3)
        
        for(r in 1:nr){
          OIM[r]<- mean(exp(logsw[r,foodindex[i]]),na.rm=TRUE) # in FFQ data this is single value 
        } 
        OIM<-OIM[!is.na(OIM)]
        # mark data points: (observed individual means)
        rug(OIM,lwd=2.5,col="#D0006F",quiet=TRUE)
        
        
        # distribution of chronic consumptions, on consumption days (absolute):
        meansmean <- exp(mus0[,foodindex[i]]+muw+0.5*Vs0+0.5*sigw^2)
        meansmedian <- exp(mus0[,foodindex[i]]+muw) 
        
        maxx <- quantile(qlnorm(input_lim,mus0[,foodindex[i]]+muw,
                                sqrt(Vs0+sigw^2)),
                         0.9,names=FALSE)
        
        plot(density(meansmedian,from=0,to=maxx,n=2048),lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("C.consumption+ (", Unit3,")"),ylab="",xlim=c(0,maxx)) 
        lines(density(meansmean,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3)
        
        xvalues <- seq(0,maxx,length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+muw,
                                             sqrt(Vs0+sigw^2)),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+muw,
                                             sqrt(Vs0+sigw^2)),
                                      input_lower,names=FALSE)
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(density(meansmedian,from=0,to=maxx,n=2048),lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="C.consumption+",ylab="",xlim=c(0,maxx),type="l") 
        lines(density(meansmean,from=0,to=maxx,n=2048),col="#F7CE3C",lwd=3)
        
        # mark data points: (individual mean consumptions from FFQ)
        rug(exp(logs[1:nr,foodindex[i]]),lwd=2.5,col="#D0006F",quiet=TRUE)
        
        
      } # end of if absolute
      
      if(input_selectscale=="Logarithmic"){
        
        # distributions of chronic consumptions, on consumption days (log per bodyweight)
        musmean <- mus0[,foodindex[i]]
        
        maxx <- quantile(qnorm(input_lim,mus0[,foodindex[i]],sqrt(Vs0) ),
                         0.9,names=FALSE)
        minn <- quantile(qnorm(0.05,mus0[,foodindex[i]],sqrt(Vs0) ),
                         0.5,names=FALSE)
        
        plot(density(musmean/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("log C.consumption/bw+ (", Unit3,"per kg)"),ylab="Probability density",xlim=c(minn/log(10),maxx/log(10))) 
        
        xvalues <- seq(minn/log(10),maxx/log(10),length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(dnorm(xvalues[xv],
                                            (mus0[,foodindex[i]])/log(10),
                                            sqrt(Vs0)/log(10) ),
                                      input_upper,names=FALSE) #0.975,names=FALSE)
          lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                            (mus0[,foodindex[i]])/log(10),
                                            sqrt(Vs0)/log(10) ),
                                      input_lower,names=FALSE) #0.025,names=FALSE)
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(density(musmean/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="log C.consumption/bw+",ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        
        for(r in 1:nr){
          OIM[r]<- log(mean(exp(logsw[r,foodindex[i]]),na.rm=TRUE)) # single value from FFQ data 
        } 
        OIM<-OIM[!is.na(OIM)]
        # mark data points: (observed individual means, in log-scale)
        rug(OIM/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
        
        
        # distribution of chronic mean consumptions, on consumption days (log):
        logsmean <- mus0[,foodindex[i]]+muw
        
        maxx <- quantile(qnorm(input_lim,mus0[,foodindex[i]]+muw,
                               sqrt(Vs0+sigw^2)),
                         0.9,names=FALSE)
        minn <- quantile(qnorm(0.05,mus0[,foodindex[i]]+muw,
                               sqrt(Vs0+sigw^2)),
                         0.5,names=FALSE)
        
        plot(density(logsmean/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("log C.consumption+ (", Unit3,")"),ylab="",xlim=c(minn/log(10),maxx/log(10))) 
        
        # mark data points: (individual mean consumptions, in log-scale) from FFQ data
        rug(logs[1:nr,foodindex[i]]/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
        
        xvalues <- seq(minn/log(10),maxx/log(10),length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(dnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+muw)/log(10),
                                            (sqrt(Vs0+sigw^2))/log(10) ),
                                      input_upper,names=FALSE) #0.975,names=FALSE)
          lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+muw)/log(10),
                                            (sqrt(Vs0+sigw^2))/log(10) ),
                                      input_lower,names=FALSE) #0.025,names=FALSE)
        }              
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(density(logsmean/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="log C.consumption+",ylab="",xlim=c(minn/log(10),maxx/log(10))) 
        
        
      } # end of if logarithmic
    } # end of if density
    
    if(input_selectdist=="Cumulative"){
      par(yaxt="s")
      cump <- seq(1,n_sim)
      cump <- cump/length(cump)
      if(input_selectscale=="Absolute"){
        
        # distributions of chronic consumptions (absolute per bodyweight)
        meansmean <- sort(exp(mus0[,foodindex[i]] +0.5*Vs0 ))
        meansmedian <- sort(exp(mus0[,foodindex[i]] ))
        
        maxx <- quantile(qlnorm(input_lim,mus0[,foodindex[i]],sqrt(Vs0) ),
                         0.9,names=FALSE)
        
        plot(meansmean,cump,col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("C.consumption/bw+ (", Unit3,"per kg)"),ylab="Cumulative probability",xlim=c(0,maxx),type="l")
        lines(meansmedian,cump,lwd=3)
        
        xvalues <- seq(0,maxx,length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(plnorm(xvalues[xv],
                                             mus0[,foodindex[i]],sqrt(Vs0) ),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(plnorm(xvalues[xv],
                                             mus0[,foodindex[i]],sqrt(Vs0) ),
                                      input_lower,names=FALSE) 
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(meansmean,cump,col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="C.consumption/bw+",ylab="",xlim=c(0,maxx),type="l")
        lines(meansmedian,cump,lwd=3)
        
        for(r in 1:nr){
          OIM[r]<- mean(exp(logsw[r,foodindex[i]]),na.rm=TRUE) # single value from FFQ data 
        } 
        OIM<-OIM[!is.na(OIM)]
        # mark data points: (observed individual means)  # from FFQ data
        rug(OIM,lwd=2.5,col="#D0006F",quiet=TRUE)
        lines(ecdf(OIM),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
        
        
        # distribution of chronic consumptions (absolute):
        smean <- sort(exp(mus0[,foodindex[i]]+0.5*Vs0+muw+0.5*sigw^2))
        smedian <- sort(exp(mus0[,foodindex[i]]+muw))
        
        maxx <- quantile(qlnorm(input_lim,mus0[,foodindex[i]]+muw,
                                sqrt(Vs0+sigw^2)),
                         0.9,names=FALSE)
        
        
        plot(smean,cump,col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("C.consumption+ (", Unit3,")"),ylab="",xlim=c(0,maxx),type="l") 
        lines(smedian,cump,lwd=3)
        
        xvalues <- seq(0,maxx,length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(plnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+muw,
                                             sqrt(Vs0+sigw^2)),
                                      input_upper,names=FALSE) #0.975,names=FALSE)
          lowervalues[xv] <- quantile(plnorm(xvalues[xv],
                                             mus0[,foodindex[i]]+muw,
                                             sqrt(Vs0+sigw^2)),
                                      input_lower,names=FALSE) #0.025,names=FALSE)
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(smean,cump,col="#F7CE3C",lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="C.consumption+",ylab="",xlim=c(0,maxx),type="l") 
        lines(smedian,cump,lwd=3)
        
        # mark data points: (individual chronic consumptions) from FFQ data
        rug(exp(logs[1:nr,foodindex[i]]),lwd=2.5,col="#D0006F",quiet=TRUE)
        lines(ecdf(exp(logs[1:nr,foodindex[i]])),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
        
        
      } # end of if absolute
      
      if(input_selectscale=="Logarithmic"){
        # distributions of chronic consumptions (log per bodyweight)
        musmean <- sort(mus0[,foodindex[i]])
        
        maxx <- quantile(qnorm(input_lim,mus0[,foodindex[i]], sqrt(Vs0) ),
                         0.9,names=FALSE)
        minn <- quantile(qnorm(0.05,mus0[,foodindex[i]], sqrt(Vs0) ),
                         0.5,names=FALSE)
        
        plot(musmean/log(10),cump,lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("log C.consumption/bw+ (", Unit3,"per kg)"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        xvalues <- seq(minn/log(10),maxx/log(10),length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                            (mus0[,foodindex[i]])/log(10),
                                            sqrt(Vs0)/log(10) ),
                                      input_upper,names=FALSE) #0.975,names=FALSE)
          lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                            (mus0[,foodindex[i]])/log(10),
                                            sqrt(Vs0)/log(10) ),
                                      input_lower,names=FALSE) #0.025,names=FALSE)
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(musmean/log(10),cump,lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="log C.consumption/bw+",ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        
        for(r in 1:nr){
          OIM[r]<- log(mean(exp(logsw[r,foodindex[i]]),na.rm=TRUE)) # single value from FFQ data 
        } 
        OIM<-OIM[!is.na(OIM)]
        # mark data points: (observed individual means, in log-scale)
        rug(OIM/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
        lines(ecdf(OIM/log(10)),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
        
        
        # distribution of chronic consumptions (log):
        logsmean <- sort(mus0[,foodindex[i]]+muw)
        maxx <- quantile(qnorm(input_lim,mus0[,foodindex[i]]+muw,
                               sqrt(Vs0+sigw^2)),
                         0.9,names=FALSE)
        minn <- quantile(qnorm(0.05,mus0[,foodindex[i]]+muw,
                               sqrt(Vs0+sigw^2)),
                         0.5,names=FALSE)
        plot(logsmean/log(10),cump,lwd=3,main=paste(foodnamesused[i],"consumption"),
             xlab=paste("log C.consumption+ (", Unit3,")"),ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        xvalues <- seq(minn/log(10),maxx/log(10),length=100)
        uppervalues <- numeric()
        lowervalues <- numeric()
        for(xv in 1:100){
          uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+muw)/log(10),
                                            (sqrt(Vs0+sigw^2))/log(10) ),
                                      input_upper,names=FALSE) 
          lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                            (mus0[,foodindex[i]]+muw)/log(10),
                                            (sqrt(Vs0+sigw^2))/log(10) ),
                                      input_lower,names=FALSE)
        }
        polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
        lines(logsmean/log(10),cump,lwd=3,main=paste(foodnamesused[i],"consumption"),xlab="log C.consumption+",ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l")
        
        
        # mark data points: (individual chronic consumptions, in log-scale) from FFQ data
        rug(logs[1:nr,foodindex[i]]/log(10),lwd=2.5,col="#D0006F",quiet=TRUE)
        lines(ecdf(logs[1:nr,foodindex[i]]/log(10)),verticals=TRUE,do.points=FALSE,lwd=2,col="#D0006F")
        
        
      } # end of if logarithmic
    } # end of if cumulative
    
  } # end of for nfused
  
  
  # legend ----
  # outside the figure, but onto the current plot, so it is part of the png file:
  mtext(paste("Estimated population proportion of consumers of", foodnamesused[i], ": ", round(quantile(100*p0[,foodindex[i]],0.5,names=FALSE),1),
              "% (posterior median)."),
        side = 1, adj = 0,line=0, cex = 1,
        outer = TRUE)
  mtext(paste("95% uncertainty interval: ", 
              round(quantile(100*p0[,foodindex[i]],0.025,names=FALSE),1),"%-", round(quantile(100*p0[,foodindex[i]],0.975,names=FALSE),1),"%"),
        side = 1, adj = 0,line=1, cex = 1,
        outer = TRUE)
  mtext(paste("Estimated Q50% C.consumption/bw+ for", foodnamesused[i], ": ", round(quantile(exp(mus0[,foodindex[i]]),0.5,names=FALSE),1),
              "(posterior median)."),
        side = 1, adj = 0,line=2, cex = 1,
        outer = TRUE)
  mtext(paste("Estimated Q50% C.consumption+ for", foodnamesused[i], ": ", round(quantile(exp(mus0[,foodindex[i]]+muw),0.5,names=FALSE),1),
              "(posterior median)."),
        side = 1, adj = 0,line=3, cex = 1,
        outer = TRUE)
  
  
  #} # end of if selectresults == "Consumptions"
}

# Plot 3: Exposures --- 
## ---- distPlot3_1 --------
distPlot3_1FFQ <- function(input_lim, unit_concen, hazard_concen, input_upper, input_lower, n_sim, input_selectdist, input_selectscale,
                        foodnamesused, nfused, foodindex, hazardnames,
                        nhused,  hazardnamesusedK, hazardnamesusedM,
                        nhusedK, nhusedM, hazardindexK, hazardindexM, Rall, Pall,nhK,nhM,nf,nr,
                        nexactK, nexactM, 
                        logs,logsw,logcK,logLOQK,logLODK,logLOQLimK,logLODLimK, logcM,logLOQM,logLODM,logLOQLimM,logLODLimM,
                        logitp0,mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                        Ss0
) {
  # generate results based on inputs from ui.R: 
  # Exposures----
  
  par(oma = c(5, 1, 0, 1),cex.lab=1.3,cex.main=1.3) # Outer margins for legend
  #par(mfrow=c(nhused,nfused),cex.lab=1.3,cex.main=1.3,yaxt="n")
  
  p0 <- exp(logitp0)/(1+exp(logitp0)) # probability to use each food at all 
  # Chemical exposures----
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
          Vs0 <- numeric() # variances in FFQ model (only the variation between individuals)  
          for(u in 1:n_sim){
            Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]] 
          }  
          
          qpos95 <- qlnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                                +mus0[,foodindex[i]]
                                +mucK[,hazardindexK[h],foodindex[i]]
                                +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                sqrt(Vs0))
          qlpos95 <- qnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                           +mus0[,foodindex[i]]
                           +mucK[,hazardindexK[h],foodindex[i]]
                           +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                           sqrt(Vs0))/log(10)
          # positive chronic exposures, 
          # posterior quantiles of variability 95%quantile:
          q95_50 <- round(quantile(qpos95,
                                   0.5,names=FALSE),3)
          q95_95 <- round(quantile(qpos95,
                                   0.95,names=FALSE),3)
          q95_05 <- round(quantile(qpos95,
                                   0.05,names=FALSE),3)
          ql95_50 <- round(quantile(qlpos95,
                                   0.5,names=FALSE),3)
          ql95_95 <- round(quantile(qlpos95,
                                   0.95,names=FALSE),3)
          ql95_05 <- round(quantile(qlpos95,
                                   0.05,names=FALSE),3)
          
          # chronic exposure (hazard i, food j) over all days, all servings (including zeros):
          V <- 2000 # variability simulations
          qtotal95 <- numeric()
          qltotal95 <- numeric()
          
          for(u in 1:n_sim){ 
            
            # calculate quantile by solving: zero-inflated distribution (due to absolute non-consumers)
            PPOSK <- p0[u,foodindex[i]] # probability to consume at all each food
            if(0.95<=(1-PPOSK)){qtotal95[u]<-0}
            if(0.95>(1-PPOSK)){
              qtotal95[u]<- qlnorm((0.95-1+PPOSK)/PPOSK,
                                   log(pK[u,hazardindexK[h],foodindex[i]]*PK[foodindex[i],hazardindexK[h]])+logRK[foodindex[i],hazardindexK[h]]
                                   +mus0[u,foodindex[i]]
                                   +mucK[u,hazardindexK[h],foodindex[i]]
                                   +0.5*sigcK[u,hazardindexK[h],foodindex[i]]^2,
                                   sqrt(Vs0[u])) 
            }
            qltotal95[u] <- log(qtotal95[u])/log(10)
            
          } # end of for u
          
          ##Density----
          if(input_selectdist=="Density"){ 
            ############## exposure.chronicKbw
            # plot posterior of the mean & median exposure/bw 
            # (expected chronic exposure for anyone)
            ###Absolute----
            if(input_selectscale=="Absolute"){
              meanexposure <- exp(logRK[foodindex[i],hazardindexK[h]]+
                                    mus0[,foodindex[i]]+
                                    mucK[,hazardindexK[h],foodindex[i]]+
                                    0.5*sigcK[,hazardindexK[h],foodindex[i]]^2+
                                    0.5*Vs0 )
              maxx <- quantile(qlnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                      +mus0[,foodindex[i]]
                                      +mucK[,hazardindexK[h],foodindex[i]]
                                      +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                      sqrt(Vs0) ),
                               0.9,names=FALSE)
              
              medianexposure <- exp(logRK[foodindex[i],hazardindexK[h]]+
                                      mus0[,foodindex[i]]+
                                      mucK[,hazardindexK[h],foodindex[i]]+
                                      0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)
              plot(density(medianexposure,from=0,to=maxx,n=2048),main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("C.exposure/bw+  (", Unit1,"per kg)"),ylab="Probability density",xlim=c(0,maxx),lwd=3) 
              
              xvalues <- seq(0,maxx,length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                
                dlnormK <- dlnorm(xvalues[xv],
                                     logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0))
                
                uppervalues[xv] <- quantile(dlnormK,input_upper,names=FALSE) #0.9,names=FALSE)
                lowervalues[xv] <- quantile(dlnormK,input_lower,names=FALSE) #0.1,names=FALSE)
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              
              lines(density(meanexposure,from=0,to=maxx,n=2048),col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="C.exposure/bw+",ylab="",xlim=c(0,maxx),lwd=3)
              lines(density(medianexposure,from=0,to=maxx,n=2048),lwd=3)
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Estimated 95% quantile for exposure+ (consumers): ",q95_50,  
                          "(posterior median). 90% uncertainty interval for the 95% quantile:", q95_05,"-", q95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Estimated 95% quantile for exposure (all): ",round(quantile(qtotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", round(quantile(qtotal95,0.05,names=FALSE),3),"-",round(quantile(qtotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if absolute
            
            ##Logarithmic----
            if(input_selectscale=="Logarithmic"){
              logmeanexposure <- logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]+
                +mucK[,hazardindexK[h],foodindex[i]]+
                0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
              maxx <- quantile(qnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0)),
                               0.9,names=FALSE)
              minn <- quantile(qnorm(0.05,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0)),
                               0.5,names=FALSE)
              
              
              plot(density(logmeanexposure/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("log (C.exposure/bw+  (", Unit1,"per kg))"),ylab="Probability density",xlim=c(minn/log(10),maxx/log(10)),lwd=3) 
                           
              
              xvalues <- seq(minn/log(10),maxx/log(10),length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                
                uppervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_upper,names=FALSE) 
                lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_lower,names=FALSE)
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              
              
              lines(density(logmeanexposure/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="log (C.exposure/bw+)",ylab="",xlim=c(minn/log(10),maxx/log(10)),lwd=3)
            
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Estimated 95% quantile for exposure+ (consumers): ",ql95_50,  
                          "(posterior median). 90% uncertainty interval for the 95% quantile:", ql95_05,"-", ql95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Estimated 95% quantile for exposure (all): ",round(quantile(qltotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", round(quantile(qltotal95,0.05,names=FALSE),3),"-",round(quantile(qltotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if logarithmic
           
          } # end of if density
          
          #Cumulative----
          if(input_selectdist=="Cumulative"){
            par(yaxt="s")
            cump <- seq(1,n_sim)
            cump <- cump/length(cump)
            ############## exposure.chronicKbw
            # plot posterior of the mean & median exposure/bw 
            # (expected chronic exposure for anyone)
            ##Absolute----
            if(input_selectscale=="Absolute"){
              meanexposure <- sort(
                exp(logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                    +mucK[,hazardindexK[h],foodindex[i]]
                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                    +0.5*Vs0) )
              maxx <- quantile(qlnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                      +mucK[,hazardindexK[h],foodindex[i]]
                                      +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                      sqrt(Vs0) ),
                               0.9,names=FALSE)
              
              medianexposure <- sort(
                exp(logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                    +mucK[,hazardindexK[h],foodindex[i]]
                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2))
              plot(meanexposure[meanexposure<maxx],cump[meanexposure<maxx],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("C.exposure/bw+  (", Unit1,"per kg)"),ylab="Cumulative probability",xlim=c(0,maxx),ylim=c(0,1),lwd=3,type="l")
              lines(medianexposure[medianexposure<maxx],cump[medianexposure<maxx],lwd=3)   
              
              xvalues <- seq(0,maxx,length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                
                plnormK <- plnorm(xvalues[xv],
                                     logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0))
                
                uppervalues[xv] <- quantile(plnormK,input_upper,names=FALSE)
                lowervalues[xv] <- quantile(plnormK,input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              
              
              # plot empirically generated cumulative exposure distributions
              OIM <- numeric()
              for(r in 1:nr){
                OIM[r]<- exp(logsw[r,foodindex[i]]) # in FFQ data: one reported mean consumption per food, from each respondent  
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
                # create 10000 simulations from each replicated data:
                sampleOIM <- sample(sampleOIM,10000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,10000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,10000,replace=TRUE)
                lines(ecdf(sampleOIM*mean(sampleconUB)*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxx),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(sampleOIM*mean(sampleconLB)*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxx),lwd=1,lty=3,col="#004F71")
              }
              lines(meanexposure[meanexposure<maxx],cump[meanexposure<maxx],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="C.exposure/bw+",ylab="",xlim=c(0,maxx),lwd=3) 
              lines(medianexposure[medianexposure<maxx],cump[medianexposure<maxx],xlim=c(0,maxx),lwd=3)
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Estimated Q95% for exposure+ (consumers): ",q95_50,  
                          "(median). 90% uncertainty interval for the Q95%: ", q95_05,"-", q95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Estimated Q95% for exposure (all): ",round(quantile(qtotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q95%: ", round(quantile(qtotal95,0.05,names=FALSE),3),"-",round(quantile(qtotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if absolute
            
            ##Logarithmic----
            if(input_selectscale=="Logarithmic"){
              logmeanexposure <- sort(
                logRK[foodindex[i],hazardindexK[h]]
                +mus0[,foodindex[i]]
                +mucK[,hazardindexK[h],foodindex[i]]
                +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)
              
              maxx <- quantile(qnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0)),
                               0.9,names=FALSE)
              minn <- quantile(qnorm(0.05,logRK[foodindex[i],hazardindexK[h]]
                                     +mus0[,foodindex[i]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                     sqrt(Vs0)), 
                               0.5,names=FALSE)
              
              plot(logmeanexposure/log(10),cump,main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("log (C.exposure/bw+  (", Unit1,"per kg))"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),lwd=3,type="l") 
              
              xvalues <- seq(minn/log(10),maxx/log(10),length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_upper,names=FALSE) 
                lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                                   +mucK[,hazardindexK[h],foodindex[i]]
                                                   +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2)/log(10),
                                                  sqrt(Vs0)/log(10) ),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              
              
              # plot empirically generated cumulative exposure distributions
              OIM <- numeric()
              for(r in 1:nr){
                OIM[r]<- exp(logsw[r,foodindex[i]]) # in FFQ data: one reported mean consumption per food, from each respondent  
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
                # create 10000 simulations from each replicated data:
                sampleOIM <- sample(sampleOIM,10000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,10000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,10000,replace=TRUE)
                lines(ecdf(log(sampleOIM*mean(sampleconUB)*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minn/log(10),maxx/log(10)),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(log(sampleOIM*mean(sampleconLB)*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minn/log(10),maxx/log(10)),lwd=1,lty=3,col="#004F71")
              }
              lines(logmeanexposure/log(10),cump,main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),xlab="log (C.exposure/bw+)",ylab="",xlim=c(minn/log(10),maxx/log(10)),lwd=3) 
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Estimated 95% quantile for exposure+ (consumers): ",ql95_50,  
                          "(posterior median). 90% uncertainty interval for the 95% quantile:", ql95_05,"-", ql95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Estimated 95% quantile for exposure (all): ",round(quantile(qltotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", round(quantile(qltotal95,0.05,names=FALSE),3),"-",round(quantile(qltotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if logarithmic    
                        
            
          } # end of if cumulative
          
          
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
  
  # Microbial exposures:  ----
  
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
          Vs0 <- numeric() # variances
          for(u in 1:n_sim){
            Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]] 
          }  
          
          # chronic exposure (hazard i, food j) over all days, all servings (including zeros):
          V <- 2000 # variability simulations
          qtotal95 <- numeric()
          qpos95 <- numeric()
          qltotal95 <- numeric()
          qlpos95 <- numeric()
          
          for(u in 1:n_sim){ 
            PPOSM <- p0[u,foodindex[i]] # probability to consume at all each food
            if(0.95<=(1-PPOSM)){qtotal95[u]<-0}
            if(0.95>(1-PPOSM)){
              qtotal95[u]<- qlnorm((0.95-1+PPOSM)/PPOSM,
                                   log(pM[u,hazardindexM[h],foodindex[i]]*PM[foodindex[i],hazardindexM[h]])+logRM[foodindex[i],hazardindexM[h]]
                                   +mus0[u,foodindex[i]]
                                   +mucM[u,hazardindexM[h],foodindex[i]]
                                   +muw[u]
                                   +0.5*sigcM[u,hazardindexM[h],foodindex[i]]^2,
                                   sqrt(Vs0[u]
                                        +sigw[u]^2))
            }
            qltotal95[u] <- log(qtotal95[u])/log(10)
            
            
            # positive chronic (mean) exposure variability quantile:
            qpos95[u] <- qlnorm(0.95,logRM[foodindex[i],hazardindexM[h]]
                                +mus0[u,foodindex[i]]
                                +muw[u]
                                +mucM[u,hazardindexM[h],foodindex[i]]
                                +0.5*sigcM[u,hazardindexM[h],foodindex[i]]^2,
                                sqrt(Vs0[u]+sigw[u]^2) )
            qlpos95[u] <- qnorm(0.95,logRM[foodindex[i],hazardindexM[h]]
                                +mus0[u,foodindex[i]]
                                +muw[u]
                                +mucM[u,hazardindexM[h],foodindex[i]]
                                +0.5*sigcM[u,hazardindexM[h],foodindex[i]]^2,
                                sqrt(Vs0[u]+sigw[u]^2) )/log(10)
            
          } # end of for u
          
          # positive chronic (poisson mean) exposures, 
          # posterior quantiles of 95% variability quantile:
          q95_05 <- round(quantile(qpos95,0.05,names=FALSE,na.rm=TRUE),2)
          q95_50 <- round(quantile(qpos95,0.5,names=FALSE,na.rm=TRUE),2)
          q95_95 <- round(quantile(qpos95,0.95,names=FALSE,na.rm=TRUE),2)
          ql95_05 <- round(quantile(qlpos95,0.05,names=FALSE,na.rm=TRUE),2)
          ql95_50 <- round(quantile(qlpos95,0.5,names=FALSE,na.rm=TRUE),2)
          ql95_95 <- round(quantile(qlpos95,0.95,names=FALSE,na.rm=TRUE),2)
          
          ##Density----
          if(input_selectdist=="Density"){
            ############## exposure.acuteM
            # plot posterior of the mean & median exposure 
            # (expected chronic exposure for anyone, FFQ data) 
            ###Absolute----
            if(input_selectscale=="Absolute"){   
              meanexposure <- exp(logRM[foodindex[i],hazardindexM[h]]
                                  +mus0[,foodindex[i]]
                                  +mucM[,hazardindexM[h],foodindex[i]]
                                  +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2 
                                  +muw
                                  +0.5*Vs0
                                  +0.5*sigw^2 )
              maxx <- quantile(qlnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]+
                                        mus0[,foodindex[i]]+
                                        mucM[,hazardindexM[h],foodindex[i]]+
                                        0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                        muw,   
                                      sqrt(Vs0+sigw^2) ),
                               0.9,names=FALSE)
              medianexposure <- exp(logRM[foodindex[i],hazardindexM[h]]+
                                      mus0[,foodindex[i]]+
                                      mucM[,hazardindexM[h],foodindex[i]]+0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+muw)
              
              plot(density(medianexposure,from=0,to=maxx,n=2048),main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("C.exposure+  (", Unit1,"per day)"),ylab="Probability density",xlim=c(0,maxx),lwd=3) 
              lines(density(meanexposure,from=0,to=maxx,n=2048),lwd=3,col="#F7CE3C")
              
              xvalues <- seq(0,maxx,length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                                   logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                     muw,
                                                   sqrt(Vs0+sigw^2)),
                                            input_upper,names=FALSE) 
                lowervalues[xv] <- quantile(dlnorm(xvalues[xv],
                                                   logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                     muw,
                                                   sqrt(Vs0+sigw^2)),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(density(meanexposure,from=0,to=maxx,n=2048),col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),xlab="C.exposure+",ylab="",xlim=c(0,maxx),lwd=3) 
              lines(density(medianexposure,from=0,to=maxx,n=2048),lwd=3)
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Estimated 95% quantile for exposure+ (consumers): ",q95_50,  
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", q95_05,"-", q95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Estimated 95% quantile for exposure (all): ",round(quantile(qtotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", round(quantile(qtotal95,0.05,names=FALSE),3),"-",round(quantile(qtotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if absolute
            
            ##Logarithmic----
            if(input_selectscale=="Logarithmic"){
              logmeanexposure <- logRM[foodindex[i],hazardindexM[h]]+
                mus0[,foodindex[i]]+mucM[,hazardindexM[h],foodindex[i]]+0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+muw
              maxx <- quantile(qnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                     +muw,
                                     sqrt(Vs0+sigw^2)),
                               0.9,names=FALSE)
              minn <- quantile(qnorm(0.05,logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                     +muw,
                                     sqrt(Vs0+sigw^2)),
                               0.5,names=FALSE)
              plot(density(logmeanexposure/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("log (C.exposure+  (", Unit1,"per day))"),ylab="Probability density",xlim=c(minn/log(10),maxx/log(10)),lwd=3) 
              
              xvalues <- seq(minn/log(10),maxx/log(10),length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                     muw)/log(10),
                                                  (sqrt(Vs0+sigw^2))/log(10) ),
                                            input_upper,names=FALSE)
                lowervalues[xv] <- quantile(dnorm(xvalues[xv],
                                                  (logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                     muw)/log(10),
                                                  (sqrt(Vs0+sigw^2))/log(10) ),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(density(logmeanexposure/log(10),from=minn/log(10),to=maxx/log(10),n=2048),col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),xlab="log (C.exposure+)",ylab="",xlim=c(minn/log(10),maxx/log(10)),lwd=3)
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Estimated 95% quantile for exposure+ (consumers): ",ql95_50,  
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", ql95_05,"-", ql95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Estimated 95% quantile for exposure (all): ",round(quantile(qltotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", round(quantile(qltotal95,0.05,names=FALSE),3),"-",round(quantile(qltotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
            } # end of if logarithmic
            
          } # end of if density
          
          #Cumulative----
          if(input_selectdist=="Cumulative"){
            par(yaxt="s")
            cump=seq(1,n_sim)
            cump=cump/length(cump)
            
            ##Absolute----
            if(input_selectscale=="Absolute"){
              meanexposure <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                                       +mus0[,foodindex[i]]
                                       +mucM[,hazardindexM[h],foodindex[i]]
                                       +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                       +muw
                                       +0.5*Vs0
                                       +0.5*sigw^2 ))
              maxx <- quantile(qlnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]+
                                        mus0[,foodindex[i]]+
                                        mucM[,hazardindexM[h],foodindex[i]]+
                                        0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                        muw,   
                                      sqrt(Vs0+sigw^2) ),
                               0.9,names=FALSE)
              medianexposure <- sort(exp(logRM[foodindex[i],hazardindexM[h]]+
                                           mus0[,foodindex[i]]+
                                           mucM[,hazardindexM[h],foodindex[i]]+
                                           0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                           muw))
              
              plot(meanexposure[meanexposure<maxx],cump[meanexposure<maxx],col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("C.exposure+  (", Unit1,"per day)"),ylab="Cumulative probability",xlim=c(0,maxx),ylim=c(0,1),type="l",lwd=3) 
              
              xvalues <- seq(0,maxx,length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(plnorm(xvalues[xv],
                                                   logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                     muw,
                                                   sqrt(Vs0+sigw^2)),
                                            input_upper,names=FALSE) #0.9,names=FALSE)
                lowervalues[xv] <- quantile(plnorm(xvalues[xv],
                                                   logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                     muw,
                                                   sqrt(Vs0+sigw^2)),
                                            input_lower,names=FALSE) #0.1,names=FALSE)
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              
              
              # plot empirically generated cumulative exposure distributions
              OIM <- numeric()
              for(r in 1:nr){
                OIM[r]<- exp(logs[r,foodindex[i]]) # in FFQ data: one reported mean consumption per food, from each respondent 
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
                sampleOIM <- sample(OIM,length(OIM),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 10000 simulations from each replicated data:
                sampleOIM <- sample(sampleOIM,10000,replace=TRUE)
                sampleconUB <- sample(sampleconUB,10000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,10000,replace=TRUE)
                lines(ecdf(sampleOIM*mean(sampleconUB)*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxx),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(sampleOIM*mean(sampleconLB)*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxx),lwd=1,lty=3,col="#004F71")
              }
              
              lines(meanexposure[meanexposure<maxx],cump[meanexposure<maxx],col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),xlab="C.exposure+",ylab="",xlim=c(0,maxx),lwd=3) 
              lines(medianexposure[medianexposure<maxx],cump[medianexposure<maxx],xlim=c(0,maxx),lwd=3)
              
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Estimated Q95% for exposure+ (consumers): ",q95_50,  
                          "(posterior median). 90% uncertainty interval for the Q95%: ", q95_05,"-", q95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Estimated Q95% for exposure (all): ",round(quantile(qtotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the Q95%: ", round(quantile(qtotal95,0.05,names=FALSE),3),"-",round(quantile(qtotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
              
              
            } # end of if absolute
            
            ##Logarithmic----
            if(input_selectscale=="Logarithmic"){
              logmeanexposure <- sort(logRM[foodindex[i],hazardindexM[h]]
                                      +mus0[,foodindex[i]]
                                      +mucM[,hazardindexM[h],foodindex[i]]
                                      +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                      +muw)
              maxx <- quantile(qnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                     +muw,
                                     sqrt(Vs0+sigw^2)),
                               0.9,names=FALSE)
              minn <- quantile(qnorm(0.05,logRM[foodindex[i],hazardindexM[h]]
                                     +mus0[,foodindex[i]]
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                     +muw,
                                     sqrt(Vs0+sigw^2)),
                               0.5,names=FALSE)
              plot(logmeanexposure/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
                   xlab=paste("log (C.exposure+  (", Unit1,"per day))"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),lwd=3,type="l") 
              
              xvalues <- seq(minn/log(10),maxx/log(10),length=100)
              uppervalues <- numeric()
              lowervalues <- numeric()
              for(xv in 1:100){
                uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                     muw)/log(10),
                                                  (sqrt(Vs0+sigw^2))/log(10) ),
                                            input_upper,names=FALSE)
                lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                                  (logRM[foodindex[i],hazardindexM[h]]+
                                                     mus0[,foodindex[i]]+
                                                     mucM[,hazardindexM[h],foodindex[i]]+
                                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+
                                                     muw)/log(10),
                                                  (sqrt(Vs0+sigw^2))/log(10) ),
                                            input_lower,names=FALSE) 
              }
              polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
              lines(logmeanexposure/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),xlab="log (C.exposure+)",ylab="",xlim=c(minn/log(10),maxx/log(10)),lwd=3) 
              
              
              # plot empirically generated cumulative exposure distributions
              OIM <- numeric()
              for(r in 1:nr){
                OIM[r]<- exp(logs[r,foodindex[i]]) # in FFQ data: one reported mean consumption per food, from each respondent 
              } 
              OIM<-OIM[!is.na(OIM)]
              # collect exact measurements & 
              # and as upper bounds those between LOD-LOQ & <LOD 
              concentrationsUB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                      logLOQM[hazardindexM[h],foodindex[i],],
                                      logLODM[hazardindexM[h],foodindex[i],]))
              # and using lower bounds
              concentrationsLB <- exp(c(logcM[hazardindexM[h],foodindex[i],],
                                       logLOQLimM[hazardindexM[h],foodindex[i],],
                                       logLODLimM[hazardindexM[h],foodindex[i],]-20))
              concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
              concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
              
              for(resample in 1:40){
                # create 40 replicate ('bootstrap') data with original nsample:
                sampleOIM <- sample(OIM,length(OIM),replace=TRUE)
                sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
                sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
                # create 10000 simulations from each replicated data:
                sampleconUB <- sample(sampleconUB,10000,replace=TRUE)
                sampleconLB <- sample(sampleconLB,10000,replace=TRUE)
                lines(ecdf(log(sampleOIM*mean(sampleconUB)*RM[foodindex[i],hazardindexM[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minn/log(10),maxx/log(10)),lwd=1,lty=3,col="#D0006F")
                lines(ecdf(log(sampleOIM*mean(sampleconLB)*RM[foodindex[i],hazardindexM[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minn/log(10),maxx/log(10)),lwd=1,lty=3,col="#004F71")
              }
              lines(logmeanexposure/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),xlab="log (C.exposure+)",ylab="",xlim=c(minn/log(10),maxx/log(10)),lwd=3) 
              
              # legend outside the figure, but onto the current plot, so it is part of the png file:
              mtext(paste("Estimated 95% quantile for exposure+ (consumers): ",ql95_50,  
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", ql95_05,"-", ql95_95),
                    side = 1, adj = 0,line=2, cex = 1,
                    outer = TRUE)
              mtext(paste("Estimated 95% quantile for exposure (all): ",round(quantile(qltotal95,0.5,names=FALSE),3),
                          "(posterior median). 90% uncertainty interval for the 95% quantile: ", round(quantile(qltotal95,0.05,names=FALSE),3),"-",round(quantile(qltotal95,0.95,names=FALSE),3)),
                    side = 1, adj = 0,line=3, cex = 1,
                    outer = TRUE)
               
            } # end of if logarithmic
            
          } # end of if cumulative
          
          
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
distPlot4_1FFQ <- function(unit_concen, hazard_concen, n_sim, input_selectscale, input_selectQ, nV,
                        nU, Rall, Pall,
                        nfused, foodindex, 
                        nexactK, nexactM, 
                        nhused, hazardnames, hazardnamesusedK, hazardnamesusedM,
                        nhusedK, nhusedM, hazardindexK, hazardindexM, nhK,nhM,nf,
                        mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                        Ss0,
                        logitp0
) {
  # generate results based on inputs from ui.R:  
  # uncertainties of variability quantiles
  
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
    
    
    # if( (nhused>0)&(nfused>0) ){
    # par(mfrow=c(nhused,1),cex.lab=1.3,cex.main=1.3,yaxt="n")
    #par(mfrow=c(1,1),cex.lab=1.3,cex.main=1.3,yaxt="n")
    # }
    
    # generate nU variability distributions (each with nV variability simulations), 
    # evaluate quantiles for each of those variability distributions:
    mc <- round(seq(1,n_sim,length=nU),0)
    
    p0 <- exp(logitp0)/(1+exp(logitp0)) # probability to consume each food at all (FFQ data)
    # Chemical exposure quantiles----
    
    if((nhusedK>0)&(nfused>0)){ 
      RK = matrix(NA,nf,nhK) # concentration factors
      RK[1:nf,1:nhK] = Rall[1:nf,is.element(hazardnames,hazardnamesusedK)]
      logRK = log(RK)
      PK = matrix(NA,nf,nhK) # prevalence factors
      PK[1:nf,1:nhK] = Pall[1:nf,is.element(hazardnames,hazardnamesusedK)]
      
        musmc <- matrix(NA,nV,nf)
        Umc <- matrix(NA,nV,nf)
        Eemc <- array(NA,dim=c(nV,nhusedK,nfused))  # for all days
        Eemcconuse <- array(NA,dim=c(nV,nhusedK,nfused)) # for contaminated consumption days
        Eetotmc <- matrix(NA,nV,nhusedK)
        Eetotmcconuse <- matrix(NA,nV,nhusedK)
        Q <- matrix(NA,nU,nhusedK) # for chronic exposure all days
        Qplus <- matrix(NA,nU,nhusedK) # for chronic exposure from consumption days
        thin <- 0 # for indexing a thinned sample of (simulated) variability distributions
        exposurevarsample <- matrix(NA,ceiling(nU/5),nV) # for thinned uncertainty sample
        
        # 2D simulation of uncertainty & variability:
        for(u in 1:nU){ # for nU parameter sets
          if(nf>1){ # if many foods
            musmc[1:nV,1:nf] <- rmvnorm(nV,mus0[mc[u],1:nf],Ss0[mc[u],1:nf,1:nf]) # individual mean amount
            for(f in 1:nf){
              Umc[1:nV,f] <- rbinom(nV,1,p0[mc[u],f])  # actual true users for each food  
            }
          } 
          if(nf==1){ # if only one food
            musmc[1:nV,1] <- rnorm(nV,mus0[mc[u],1],sqrt(Ss0[mc[u],1,1])) # individual mean amount
            Umc[1:nV,1] <- rbinom(nV,1,p0[mc[u],1])  # actual true users for the food 
          }
          
          
          for(v in 1:nV){ # for nV variable values per each parameter set
            
            for(h in 1:nhusedK){
              for(i in 1:nfused){
                if(nexactK[hazardindexK[h],foodindex[i]]==0){ # hazard-food not modeled
                  Eemc[v,h,i]<- 0
                  Eemcconuse[v,h,i] <- 0
                }
                if(nexactK[hazardindexK[h],foodindex[i]]>0){ 
                  # evaluate mean exposure of hazard h in food i, variable individual v,
                  # for all days (consumed or not, contaminated or not)
                  Eemc[v,h,i]<-pK[mc[u],hazardindexK[h],foodindex[i]]*
                    PK[foodindex[i],hazardindexK[h]]*
                    Umc[v,foodindex[i]]*
                   exp(logRK[foodindex[i],hazardindexK[h]]
                                            +musmc[v,foodindex[i]]
                                            +mucK[mc[u],hazardindexK[h],foodindex[i]]
                                            +0.5*sigcK[mc[u],hazardindexK[h],foodindex[i]]^2)  
                  
                  # evaluate mean exposure of hazard h in food i, variable individual v,
                  # for actual consumption days for actual contaminated occurrences
                  Eemcconuse[v,h,i] <- exp(logRK[foodindex[i],hazardindexK[h]]
                                           +musmc[v,foodindex[i]]
                                           +mucK[mc[u],hazardindexK[h],foodindex[i]]
                                           +0.5*sigcK[mc[u],hazardindexK[h],foodindex[i]]^2)
                } # end of if nexactK>0
              } # end of i
              # simulated total chronic exposure for individual, hazard h, all foods:
              Eetotmc[v,h] <- sum(Eemc[v,h,1:nfused]) 
              # simulated total chronic exposure for individual,
              # for contaminated consumption days, hazard h, all foods 
              Eetotmcconuse[v,h] <- sum(Eemcconuse[v,h,1:nfused])   
            }
          } # end of v (variability) 
          for(h in 1:nhusedK){
            # variability quantile of total exposure, hazard h, all days
            Q[u,h]<-quantile(Eetotmc[,h],theQ/100,names=FALSE)
            # variability quantile of total exposure, hazard h, 
            # for contaminated consumption days
            Qplus[u,h]<-quantile(Eetotmcconuse[,h],theQ/100,names=FALSE)
          }
          #######################################################
          # pick out thinned sample:
          if(ceiling(u/5)==floor(u/5)){ thin<-thin+1; exposurevarsample[thin,1:nV]<- t(Eetotmcconuse[1:nV,h]) }
          #######################################################
        } # end of u (uncertainty)
        
        for(h in 1:nhusedK){
          Unit <- unit_concen[hazard_concen == hazardnamesusedK[h]] # the measurement unit used for hazard concentration
          Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
          
          ##Absolute----
          if(input_selectscale=="Absolute"){
            #########################################
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotK <- sum(nexactK[hazardindexK[h],foodindex]>0)
            xmin <- min(exposurevarsample[1:thin,1:nV])
            xmax <- quantile(exposurevarsample[1:thin,1:nV],0.99,names=FALSE)
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
            quplim <- quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE)
            qlolim <- quantile(Qplus[,h],0.05,names=FALSE,na.rm=TRUE)
            lines(density(Qplus[,h],from=qlolim,to=quplim)$x,density(Qplus[,h],from=qlolim,to=quplim)$y/max(density(Qplus[,h],from=qlolim,to=quplim)$y),lwd=3)   
            lines(quantile(Qplus[,h],c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(Qplus[,h],c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste("Estimated Q",theQ,"% for the mean exposures (consumers): ",round(quantile(Qplus[,h],0.5,names=FALSE),2),  
                        "(posterior median). 90% uncertainty interval: ", round(quantile(Qplus[,h],0.05,names=FALSE),2),"-", round(quantile(Qplus[,h],0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste("Estimated Q",theQ,"% for mean exposures (all): ",round(quantile(Q[,h],0.5,names=FALSE),2),  
                        "(posterior median). 90% uncertainty interval: ", round(quantile(Q[,h],0.05,names=FALSE),2),"-", round(quantile(Q[,h],0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
            
          }
          ##Logarithmic----
          if(input_selectscale=="Logarithmic"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotK <- sum(nexactK[hazardindexK[h],foodindex]>0) 
            xmin <- log10(min(exposurevarsample[1:thin,1:nV],na.rm=TRUE))
            xmax <- log10(quantile(exposurevarsample[1:thin,1:nV],0.99,na.rm=TRUE,names=FALSE))
            plot(ecdf(log(exposurevarsample[1,1:nV])/log(10)),verticals=TRUE,do.points=FALSE,yaxt="s",
                 xlim=c(xmin,xmax),ylim=c(0,1),
                 lwd=1,lty=3,col="#D0006F",ylab="Cumulative probability",
                 xlab=paste("log (C.exposure/bw+)(", Unit1,"per kg)"),
                 main=paste("Exposure:",hazardnamesusedK[h],"total from",nftotK,"foods (chronic)"))
            for(a in 2:thin){
              lines(ecdf(log(exposurevarsample[a,1:nV])/log(10)),verticals=TRUE,do.points=FALSE,
                    xlim=c(xmin,xmax),
                    lwd=1,lty=3,col="#D0006F")     
            }
            quplim <- quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE)
            qlolim <- quantile(log10(Qplus[,h]),0.05,names=FALSE,na.rm=TRUE)
            lines(density(log10(Qplus[,h]),from=qlolim,to=quplim)$x,density(log10(Qplus[,h]),from=qlolim,to=quplim)$y/max(density(log10(Qplus[,h]),from=qlolim,to=quplim)$y),lwd=3)   
            lines(quantile(log10(Qplus[,h]),c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste("Estimated Q",theQ,"% for the mean exposures (consumers): ",round(quantile(log10(Qplus[,h]),0.5,names=FALSE),2),  
                        "(posterior median). 90% uncertainty interval: ", round(quantile(log10(Qplus[,h]),0.05,names=FALSE),2),"-", 
                        round(quantile(log10(Qplus[,h]),0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste("Estimated Q",theQ,"% for the mean exposures (all): ",round(quantile(log10(Q[,h]),0.5,names=FALSE),2),  
                        "(posterior median). 90% uncertainty interval: ", round(quantile(log10(Q[,h]),0.05,names=FALSE),2),"-", 
                        round(quantile(log10(Q[,h]),0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          }    
        } # uncertainty distribution of Q% exposure for hth chemical
      
      ########################################

      
    } # end of if nhusedK>0 nfused>0
    
    #######################################################################
    
    # quantiles when FFQ data (i.e. only 'chronic exposures' also for microbial)
    # Microbial exposure quantiles----
    if((nhusedM>0)&(nfused>0)){  
      
      RM = matrix(NA,nf,nhM) # factors for concentrations
      RM[1:nf,1:nhM] = Rall[1:nf,is.element(hazardnames,hazardnamesusedM)]
      logRM = log(RM)
      PM = matrix(NA,nf,nhM) # factors for prevalence
      PM[1:nf,1:nhM] = Pall[1:nf,is.element(hazardnames,hazardnamesusedM)]
        
        wmc <- numeric()
        musmc <- matrix(NA,nV,nf)
        Umc <- matrix(NA,nV,nf)
        cmc <-  array(NA,dim=c(nV,nhM,nf)) 
        nplus<-matrix(NA,nU,nhusedM)
        poissonmeansall <- array(NA,dim=c(nV,nhusedM,nfused))
        poissonmeanspos <- array(NA,dim=c(nV,nhusedM,nfused))
        expoall <- matrix(NA,nV,nhusedM)
        expopos <- matrix(NA,nV,nhusedM)
        Q <- matrix(NA,nU,nhusedM); Qplus <- matrix(NA,nU,nhusedM) 
        thin <- 0 # for indexing a thinned sample of (simulated) variability distributions
        exposurevarsample <- matrix(NA,ceiling(nU/5),nV) # for thinned uncertainty sample
        
        # 2D simulation of uncertainty & variability:
        for(u in 1:nU){ # for nU parameter sets
          
          wmc[1:nV] <- rlnorm(nV,muw[mc[u]],sigw[mc[u]]) # bodyweight for v:th individual
            if(nf>1){ # if many foods
              musmc[1:nV,1:nf] <- rmvnorm(nV,mus0[mc[u],1:nf],Ss0[mc[u],1:nf,1:nf])
              for(f in 1:nf){
                Umc[1:nV,f] <- rbinom(nV,1,p0[mc[u],f])  # actual true users for each food  
              }
            }
          if(nf==1){ # if only one food
            musmc[1:nV,1] <- rnorm(nV,mus0[mc[u],1],sqrt(Ss0[mc[u],1,1]))
            Umc[1:nV,1] <- rbinom(nV,1,p0[mc[u],1])  # actual true users for the food 
          }
          
          for(h in 1:nhusedM){
            for(i in 1:nfused){
              if(nexactM[hazardindexM[h],foodindex[i]]==0){ # hazard-food not modeled
                poissonmeansall[1:nV,h,i] <- rep(0,nV)
                poissonmeanspos[1:nV,h,i] <- rep(0,nV)
              }
              if(nexactM[hazardindexM[h],foodindex[i]]>0){ 
                # (poisson)mean exposure for day serving when contaminated & consumed:
                poissonmeanspos[1:nV,h,i] <-  exp(musmc[1:nV,foodindex[i]])*wmc[1:nV]*
                  RM[foodindex[i],hazardindexM[h]]*exp(mucM[mc[u],hazardindexM[h],foodindex[i]]+0.5*sigcM[mc[u],hazardindexM[h],foodindex[i]]^2 )
                # (poisson)mean exposure for any day incl. zeros: 
                poissonmeansall[1:nV,h,i] <-
                  Umc[1:nV,foodindex[i]]*pM[mc[u],hazardindexM[h],foodindex[i]]*PM[foodindex[i],hazardindexM[h]]*
                  exp(musmc[1:nV,foodindex[i]])*wmc[1:nV]*
                  RM[foodindex[i],hazardindexM[h]]*exp(mucM[mc[u],hazardindexM[h],foodindex[i]]+0.5*sigcM[mc[u],hazardindexM[h],foodindex[i]]^2 )
              } # end of if nexactM>0
            } # end of for i
          } # end of for h
          
          for(v in 1:nV){ # for nV variable values per each parameter set
            for(h in 1:nhusedM){
          expopos[v,h] <- sum(poissonmeanspos[v,h,1:nfused])
          expoall[v,h] <- sum(poissonmeansall[v,h,1:nfused])
            } # end of h
          } # end of v (variability)
          
          for(h in 1:nhusedM){
            Qplus[u,h] <- quantile(expopos[1:nV,h],theQ/100,names=FALSE)
            Q[u,h] <- quantile(expoall[1:nV,h],theQ/100,names=FALSE)
        
          }
          #######################################################
          # pick out thinned sample of positive acute exposures:
          if(ceiling(u/5)==floor(u/5)){ 
            thin<-thin+1 
              exposurevarsample[thin,1:nV] <- t(expopos[1:nV,h]) 
          }
          #######################################################
          
        } # end of u (uncertainty)
        
        for(h in 1:nhusedM){
          Unit <- unit_concen[hazard_concen == hazardnamesusedM[h]] # the measurement unit used for hazard concentration
          Unit1 <- sub(".p.*", "", Unit) # Extract characters before pattern
          
          ###Absolute----
          if(input_selectscale=="Absolute"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotM <- sum(nexactM[hazardindexM[h],foodindex]>0)
              
              xmin <- min(exposurevarsample[1:thin,1:nV],na.rm=TRUE)
              xmax <- quantile(exposurevarsample[1:thin,1:nV],0.99,na.rm=TRUE,names=FALSE)
              plot(ecdf(exposurevarsample[1,1:nV]),verticals=TRUE,
                   do.points=FALSE,yaxt="s",
                   xlim=c(xmin,xmax),ylim=c(0,1),
                   lwd=1,lty=3,col="#D0006F",
                   xlab=paste("C.dose+ (", Unit1,"per day)"),ylab="Cumulative probability",
                   main=paste("Exposure:",hazardnamesusedM[h],"total from",nftotM,"foods (acute)"))
              
            for(a in 2:thin){
              lines(ecdf(exposurevarsample[a,1:nV]),
                    verticals=TRUE,do.points=FALSE,
                    xlim=c(xmin,xmax),
                    ylim=c(0,1),
                    lwd=1,lty=3,col="#D0006F") 
            }
            quplim <- quantile(Qplus[,h],0.95,names=FALSE,na.rm=TRUE)
            qlolim <- quantile(Qplus[,h],0.05,names=FALSE,na.rm=TRUE)
            lines(density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$x,density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$y/max(density(Qplus[,h],na.rm=TRUE,from=qlolim,to=quplim)$y),lwd=3)   
            lines(quantile(Qplus[,h],c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(Qplus[,h],c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste("Estimated Q",theQ,"% for the mean exposures (consumers): ",round(quantile(Qplus[,h],0.5,na.rm=TRUE,names=FALSE),2),  
                        "(posterior median). 90% uncertainty interval:", round(quantile(Qplus[,h],0.05,na.rm=TRUE,names=FALSE),2),"-", round(quantile(Qplus[,h],0.95,na.rm=TRUE,names=FALSE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste("Estimated Q",theQ,"% for the mean exposures (all): ",round(quantile(Q[,h],0.5,names=FALSE),2),  
                        "(posterior median). 90% uncertainty interval: ", round(quantile(Q[,h],0.05,names=FALSE),2),"-", round(quantile(Q[,h],0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          }
          
          ###Logarithmic----
          if(input_selectscale=="Logarithmic"){
            # count how many hazard-food combinations actually exist (some had no data, were excluded)
            nftotM <- sum(nexactM[hazardindexM[h],foodindex]>0)
              
              xmin <- log10(min(exposurevarsample[1:thin,1:nV]))
              xmax <- log10(quantile(exposurevarsample[1:thin,1:nV],0.99,names=FALSE))
              plot(ecdf(log10(exposurevarsample[1,1:nV])),
                   verticals=TRUE,do.points=FALSE,yaxt="s",
                   xlim=c(xmin,xmax),ylim=c(0,1),
                   lwd=1,lty=3,col="#D0006F",
                   xlab=paste("log C.dose+ (", Unit1,"per day)"),ylab="Cumulative probability",
                   main=paste("Exposure:",hazardnamesusedM[h],"total from",nftotM,"foods (acute)"))
            for(a in 2:thin){
              lines(ecdf(log10(exposurevarsample[a,1:nV])),verticals=TRUE,do.points=FALSE,
                    xlim=c(xmin,xmax),
                    lwd=1,lty=3,col="#D0006F")
            }
            quplim <- quantile(log10(Qplus[,h]),0.95,names=FALSE,na.rm=TRUE)
            qlolim <- quantile(log10(Qplus[,h]),0.05,names=FALSE,na.rm=TRUE)
            lines(density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$x,density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$y/max(density(log10(Qplus[,h]),na.rm=TRUE,from=qlolim,to=quplim)$y),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.05,0.05),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            lines(quantile(log10(Qplus[,h]),c(0.95,0.95),names=FALSE,na.rm=TRUE),c(0,1),lwd=3)
            
            # legend outside the figure, but onto the current plot, so it is part of the png file:
            mtext(paste("Estimated Q",theQ,"% for the mean exposures (consumers): ",round(quantile(log10(Qplus[,h]),0.5,na.rm=TRUE,names=FALSE),2),  
                        "(posterior median). 90% uncertainty interval: ", round(quantile(log10(Qplus[,h]),0.05,na.rm=TRUE,names=FALSE),2),"-", 
                        round(quantile(log10(Qplus[,h]),0.95,na.rm=TRUE,names=FALSE),2)),
                  side = 1, adj = 0,line=1, cex = 1,
                  outer = TRUE)
            
            mtext(paste("Estimated Q",theQ,"% for the mean exposures (all): ",round(quantile(log10(Q[,h]),0.5,names=FALSE),2),  
                        "(posterior median). 90% uncertainty interval: ", round(quantile(log10(Q[,h]),0.05,names=FALSE),2),"-", 
                        round(quantile(log10(Q[,h]),0.95,names=FALSE),2)),
                  side = 1, adj = 0,line=2, cex = 1,
                  outer = TRUE)
            
          }
        } # end of uncertainty distribution of Q% exposure for hth microbe
      
    } # end of if nhusedM>0 nfused>0
    
  } # end of if theresults
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
distPlot5_2FFQ <- function(n_sim,foodnamesused, nfused, foodindex,
                        nf,
                        mus0,logitp0,
                        Ss0
) {
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
     Vs0 <- numeric() # variances
    for(u in 1:n_sim){
      Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]]  
    }
    
    plot(mus0[,foodindex[i]]/log(10), y= 1:length(mus0[,foodindex[i]]), pch=16,cex=0.5,col="#D0006F") 
    plot(density(mus0[,foodindex[i]]/log(10))$x, 0.3*n_sim/max(density(mus0[,foodindex[i]]/log(10))$y)*density(mus0[,foodindex[i]]/log(10))$y,main=bquote(.(foodnamesused[i])~":"~mu),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
    
    plot(Vs0/log(10),y= 1:length(Vs0),pch=16,cex=0.5,col="#D0006F") 
    plot(density(Vs0/log(10))$x,0.3*n_sim/max(density(Vs0/log(10))$y)*density(Vs0/log(10))$y,main=bquote(.(foodnamesused[i])~":"~sigma),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
    
    plot(p0[,foodindex[i]],y= 1:length(p0[,foodindex[i]]),pch=16,cex=0.5,col="#D0006F") 
    plot(density(p0[,foodindex[i]])$x,0.3*n_sim/max(density(p0[,foodindex[i]])$y)*density(p0[,foodindex[i]])$y,main=bquote(.(foodnamesused[i])~":"~p),type = "l",lty = 1,lwd=1, xaxt = "n", yaxt = "n")
    
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
  
}

# Plot 7: Mean serving correlations----
## ---- distPlot7_1 --------
distPlot7_1FFQ <- function(food_consum, unit_consum, n_sim, foodnamesused, nfused, foodindex,
                        nf,nr,logsw,
                        mus0,
                        Ss0
) {     
  # generate results based on inputs from ui.R: 
  # Correlation plots for mean consumptions
  
  Unit <- character()
  Unit3 <- character()
  for(k in 1:length(foodnamesused)){
  Unit[k] <- unit_consum[food_consum == foodnamesused[k]] # the measurement units used for food consumptions
  Unit3 <- paste(Unit3,sub(".*p.", "", Unit[k])) # Extract characters after pattern
  }  
  
  if(nfused>1){  
    # generate a model predicted sample of positive mean consumptions, 
    # and plot these in pairs (with data points)
    nsample <- 1000 # number of samples to generate
    sampledmus <- matrix(NA,nsample,nf) # for the means in log-scale
    sampledmeans <- matrix(NA,nsample,nf) # for the means in absolute scale
    mc <- round(seq(1,n_sim,length=nsample))
    
    for(i in 1:nsample){
      sampledmus[i,1:nf] <- rmvnorm(1,mus0[mc[i],1:nf],Ss0[mc[i],1:nf,1:nf])
      sampledmeans[i,1:nf] <- exp(sampledmus[i,1:nf]) 
    }
    
    datameansw <- matrix(NA,nr,nf)
    for(r in 1:nr){ # consumers
      for(i in 1:nf){ # individual mean consumptions from FFQ data:
        datameansw[r,i]<- exp(logsw[r,i])  # single value per individual in FFQ data
      }
    }
    
    group <- c(rep(1,nr),rep(2,nsample)) # groups for data values and simulated values
    DF1 <- data.frame(log10(datameansw[1:nr,foodindex]))
    DF2 <- data.frame(log10(sampledmeans[1:nsample,foodindex]))
    colnames(DF1) <- foodnamesused
    colnames(DF2) <- foodnamesused
    par(xpd=TRUE)
    pairs(rbind(DF1,DF2),
          main=paste("Pairwise scatterplots of log (E(consumption/bw+(", Unit3,"per kg)))"),
          upper.panel=NULL,omd=c(1,1,15,1),
          cex=c(1,0.4)[group],pch=c(16,16)[group],col=c("#004F71","#D0006F")[group])
     legend("right",legend=c("data","predicted"),
            pch=c(16,16),pt.cex=c(1,0.4),col=c("#004F71","#D0006F"))
  } 
  
}


