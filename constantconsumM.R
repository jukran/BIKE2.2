  # consumption is assumed constant over all actual consumption days. Chronic exposure =E(c)*consum, only uncertainty distribution of chronic exposure.
  # but uncertainty for variability of acute exposures.
  
if(input_selectdist=="Density"){ 
  
  ##Empty plot----
  {
    par(mar = c(0,0,0,0))
    plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
    text(x = 0.5, y = 0.8, paste("Density plot not available here. \n",
                                 "Please use cumulative plot instead."), 
         cex = 1.6, col = "#D0006F")
    par(mar = c(5, 4, 4, 2) + 0.1)
  } 
  
}

if(input_selectdist=="Cumulative"){
  par(mfrow=c(2,2),cex.lab=1.3,cex.main=1.3,yaxt="n")  # for plotting exposure/bw and absolute exposure, both chronic and acute
  
  # Set the constant value for consumptions, over all days:
  swconstant <- exp(logsw[,1:nd,foodindex[i]]) # per bodyweight
  sconstant <- exp(logs[,1:nd,foodindex[i]])   # absolute
  swconstant[is.na(swconstant)==TRUE]<-0  # days when not consumed in data
  sconstant[is.na(sconstant)==TRUE]<-0   # days when not consumed in data
  # consumptions assumed to be constants:
  SW <- mean(swconstant) # per bodyweight
  S <- mean(sconstant)   # absolute
  

    par(yaxt="s")
    cump <- seq(1,n_sim)
    cump <- cump/length(cump)
    if(input_selectscale=="Absolute"){   # under constant consumption S
      
      # mean exposure=  E(concentration)*serving    (serving is constant, mean exposures do not variate between individuals)
      #              =  exp(mucM+0.5*sigM^2)*S
      meanexposureposM <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                                   +mucM[,hazardindexM[h],foodindex[i]]
                                   +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                   +log(S)) )
      meanexposurebwposM <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                                     +mucM[,hazardindexM[h],foodindex[i]]
                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                     +log(SW)) )
      
      # acute exposure=  concentration*serving    (serving is constant, acute exposures variate from day to day)
      # E(acute exposure) = exp(mucM+0.5*sigM^2)*S
      acuteexposureposM <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                                    +mucM[,hazardindexM[h],foodindex[i]]
                                    +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                    +log(S)))
      acuteexposurebwposM <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                                      +mucM[,hazardindexM[h],foodindex[i]]
                                      +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                      +log(SW)))
      
      meanexposureallM <- sort(exp(logRM[foodindex[i],hazardindexM[h]]+mucM[,hazardindexM[h],foodindex[i]]
                                   +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2+log(S))*
                                 pM[,hazardindexM[h],foodindex[i]]*
                                 PM[foodindex[i],hazardindexM[h]] )
      
      acuteexposureallM <- sort(exp(logRM[foodindex[i],hazardindexM[h]]
                                    +mucM[,hazardindexM[h],foodindex[i]]
                                    +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                    +log(S))*
                                  pM[,hazardindexM[h],foodindex[i]]*
                                  PM[foodindex[i],hazardindexM[h]] )
      
      maxx1 <- quantile(qlnorm(input_lim,        
                                (logRM[foodindex[i],hazardindexM[h]]
                                +mucM[,hazardindexM[h],foodindex[i]]
                                +log(S)),
                                sigcM[,hazardindexM[h],foodindex[i]] ),0.99,names=FALSE)
      maxxbw1 <- quantile(qlnorm(input_lim,        
                               (logRM[foodindex[i],hazardindexM[h]]
                                +mucM[,hazardindexM[h],foodindex[i]]
                                +log(SW)),
                               sigcM[,hazardindexM[h],foodindex[i]] ),0.99,names=FALSE)
      
      maxx2 <- quantile(meanexposureposM,0.95)
      maxxbw2 <- quantile(meanexposurebwposM,0.95)
      
      maxx <- max(maxx1,maxx2)
      maxxbw <- max(maxxbw1,maxxbw2)
      
      
      # probability to have exposure under the limit, on positive exposure days:
      Plimitpos <- plnorm(limitexpoM[hazardindexM[h]],logRM[foodindex[i],hazardindexM[h]]
                          +mucM[,hazardindexM[h],foodindex[i]]+log(S),
                          sigcM[,hazardindexM[h],foodindex[i]])
      # probability to have exposure under the limit, on all days:
      Plimitall <- 
        (1-PM[foodindex[i],hazardindexM[h]]*pM[,hazardindexM[h],foodindex[i]])*1+
        PM[foodindex[i],hazardindexM[h]]*
        pM[,hazardindexM[h],foodindex[i]]*
        Plimitpos 
      
      # uncertainty about chronic (mean) exposure/bw:  
      plot(meanexposurebwposM[meanexposurebwposM<maxxbw],cump[meanexposurebwposM<maxxbw],col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
           xlab=paste("C.exposure/bw+  ( E(",Unit1,") per kg)"),ylab="Cumulative probability",xlim=c(0,maxxbw),ylim=c(0,1),lwd=3,type="l")
      # uncertainty about chronic (mean) exposure:  
      plot(meanexposureposM[meanexposureposM<maxx],cump[meanexposureposM<maxx],col="#F7CE3C",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
           xlab=paste("C.exposure+ ( E(",Unit1,") )"),ylab="Cumulative probability",xlim=c(0,maxx),ylim=c(0,1),lwd=3,type="l")
      lines(c(limitexpoM[hazardindexM[h]],limitexpoM[hazardindexM[h]]),c(0,1),lwd=2,col="blue")
      
      xvaluesbw <- seq(0,maxxbw,length=100)
      xvalues <- seq(0,maxx,length=100)
      uppervaluesbw <- numeric()  
      lowervaluesbw <- numeric() 
      uppervalues <- numeric()  
      lowervalues <- numeric() 
      for(xv in 1:100){
        # variation due to random concentrations in single but constant sized servings:
        # = variation of Poisson means of bacteria counts
        
        uppervaluesbw[xv] <- quantile(plnorm(xvaluesbw[xv],        
                                             (logRM[foodindex[i],hazardindexM[h]]
                                              +mucM[,hazardindexM[h],foodindex[i]]
                                              +log(SW)),
                                             sigcM[,hazardindexM[h],foodindex[i]]),input_upper,names=FALSE) 
        lowervaluesbw[xv] <- quantile(plnorm(xvaluesbw[xv],        
                                             (logRM[foodindex[i],hazardindexM[h]]
                                              +mucM[,hazardindexM[h],foodindex[i]]
                                              +log(SW)),
                                             sigcM[,hazardindexM[h],foodindex[i]]),input_lower,names=FALSE)
        uppervalues[xv] <- quantile(plnorm(xvalues[xv],        
                                           (logRM[foodindex[i],hazardindexM[h]]
                                            +mucM[,hazardindexM[h],foodindex[i]]
                                            +log(S)), 
                                           sigcM[,hazardindexM[h],foodindex[i]]),input_upper,names=FALSE) 
        lowervalues[xv] <- quantile(plnorm(xvalues[xv],        
                                           (logRM[foodindex[i],hazardindexM[h]]
                                            +mucM[,hazardindexM[h],foodindex[i]]
                                            +log(S)), 
                                           sigcM[,hazardindexM[h],foodindex[i]]),input_lower,names=FALSE)
        
      }
      # uncertainty about variability of single positive exposures/bw
      plot(0,0,xlim=c(0,maxxbw),ylim=c(0,1),pch=16,cex=0.01,xlab=paste("A.exposure/bw+ ( E(",Unit1,") per kg)"),ylab="Cumulative probability",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"))
      polygon(c(xvaluesbw,xvaluesbw[100:1]),c(uppervaluesbw,lowervaluesbw[100:1]),col="#CEB888")
      
      # plot empirically generated cumulative exposure/bw distributions
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
        sampleser <- SW #sample(servings,length(servings),replace=TRUE)
        sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
        sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
        # create 2000 simulations from each replicated data:
        sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
        sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
        lines(ecdf(sampleser*sampleconUB*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxbw),lwd=1,lty=3,col="#D0006F")
        lines(ecdf(sampleser*sampleconLB*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxbw),lwd=1,lty=3,col="#004F71")
      }
      lines(meanexposurebwposM[meanexposurebwposM<maxxbw],cump[meanexposurebwposM<maxxbw],col="#F7CE3C",lwd=3,xlim=c(0,maxxbw),ylim=c(0,1))
      lines(acuteexposurebwposM[acuteexposurebwposM<maxxbw],cump[acuteexposurebwposM<maxxbw],xlim=c(0,maxxbw),ylim=c(0,1),col="#F7CE3C",lwd=3,lty="dashed") 
      
      
      # uncertainty about variability of single positive exposures
      plot(0,0,xlim=c(0,maxx),ylim=c(0,1),pch=16,cex=0.01,xlab=paste("A.exposure+ ( E(",Unit1,") )"),ylab="Cumulative probability",main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"))
      polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
      
      # plot empirically generated cumulative exposure distributions
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
        sampleser <- S # constant consumption
        sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
        sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
        # create 2000 simulations from each replicated data:
        sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
        sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
        lines(ecdf(sampleser*sampleconUB*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxx),lwd=1,lty=3,col="#D0006F")
        lines(ecdf(sampleser*sampleconLB*RM[foodindex[i],hazardindexM[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxx),lwd=1,lty=3,col="#004F71")
      }
      lines(meanexposureposM[meanexposureposM<maxx],cump[meanexposureposM<maxx],col="#F7CE3C",lwd=3,xlim=c(0,maxx),ylim=c(0,1))
      lines(acuteexposureposM[acuteexposureposM<maxx],cump[acuteexposureposM<maxx],xlim=c(0,maxx),ylim=c(0,1),col="#F7CE3C",lwd=3,lty="dashed") 
      lines(c(limitexpoM[hazardindexM[h]],limitexpoM[hazardindexM[h]]),c(0,1),lwd=2,col="blue")
      
      # note: limitexpoM was defined as absolute exposure, not as per bodyweight
      maxAconsumlimit<-numeric()
      for(u in 1:n_sim){
        Facute <- function(logXW){
          (limitexpoM[hazardindexM[h]]/mean(Weight) - qlnorm(0.95,
                                                logRM[foodindex[i],hazardindexM[h]]
                                                +mucM[u,hazardindexM[h],foodindex[i]]+logXW,
                                                sigcM[u,hazardindexM[h],foodindex[i]]))^2
        }
        findmin <- optimize(Facute,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxAconsumlimit[u] <- exp(findmin$minimum)*mean(Weight) # solved acute consumption amount / day, for which Q95 equals the given expo limit  
      }
      
      # single solution from uncertainty distribution quantile:
      Fchronic <- function(logXW){
        (limitexpoM[hazardindexM[h]]/mean(Weight) - quantile(exp(logRM[foodindex[i],hazardindexM[h]]
                                                    +mucM[,hazardindexM[h],foodindex[i]]
                                                    +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                                    +logXW),0.95,names=FALSE)  )^2
      }
      findmin <- optimize(Fchronic,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
      maxCconsumlimit <- exp(findmin$minimum)*mean(Weight) # solved chronic consumption amount / day, for which Q95 equals the given expo limit 
      
      maxAconsumlimitall<-numeric()
      for(u in 1:n_sim){
        Facuteall <- function(logXW){
          POS <-  PM[foodindex[i],hazardindexM[h]]*pM[u,hazardindexM[h],foodindex[i]]
          if(0.95<=(1-POS)){Qtotal95 <-0}
          if(0.95>(1-POS)){  
            Qtotal95 <- qlnorm((0.95-1+POS)/POS,
                               logRM[foodindex[i],hazardindexM[h]]
                               +mucM[u,hazardindexM[h],foodindex[i]]+logXW,
                               sigcM[u,hazardindexM[h],foodindex[i]]) 
          }
          (limitexpoM[hazardindexM[h]]/mean(Weight) - Qtotal95)^2
        }
        findmin <- optimize(Facuteall,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxAconsumlimitall[u] <- exp(findmin$minimum)*mean(Weight) # solved acute consumption amount / day, for which Q95 equals the given expo limit  
      }
      
      # single solution from uncertainty distribution quantile:
      Fchronicall <- function(logXW){
        (limitexpoM[hazardindexM[h]]/mean(Weight) - quantile(exp(logRM[foodindex[i],hazardindexM[h]]
                                                    +mucM[,hazardindexM[h],foodindex[i]]
                                                    +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                                    +logXW)*PM[foodindex[i],hazardindexM[h]]*pM[,hazardindexM[h],foodindex[i]],0.95,names=FALSE ) )^2
      }
      findmin <- optimize(Fchronicall,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
      maxCconsumlimitall <- exp(findmin$minimum)*mean(Weight) # solved chronic consumption amount / day, for which Q95 equals the given expo limit 
      
    } # end of if absolute
  
    
    ## Logarithmic----     ###########################################################################
    if(input_selectscale=="Logarithmic"){     # under constant consumption S:
      
      # log( E(c)*S ) = log(exp(mucM+0.5*sigcM^2))+log(S)
      logmeanexposureposM <- sort(logRM[foodindex[i],hazardindexM[h]]
                                  +mucM[,hazardindexM[h],foodindex[i]]
                                  +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                  +log(S))
      logmeanexposurebwposM <- sort(logRM[foodindex[i],hazardindexM[h]]
                                    +mucM[,hazardindexM[h],foodindex[i]]
                                    +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                    +log(SW))
      
      # log( c*S ) ~ norm(mucM+log(S),sigcM^2)  --> E(log(c*S)) = mucM+log(S)
      logacuteexposureposM <- sort(logRM[foodindex[i],hazardindexM[h]]
                                  +mucM[,hazardindexM[h],foodindex[i]]
                                  +log(S))
      logacuteexposurebwposM <- sort(logRM[foodindex[i],hazardindexM[h]]
                                   +mucM[,hazardindexM[h],foodindex[i]]
                                   +log(SW))
      
      logmeanexposureallM <- sort(logRM[foodindex[i],hazardindexM[h]]
                                  +mucM[,hazardindexM[h],foodindex[i]]
                                  +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                  +log(S)
                                  +log(PM[foodindex[i],hazardindexM[h]])
                                  +log(pM[,hazardindexM[h],foodindex[i]]) )
      
      logacuteexposureallM <- sort(logRM[foodindex[i],hazardindexM[h]]
                                   +mucM[,hazardindexM[h],foodindex[i]]
                                   +log(S)
                                   +log(PM[foodindex[i],hazardindexM[h]])
                                   +log(pM[,hazardindexM[h],foodindex[i]]))
      
      # probability to have (acute) exposure under the limit, on positive exposure days:
      Plimitpos <- pnorm(log(limitexpoM[hazardindexM[h]]),logRM[foodindex[i],hazardindexM[h]]
                          +mucM[,hazardindexM[h],foodindex[i]]+log(S),
                          sigcM[,hazardindexM[h],foodindex[i]])
      # probability to have (acute) exposure under the limit, on all days:
      Plimitall <- 
        (1-PM[foodindex[i],hazardindexM[h]]*pM[,hazardindexM[h],foodindex[i]])*1+
        PM[foodindex[i],hazardindexM[h]]*
        pM[,hazardindexM[h],foodindex[i]]*
        Plimitpos 
      
      maxx1 <- quantile(qnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]
                             +mucM[,hazardindexM[h],foodindex[i]]
                             +log(S),
                             sigcM[,hazardindexM[h],foodindex[i]]
                             ),0.99,names=FALSE)
      maxxbw1 <- quantile(qnorm(input_lim,logRM[foodindex[i],hazardindexM[h]]
                              +mucM[,hazardindexM[h],foodindex[i]]
                              +log(SW),
                              sigcM[,hazardindexM[h],foodindex[i]]
                             ),0.99,names=FALSE)
      minn <- quantile(qnorm(0.01,logRM[foodindex[i],hazardindexM[h]]
                             +mucM[,hazardindexM[h],foodindex[i]]
                             +log(S),
                             sigcM[,hazardindexM[h],foodindex[i]]
                             ),0.05,names=FALSE)
      minnbw <- quantile(qnorm(0.01,logRM[foodindex[i],hazardindexM[h]]
                             +mucM[,hazardindexM[h],foodindex[i]]
                             +log(SW),
                             sigcM[,hazardindexM[h],foodindex[i]]
                             ),0.05,names=FALSE)
      
      maxx2 <- quantile(logmeanexposureposM,0.95)
      maxxbw2 <- quantile(logmeanexposurebwposM,0.95)
      
      maxx <- max(maxx1,maxx2)
      maxxbw <- max(maxxbw1,maxxbw2)
      
      
      # uncertainty distribution of chronic exposure/bw
      plot(logmeanexposurebwposM/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
           xlab=paste("log (C.exposure/bw+  ( E(",Unit1,") per kg))"),ylab="Cumulative probability",xlim=c(minnbw/log(10),maxxbw/log(10)),col="#F7CE3C",lwd=3,type="l")
      
      # uncertainty distribution of chronic exposure
      plot(logmeanexposureposM/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(chronic)"),
           xlab=paste("log (C.exposure+  ( E(",Unit1,") ))"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),col="#F7CE3C",lwd=3,type="l")
      lines(c(log10(limitexpoM[hazardindexM[h]]),log10(limitexpoM[hazardindexM[h]])),c(0,1),lwd=2,col="blue")
      
      # uncertainty about variability of acute exposure/bw
      xvaluesbw <- seq(minnbw,maxxbw,length=100)
      xvalues <- seq(minn,maxx,length=100)
      
      uppervaluesbw <- numeric()
      lowervaluesbw <- numeric()
      uppervalues <- numeric()
      lowervalues <- numeric()
      
      for(xv in 1:100){ 
        uppervaluesbw[xv] <- quantile(pnorm(xvaluesbw[xv],
                                          (logRM[foodindex[i],hazardindexM[h]]+
                                             mucM[,hazardindexM[h],foodindex[i]]
                                           +log(SW)
                                          ), 
                                          sigcM[,hazardindexM[h],foodindex[i]] 
                                          ),input_upper,names=FALSE) 
        lowervaluesbw[xv] <- quantile(pnorm(xvaluesbw[xv],
                                          (logRM[foodindex[i],hazardindexM[h]]+
                                             mucM[,hazardindexM[h],foodindex[i]]
                                           +log(SW)
                                          ), 
                                          sigcM[,hazardindexM[h],foodindex[i]] 
                                          ),input_lower,names=FALSE) 
        uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                          (logRM[foodindex[i],hazardindexM[h]]+
                                             mucM[,hazardindexM[h],foodindex[i]]
                                           +log(S)
                                          ), 
                                          sigcM[,hazardindexM[h],foodindex[i]] 
                                          ),input_upper,names=FALSE) 
        lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                          (logRM[foodindex[i],hazardindexM[h]]+
                                             mucM[,hazardindexM[h],foodindex[i]]
                                           +log(S)
                                          ),  
                                          sigcM[,hazardindexM[h],foodindex[i]] 
                                          ),input_lower,names=FALSE) 
      }
      
      # uncertainty about variability of acute positive exposures/bw
      plot(logacuteexposurebwposM/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),
           xlab=paste("log (A.exposure/bw+  ( E(",Unit1,") per kg))"),ylab="Cumulative probability",xlim=c(minnbw/log(10),maxxbw/log(10)),col="#F7CE3C",lwd=3,type="l") 
      
      polygon(c(xvaluesbw/log(10),xvaluesbw[100:1]/log(10)),c(uppervaluesbw,lowervaluesbw[100:1] ),col="#CEB888")
      
      # plot empirically generated cumulative exposure/bw distributions
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
        sampleser <- SW # constant consumption
        sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
        sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
        # create 2000 simulations from each replicated data:
        sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
        sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
        lines(ecdf(log(sampleser*sampleconUB*RM[foodindex[i],hazardindexM[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minnbw/log(10),maxxbw/log(10)),lwd=1,lty=3,col="#D0006F")
        lines(ecdf(log(sampleser*sampleconLB*RM[foodindex[i],hazardindexM[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minnbw/log(10),maxxbw/log(10)),lwd=1,lty=3,col="#004F71")
      }
      # uncertainty for mean log-chronic exposure  E(log E(e^+)) 
      lines(logmeanexposurebwposM/log(10),cump,lwd=3,col="#F7CE3C",lty="dashed")
      # uncertainty for mean log-acute exposure  E(log e^+)
      lines(logacuteexposurebwposM/log(10),cump,col="#F7CE3C",lwd=3) 
      
      
      # uncertainty about variability of acute positive exposures
      plot(logacuteexposureposM/log(10),cump,main=paste(hazardnamesusedM[h],"from",foodnamesused[i],"(acute)"),xlab=paste("log (A.exposure+  ( E(",Unit1,") ))"),ylab="",xlim=c(minn/log(10),maxx/log(10)),type="l",col="#F7CE3C",lwd=3) 
      polygon(c(xvalues/log(10),xvalues[100:1]/log(10)),c(uppervalues,lowervalues[100:1] ),col="#CEB888")
      
      
      # plot empirically generated cumulative exposure distributions
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
        sampleser <- S # constant consumption
        sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
        sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
        # create 2000 simulations from each replicated data:
        sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
        sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
        lines(ecdf(log(sampleser*sampleconUB*RM[foodindex[i],hazardindexM[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minn/log(10),maxx/log(10)),lwd=1,lty=3,col="#D0006F")
        lines(ecdf(log(sampleser*sampleconLB*RM[foodindex[i],hazardindexM[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minn/log(10),maxx/log(10)),lwd=1,lty=3,col="#004F71")
      }
      # uncertainty for mean log-chronic exposure  E(log E(e^+)) 
      lines(logmeanexposureposM/log(10),cump,lwd=3,col="#F7CE3C",lty="dashed")
      # uncertainty for mean log-acute exposure  E(log e^+)
      lines(logacuteexposureposM/log(10),cump,col="#F7CE3C",lwd=3) 
      lines(c(log10(limitexpoM[hazardindexM[h]]),log10(limitexpoM[hazardindexM[h]])),c(0,1),lwd=2,col="blue")
      
      # note: limitexpoM was defined as absolute exposure, not as per bodyweight
      maxlogAconsumlimit<-numeric()
      for(u in 1:n_sim){
        Facute <- function(logXW){
          (log(limitexpoM[hazardindexM[h]]/mean(Weight)) - qnorm(0.95,
                                                    logRM[foodindex[i],hazardindexM[h]]
                                                    +mucM[u,hazardindexM[h],foodindex[i]]+logXW,
                                                    sigcM[u,hazardindexM[h],foodindex[i]]))^2
        }
        findmin <- optimize(Facute,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxlogAconsumlimit[u] <- (findmin$minimum+log(mean(Weight)))/log(10) # solved acute consumption amount / day, for which Q95 equals the given expo limit  
      }
      
      maxlogAconsumlimitall<-numeric()
      for(u in 1:n_sim){
        Facuteall <- function(logXW){
          POS <-  PM[foodindex[i],hazardindexM[h]]*pM[u,hazardindexM[h],foodindex[i]]
          if(0.95<=(1-POS)){Qtotal95 <- log(0.0001)}
          if(0.95>(1-POS)){  
            Qtotal95 <- qnorm((0.95-1+POS)/POS,
                              logRM[foodindex[i],hazardindexM[h]]
                              +mucM[u,hazardindexM[h],foodindex[i]]+logXW,
                              sigcM[u,hazardindexM[h],foodindex[i]]) 
          }
          (log(limitexpoM[hazardindexM[h]]/mean(Weight)) - Qtotal95)^2
        }
        findmin <- optimize(Facuteall,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxlogAconsumlimitall[u] <- (findmin$minimum+log(mean(Weight)))/log(10) # solved acute consumption amount / day, for which Q95 equals the given expo limit  
      }
      
      # single solution from uncertainty distribution quantile:
      Fchronic <- function(logXW){
        (log(limitexpoM[hazardindexM[h]]/mean(Weight)) - quantile(logRM[foodindex[i],hazardindexM[h]] 
                                                     +mucM[,hazardindexM[h],foodindex[i]]
                                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                                     +logXW,0.95,names=FALSE) )^2
      }
      findmin <- optimize(Fchronic,interval=c(-8,8))
      maxlogCconsumlimit <- (findmin$minimum+log(mean(Weight)))/log(10)
      
      # single solution from uncertainty distribution quantile:
      Fchronicall <- function(logXW){
        (log(limitexpoM[hazardindexM[h]]/mean(Weight)) - quantile(logRM[foodindex[i],hazardindexM[h]] 
                                                     +mucM[,hazardindexM[h],foodindex[i]]
                                                     +0.5*sigcM[,hazardindexM[h],foodindex[i]]^2
                                                     +logXW+log(PM[foodindex[i],hazardindexM[h]]*pM[u,hazardindexM[h],foodindex[i]]),0.95,names=FALSE))^2
      }
      findmin <- optimize(Fchronicall,interval=c(-8,8))
      maxlogCconsumlimitall <- (findmin$minimum+log(mean(Weight)))/log(10)
      
    } # end of if logarithmic 
    
  
  
  if(input_selectscale=="Absolute"){
    mtext(paste("P(A.exposure+ <",limitexpoM[hazardindexM[h]],")=",round(quantile(Plimitpos,0.025),2),"/",round(median(Plimitpos),2),"/",round(quantile(Plimitpos,0.975),2)," (Q2.5%/Q50%/Q97.5%)"),
          side = 1, adj = 0,line=0, cex = 1,
          outer = TRUE) 
    mtext(paste("P(A.exposure <",limitexpoM[hazardindexM[h]],")=",round(quantile(Plimitall,0.025),2),"/",round(median(Plimitall),2),"/",round(quantile(Plimitall,0.975),2)," (Q2.5%/Q50%/Q97.5%)"),
          side = 1, adj = 0,line=1, cex = 1,
          outer = TRUE)
    mtext(paste("P(C.exposure+ <",limitexpoM[hazardindexM[h]],")=",round(mean(meanexposureposM<limitexpoM[hazardindexM[h]]),2),". P(C.exposure <",limitexpoM[hazardindexM[h]],")=",round(mean(meanexposureallM<limitexpoM[hazardindexM[h]]),2)),
          side = 1, adj = 0,line=2, cex = 1,
          outer = TRUE)
    mtext(paste("Safe95% Max A.consum/day+ =",round(quantile(maxAconsumlimit,0.025),2),"/",round(median(maxAconsumlimit),2),"/",round(quantile(maxAconsumlimit,0.975),2),". Safe95% Max C.consum/day+ =",round(maxCconsumlimit,2)),
          side = 1, adj = 0, line=3, cex = 1, outer=TRUE)
    mtext(paste("Safe95% Max A.consum/day =",round(quantile(maxAconsumlimitall,0.025),2),"/",round(median(maxAconsumlimitall),2),"/",round(quantile(maxAconsumlimitall,0.975),2),". Safe95% Max C.consum/day =",round(maxCconsumlimitall,2)),
          side = 1, adj = 0, line=4, cex = 1, outer=TRUE)
  }
  if(input_selectscale=="Logarithmic"){ 
    mtext(paste("P(log(A.exposure+) <",round(log10(limitexpoM[hazardindexM[h]]),2),")=",round(quantile(Plimitpos,0.025),2),"/",round(median(Plimitpos),2),"/",round(quantile(Plimitpos,0.975),2)," (Q2.5%/Q50%/Q97.5%)"),
          side = 1, adj = 0,line=0, cex = 1,
          outer = TRUE) 
    mtext(paste("P(log(A.exposure) <",round(log10(limitexpoM[hazardindexM[h]]),2),")=",round(quantile(Plimitall,0.025),2),"/",round(median(Plimitall),2),"/",round(quantile(Plimitall,0.975),2)," (Q2.5%/Q50%/Q97.5%)"),
          side = 1, adj = 0,line=1, cex = 1,
          outer = TRUE) 
    mtext(paste("P(log(C.exposure+) <",round(log10(limitexpoM[hazardindexM[h]]),2),")=",round(mean(logmeanexposureposM/log(10)<log10(limitexpoM[hazardindexM[h]])),2),". P(log(C.exposure) <",round(log10(limitexpoM[hazardindexM[h]]),2),")=",round(mean(logmeanexposureallM/log(10)<log10(limitexpoM[hazardindexM[h]])),2)),
          side = 1, adj = 0,line=2, cex = 1,
          outer = TRUE)
    mtext(paste("Safe95% Max logA.consum/day+ =",round(quantile(maxlogAconsumlimit,0.025),2),"/",round(median(maxlogAconsumlimit),2),"/",round(quantile(maxlogAconsumlimit,0.975),2),". Safe95% Max logC.consum/day+ =",round(maxlogCconsumlimit,2)),
          side = 1, adj = 0, line=3, cex = 1, outer=TRUE)
    mtext(paste("Safe95% Max logA.consum/day =",round(quantile(maxlogAconsumlimitall,0.025),2),"/",round(median(maxlogAconsumlimitall),2),"/",round(quantile(maxlogAconsumlimitall,0.975),2),". Safe95% Max logC.consum/day =",round(maxlogCconsumlimitall,2)),
          side = 1, adj = 0, line=4, cex = 1, outer=TRUE)
  }
  
} # end of if cumulative