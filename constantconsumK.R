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
  
  # mean exposure=  E(concentration)*serving    (serving is constant, mean exposures do not variate between individuals)
  #              =  exp(mucK+0.5*sigK^2)*SW
  meanexposurebwposK <- sort(exp(logRK[foodindex[i],hazardindexK[h]]
                                 +mucK[,hazardindexK[h],foodindex[i]]
                                 +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                 +log(SW)) )
  meanexposureposK <- sort(exp(logRK[foodindex[i],hazardindexK[h]]
                               +mucK[,hazardindexK[h],foodindex[i]]
                               +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                               +log(S)) )
  # acute exposure=  concentration*serving    (serving is constant, acute exposures variate from day to day)
  # E(acute exposure) = exp(mucK+0.5*sigK^2)*SW
  acuteexposurebwposK <- sort(exp(logRK[foodindex[i],hazardindexK[h]]
                                  +mucK[,hazardindexK[h],foodindex[i]]
                                  +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                  +log(SW)))
  acuteexposureposK <- sort(exp(logRK[foodindex[i],hazardindexK[h]]
                                +mucK[,hazardindexK[h],foodindex[i]]
                                +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                +log(S)))
  
  meanexposurebwallK <- sort(exp(logRK[foodindex[i],hazardindexK[h]]+mucK[,hazardindexK[h],foodindex[i]]
                                 +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2+log(SW))*
                               pK[,hazardindexK[h],foodindex[i]]*
                               PK[foodindex[i],hazardindexK[h]] )
  meanexposureallK <- sort(exp(logRK[foodindex[i],hazardindexK[h]]+mucK[,hazardindexK[h],foodindex[i]]
                               +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2+log(S))*
                             pK[,hazardindexK[h],foodindex[i]]*
                             PK[foodindex[i],hazardindexK[h]] )
  
  acuteexposurebwallK <- sort(exp(logRK[foodindex[i],hazardindexK[h]]
                                  +mucK[,hazardindexK[h],foodindex[i]]
                                  +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                  +log(SW))*
                                pK[,hazardindexK[h],foodindex[i]]*
                                PK[foodindex[i],hazardindexK[h]]  )
  acuteexposureallK <- sort(exp(logRK[foodindex[i],hazardindexK[h]]
                                +mucK[,hazardindexK[h],foodindex[i]]
                                +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                +log(S))*
                              pK[,hazardindexK[h],foodindex[i]]*
                              PK[foodindex[i],hazardindexK[h]] )
  
  
    par(yaxt="s")
    cump <- seq(1,n_sim)
    cump <- cump/length(cump)
    if(input_selectscale=="Absolute"){  # under constant consumption 
      
      # probability to have (acute) exposure under the limit, on positive exposure days:
      Plimitpos <- plnorm(limitexpoK[hazardindexK[h]],logRK[foodindex[i],hazardindexK[h]]
                          +mucK[,hazardindexK[h],foodindex[i]]+log(SW),
                          sigcK[,hazardindexK[h],foodindex[i]])
      # probability to have (acute) exposure under the limit, on all days:
      Plimitall <- 
        (1-PK[foodindex[i],hazardindexK[h]]*pK[,hazardindexK[h],foodindex[i]])*1+
        PK[foodindex[i],hazardindexK[h]]*
        pK[,hazardindexK[h],foodindex[i]]*
        Plimitpos
      
      maxxbw1 <- quantile(qlnorm(input_lim,        
                          logRK[foodindex[i],hazardindexK[h]]
                          +mucK[,hazardindexK[h],foodindex[i]]
                          +log(SW),
                          sigcK[,hazardindexK[h],foodindex[i]]),0.99,names=FALSE )
      maxx1 <- quantile(qlnorm(input_lim,        
                                logRK[foodindex[i],hazardindexK[h]]
                                +mucK[,hazardindexK[h],foodindex[i]]
                                +log(S),
                                sigcK[,hazardindexK[h],foodindex[i]]),0.99,names=FALSE )
      maxxbw2 <- quantile(meanexposurebwposK,0.95)
      maxx2 <- quantile(meanexposureposK,0.95)
      maxxbw <- max(maxxbw1,maxxbw2)
      maxx <- max(maxx1,maxx2)
      
      # uncertainty about chronic (mean) exposure/bw:  
      plot(meanexposurebwposK[meanexposurebwposK<maxxbw],cump[meanexposurebwposK<maxxbw],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
           xlab=paste("C.exposure/bw+  (", Unit1,"per kg)"),ylab="Cumulative probability",xlim=c(0,maxxbw),ylim=c(0,1),lwd=3,type="l")
      lines(c(limitexpoK[hazardindexK[h]],limitexpoK[hazardindexK[h]]),c(0,1),lwd=2,col="blue")
      
      # uncertainty about chronic (mean) exposure:  
      plot(meanexposureposK[meanexposureposK<maxx],cump[meanexposureposK<maxx],col="#F7CE3C",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
           xlab=paste("C.exposure+  (", Unit1,")"),ylab="Cumulative probability",xlim=c(0,maxx),ylim=c(0,1),lwd=3,type="l")
      
      xvaluesbw <- seq(0,maxxbw,length=100)
      xvalues <- seq(0,maxx,length=100)
      uppervaluesbw <- numeric()  
      lowervaluesbw <- numeric() 
      uppervalues <- numeric()  
      lowervalues <- numeric() 
      for(xv in 1:100){
        # variation due to random concentrations in single but constant sized servings:
        
        uppervaluesbw[xv] <- quantile(plnorm(xvaluesbw[xv],        
                                             logRK[foodindex[i],hazardindexK[h]]
                                             +mucK[,hazardindexK[h],foodindex[i]]
                                             +log(SW),
                                             sigcK[,hazardindexK[h],foodindex[i]]),input_upper,names=FALSE) 
        lowervaluesbw[xv] <- quantile(plnorm(xvaluesbw[xv],        
                                             logRK[foodindex[i],hazardindexK[h]]
                                             +mucK[,hazardindexK[h],foodindex[i]]
                                             +log(SW),
                                             sigcK[,hazardindexK[h],foodindex[i]]),input_lower,names=FALSE)
        uppervalues[xv] <- quantile(plnorm(xvalues[xv],        
                                           logRK[foodindex[i],hazardindexK[h]]
                                           +mucK[,hazardindexK[h],foodindex[i]]
                                           +log(S),
                                           sigcK[,hazardindexK[h],foodindex[i]]),input_upper,names=FALSE) 
        lowervalues[xv] <- quantile(plnorm(xvalues[xv],        
                                           logRK[foodindex[i],hazardindexK[h]]
                                           +mucK[,hazardindexK[h],foodindex[i]]
                                           +log(S),
                                           sigcK[,hazardindexK[h],foodindex[i]]),input_lower,names=FALSE)
        
      }
      # uncertainty about variability of single positive exposures /bw
      plot(0,0,xlim=c(0,maxxbw),ylim=c(0,1),pch=16,cex=0.01,xlab=paste("A.exposure/bw+  (", Unit1,"per kg)"),ylab="Cumulative probability",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(acute)"))
      polygon(c(xvaluesbw,xvaluesbw[100:1]),c(uppervaluesbw,lowervaluesbw[100:1]),col="#CEB888")
      
      
      # plot empirically generated cumulative exposure/bw distributions
      # collect exact measurements & 
      # and as upper bounds those between LOD-LOQ & <LOD 
      concentrationsUB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                logLOQK[hazardindexK[h],foodindex[i],],
                                logLODK[hazardindexK[h],foodindex[i],]))
      # and using lower bounds
      concentrationsLB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                logLOQLimK[hazardindexK[h],foodindex[i],],
                                logLODLimK[hazardindexK[h],foodindex[i],]-20))
      concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
      concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
      
      for(resample in 1:40){
        # create 40 replicate ('bootstrap') data with original nsample:   
        sampleser <- SW   # constant consumption
        sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
        sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
        # create 2000 simulations from each replicated data:
        sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
        sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
        lines(ecdf(sampleser*sampleconUB*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxbw),lwd=1,lty=3,col="#D0006F")
        lines(ecdf(sampleser*sampleconLB*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxxbw),lwd=1,lty=3,col="#004F71")
      }
      lines(meanexposurebwposK[meanexposurebwposK<maxxbw],cump[meanexposurebwposK<maxxbw],col="#F7CE3C",lwd=3,xlim=c(0,maxxbw),ylim=c(0,1))
      lines(acuteexposurebwposK[acuteexposurebwposK<maxxbw],cump[acuteexposurebwposK<maxxbw],xlim=c(0,maxxbw),ylim=c(0,1),col="#F7CE3C",lwd=3,lty="dashed") 
      lines(c(limitexpoK[hazardindexK[h]],limitexpoK[hazardindexK[h]]),c(0,1),lwd=2,col="blue")
      
      # uncertainty about variability of single positive exposures
      plot(0,0,xlim=c(0,maxx),ylim=c(0,1),pch=16,cex=0.01,xlab=paste("A.exposure+  (", Unit1,")"),ylab="Cumulative probability",main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(acute)"))
      polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")
      
      # plot empirically generated cumulative exposure distributions
      # collect exact measurements & 
      # and as upper bounds those between LOD-LOQ & <LOD 
      concentrationsUB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                logLOQK[hazardindexK[h],foodindex[i],],
                                logLODK[hazardindexK[h],foodindex[i],]))
      # and using lower bounds
      concentrationsLB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                logLOQLimK[hazardindexK[h],foodindex[i],],
                                logLODLimK[hazardindexK[h],foodindex[i],]-20))
      concentrationsUB <- concentrationsUB[!is.na(concentrationsUB)]
      concentrationsLB <- concentrationsLB[!is.na(concentrationsLB)]
      
      for(resample in 1:40){
        # create 40 replicate ('bootstrap') data with original nsample:   
        sampleser <- S  # sample(servings,length(servings),replace=TRUE)
        sampleconUB <- sample(concentrationsUB,length(concentrationsUB),replace=TRUE)
        sampleconLB <- sample(concentrationsLB,length(concentrationsLB),replace=TRUE)
        # create 2000 simulations from each replicated data:
        sampleconUB <- sample(sampleconUB,2000,replace=TRUE)
        sampleconLB <- sample(sampleconLB,2000,replace=TRUE)
        lines(ecdf(sampleser*sampleconUB*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxx),lwd=1,lty=3,col="#D0006F")
        lines(ecdf(sampleser*sampleconLB*RK[foodindex[i],hazardindexK[h]]),verticals=TRUE,do.points=FALSE,xlim=c(0,maxx),lwd=1,lty=3,col="#004F71")
      }
      lines(meanexposureposK[meanexposureposK<maxx],cump[meanexposureposK<maxx],col="#F7CE3C",lwd=3,xlim=c(0,maxx),ylim=c(0,1))
      lines(acuteexposureposK[acuteexposureposK<maxx],cump[acuteexposureposK<maxx],xlim=c(0,maxx),ylim=c(0,1),col="#F7CE3C",lwd=3,lty="dashed") 
      
      maxAconsumlimit<-numeric()
      for(u in 1:n_sim){
        Facute <- function(logXW){
          (limitexpoK[hazardindexK[h]] - qlnorm(0.95,
                                               logRK[foodindex[i],hazardindexK[h]]
                                               +mucK[u,hazardindexK[h],foodindex[i]]+logXW,
                                               sigcK[u,hazardindexK[h],foodindex[i]]))^2
        }
        findmin <- optimize(Facute,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxAconsumlimit[u] <- exp(findmin$minimum)*mean(Weight) # solved acute consumption amount / day, for which Q95 equals the given expo limit  
      }
      
      # single solution from uncertainty distribution quantile:
        Fchronic <- function(logXW){
          (limitexpoK[hazardindexK[h]] - quantile(exp(logRK[foodindex[i],hazardindexK[h]]
                                            +mucK[,hazardindexK[h],foodindex[i]]
                                            +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                            +logXW ),0.95,names=FALSE)  )^2
        }
        findmin <- optimize(Fchronic,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxCconsumlimit <- exp(findmin$minimum)*mean(Weight) # solved chronic consumption amount / day, for which Q95 equals the given expo limit 
         
      maxAconsumlimitall<-numeric()
      for(u in 1:n_sim){
        Facuteall <- function(logXW){
          POS <-  PK[foodindex[i],hazardindexK[h]]*pK[u,hazardindexK[h],foodindex[i]]
          if(0.95<=(1-POS)){Qtotal95 <-0}
          if(0.95>(1-POS)){  
            Qtotal95 <- qlnorm((0.95-1+POS)/POS,
                               logRK[foodindex[i],hazardindexK[h]]
                               +mucK[u,hazardindexK[h],foodindex[i]]+logXW,
                               sigcK[u,hazardindexK[h],foodindex[i]]) 
          }
          (limitexpoK[hazardindexK[h]] - Qtotal95)^2
        }
        findmin <- optimize(Facuteall,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxAconsumlimitall[u] <- exp(findmin$minimum)*mean(Weight) # solved acute consumption amount / day, for which Q95 equals the given expo limit  
      }
      
      # single solution from uncertainty distribution quantile:
        Fchronicall <- function(logXW){
          (limitexpoK[hazardindexK[h]] - quantile(exp(logRK[foodindex[i],hazardindexK[h]]
                                            +mucK[,hazardindexK[h],foodindex[i]]
                                            +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                            +logXW )*PK[foodindex[i],hazardindexK[h]]*pK[,hazardindexK[h],foodindex[i]],0.95,names=FALSE ) )^2
        }
        findmin <- optimize(Fchronicall,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxCconsumlimitall <- exp(findmin$minimum)*mean(Weight) # solved chronic consumption amount / day, for which Q95 equals the given expo limit 
      
    }  # end of if absolute
    
    
    ## Logarithmic----   ################################################################
    if(input_selectscale=="Logarithmic"){   # under constant consumption SW (per bodyweight):
  
      # log( E(c)*SW ) = log(exp(mucK+0.5*sigcK^2))+log(SW)    
      logmeanexposurebwposK <- sort(logRK[foodindex[i],hazardindexK[h]]
                                    +mucK[,hazardindexK[h],foodindex[i]]
                                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                    +log(SW))
      logmeanexposureposK <- sort(logRK[foodindex[i],hazardindexK[h]]
                                    +mucK[,hazardindexK[h],foodindex[i]]
                                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                    +log(S))
      
      # log( c*SW ) ~ norm(mucK+log(SW),sigcK^2)  --> E(log(c*SW)) = mucK+log(SW)
      logacuteexposurebwposK <- sort(logRK[foodindex[i],hazardindexK[h]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +log(SW))
      logacuteexposureposK <- sort(logRK[foodindex[i],hazardindexK[h]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +log(S))
      
      logmeanexposurebwallK <- sort(logRK[foodindex[i],hazardindexK[h]]
                                    +mucK[,hazardindexK[h],foodindex[i]]
                                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                    +log(SW)
                                    +log(PK[foodindex[i],hazardindexK[h]])
                                    +log(pK[,hazardindexK[h],foodindex[i]]) )
      
      logacuteexposurebwallK <- sort(logRK[foodindex[i],hazardindexK[h]]
                                     +mucK[,hazardindexK[h],foodindex[i]]
                                     +log(SW)
                                     +log(PK[foodindex[i],hazardindexK[h]])
                                     +log(pK[,hazardindexK[h],foodindex[i]]))
      
      # probability to have (acute) exposure under the limit, on positive exposure days:
      Plimitpos <- pnorm(log(limitexpoK[hazardindexK[h]]),logRK[foodindex[i],hazardindexK[h]]
                         +mucK[,hazardindexK[h],foodindex[i]]+log(SW),
                         sigcK[,hazardindexK[h],foodindex[i]])  
      # probability to have (acute) exposure under the limit, on all days:
      Plimitall <- 
        (1-PK[foodindex[i],hazardindexK[h]]*pK[,hazardindexK[h],foodindex[i]])*1+
        PK[foodindex[i],hazardindexK[h]]*
        pK[,hazardindexK[h],foodindex[i]]*
        Plimitpos 
      
      
      maxxbw1 <- quantile(qnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                               +mucK[,hazardindexK[h],foodindex[i]]
                               +log(SW),
                               sigcK[,hazardindexK[h],foodindex[i]]
                               ),0.99,names=FALSE)
      maxx1 <- quantile(qnorm(input_lim,logRK[foodindex[i],hazardindexK[h]]
                                +mucK[,hazardindexK[h],foodindex[i]]
                                +log(S),
                                sigcK[,hazardindexK[h],foodindex[i]]
                               ),0.99,names=FALSE)
      minnbw <- quantile(qnorm(0.01,logRK[foodindex[i],hazardindexK[h]]
                               +mucK[,hazardindexK[h],foodindex[i]]
                               +log(SW),
                               sigcK[,hazardindexK[h],foodindex[i]]
                               ),0.05,names=FALSE) 
      minn <- quantile(qnorm(0.01,logRK[foodindex[i],hazardindexK[h]]
                               +mucK[,hazardindexK[h],foodindex[i]]
                               +log(S),
                               sigcK[,hazardindexK[h],foodindex[i]]
                               ),0.05,names=FALSE) 
      
      maxxbw2 <- quantile(logmeanexposurebwposK,0.95)
      maxx2 <- quantile(logmeanexposureposK,0.95)
      maxxbw <- max(maxxbw1,maxxbw2) 
      maxx <- max(maxx1,maxx2) 
      
      # uncertainty distribution of chronic exposure/bw
      plot(logmeanexposurebwposK/log(10),cump,main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
           xlab=paste("log (C.exposure/bw+  (", Unit1,"per kg))"),ylab="Cumulative probability",xlim=c(minnbw/log(10),maxxbw/log(10)),col="#F7CE3C",lwd=3,type="l")
      lines(c(log10(limitexpoK[hazardindexK[h]]),log10(limitexpoK[hazardindexK[h]])),c(0,1),lwd=2,col="blue")
      
      # uncertainty distribution of chronic exposure
      plot(logmeanexposureposK/log(10),cump,main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(chronic)"),
           xlab=paste("log (C.exposure+  (", Unit1,"))"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),lwd=3,col="#F7CE3C",type="l")
      
      # uncertainty about variability of acute exposure/bw
      xvaluesbw <- seq(minnbw/log(10),maxxbw/log(10),length=100)
      xvalues <- seq(minn/log(10),maxx/log(10),length=100)
      
      uppervaluesbw <- numeric()
      lowervaluesbw <- numeric()
      uppervalues <- numeric()
      lowervalues <- numeric()
      
      for(xv in 1:100){
        uppervaluesbw[xv] <- quantile(pnorm(xvaluesbw[xv],
                                            (logRK[foodindex[i],hazardindexK[h]]+
                                               mucK[,hazardindexK[h],foodindex[i]]
                                             +log(SW) 
                                            )/log(10),
                                            sigcK[,hazardindexK[h],foodindex[i]]/log(10)
                                            ),input_upper,names=FALSE) 
        lowervaluesbw[xv] <- quantile(pnorm(xvaluesbw[xv],
                                            (logRK[foodindex[i],hazardindexK[h]]+
                                               mucK[,hazardindexK[h],foodindex[i]]
                                             +log(SW) 
                                            )/log(10),
                                            sigcK[,hazardindexK[h],foodindex[i]]/log(10)
                                            ),input_lower,names=FALSE)
        uppervalues[xv] <- quantile(pnorm(xvalues[xv],
                                          (logRK[foodindex[i],hazardindexK[h]]+
                                             mucK[,hazardindexK[h],foodindex[i]]
                                           +log(S) 
                                          )/log(10),
                                          sigcK[,hazardindexK[h],foodindex[i]]/log(10)
                                          ),input_upper,names=FALSE) 
        lowervalues[xv] <- quantile(pnorm(xvalues[xv],
                                          (logRK[foodindex[i],hazardindexK[h]]+
                                             mucK[,hazardindexK[h],foodindex[i]]
                                           +log(S) 
                                          )/log(10),
                                          sigcK[,hazardindexK[h],foodindex[i]]/log(10)
                                          ),input_lower,names=FALSE)
        
      }
      # uncertainty about variability of single positive exposures/bw
      plot(logacuteexposurebwposK/log(10),cump,main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(acute)"),
           xlab=paste("log (A.exposure/bw+  (", Unit1,"per kg))"),ylab="Cumulative probability",xlim=c(minnbw/log(10),maxxbw/log(10)),col="#F7CE3C",lwd=3,type="l")
      polygon(c(xvaluesbw,xvaluesbw[100:1]),c(uppervaluesbw,lowervaluesbw[100:1]),col="#CEB888")
      
      # plot empirically generated cumulative exposure/bw distributions
      # collect exact measurements & 
      # and as upper bounds those between LOD-LOQ & <LOD 
      concentrationsUB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                logLOQK[hazardindexK[h],foodindex[i],],
                                logLODK[hazardindexK[h],foodindex[i],]))
      # and using lower bounds
      concentrationsLB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                logLOQLimK[hazardindexK[h],foodindex[i],],
                                logLODLimK[hazardindexK[h],foodindex[i],]-20))
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
        lines(ecdf(log(sampleser*sampleconUB*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minnbw/log(10),maxxbw/log(10)),lwd=1,lty=3,col="#D0006F")
        lines(ecdf(log(sampleser*sampleconLB*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minnbw/log(10),maxxbw/log(10)),lwd=1,lty=3,col="#004F71")
      }
      
      # uncertainty for mean log-chronic exposure  E(log E(e^+)) 
      lines(logmeanexposurebwposK/log(10),cump,lwd=3,col="#F7CE3C",lty="dashed")
      # uncertainty for mean log-acute exposure  E(log e^+)
      lines(logacuteexposurebwposK/log(10),cump,col="#F7CE3C",lwd=3) 
      lines(c(log10(limitexpoK[hazardindexK[h]]),log10(limitexpoK[hazardindexK[h]])),c(0,1),lwd=2,col="blue")      
      
      # uncertainty about variability of acute exposure distributions
      plot(logacuteexposureposK/log(10),cump,main=paste(hazardnamesusedK[h],"from",foodnamesused[i],"(acute)"),
           xlab=paste("log (A.exposure+  (", Unit1,"))"),ylab="Cumulative probability",xlim=c(minn/log(10),maxx/log(10)),col="#F7CE3C",lwd=3,type="l")
      polygon(c(xvalues,xvalues[100:1]),c(uppervalues,lowervalues[100:1]),col="#CEB888")

      
      # plot empirically generated cumulative exposure distributions
      # collect exact measurements & 
      # and as upper bounds those between LOD-LOQ & <LOD 
      concentrationsUB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                logLOQK[hazardindexK[h],foodindex[i],],
                                logLODK[hazardindexK[h],foodindex[i],]))
      # and using lower bounds
      concentrationsLB <- exp(c(logcK[hazardindexK[h],foodindex[i],],
                                logLOQLimK[hazardindexK[h],foodindex[i],],
                                logLODLimK[hazardindexK[h],foodindex[i],]-20))
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
        lines(ecdf(log(sampleser*sampleconUB*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minn/log(10),maxx/log(10)),lwd=1,lty=3,col="#D0006F")
        lines(ecdf(log(sampleser*sampleconLB*RK[foodindex[i],hazardindexK[h]])/log(10)),verticals=TRUE,do.points=FALSE,xlim=c(minn/log(10),maxx/log(10)),lwd=1,lty=3,col="#004F71")
      }
      # uncertainty for mean log-chronic exposure  E(log E(e^+)) 
      lines(logmeanexposureposK/log(10),cump,lwd=3,col="#F7CE3C",lty="dashed")
      # uncertainty for mean log-acute exposure  E(log e^+)
      lines(logacuteexposureposK/log(10),cump,col="#F7CE3C",lwd=3) 
      
      maxlogAconsumlimit<-numeric()
      for(u in 1:n_sim){
        Facute <- function(logXW){
          (log(limitexpoK[hazardindexK[h]]) - qnorm(0.95,
                                                   logRK[foodindex[i],hazardindexK[h]]
                                                   +mucK[u,hazardindexK[h],foodindex[i]]+logXW,
                                                   sigcK[u,hazardindexK[h],foodindex[i]]))^2
        }
        findmin <- optimize(Facute,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxlogAconsumlimit[u] <- (findmin$minimum+log(mean(Weight)))/log(10) # solved acute consumption amount / day, for which Q95 equals the given expo limit  
      }
      
      maxlogAconsumlimitall<-numeric()
      for(u in 1:n_sim){
        Facuteall <- function(logXW){
          POS <-  PK[foodindex[i],hazardindexK[h]]*pK[u,hazardindexK[h],foodindex[i]]
          if(0.95<=(1-POS)){Qtotal95 <- log(0.0001)}
          if(0.95>(1-POS)){  
            Qtotal95 <- qnorm((0.95-1+POS)/POS,
                              logRK[foodindex[i],hazardindexK[h]]
                              +mucK[u,hazardindexK[h],foodindex[i]]+logXW,
                              sigcK[u,hazardindexK[h],foodindex[i]]) 
          }
          (log(limitexpoK[hazardindexK[h]]) - Qtotal95)^2
        }
        findmin <- optimize(Facuteall,interval=c(-8,8))   # find the log-consumption at which P(exposure+ < limit) = 0.95 
        maxlogAconsumlimitall[u] <- (findmin$minimum+log(mean(Weight)))/log(10) # solved acute consumption amount / day, for which Q95 equals the given expo limit  
      }
      
      # single solution from uncertainty distribution quantile:
      Fchronic <- function(logXW){
        (log(limitexpoK[hazardindexK[h]]) - quantile(logRK[foodindex[i],hazardindexK[h]] 
                                                    +mucK[,hazardindexK[h],foodindex[i]]
                                                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                                    +logXW,0.95,names=FALSE) )^2
      }
      findmin <- optimize(Fchronic,interval=c(-8,8))
      maxlogCconsumlimit <- (findmin$minimum+log(mean(Weight)))/log(10)
      
      # single solution from uncertainty distribution quantile:
      Fchronicall <- function(logXW){
        (log(limitexpoK[hazardindexK[h]]) - quantile(logRK[foodindex[i],hazardindexK[h]] 
                                                    +mucK[,hazardindexK[h],foodindex[i]]
                                                    +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2
                                                    +logXW +log(PK[foodindex[i],hazardindexK[h]]*pK[u,hazardindexK[h],foodindex[i]]),0.95,names=FALSE))^2
      }
      findmin <- optimize(Fchronicall,interval=c(-8,8))
      maxlogCconsumlimitall <- (findmin$minimum+log(mean(Weight)))/log(10)
      
    }  # end of if logarithmic  
    
  
  if(input_selectscale=="Absolute"){ 
    mtext(paste("P(A.exposure/bw+ <",limitexpoK[hazardindexK[h]],")=",round(quantile(Plimitpos,0.025),2),"/",round(median(Plimitpos),2),"/",round(quantile(Plimitpos,0.975),2)," (Q2.5%/Q50%/Q97.5%)"),
          side = 1, adj = 0,line=0, cex = 1,
          outer = TRUE) 
    mtext(paste("P(A.exposure/bw <",limitexpoK[hazardindexK[h]],")=",round(quantile(Plimitall,0.025),2),"/",round(median(Plimitall),2),"/",round(quantile(Plimitall,0.975),2)," (Q2.5%/Q50%/Q97.5%)"),
          side = 1, adj = 0,line=1, cex = 1,
          outer = TRUE)
    mtext(paste("P(C.exposure/bw+ <",limitexpoK[hazardindexK[h]],")=",round(mean(meanexposurebwposK<limitexpoK[hazardindexK[h]]),2),". P(C.exposure/bw <",limitexpoK[hazardindexK[h]],")=",round(mean(meanexposurebwallK<limitexpoK[hazardindexK[h]]),2)),
          side = 1, adj = 0,line=2, cex = 1,
          outer = TRUE)
    mtext(paste("Safe95% Max A.consum/day+ =",round(quantile(maxAconsumlimit,0.025),2),"/",round(median(maxAconsumlimit),2),"/",round(quantile(maxAconsumlimit,0.975),2),". Safe95% Max C.consum/day+ =",round(maxCconsumlimit,2)),
          side = 1, adj = 0, line=3, cex = 1, outer=TRUE)
    mtext(paste("Safe95% Max A.consum/day =",round(quantile(maxAconsumlimitall,0.025),2),"/",round(median(maxAconsumlimitall),2),"/",round(quantile(maxAconsumlimitall,0.975),2),". Safe95% Max C.consum/day =",round(maxCconsumlimitall,2)),
          side = 1, adj = 0, line=4, cex = 1, outer=TRUE)
  }
  if(input_selectscale=="Logarithmic"){ 
    mtext(paste("P(log(A.exposure/bw+) <",round(log10(limitexpoK[hazardindexK[h]]),2),")=",round(quantile(Plimitpos,0.025),2),"/",round(median(Plimitpos),2),"/",round(quantile(Plimitpos,0.975),2)," (Q2.5%/Q50%/Q97.5%)"),
          side = 1, adj = 0,line=0, cex = 1,
          outer = TRUE) 
    mtext(paste("P(log(A.exposure/bw) <",round(log10(limitexpoK[hazardindexK[h]]),2),")=",round(quantile(Plimitall,0.025),2),"/",round(median(Plimitall),2),"/",round(quantile(Plimitall,0.975),2)," (Q2.5%/Q50%/Q97.5%)"),
          side = 1, adj = 0,line=1, cex = 1,
          outer = TRUE) 
    mtext(paste("P(log(C.exposure/bw+) <",round(log10(limitexpoK[hazardindexK[h]]),2),")=",round(mean(logmeanexposurebwposK/log(10)<log10(limitexpoK[hazardindexK[h]])),2),". P(log(C.exposure/bw) <",round(log10(limitexpoK[hazardindexK[h]]),2),")=",round(mean(logmeanexposurebwallK/log(10)<log10(limitexpoK[hazardindexK[h]])),2)),
          side = 1, adj = 0,line=2, cex = 1,
          outer = TRUE)
    mtext(paste("Safe95% Max logA.consum/day+ =",round(quantile(maxlogAconsumlimit,0.025),2),"/",round(median(maxlogAconsumlimit),2),"/",round(quantile(maxlogAconsumlimit,0.975),2),". Safe95% Max logC.consum/day+ =",round(maxlogCconsumlimit,2)),
          side = 1, adj = 0, line=3, cex = 1, outer=TRUE)
    mtext(paste("Safe95% Max logA.consum/day =",round(quantile(maxlogAconsumlimitall,0.025),2),"/",round(median(maxlogAconsumlimitall),2),"/",round(quantile(maxlogAconsumlimitall,0.975),2),". Safe95% Max logC.consum/day =",round(maxlogCconsumlimitall,2)),
          side = 1, adj = 0, line=4, cex = 1, outer=TRUE)
  }
  
}  # end of if cumulative