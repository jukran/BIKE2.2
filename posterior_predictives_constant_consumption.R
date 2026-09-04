

# posterior predictive distributions, in the case of constant consumption, for: 
# concentrations,
# acute (single) exposures (due to variability of concentrations day-by-day)


#  give a message if the result is not available
DF <- data.frame(Results="No food-hazard selected, consumption is constant")


if(is.element("Concentrations",theresults)){
  
  if((nhusedK>0)&(nfused>0)){  # if some chemical hazard in some food selected
    
    cKmc <- array(NA,dim=c(n_sim,nhusedK,nfused))   
    for(mc in 1:(n_sim)){  # simulate posterior predictive concentrations
      for(h in 1:nhusedK){ # actual contamination level:
        cKmc[mc,h,1:nfused] <- rlnorm(nfused,
                                      mucK[mc,hazardindexK[h],foodindex[1:nfused]],
                                      sigcK[mc,hazardindexK[h],foodindex[1:nfused]])
        # contamination zero if hazard-food not modeled:
        cKmc[mc,h,1:nfused] <- cKmc[mc,h,1:nfused]*(nexactK[hazardindexK[h],foodindex[1:nfused]]>0)
      } # end of h
    } # end of mc
    hazardnamesusedKinfoodnamesused <- array(NA,nhusedK*nfused) # collect the names of used hazard-food combinations
    hlo98cK <- numeric()
    hup98cK <- numeric()
    hlo90cK <- numeric()
    hup90cK <- numeric()
    hlo80cK <- numeric()
    hup80cK <- numeric()
    hmediancK <- numeric()
    counterK <- 0
    for(i in 1:nhusedK){
      for(j in 1:nfused){
        counterK <- counterK +1
        hazardnamesusedKinfoodnamesused[counterK] <- paste0(hazardnamesusedK[i]," in ",foodnamesused[j],":") 
        hlo98cK[counterK] <- quantile(cKmc[,i,j],c(0.01),names=FALSE) # calculate quantile
        hup98cK[counterK] <- quantile(cKmc[,i,j],c(0.99),names=FALSE) # calculate quantile
        hlo90cK[counterK] <- quantile(cKmc[,i,j],c(0.05),names=FALSE) # calculate quantile
        hup90cK[counterK] <- quantile(cKmc[,i,j],c(0.95),names=FALSE) # calculate quantile
        hlo80cK[counterK] <- quantile(cKmc[,i,j],c(0.10),names=FALSE) # calculate quantile
        hup80cK[counterK] <- quantile(cKmc[,i,j],c(0.90),names=FALSE) # calculate quantile
        hmediancK[counterK] <- quantile(cKmc[,i,j],c(0.5),names=FALSE) # calculate quantile
      }
    } 
    DFKconcentrations <- data.frame(
      Quantity_ = paste(hazardnamesusedKinfoodnamesused),
      Quantity = paste("concentr+"),
      Q01 = as.character(round(hlo98cK[1:counterK],2)),
      Q05 = as.character(round(hlo90cK[1:counterK],2)),
      Q10 = as.character(round(hlo80cK[1:counterK],2)),
      Median = as.character(round(hmediancK[1:counterK],2)),
      Q90 = as.character(round(hup80cK[1:counterK],2)),
      Q95 = as.character(round(hup90cK[1:counterK],2)),
      Q99 = as.character(round(hup98cK[1:counterK],2)),
      stringsAsFactors=FALSE)
  } # end of if nhusedK nfused  
  
  if((nhusedM>0)&(nfused>0)){  # if some microbial hazard in some food selected
    
    cMmc <- array(NA,dim=c(n_sim,nhusedM,nfused))
    for(mc in 1:(n_sim)){  # simulate posterior predictive concentrations
      for(h in 1:nhusedM){  # actual contamination level:
        cMmc[mc,h,1:nfused] <- rlnorm(nfused,
                                      mucM[mc,hazardindexM[h],foodindex[1:nfused]],
                                      sigcM[mc,hazardindexM[h],foodindex[1:nfused]]) 
        # contamination zero if hazard-food not modeled:
        cMmc[mc,h,1:nfused] <- cMmc[mc,h,1:nfused]*(nexactM[hazardindexM[h],foodindex[1:nfused]]>0)
      } # end of h
    } # end of mc
    hazardnamesusedMinfoodnamesused <- array(NA,nhusedM*nfused) # collect the names of used hazard-food combinations
    hlo98cM <- numeric()
    hup98cM <- numeric()
    hlo90cM <- numeric()
    hup90cM <- numeric()
    hlo80cM <- numeric()
    hup80cM <- numeric()
    hmediancM <- numeric()
    counterM <- 0
    for(i in 1:nhusedM){
      for(j in 1:nfused){
        counterM <- counterM +1
        hazardnamesusedMinfoodnamesused[counterM] <- paste0(hazardnamesusedM[i]," in ",foodnamesused[j],":") 
        hlo98cM[counterM] <- quantile(cMmc[,i,j],c(0.01),names=FALSE) # calculate quantile
        hup98cM[counterM] <- quantile(cMmc[,i,j],c(0.99),names=FALSE) # calculate quantile
        hlo90cM[counterM] <- quantile(cMmc[,i,j],c(0.05),names=FALSE) # calculate quantile
        hup90cM[counterM] <- quantile(cMmc[,i,j],c(0.95),names=FALSE) # calculate quantile
        hlo80cM[counterM] <- quantile(cMmc[,i,j],c(0.10),names=FALSE) # calculate quantile
        hup80cM[counterM] <- quantile(cMmc[,i,j],c(0.90),names=FALSE) # calculate quantile
        hmediancM[counterM] <- quantile(cMmc[,i,j],c(0.5),names=FALSE) # calculate quantile
      }
    } 
    DFMconcentrations <- data.frame(
      Quantity_ = paste(hazardnamesusedMinfoodnamesused),
      Quantity = paste("concentr+"),
      Q01 = as.character(round(hlo98cM[1:counterM],2)),
      Q05 = as.character(round(hlo90cM[1:counterM],2)),
      Q10 = as.character(round(hlo80cM[1:counterM],2)),
      Median = as.character(round(hmediancM[1:counterM],2)),
      Q90 = as.character(round(hup80cM[1:counterM],2)),
      Q95 = as.character(round(hup90cM[1:counterM],2)),
      Q99 = as.character(round(hup98cM[1:counterM],2)),
      stringsAsFactors=FALSE)
  } # end of if nhusedM nfused  
  
} # end of concentrations

###################
#### simulate acute exposure variability due to concentration variability (constant consumption):

if(nhusedM>0){ # microbial contaminations
  ImcM <- array(NA,dim=c(n_sim,nhM,nf))
  cmcM <- array(NA,dim=c(n_sim,nhM,nf)) 
  EemcM <- array(0,dim=c(n_sim,nhusedM,nfused)) # default = 0 
  acutetotM <- matrix(NA,n_sim,nhM) 
  RM = matrix(NA,nf,nhM)
  RM[1:nf,1:nhM] = Rall[1:nf,is.element(hazardnames,hazardnamesusedM)]
  logRM = log(RM)
  PM = matrix(NA,nf,nhM)
  PM[1:nf,1:nhM] = Pall[1:nf,is.element(hazardnames,hazardnamesusedM)]
  hlo90totacuteM <- numeric(nhusedM)
  hup90totacuteM <- numeric(nhusedM)
  hmediantotacuteM <- numeric(nhusedM)
  hlo80totacuteM <- numeric(nhusedM)
  hup80totacuteM <- numeric(nhusedM)
  hlo98totacuteM <- numeric(nhusedM)
  hup98totacuteM <- numeric(nhusedM)
}  
if(nhusedK>0){ # chemical contaminations
  ImcK <- array(NA,dim=c(n_sim,nhK,nf))
  cmcK <- array(NA,dim=c(n_sim,nhK,nf)) 
  EemcK <- array(0,dim=c(n_sim,nhusedK,nfused)) # default = 0 
  acutetotbwK <- matrix(NA,n_sim,nhK) 
  RK = matrix(NA,nf,nhK)
  RK[1:nf,1:nhK] = Rall[1:nf,is.element(hazardnames,hazardnamesusedK)]
  logRK = log(RK)
  PK = matrix(NA,nf,nhK)
  PK[1:nf,1:nhK] = Pall[1:nf,is.element(hazardnames,hazardnamesusedK)]
  hlo90totbwK <- numeric(nhusedK)
  hup90totbwK <- numeric(nhusedK)
  hmediantotbwK <- numeric(nhusedK)
  hlo80totbwK <- numeric(nhusedK)
  hup80totbwK <- numeric(nhusedK)
  hlo98totbwK <- numeric(nhusedK)
  hup98totbwK <- numeric(nhusedK)
}

# Set the constant value for consumptions, over all days: 
SW <- numeric(); S <- numeric()
for(i in 1:nf){
  swconstant <- exp(logsw[,,i]) # per bodyweight
  sconstant <- exp(logs[,,i])   # absolute
  swconstant[is.na(swconstant)==TRUE]<-0  # days when not consumed in data
  sconstant[is.na(sconstant)==TRUE]<-0   # days when not consumed in data
  # consumptions assumed to be constants:
  SW[i] <- mean(swconstant)
  S[i] <- mean(sconstant)
}
for(mc in 1:(n_sim)){
  
  if((nhusedM>0)&(nfused>0)){ # microbial contaminations
    for(h in 1:nhM){
      ImcM[mc,h,1:nf] <- rbinom(nf,rep(1,nf),pM[mc,h,1:nf]*PM[1:nf,h]) # actual contamination yes/no
      cmcM[mc,h,1:nf] <- rlnorm(nf,mucM[mc,h,1:nf],sigcM[mc,h,1:nf]) # actual microbial contamination level
    }
  }
  if((nhusedK>0)&(nfused>0)){ # chemical contaminations
    for(h in 1:nhK){
      ImcK[mc,h,1:nf] <- rbinom(nf,rep(1,nf),pK[mc,h,1:nf]*PK[1:nf,h]) # actual contamination yes/no
      cmcK[mc,h,1:nf] <- rlnorm(nf,mucK[mc,h,1:nf],sigcK[mc,h,1:nf]) # actual chemical contamination level
    }
  }
  
  if((nhusedM>0)&(nfused>0)){  # microbial exposures
    for(h in 1:nhusedM){ #Predict final count with poisson distribution:
      for(i in 1:nfused){
        # acute exposure for a random consumer, hazard h, food i:
        if(nexactM[hazardindexM[h],foodindex[i]]>0){ # hazard-food is modeled
          EemcM[mc,h,i] <- ImcM[mc,hazardindexM[h],foodindex[i]]*
            S[foodindex[i]]*    
            RM[foodindex[i],hazardindexM[h]]*
            cmcM[mc,hazardindexM[h],foodindex[i]]
        }
      }  
      if(sum(EemcM[mc,h,1:nfused])<=5000){ # use Poisson when the mean is 'small'
        acutetotM[mc,h] <- rpois(1,sum(EemcM[mc,h,1:nfused])) # sum over foods
      }
      if(sum(EemcM[mc,h,1:nfused])>5000){ # use rounded Normal when the mean is 'large'
        acutetotM[mc,h] <- round(rnorm(1,sum(EemcM[mc,h,1:nfused]),sqrt(sum(EemcM[mc,h,1:nfused])))) # sum over foods 
      }
    } # end of h
  } # end of if microbial
  
  if((nhusedK>0)&(nfused>0)){  # chemical exposures
    for(h in 1:nhusedK){ #Predict final acute exposure:
      for(i in 1:nfused){
        # acute exposure for a random consumer, hazard h, food i:
        if(nexactK[hazardindexK[h],foodindex[i]]>0){ # hazard-food is modeled
          EemcK[mc,h,i] <- ImcK[mc,hazardindexK[h],foodindex[i]]*
            SW[foodindex[i]]*
            RK[foodindex[i],hazardindexK[h]]*
            cmcK[mc,hazardindexK[h],foodindex[i]]
        }
      }  
      acutetotbwK[mc,h] <- sum(EemcK[mc,h,1:nfused]) # sum over foods
    } # end of h
  } # end of if chemical
  
} # end of mc
###################
############ Get posterior predictive results into data frames:  #################

if(nhusedK>0){
  for(h in 1:nhusedK){ # posterior predictive summaries (quantiles) of individual exposures (acute)
    hlo90totbwK[h] <- quantile(acutetotbwK[,h],0.05,names=FALSE)
    hmediantotbwK[h] <- quantile(acutetotbwK[,h],0.5,names=FALSE)
    hup90totbwK[h] <- quantile(acutetotbwK[,h],0.95,names=FALSE)
    hlo80totbwK[h] <- quantile(acutetotbwK[,h],0.10,names=FALSE)
    hup80totbwK[h] <- quantile(acutetotbwK[,h],0.90,names=FALSE)
    hlo98totbwK[h] <- quantile(acutetotbwK[,h],0.01,names=FALSE)
    hup98totbwK[h] <- quantile(acutetotbwK[,h],0.99,names=FALSE)
  }
}
if(nhusedM>0){
  for(h in 1:nhusedM){  # posterior predictive summaries (quantiles) of individual exposures (acute)
    hlo90totacuteM[h] <- round(quantile(acutetotM[,h],0.05,names=FALSE))
    hmediantotacuteM[h] <- round(quantile(acutetotM[,h],0.5,names=FALSE))
    hup90totacuteM[h] <- round(quantile(acutetotM[,h],0.95,names=FALSE))
    hlo80totacuteM[h] <- round(quantile(acutetotM[,h],0.10,names=FALSE))
    hup80totacuteM[h] <- round(quantile(acutetotM[,h],0.90,names=FALSE))
    hlo98totacuteM[h] <- round(quantile(acutetotM[,h],0.01,names=FALSE))
    hup98totacuteM[h] <- round(quantile(acutetotM[,h],0.99,names=FALSE))
  }
}
###################
# Compose data frame for chemical (acute) exposure
if(nhusedK>0){
  DF1K <- data.frame(
    Quantity_ = paste(hazardnamesusedK),
    Quantity = paste("total acute exposure/bw"),
    Q01 = as.character(round(hlo98totbwK[1:nhusedK],2)),
    Q05 = as.character(round(hlo90totbwK[1:nhusedK],2)),
    Q10 = as.character(round(hlo80totbwK[1:nhusedK],2)),
    Median = as.character(round(hmediantotbwK[1:nhusedK],2)),
    Q90 = as.character(round(hup80totbwK[1:nhusedK],2)),
    Q95 = as.character(round(hup90totbwK[1:nhusedK],2)),
    Q99 = as.character(round(hup98totbwK[1:nhusedK],2)),
    stringsAsFactors=FALSE)
}
# Compose data frame for microbial (acute) exposure   
if(nhusedM>0){
  DF1M <- data.frame(
    Quantity_ = paste(hazardnamesusedM),
    Quantity = paste("total acute exposure"),
    Q01 = as.character(round(hlo98totacuteM[1:nhusedM],2)),
    Q05 = as.character(round(hlo90totacuteM[1:nhusedM],2)),
    Q10 = as.character(round(hlo80totacuteM[1:nhusedM],2)),
    Median = as.character(round(hmediantotacuteM[1:nhusedM],2)),
    Q90 = as.character(round(hup80totacuteM[1:nhusedM],2)),
    Q95 = as.character(round(hup90totacuteM[1:nhusedM],2)),
    Q99 = as.character(round(hup98totacuteM[1:nhusedM],2)),
    stringsAsFactors=FALSE)
}
###################
if(!is.element("Concentrations",theresults)){   
  if(is.element("Exposures",theresults) ){  
    if((nhusedK>0)&(nhusedM>0)){ DF <- rbind.data.frame(DF1K,DF1M) }
    if((nhusedK>0)&(nhusedM==0)){ DF <- rbind.data.frame(DF1K) }
    if((nhusedK==0)&(nhusedM>0)){ DF <- rbind.data.frame(DF1M) }
  }
} # end of if !Concentrations
if(is.element("Concentrations",theresults)){
  if((nhusedK>0)&(nhusedM==0)){DF <- rbind.data.frame(DFKconcentrations)}
  if((nhusedK==0)&(nhusedM>0)){DF <- rbind.data.frame(DFMconcentrations)}
  if((nhusedK>0)&(nhusedM>0)){DF <- rbind.data.frame(DFKconcentrations,DFMconcentrations)}
  
  if(is.element("Exposures",theresults)){
    if((nhusedK>0)&(nhusedM>0)){ DF <- rbind.data.frame(DF,DF1K,DF1M) }
    if((nhusedK>0)&(nhusedM==0)){ DF <- rbind.data.frame(DF,DF1K) }
    if((nhusedK==0)&(nhusedM>0)){ DF <- rbind.data.frame(DF,DF1M) }
  }
} # end of if Concentrations

###################
