
  # Run the BUGS model code (MCMC sampling of posterior distribution) for given number of iterations.
  # This file runs the model version with independent consumption days.
  # For this: data and initial values for parameters need to be defined,
  # and the names of parameters to be monitored in outputs.
  # --> definitions for 'data', 'inits', 'parameters'.
  # --> call bugs.
  # --> return MCMC outputs.
  

# if both chemical and microbiological hazards:    
if((nhK>0)&(nhM>0)){

data <- list("logcM","logcK","nexactM","nexactK") #,
               #"nf","nr","nd")  
  
if(input$priorchoice == "sigma_uniform"){
data <- append(data,c("sdpriorlimM","sdpriorlimK")) # assign limits for prior range 
}
if(sum(nbelowLOQM)>0){ 
data <- append(data,c("logLOQM","logLOQLimM","nbelowLOQM"))
}
if(sum(nbelowLODM)>0){ 
data <- append(data,c("logLODM","nbelowLODM"))
}
if(sum(nbelowLOQK)>0){ 
data <- append(data,c("logLOQK","logLOQLimK","nbelowLOQK"))
}
if(sum(nbelowLODK)>0){ 
data <- append(data,c("logLODK","nbelowLODK"))
}

# initial values:
initmucK <- matrix(NA,nhK,nf)
initsigcK <- matrix(NA,nhK,nf)
for(i in 1:nhK){
  for(j in 1:nf){ 
    if(nexactK[i,j]>0){ # there were concentration data > LOQ
    initmucK[i,j] <- mean(logcK[i,j,],na.rm=TRUE)
    initsigcK[i,j] <- sdpriorlimK[i,j]/2
    }
    if(nexactK[i,j]==0){ # there were no concentration data > LOQ
    initmucK[i,j] <- 1 # arbitrary initial value, hazard-food combination not in model
    initsigcK[i,j] <- 1 # arbitrary initial value, hazard-food combination not in model    
    } 
}}
initmucM <- matrix(NA,nhM,nf)
initsigcM <- matrix(NA,nhM,nf)
for(i in 1:nhM){
  for(j in 1:nf){
    if(nexactM[i,j]>0){ # there were concentration data > LOQ   
    initmucM[i,j] <- mean(logcM[i,j,],na.rm=TRUE)
    initsigcM[i,j] <- sdpriorlimM[i,j]/2 
    }
    if(nexactM[i,j]==0){ # there were no concentration data > LOQ   
    initmucM[i,j] <- 1 # arbitrary initial value, hazard-food combination not in model
    initsigcM[i,j] <- 1 # arbitrary initial value, hazard-food combination not in model
    }  
}}
} # end of both chem and microb

# if only chemical hazards:  
if((nhK>0)&(nhM==0)){

data <- list("logcK","nexactK") #,
               #"nf","nr","nd")  
  
if(input$priorchoice == "sigma_uniform"){
data <- append(data,"sdpriorlimK")  # assign limits for prior range 
}
if(sum(nbelowLOQK)>0){ 
data <- append(data,c("logLOQK","logLOQLimK","nbelowLOQK"))
}
if(sum(nbelowLODK)>0){ 
data <- append(data,c("logLODK","nbelowLODK"))
}

# initial values:
initmucK <- matrix(NA,nhK,nf)
initsigcK <- matrix(NA,nhK,nf)
for(i in 1:nhK){
  for(j in 1:nf){
    if(nexactK[i,j]>0){ # there were concentration data > LOQ
    initmucK[i,j] <- mean(logcK[i,j,],na.rm=TRUE)
    initsigcK[i,j] <- sdpriorlimK[i,j]/2
    }
    if(nexactK[i,j]==0){ # there were no concentration data > LOQ
      initmucK[i,j] <- 1 # arbitrary initial value, hazard-food combination not in model
      initsigcK[i,j] <- 1 # arbitrary initial value, hazard-food combination not in model
    }
}}
} # end of only chemical 

# if only microbiological hazards:  
if((nhK==0)&(nhM>0)){

data <- list("logcM","nexactM") #,
               #"nf","nr","nd")  

if(input$priorchoice == "sigma_uniform"){
data <- append(data,"sdpriorlimM")  # assign limits for prior range 
}
if(sum(nbelowLOQM)>0){ 
data <- append(data,c("logLOQM","logLOQLimM","nbelowLOQM"))
}
if(sum(nbelowLODM)>0){ 
data <- append(data,c("logLODM","nbelowLODM"))
}

# initial values:
initmucM <- matrix(NA,nhM,nf)
initsigcM <- matrix(NA,nhM,nf)
for(i in 1:nhM){
  for(j in 1:nf){
    if(nexactM[i,j]>0){ # there were concentration data > LOQ
    initmucM[i,j] <- mean(logcM[i,j,],na.rm=TRUE)
    initsigcM[i,j] <- sdpriorlimM[i,j]/2
    }
    if(nexactM[i,j]==0){ # there were no concentration data > LOQ
      initmucM[i,j] <- 1 # arbitrary initial value, hazard-food combination not in model
      initsigcM[i,j] <- 1 # arbitrary initial value, hazard-food combination not in model
    }
}}
} # end of only microbiological 
  
  
  if(input$priorchoice == "sigma_uniform" ){  
    # then prior for sigma is needed
    if((nhK>0)&(nhM>0)){
        inits <- function(){list(mucK=initmucK,sigcK=initsigcK,mucM=initmucM,sigcM=initsigcM,pM=matrix(0.5,nhM,nf)) }
        parameters=c("mucK","mucM","sigcK","sigcM","pM","pK")
    }
    if((nhK>0)&(nhM==0)){
        inits <- function(){list(mucK=initmucK,sigcK=initsigcK) } 
        parameters=c("mucK","sigcK","pK")
    }
    if((nhK==0)&(nhM>0)){
        inits <- function(){list(mucM=initmucM,sigcM=initsigcM,pM=matrix(0.5,nhM,nf)) }  
        parameters=c("mucM","sigcM","pM")
    }  
  } # end of prior choice
  
  if(input$priorchoice == "tau_gamma" ){  
    # then prior for tau is needed,
    if((nhK>0)&(nhM>0)){
        inits <- function(){list(mucK=initmucK,taucK=initsigcK^(-2),mucM=initmucM,taucM=initsigcM^(-2),pM=matrix(0.5,nhM,nf)) }
        parameters=c("mucK","mucM","sigcK","sigcM","pM","pK")
    }
    if((nhK>0)&(nhM==0)){
        inits <- function(){list(mucK=initmucK,taucK=initsigcK^(-2))  }  
        parameters=c("mucK","sigcK","pK")
    }
    if((nhK==0)&(nhM>0)){
        inits <- function(){list(mucM=initmucM,taucM=initsigcM^(-2),pM=matrix(0.5,nhM,nf)) }  
        parameters=c("mucM","sigcM","pM")
    }  
  } # end of prior choice
  
