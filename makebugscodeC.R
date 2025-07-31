
# Creates BUGS-code for a model for FFQ data
tempBike <- file.path(tempdir(), "bikemodel.txt")
file.copy("bikemodel.txt", tempBike, overwrite = TRUE)

fileConn<-file(tempBike)
tempfile("bikemodel.txt")
fileConn<-file("bikemodel.txt")
cat("model{",file="bikemodel.txt",sep="\n")
cat("#-------------",file="bikemodel.txt",sep="\n",append=TRUE)
cat("# Concentration code",file="bikemodel.txt",sep="\n",append=TRUE)
cat("# concentration measurements data",file="bikemodel.txt",sep="\n",append=TRUE)

# BUGS code for exact chemical measurements: 
if(nhK > 0){
  for(i in 1:nhK){
    for(j in 1:nf){
      if(nexactK[i,j]>0){ # there were concentration data > LOQ
      if(OTK[i,j]=="all"){   # both true zeros and true positives possible
        cat(paste("for(k in 1:nexactK[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat(paste("logcK[",i,",",j,",k] ~ dnorm(mucK[",i,",",j,"],taucK[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat(paste("positive1K[",i,",",j,",k] ~ dbern(pK[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat(paste("positive1K[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat("}",file="bikemodel.txt",sep="\n",append=TRUE)   
      }
      if(OTK[i,j]=="positives"){ # only positives
        cat(paste("for(k in 1:nexactK[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat(paste("logcK[",i,",",j,",k] ~ dnorm(mucK[",i,",",j,"],taucK[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat("}",file="bikemodel.txt",sep="\n",append=TRUE)
      }
      }  
}}}

# BUGS code for exact microbiological measurements:
if(nhM > 0){
  for(i in 1:nhM){
    for(j in 1:nf){
      if(nexactM[i,j]>0){ # there were concentration data > LOQ
      if(OTM[i,j]=="all"){
        cat(paste("for(k in 1:nexactM[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat(paste("logcM[",i,",",j,",k] ~ dnorm(mucM[",i,",",j,"],taucM[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat(paste("positive1M[",i,",",j,",k] ~ dbern(pM[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat(paste("positive1M[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat("}",file="bikemodel.txt",sep="\n",append=TRUE)
      }
      if(OTM[i,j]=="positives"){
        cat(paste("for(k in 1:nexactM[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
        cat(paste("logcM[",i,",",j,",k] ~ dnorm(mucM[",i,",",j,"],taucM[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE) 
        cat("}",file="bikemodel.txt",sep="\n",append=TRUE) 
      }
      }  
}}}

cat("# Censored data",file="bikemodel.txt",sep="\n",append=TRUE)

# BUGS code for censored chemical measurements between LOQ and LOD
if(nhK > 0){
for(i in 1:nhK){
for(j in 1:nf){
if(nbelowLOQK[i,j]>0){
  if(OTK[i,j]=="all"){  
cat(paste("for(k in 1:nbelowLOQK[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("positive2K[",i,",",j,",k] ~ dbern(PRintervalK[",i,",",j,",k])"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("PRintervalK[",i,",",j,",k]<-pK[",i,",",j,"]*(phi((logLOQK[",i,",",j,",k]-mucK[",i,",",j,"])*pow(taucK[",i,",",j,"],0.5))-phi((logLOQLimK[",i,",",j,",k]-mucK[",i,",",j,"])*pow(taucK[",i,",",j,"],0.5)))"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("positive2K[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
cat("}",file="bikemodel.txt",sep="\n",append=TRUE) 
  }
  if(OTK[i,j]=="positives"){
cat(paste("for(k in 1:nbelowLOQK[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("positive2K[",i,",",j,",k] ~ dbern(PRintervalK[",i,",",j,",k])"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("PRintervalK[",i,",",j,",k]<-(phi((logLOQK[",i,",",j,",k]-mucK[",i,",",j,"])*pow(taucK[",i,",",j,"],0.5))-phi((logLOQLimK[",i,",",j,",k]-mucK[",i,",",j,"])*pow(taucK[",i,",",j,"],0.5)))"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("positive2K[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
cat("}",file="bikemodel.txt",sep="\n",append=TRUE)    
  }
}
}}}

# BUGS code for censored chemical measurements below LOD
if(nhK>0){
for(i in 1:nhK){
for(j in 1:nf){
if(nbelowLODK[i,j]>0){
  if(OTK[i,j]=="all"){   
cat(paste("for(k in 1:nbelowLODK[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("positive3K[",i,",",j,",k] ~ dbern(PRleftK[",i,",",j,",k])"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("PRleftK[",i,",",j,",k]<- 1-pK[",i,",",j,"]*(1-phi((logLODK[",i,",",j,",k]-mucK[",i,",",j,"])*pow(taucK[",i,",",j,"],0.5)))"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("positive3K[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
cat("}",file="bikemodel.txt",sep="\n",append=TRUE)
  }
  if(OTK[i,j]=="positives"){
cat(paste("for(k in 1:nbelowLODK[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("positive3K[",i,",",j,",k] ~ dbern(PRleftK[",i,",",j,",k])"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("PRleftK[",i,",",j,",k]<- phi((logLODK[",i,",",j,",k]-mucK[",i,",",j,"])*pow(taucK[",i,",",j,"],0.5))"),file="bikemodel.txt",sep="\n",append=TRUE)
cat(paste("positive3K[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
cat("}",file="bikemodel.txt",sep="\n",append=TRUE)   
  }
}
}}}   


# BUGS code for censored microbiological measurements between LOD & LOQ and possibly including true zeros
if(nhM > 0){
  for(i in 1:nhM){
    for(j in 1:nf){
      if(nbelowLOQM[i,j]>0){
        if(OTM[i,j]=="all"){  
          cat(paste("for(k in 1:nbelowLOQM[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("positive2M[",i,",",j,",k] ~ dbern(PRintervalM[",i,",",j,",k])"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("PRintervalM[",i,",",j,",k]<-pM[",i,",",j,"]*(phi((logLOQM[",i,",",j,",k]-mucM[",i,",",j,"])*pow(taucM[",i,",",j,"],0.5))-phi((logLOQLimM[",i,",",j,",k]-mucM[",i,",",j,"])*pow(taucM[",i,",",j,"],0.5)))"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("positive2M[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat("}",file="bikemodel.txt",sep="\n",append=TRUE) 
        }
        if(OTM[i,j]=="positives"){
          cat(paste("for(k in 1:nbelowLOQM[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("positive2M[",i,",",j,",k] ~ dbern(PRintervalM[",i,",",j,",k])"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("PRintervalM[",i,",",j,",k]<-(phi((logLOQM[",i,",",j,",k]-mucM[",i,",",j,"])*pow(taucM[",i,",",j,"],0.5))-phi((logLOQLimM[",i,",",j,",k]-mucM[",i,",",j,"])*pow(taucM[",i,",",j,"],0.5)))"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("positive2M[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat("}",file="bikemodel.txt",sep="\n",append=TRUE)    
        }
      } # end of if positives  
}}}

# BUGS code for censored microbiological measurements below LOD and possibly including true zeros
if(nhM > 0){
  for(i in 1:nhM){
    for(j in 1:nf){
      if(nbelowLODM[i,j]>0){
        if(OTM[i,j]=="all"){   
          cat(paste("for(k in 1:nbelowLODM[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("positive3M[",i,",",j,",k] ~ dbern(PRleftM[",i,",",j,",k])"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("PRleftM[",i,",",j,",k]<- 1-pM[",i,",",j,"]*(1-phi((logLODM[",i,",",j,",k]-mucM[",i,",",j,"])*pow(taucM[",i,",",j,"],0.5)))"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("positive3M[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat("}",file="bikemodel.txt",sep="\n",append=TRUE)
        }
        if(OTM[i,j]=="positives"){
          cat(paste("for(k in 1:nbelowLODM[",i,",",j,"]){"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("positive3M[",i,",",j,",k] ~ dbern(PRleftM[",i,",",j,",k])"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("PRleftM[",i,",",j,",k]<- phi((logLODM[",i,",",j,",k]-mucM[",i,",",j,"])*pow(taucM[",i,",",j,"],0.5))"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("positive3M[",i,",",j,",k] <- 1"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat("}",file="bikemodel.txt",sep="\n",append=TRUE)   
        }
      } # end of if positives  
}}}


if(nhK>0){
cat("# Priors:",file="bikemodel.txt",sep="\n",append=TRUE)  
for(i in 1:nhK){  
for(j in 1:nf){
if(nexactK[i,j]>0){
  cat(paste("mucK[",i,",",j,"] ~ dunif(-10,10)"),file="bikemodel.txt",sep="\n",append=TRUE)
  if(input$priorchoice=="sigma_uniform"){
    cat(paste("taucK[",i,",",j,"] <- pow(sigcK[",i,",",j,"],-2)"),file="bikemodel.txt",sep="\n",append=TRUE)
    cat(paste("sigcK[",i,",",j,"] ~ dunif(0,sdpriorlimK[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
  }
  if(input$priorchoice=="tau_gamma"){
    cat(paste("taucK[",i,",",j,"] ~ dgamma(0.01,0.01)"),file="bikemodel.txt",sep="\n",append=TRUE)
    cat(paste("sigcK[",i,",",j,"] <- pow(taucK[",i,",",j,"],-0.5)"),file="bikemodel.txt",sep="\n",append=TRUE)
  }
}
  if(nexactK[i,j]==0){
    cat(paste("mucK[",i,",",j,"] ~ dunif(-10,10)"),file="bikemodel.txt",sep="\n",append=TRUE)
    if(input$priorchoice=="sigma_uniform"){
      cat(paste("taucK[",i,",",j,"] <- pow(sigcK[",i,",",j,"],-2)"),file="bikemodel.txt",sep="\n",append=TRUE)
      cat(paste("sigcK[",i,",",j,"] ~ dunif(0,sdpriorlimK[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
    }
    if(input$priorchoice=="tau_gamma"){
      cat(paste("taucK[",i,",",j,"] ~ dgamma(1,1)"),file="bikemodel.txt",sep="\n",append=TRUE)
      cat(paste("sigcK[",i,",",j,"] <- pow(taucK[",i,",",j,"],-0.5)"),file="bikemodel.txt",sep="\n",append=TRUE)
    }
  }  
}}  

for(i in 1:nhK){
  for(j in 1:nf){
    if(nexactK[i,j]>0){
      if(OTK[i,j]=="all"){
        cat(paste("pK[",i,",",j,"] ~ dbeta(1,1)"),file="bikemodel.txt",sep="\n",append=TRUE)
      }
      if(OTK[i,j]=="positives"){
        cat(paste("pK[",i,",",j,"] ~ dbeta(",nposK[i,j]+1,",",nsamK[i,j]-nposK[i,j]+1,")"),file="bikemodel.txt",sep="\n",append=TRUE)
      }
    }
    if(nexactK[i,j]==0){
      cat(paste("pK[",i,",",j,"] ~ dbeta(1,1) # dummy prior for 'no data' -parameter"),file="bikemodel.txt",sep="\n",append=TRUE) 
    }
  }}
}


if(nhM>0){
  cat("# Priors:",file="bikemodel.txt",sep="\n",append=TRUE)  
  for(i in 1:nhM){  
    for(j in 1:nf){
      if(nexactM[i,j]>0){
        cat(paste("mucM[",i,",",j,"] ~ dunif(-10,10)"),file="bikemodel.txt",sep="\n",append=TRUE)
        if(input$priorchoice=="sigma_uniform"){
          cat(paste("taucM[",i,",",j,"] <- pow(sigcM[",i,",",j,"],-2)"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("sigcM[",i,",",j,"] ~ dunif(0,sdpriorlimM[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
        }
        if(input$priorchoice=="tau_gamma"){
          cat(paste("taucM[",i,",",j,"] ~ dgamma(0.01,0.01)"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("sigcM[",i,",",j,"] <- pow(taucM[",i,",",j,"],-0.5)"),file="bikemodel.txt",sep="\n",append=TRUE)
        }
      }
      if(nexactM[i,j]==0){
        cat(paste("mucM[",i,",",j,"] ~ dunif(-10,10)"),file="bikemodel.txt",sep="\n",append=TRUE)
        if(input$priorchoice=="sigma_uniform"){
          cat(paste("taucM[",i,",",j,"] <- pow(sigcM[",i,",",j,"],-2)"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("sigcM[",i,",",j,"] ~ dunif(0,sdpriorlimM[",i,",",j,"])"),file="bikemodel.txt",sep="\n",append=TRUE)
        }
        if(input$priorchoice=="tau_gamma"){
          cat(paste("taucM[",i,",",j,"] ~ dgamma(1,1)"),file="bikemodel.txt",sep="\n",append=TRUE)
          cat(paste("sigcM[",i,",",j,"] <- pow(taucM[",i,",",j,"],-0.5)"),file="bikemodel.txt",sep="\n",append=TRUE)
        }
      }  
    }}  

  
  for(i in 1:nhM){ 
    for(j in 1:nf){
      if(nexactM[i,j]>0){  
        if(OTM[i,j]=="all"){
          cat(paste("pM[",i,",",j,"] ~ dbeta(1,1)"),file="bikemodel.txt",sep="\n",append=TRUE)
        }
        if(OTM[i,j]=="positives"){
          cat(paste("pM[",i,",",j,"] ~ dbeta(",nposM[i,j]+1,",",nsamM[i,j]-nposM[i,j]+1,")"),file="bikemodel.txt",sep="\n",append=TRUE)
        }
      }
      if(nexactM[i,j]==0){
        cat(paste("pM[",i,",",j,"] ~ dbeta(1,1) # dummy prior for 'no data' -parameter"),file="bikemodel.txt",sep="\n",append=TRUE)
      }  
    }}
}

cat("#-----------------",file="bikemodel.txt",sep="\n",append=TRUE)
cat("# Consumption code",file="bikemodel.txt",sep="\n",append=TRUE)

cat("# Body weight model:",file="bikemodel.txt",sep="\n",append=TRUE)
cat("for(r in 1:nr){ logWeight[r] ~ dnorm(muw,tauw); logWeight[r] <- log(Weight[r]) }",file="bikemodel.txt",sep="\n",append=TRUE)
cat("muw ~ dunif(-10,10)",file="bikemodel.txt",sep="\n",append=TRUE)
if(input$priorchoice=="sigma_uniform"){
  cat("tauw <- pow(sigw,-2)",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("sigw ~ dunif(0,10)",file="bikemodel.txt",sep="\n",append=TRUE)
} 
if(input$priorchoice=="tau_gamma"){
  cat("tauw ~ dgamma(0.1,0.1)",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("sigw <- sqrt(1/tauw)",file="bikemodel.txt",sep="\n",append=TRUE)
} 
cat("  ",file="bikemodel.txt",sep="\n",append=TRUE)
cat("  ",file="bikemodel.txt",sep="\n",append=TRUE)


  # model for FFQ data
  cat("# Consumption measurements data (interpreted as mean daily amounts)",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("for(r in 1:nr){ # individual respondent",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("# zero-consumed foods are here as 'NA' so that this",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("# distribution is for what the consumption would be if it was positive:",file="bikemodel.txt",sep="\n",append=TRUE)
  
  if(nf>1){
    cat("# Individual consumption means can be correlated among food types",file="bikemodel.txt",sep="\n",append=TRUE)
    cat("logsw[r,1:nf] ~ dmnorm(mus0[1:nf],Ts0[1:nf,1:nf])",file="bikemodel.txt",sep="\n",append=TRUE)
  }
  if(nf==1){
    cat("logsw[r,1] ~ dnorm(mus0[1],Ts0[1,1])",file="bikemodel.txt",sep="\n",append=TRUE)  
  }
  
  cat("}",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("# Priors for food consumption mean amounts:",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("for(j in 1:nf){ ",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("mus0[j] ~ dunif(-10,10)",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("}",file="bikemodel.txt",sep="\n",append=TRUE)
  
  mchoice4 <- input$modelchoice4FFQ
  if(nf>1){
    if(mchoice4=="Yes"){  
      cat("# Correlations between food type means:",file="bikemodel.txt",sep="\n",append=TRUE)
      cat("Ts0[1:nf,1:nf] ~ dwish(DI[1:nf,1:nf],nf2s0); nf2s0<-nf+2",file="bikemodel.txt",sep="\n",append=TRUE)
    }
    if(mchoice4=="No"){  
      cat("# No Correlations between food type means:",file="bikemodel.txt",sep="\n",append=TRUE)
      cat("for(i in 1:nf){for(j in 1:nf){Ts0[i,j]<-equals(i,j)*ts0[i] }}",file="bikemodel.txt",sep="\n",append=TRUE)
      cat("for(i in 1:nf){ts0[i] ~ dgamma(1,1)}",file="bikemodel.txt",sep="\n",append=TRUE)
    }
  }
  
  if(nf==1){
    cat("Ts0[1,1] ~ dgamma(1,1)",file="bikemodel.txt",sep="\n",append=TRUE)
  }

  cat("# Consumption probability data (food consumed yes/no)",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("for(r in 1:nr){",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("# Consumption probabilities of foods are population parameters (not individual):",file="bikemodel.txt",sep="\n",append=TRUE)
  
  cat("for(j in 1:nf){",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("usedfoods[r,j] ~ dbern(p0[j])",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("}}",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("# Prior for consumption probability of each food type:",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("tp0<-1/2.71",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("for(j in 1:nf){",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("# Probability to consume food j at all:",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("logitp0[j] ~ dnorm(0,tp0)",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("p0[j] <-  exp(logitp0[j])/(1+exp(logitp0[j])) ",file="bikemodel.txt",sep="\n",append=TRUE)
  cat("}",file="bikemodel.txt",sep="\n",append=TRUE)
  

  if(nf>1){
    cat("# Unit matrix, for covariance matrix construction, if needed:",file="bikemodel.txt",sep="\n",append=TRUE)   
    cat("for(i in 1:nf){for(j in 1:nf){ DI[i,j]<-equals(i,j) }}",file="bikemodel.txt",sep="\n",append=TRUE)
  }    

cat("}",file="bikemodel.txt",sep="\n",append=TRUE)  


close(fileConn)

