
# Table 1:----

# generate results based on inputs from ui.R: 
# create data frame containing posterior predictive summaries


## ---- resultValues -------- 
table1 <- function(n_sim, input_modelchoice,input_modelchoice2,input_modelchoice3,input_modelchoice4,input_modelchoice5,
                   theresults, foodnamesused, nfused, foodindex, hazardnames, 
                   hazardnamesusedK,hazardnamesusedM, nhusedK, nhusedM, hazardindexK, hazardindexM,
                   Rall, Pall,nhK,nhM,nf,nexactK,nexactM,
                   mucK,mucM,mus0,muw,pK,pM,sigcK,sigcM,sigw,
                   logitp0,
                   Ss,Ss0,Sp
){
  # generate results based on inputs from ui.R: 
  # create data frame containing posterior predictive summaries----
  
  
  if(is.element("Concentrations",theresults)){
    
    DF <- data.frame(Results="No food-hazard selected")
    
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
      # redefine dimensions if scalars were returned from BUGS:
      
      
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
    } # end of if nhusedK nfused  
    
  } # end of concentrations
  
  if(is.element("Consumptions",theresults)|is.element("Exposures",theresults)){
    
    
    
    DF <- data.frame(Results="No food-hazard selected")
    
    if(nfused>0){
      
      if(nhusedK>0){ # formatting for posterior predictive distributions for chemical hazards
        logitpmc <- matrix(NA,n_sim,nf)
        pmc <- matrix(NA,n_sim,nf)
        musmc <- matrix(NA,n_sim,nf)
        EemcK <- array(0,dim=c(n_sim,nhusedK,nfused)) # default = 0 
        chronictotbwK <- matrix(NA,(n_sim),nhusedK)
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
      } # end of nhusedK
      if(nhusedM>0){ # formatting for posterior predictive distributions for microbial hazards
        logitpmc <- matrix(NA,n_sim,nf)
        pmc <- matrix(NA,n_sim,nf)
        musmc <- matrix(NA,n_sim,nf)
        wmc <- numeric()
        Umc <- matrix(NA,n_sim,nf)
        smc <- matrix(NA,n_sim,nf)
        Imc <- array(NA,dim=c(n_sim,nhM,nf))
        cmc <- array(NA,dim=c(n_sim,nhM,nf)) 
        EemcM <- array(0,dim=c(n_sim,nhusedM,nfused)) # default = 0    
        acutetotM <- matrix(NA,(n_sim),nhusedM)
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
      } # end of nhusedM
      
      if(nhusedK>0){ # simulate posterior predictive distributions for chemical hazards
        
        
        if(input_modelchoice == "Independent days"){
          
          #invTs <- array(NA,dim=c(n_sim,nf,nf)) 
          for(mc in 1:(n_sim)){
            
            # variances for consumptions:
            #invTs[mc,1:nf,1:nf] <- Ss[mc,1:nf,1:nf]
            
            
            if(nf>1){ # many foods
              if(input_modelchoice2=="Yes"){ # variability between users frequencies 
                logitpmc[mc,1:nf] <- rmvnorm(1,logitp0[mc,1:nf],Sp[mc,1:nf,1:nf])
                
              }
              if(input_modelchoice2=="No"){ # no variability between users frequencies
                logitpmc[mc,1:nf] <- logitp0[mc,1:nf]       
              }   
              pmc[mc,1:nf] <- exp(logitpmc[mc,1:nf])/(1+exp(logitpmc[mc,1:nf])) # individual use probability
              musmc[mc,1:nf] <- rmvnorm(1,mus0[mc,1:nf],Ss0[mc,1:nf,1:nf]) # individual mean log amount
              
            }
            if(nf==1){ # only one food
              if(input_modelchoice2=="Yes"){ # variability between users frequencies 
                logitpmc[mc,1] <- rnorm(1,logitp0[mc,1],sqrt(Sp[mc,1,1]))
              }
              if(input_modelchoice2=="No"){ # no variability between users frequencies
                logitpmc[mc,1] <- logitp0[mc,1]    
              }   
              pmc[mc,1] <- exp(logitpmc[mc,1])/(1+exp(logitpmc[mc,1])) # individual use probability
              musmc[mc,1] <- rnorm(1,mus0[mc,1],sqrt(Ss0[mc,1,1])) # individual mean log amount
            }
            
            for(h in 1:nhusedK){
              for(i in 1:nfused){
                if(nexactK[hazardindexK[h],foodindex[i]]>0){ # hazard-food is modeled
                  # chronic (mean) exposure for a random consumer, hazard h, food i:
                  
                  EemcK[mc,h,i] <- pK[mc,hazardindexK[h],foodindex[i]]*
                    PK[foodindex[i],hazardindexK[h]]*
                    pmc[mc,foodindex[i]]*
                    exp(logRK[foodindex[i],hazardindexK[h]]
                        +musmc[mc,foodindex[i]]
                        +0.5*Ss[mc,foodindex[i],foodindex[i]]
                        +mucK[mc,hazardindexK[h],foodindex[i]]
                        +0.5*sigcK[mc,hazardindexK[h],foodindex[i]]^2)    # invTs[] replaced with Ss
                }
              }
              chronictotbwK[mc,h] <- sum(EemcK[mc,h,1:nfused]) # sum over foods
            } # end of h
          } # end of mc
          
        } # end of independent days
        
        if(input_modelchoice == "Dependent days"){
          
          #invTs <- array(NA,dim=c(n_sim,nf,nf))
          p0 <- exp(logitp0)/(1+exp(logitp0))
          
          for(mc in 1:(n_sim)){
            
            # variances for consumptions:
            #invTs[mc,1:nf,1:nf] <- Ss[mc,1:nf,1:nf]
            
            if(nf>1){ # many foods
              musmc[mc,1:nf] <- rmvnorm(1,mus0[mc,1:nf],Ss0[mc,1:nf,1:nf]) # individual mean log amount 
              
            }
            if(nf==1){ # only one food
              musmc[mc,1] <- rnorm(1,mus0[mc,1],sqrt(Ss0[mc,1,1])) # individual mean log amount   
              
            }
            for(h in 1:nhusedK){
              for(i in 1:nfused){
                if(nexactK[hazardindexK[h],foodindex[i]]>0){ # hazard-food is modeled
                  # chronic (mean) exposure for a random consumer, hazard h, food i:
                  
                  EemcK[mc,h,i] <- pK[mc,hazardindexK[h],foodindex[i]]*
                    PK[foodindex[i],hazardindexK[h]]*
                    p0[mc,foodindex[i]]*
                    exp(logRK[foodindex[i],hazardindexK[h]]
                        +musmc[mc,foodindex[i]]
                        +0.5*Ss[mc,foodindex[i],foodindex[i]]
                        +mucK[mc,hazardindexK[h],foodindex[i]]
                        +0.5*sigcK[mc,hazardindexK[h],foodindex[i]]^2)  # invTs replaced with Ss  
                }
              }
              chronictotbwK[mc,h] <- sum(EemcK[mc,h,1:nfused]) # sum over foods
            } # end of h
          } # end of mc
          
        } # end of dependent days
        
      } # end of nhusedK 
      
      if(nhusedM>0){  # simulate posterior predictive distributions for microbial hazards
        
        
        if(input_modelchoice == "Independent days"){  
          
          #invTs <- array(NA,dim=c(n_sim,nf,nf))
          
          for(mc in 1:(n_sim)){
            
            # variances for consumptions:
            #invTs[mc,1:nf,1:nf] <- Ss[mc,1:nf,1:nf] 
            
            if(nf>1){  # many foods
              if(input_modelchoice2=="Yes"){ # variability between users frequencies
                logitpmc[mc,1:nf] <- rmvnorm(1,logitp0[mc,1:nf],Sp[mc,1:nf,1:nf])  
              }
              if(input_modelchoice2=="No"){ # no variability between users frequencies
                logitpmc[mc,1:nf] <- logitp0[mc,1:nf]   
              }   
              pmc[mc,1:nf] <- exp(logitpmc[mc,1:nf])/(1+exp(logitpmc[mc,1:nf])) # individual use probability
              musmc[mc,1:nf] <- rmvnorm(1,mus0[mc,1:nf],Ss0[mc,1:nf,1:nf])  
              
              if(input_modelchoice3=="No"){ # no correlation of serving sizes
                smc[mc,1:nf] <- exp(rmvnorm(1,musmc[mc,1:nf],diag(diag(Ss[mc,1:nf,1:nf])))) # actual random amount  
              }
              if(input_modelchoice3=="Yes"){
                smc[mc,1:nf] <- exp(rmvnorm(1,musmc[mc,1:nf],Ss[mc,1:nf,1:nf] )) # actual random amount  
              }
              
            } 
            
            if(nf==1){ # only one food
              if(input_modelchoice2=="Yes"){ # variability between users frequencies
                logitpmc[mc,1] <- rnorm(1,logitp0[mc,1],sqrt(Sp[mc,1,1]))
              }
              if(input_modelchoice2=="No"){ # no variability between users frequencies
                logitpmc[mc,1] <- logitp0[mc,1]     
              }   
              pmc[mc,1] <- exp(logitpmc[mc,1])/(1+exp(logitpmc[mc,1])) # individual use probability
              musmc[mc,1] <- rnorm(1,mus0[mc,1],sqrt(Ss0[mc,1,1])) # individual mean amount 
              
              smc[mc,1] <- rlnorm(1,musmc[mc,1],sqrt(Ss[mc,1,1])) # actual random amount   
            }
            
            wmc[mc] <- rlnorm(1,muw[mc],sigw[mc]) # bodyweight for random individual
            Umc[mc,1:nf] <- rbinom(nf,rep(1,nf),pmc[mc,1:nf]) # actual random use
            
            
            
            for(h in 1:nhM){
              Imc[mc,h,1:nf] <- rbinom(nf,rep(1,nf),pM[mc,h,1:nf]*PM[1:nf,h]) # actual contamination yes/no
              cmc[mc,h,1:nf] <- rlnorm(nf,mucM[mc,h,1:nf],sigcM[mc,h,1:nf]) # actual contamination level
            } 
            
            for(h in 1:nhusedM){ #Predict final count with poisson distribution:
              for(i in 1:nfused){
                # acute exposure for a random consumer, hazard h, food i:
                if(nexactM[hazardindexM[h],foodindex[i]]>0){ # hazard-food is modeled
                  EemcM[mc,h,i] <- Imc[mc,hazardindexM[h],foodindex[i]]*Umc[mc,foodindex[i]]*
                    smc[mc,foodindex[i]]*RM[foodindex[i],hazardindexM[h]]*
                    cmc[mc,hazardindexM[h],foodindex[i]]*wmc[mc]
                }
              }  
              # sum(exposure.acuteM[mc,hazardindexM[h],foodindex[1:nfused]])<=10000
              if(sum(EemcM[mc,h,1:nfused])<=5000){ # use Poisson when the mean is 'small'
                acutetotM[mc,h] <- rpois(1,sum(EemcM[mc,h,1:nfused])) 
              }
              # sum(exposure.acuteM[mc,hazardindexM[h],foodindex[1:nfused]])>10000
              if(sum(EemcM[mc,h,1:nfused])>5000){ # use rounded Normal when the mean is 'large'
                acutetotM[mc,h] <- round(rnorm(1,sum(EemcM[mc,h,1:nfused]),sqrt(sum(EemcM[mc,h,1:nfused])))) 
              }
            } # end of h
          } # end of mc
        } # end of independent days
        
        if(input_modelchoice == "Dependent days"){ 
          #invTs <- array(NA,dim=c(n_sim,nf,nf))
          p0 <- exp(logitp0)/(1+exp(logitp0)) 
          
          for(mc in 1:(n_sim)){
            
            # variances for consumptions:
            #invTs[mc,1:nf,1:nf] <- Ss[mc,1:nf,1:nf]
            
            if(nf>1){ # many foods
              musmc[mc,1:nf] <- rmvnorm(1,mus0[mc,1:nf],Ss0[mc,1:nf,1:nf])
              
              if(input_modelchoice3=="No"){ # no correlation of serving sizes
                smc[mc,1:nf] <- exp(rmvnorm(1,musmc[mc,1:nf],diag(diag(Ss[mc,1:nf,1:nf])))) # actual random amount 
              }
              if(input_modelchoice3=="Yes"){ # correlation of servings sizes
                smc[mc,1:nf] <- exp(rmvnorm(1,musmc[mc,1:nf],Ss[mc,1:nf,1:nf])) # actual random amount  
              }
                             
            }
            if(nf==1){ # only one food
              musmc[mc,1] <- rnorm(1,mus0[mc,1],sqrt(Ss0[mc,1,1])) # individual mean log amount
              smc[mc,1] <- rlnorm(1,musmc[mc,1],sqrt(Ss[mc,1,1]))    
            }
            wmc[mc] <- rlnorm(1,muw[mc],sigw[mc]) # bodyweight for random individual
            Umc[mc,1:nf] <- rbinom(nf,rep(1,nf),p0[mc,1:nf]) # actual random use
            
            
            for(h in 1:nhM){
              Imc[mc,h,1:nf] <- rbinom(nf,rep(1,nf),pM[mc,h,1:nf]*PM[1:nf,h]) # actual contamination yes/no
              cmc[mc,h,1:nf] <- rlnorm(nf,mucM[mc,h,1:nf],sigcM[mc,h,1:nf]) # actual contamination level
            } 
            
            for(h in 1:nhusedM){ #Predict final count with poisson distribution:
              for(i in 1:nfused){
                # acute exposure for a random consumer, hazard h, food i:
                if(nexactM[hazardindexM[h],foodindex[i]]>0){ # hazard-food is modeled
                  EemcM[mc,h,i] <- Imc[mc,hazardindexM[h],foodindex[i]]*Umc[mc,foodindex[i]]*
                    smc[mc,foodindex[i]]*RM[foodindex[i],hazardindexM[h]]*
                    cmc[mc,hazardindexM[h],foodindex[i]]*wmc[mc]
                }
              }  
              # sum(exposure.acuteM[mc,hazardindexM[h],foodindex[1:nfused]])<=10000
              if(sum(EemcM[mc,h,1:nfused])<=5000){ # use Poisson when the mean is 'small'
                acutetotM[mc,h] <- rpois(1,sum(EemcM[mc,h,1:nfused])) 
              }
              # sum(exposure.acuteM[mc,hazardindexM[h],foodindex[1:nfused]])>10000
              if(sum(EemcM[mc,h,1:nfused])>5000){ # use rounded Normal when the mean is 'large'
                acutetotM[mc,h] <- round(rnorm(1,sum(EemcM[mc,h,1:nfused]),sqrt(sum(EemcM[mc,h,1:nfused]))))
              }
            } # end of h
          } # end of mc
        } # end of dependent days
        
      } # end of nhusedM
      
      ############ Get posterior predictive results into data frames:  #################
      
      if(nhusedK>0){
        for(h in 1:nhusedK){ # posterior predictive summaries (quantiles) of individual exposures (chronic)
          hlo90totbwK[h] <- quantile(chronictotbwK[,h],0.05,names=FALSE)
          hmediantotbwK[h] <- quantile(chronictotbwK[,h],0.5,names=FALSE)
          hup90totbwK[h] <- quantile(chronictotbwK[,h],0.95,names=FALSE)
          hlo80totbwK[h] <- quantile(chronictotbwK[,h],0.10,names=FALSE)
          hup80totbwK[h] <- quantile(chronictotbwK[,h],0.90,names=FALSE)
          hlo98totbwK[h] <- quantile(chronictotbwK[,h],0.01,names=FALSE)
          hup98totbwK[h] <- quantile(chronictotbwK[,h],0.99,names=FALSE)
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
      
      if(is.element("Consumptions",theresults)){
        musmc <- matrix(NA,n_sim,nf)
        fconslo90bw <- numeric(nfused)
        fconsup90bw <- numeric(nfused)
        fconsmedianbw <- numeric(nfused)
        fconslo80bw <- numeric(nfused)
        fconsup80bw <- numeric(nfused)
        fconslo98bw <- numeric(nfused)
        fconsup98bw <- numeric(nfused)
        fconslo90 <- numeric(nfused)
        fconsup90 <- numeric(nfused)
        fconsmedian <- numeric(nfused)
        fconslo80 <- numeric(nfused)
        fconsup80 <- numeric(nfused)
        fconslo98 <- numeric(nfused)
        fconsup98 <- numeric(nfused)
        for(mc in 1:(n_sim)){
          if(nf>1){ # many foods
            musmc[mc,1:nf] <- rmvnorm(1,mus0[mc,1:nf],Ss0[mc,1:nf,1:nf])
          }
          if(nf==1){ # only one food
            musmc[mc,1] <- rnorm(1,mus0[mc,1],sqrt(Ss0[mc,1,1]))
          }
        }
        for(i in 1:nfused){ # posterior predictive summaries (quantiles) of individual chronic consumptions (/bw and absolute)
          Vs <- numeric() # variances
          for(mc in 1:(n_sim)){ # variances for consumptions
            Vs[mc] <-Ss[mc,foodindex[i],foodindex[i]]
          }
          
          fconslo90bw[i] <- quantile(exp(musmc[,foodindex[i]]+0.5*Vs ),0.05,names=FALSE)
          fconslo90[i] <- quantile(exp(musmc[,foodindex[i]]+muw+0.5*Vs+0.5*sigw^2),0.05,names=FALSE)
          fconsup90bw[i] <- quantile(exp(musmc[,foodindex[i]]+0.5*Vs ),0.95,names=FALSE)
          fconsup90[i] <- quantile(exp(musmc[,foodindex[i]]+muw+0.5*Vs+0.5*sigw^2),0.95,names=FALSE) 
          fconslo80bw[i] <- quantile(exp(musmc[,foodindex[i]]+0.5*Vs ),0.10,names=FALSE)
          fconslo80[i] <- quantile(exp(musmc[,foodindex[i]]+muw+0.5*Vs+0.5*sigw^2),0.10,names=FALSE)
          fconsup80bw[i] <- quantile(exp(musmc[,foodindex[i]]+0.5*Vs ),0.90,names=FALSE)
          fconsup80[i] <- quantile(exp(musmc[,foodindex[i]]+muw+0.5*Vs +0.5*sigw^2),0.90,names=FALSE)
          fconslo98bw[i] <- quantile(exp(musmc[,foodindex[i]]+0.5*Vs ),0.01,names=FALSE) 
          fconslo98[i] <- quantile(exp(musmc[,foodindex[i]]+muw+0.5*Vs +0.5*sigw^2),0.01,names=FALSE)
          fconsup98bw[i] <- quantile(exp(musmc[,foodindex[i]]+0.5*Vs ),0.99,names=FALSE) 
          fconsup98[i] <- quantile(exp(musmc[,foodindex[i]]+muw+0.5*Vs +0.5*sigw^2),0.99,names=FALSE)
          fconsmedianbw[i] <- quantile(exp(musmc[,foodindex[i]]+0.5*Vs ),0.5,names=FALSE)
          fconsmedian[i] <- quantile(exp(musmc[,foodindex[i]]+muw+0.5*Vs +0.5*sigw^2),0.5,names=FALSE)
        } # end of nfused
      } # end of if consumptions
      
      # Compose data frame for chemical exposure
      if(nhusedK>0){
        DF1K <- data.frame(
          
          Quantity_ = paste(hazardnamesusedK),
          Quantity = paste("total chronic exposure/bw"),
          Q01 = as.character(round(hlo98totbwK[1:nhusedK],2)),
          Q05 = as.character(round(hlo90totbwK[1:nhusedK],2)),
          Q10 = as.character(round(hlo80totbwK[1:nhusedK],2)),
          Median = as.character(round(hmediantotbwK[1:nhusedK],2)),
          Q90 = as.character(round(hup80totbwK[1:nhusedK],2)),
          Q95 = as.character(round(hup90totbwK[1:nhusedK],2)),
          Q99 = as.character(round(hup98totbwK[1:nhusedK],2)),
          stringsAsFactors=FALSE)
      }
      # Compose data frame for microbial exposure   
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
      
      # Compose data frame for consumptions   
      if(is.element("Consumptions",theresults)){   
        DF4 <- data.frame(
          
          Quantity_ = paste(foodnamesused),
          Quantity = paste("mean daily use/bw+"),
          Q01 = as.character(round(fconslo98bw[1:nfused],2)),
          Q05 = as.character(round(fconslo90bw[1:nfused],2)),
          Q10 = as.character(round(fconslo80bw[1:nfused],2)),
          Median = as.character(round(fconsmedianbw[1:nfused],2)),
          Q90 = as.character(round(fconsup80bw[1:nfused],2)),
          Q95 = as.character(round(fconsup90bw[1:nfused],2)),
          Q99 = as.character(round(fconsup98bw[1:nfused],2)),
          stringsAsFactors=FALSE)
        DF5 <- data.frame(
          
          Quantity_ = paste(foodnamesused),
          Quantity = paste("mean daily use+"),
          Q01 = as.character(round(fconslo98[1:nfused],2)),
          Q05 = as.character(round(fconslo90[1:nfused],2)),
          Q10 = as.character(round(fconslo80[1:nfused],2)),
          Median = as.character(round(fconsmedian[1:nfused],2)),
          Q90 = as.character(round(fconsup80[1:nfused],2)),
          Q95 = as.character(round(fconsup90[1:nfused],2)),
          Q99 = as.character(round(fconsup98[1:nfused],2)),
          stringsAsFactors=FALSE)
      }
      
    } # end of nfused>0
  } # end of consumption or exposure
  
  if(!is.element("Concentrations",theresults)){   
    if(is.element("Consumptions",theresults)&!is.element("Exposures",theresults)){
      DF <- rbind.data.frame(DF4,DF5)
    }
    if(is.element("Exposures",theresults)&!is.element("Consumptions",theresults)){
      if((nhusedK>0)&(nhusedM>0)){ DF <- rbind.data.frame(DF1K,DF1M) }
      if((nhusedK>0)&(nhusedM==0)){ DF <- rbind.data.frame(DF1K) }
      if((nhusedK==0)&(nhusedM>0)){ DF <- rbind.data.frame(DF1M) }
    }
    if(is.element("Exposures",theresults)&is.element("Consumptions",theresults)){
      if((nhusedK>0)&(nhusedM>0)){ DF <- rbind.data.frame(DF1K,DF1M,DF4,DF5) }
      if((nhusedK>0)&(nhusedM==0)){ DF <- rbind.data.frame(DF1K,DF4,DF5) }
      if((nhusedK==0)&(nhusedM>0)){ DF <- rbind.data.frame(DF1M,DF4,DF5) }
    }
  } # end of if !Concentrations
  if(is.element("Concentrations",theresults)){
    if((nhusedK>0)&(nhusedM==0)){DF <- rbind.data.frame(DFKconcentrations)}
    if((nhusedK==0)&(nhusedM>0)){DF <- rbind.data.frame(DFMconcentrations)}
    if((nhusedK>0)&(nhusedM>0)){DF <- rbind.data.frame(DFKconcentrations,DFMconcentrations)}
    
    if(is.element("Consumptions",theresults)&!is.element("Exposures",theresults)){
      DF <- rbind.data.frame(DF,DF4,DF5)
    }
    if(is.element("Exposures",theresults)&!is.element("Consumptions",theresults)){
      if((nhusedK>0)&(nhusedM>0)){ DF <- rbind.data.frame(DF,DF1K,DF1M) }
      if((nhusedK>0)&(nhusedM==0)){ DF <- rbind.data.frame(DF,DF1K) }
      if((nhusedK==0)&(nhusedM>0)){ DF <- rbind.data.frame(DF,DF1M) }
    }
    if(is.element("Exposures",theresults)&is.element("Consumptions",theresults)){
      if((nhusedK>0)&(nhusedM>0)){ DF <- rbind.data.frame(DF,DF1K,DF1M,DF4,DF5) }
      if((nhusedK>0)&(nhusedM==0)){ DF <- rbind.data.frame(DF,DF1K,DF4,DF5) }
      if((nhusedK==0)&(nhusedM>0)){ DF <- rbind.data.frame(DF,DF1M,DF4,DF5) }
    }
  } # end of if Concentrations
  
  
  DF # results collected as data frame
  
}



# Table 2: ----

## ---- resultProbs -------- # mus0,ppred
table2 <- function(n_sim, input_modelchoice,input_modelchoice2,foodnamesused,nfused,foodindex,hazardnames,
                   hazardnamesused,hazardtypesused,hazardnamesK,hazardnamesM,
                   hazardnamesusedK,hazardnamesusedM,nhusedK,nhusedM,hazardindexK,hazardindexM,
                   Rall,Pall,nhK,nhM,nf,nexactK,nexactM,limitexpoK,limitexpoM,
                   mus0,mucK,mucM,sigcK,sigcM,pK,pM,logitp0,muw,sigw,
                   Ss,Ss0,Sp 
){
  # generate results based on inputs from ui.R: 
  # create data frame containing exposure limit analysis table----
  
  DFplim <- data.frame()
  DF95 <- data.frame()
  
  
  
  
  # Chemical exposures
  if((nhusedK>0)&(nfused>0)){
    # redefine dimensions if scalars were returned from BUGS:
    
    RK = matrix(NA,nf,nhK) # factors for concentrations
    RK[1:nf,1:nhK] = Rall[1:nf,is.element(hazardnames,hazardnamesusedK)]
    logRK = log(RK)
    PK = matrix(NA,nf,nhK) # factors for prevalence
    PK[1:nf,1:nhK] = Pall[1:nf,is.element(hazardnames,hazardnamesusedK)]
    
    for(h in 1:nhusedK){
      for(i in 1:nfused){
        if(nexactK[hazardindexK[h],foodindex[i]]>0){ # this hazard-food is modeled
          
          # numerical results:
          
          # chronic exposure (hazard i, food j) over all days, all servings (including zeros):
          V <- 5000 # variability simulations
          logitpconsume <- matrix(NA,nf,V)
          pconsume <- matrix(NA,nf,V)
          cmeanpos <- numeric()
          qutotal95 <- numeric()
          punderlimitposK <- numeric()
          punderlimitallK <- numeric()
          
          Vs <- numeric() # variances
          Vs0 <- numeric() # variances
          p0 <- exp(logitp0)/(1+exp(logitp0))
          
          for(u in 1:(n_sim)){ 
            # simulate variability for V individuals, 
            # per each uncertain parameter:
            Vs[u] <- Ss[u,foodindex[i],foodindex[i]] 
            Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]] 
            
            if(input_modelchoice=="Independent days"){
              if(input_modelchoice2 =="Yes"){
                logitpconsume[foodindex[i],1:V] <- rnorm(V,logitp0[u,foodindex[i]],sqrt(Sp[u,foodindex[i],foodindex[i]]))
              }
              if(input_modelchoice2 == "No"){
                logitpconsume[foodindex[i],1:V] <- rep(logitp0[u,foodindex[i]],V)   
              }
              pconsume[foodindex[i],1:V] <- exp(logitpconsume[foodindex[i],1:V])/(1+exp(logitpconsume[foodindex[i],1:V]))  
            }
            if(input_modelchoice=="Dependent days"){
              pconsume[foodindex[i],1:V] <- rep(p0[u,foodindex[i]],V)
            }
            
            fornormK <- logRK[foodindex[i],hazardindexK[h]]+
              mus0[u,foodindex[i]]+
              0.5*Vs[u]+
              mucK[u,hazardindexK[h],foodindex[i]]+
              0.5*sigcK[u,hazardindexK[h],foodindex[i]]^2
            
            cmeanpos[1:V] <- rlnorm(V,fornormK,
                                    sqrt(Vs0[u]) )  
            qutotal95[u]<-quantile(
              pconsume[foodindex[i],1:V]*
                pK[u,hazardindexK[h],foodindex[i]]*
                PK[foodindex[i],hazardindexK[h]]*
                cmeanpos[1:V],0.95,names=FALSE)
            
            plnormK <- plnorm(limitexpoK[h],fornormK,
                              sqrt(Vs0[u]) )
            
            meanK <- mean(pconsume[foodindex[i],1:V])
            
            # Monte Carlo estimate for P(exposure < limit) over variability dimension:
            # all days: (two possible cases: (1-P(contamination,use))*1 + P(contamination,use)*F(limit))
            punderlimitallK[u] <- 
              (1-meanK*
                 PK[foodindex[i],hazardindexK[h]]*
                 pK[u,hazardindexK[h],foodindex[i]])*1+
              meanK*
              PK[foodindex[i],hazardindexK[h]]*
              pK[u,hazardindexK[h],foodindex[i]]*  
              plnormK
            # contaminated consumptions: (one possible case: F(limit) cumulative prob.)
            punderlimitposK[u] <- plnormK
            
          } # end of for u
          
          
          # get the probability (of not exceeding limit) into data frame:
          DFplimposK <- data.frame(Quantity = paste0("P(chronic<",limitexpoK[h], ")"),
                                   From = paste0("pos days"),
                                   Hazard = paste0(hazardnamesusedK[h]),
                                   Food = paste0(foodnamesused[i]),
                                   # Limit = paste0("<",limitexpoK[h]),
                                   #Quantity = paste0("P(chronic ",hazardnamesusedK[h]," from pos days, ",foodnamesused[i],", <",limitexpoK[h],")"),
                                   Q05 = as.character(round(quantile(punderlimitposK,c(0.05),names=FALSE,na.rm=TRUE),2)),
                                   Q50 = as.character(round(quantile(punderlimitposK,c(0.5),names=FALSE,na.rm=TRUE),2)),
                                   Q95 = as.character(round(quantile(punderlimitposK,c(0.95),names=FALSE,na.rm=TRUE),2)),
                                   stringsAsFactors=FALSE)
          DFplimallK <- data.frame(Quantity = paste0("P(chronic<",limitexpoK[h], ")"),
                                   From = paste0("all days"),
                                   Hazard = paste0(hazardnamesusedK[h]),
                                   Food = paste0(foodnamesused[i]),
                                   #  Limit = paste0("<",limitexpoK[h]),
                                   #Quantity = paste0("P(chronic ",hazardnamesusedK[h]," from all days, ",foodnamesused[i],", <",limitexpoK[h],")"),
                                   Q05 = as.character(round(quantile(punderlimitallK,c(0.05),names=FALSE,na.rm=TRUE),2)),
                                   Q50 = as.character(round(quantile(punderlimitallK,c(0.5),names=FALSE,na.rm=TRUE),2)),
                                   Q95 = as.character(round(quantile(punderlimitallK,c(0.95),names=FALSE,na.rm=TRUE),2)),
                                   stringsAsFactors=FALSE)
          DFplim <- rbind.data.frame(DFplim,DFplimallK,DFplimposK)
          
          DF95allK <- data.frame(Quantity = paste0("Q95(chronic)"), 
                                 From = paste0("all days"),
                                 Hazard = paste0(hazardnamesusedK[h]),
                                 Food = paste0(foodnamesused[i]),
                                 # Limit = paste0(""),
                                 #Quantity = paste0("Q95 chronic ",hazardnamesusedK[h]," from all days, ",foodnamesused[i]), 
                                 Q05 = as.character(round(quantile(qutotal95,0.05,names=FALSE,na.rm=TRUE),2)),
                                 Q50 = as.character(round(quantile(qutotal95,0.5,names=FALSE,na.rm=TRUE),2)),
                                 Q95 = as.character(round(quantile(qutotal95,0.95,names=FALSE,na.rm=TRUE),2)), 
                                 stringsAsFactors=FALSE)   
          
          # positive chronic exposures, 
          # posterior quantiles of variability 95% quantile (contaminated consumptions only):
          qu95_50 <- round(quantile(qlnorm(0.95,logRK[foodindex[i],hazardindexK[h]]
                                           +mus0[,foodindex[i]]
                                           +0.5*Vs
                                           +mucK[,hazardindexK[h],foodindex[i]]
                                           +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                           sqrt(Vs0) ),
                                    0.5,names=FALSE,na.rm=TRUE),3)
          qu95_95 <- round(quantile(qlnorm(0.95,logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                           +0.5*Vs
                                           +mucK[,hazardindexK[h],foodindex[i]]
                                           +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                           sqrt(Vs0) ),
                                    0.95,names=FALSE,na.rm=TRUE),3)
          qu95_05 <- round(quantile(qlnorm(0.95,logRK[foodindex[i],hazardindexK[h]]+mus0[,foodindex[i]]
                                           +0.5*Vs
                                           +mucK[,hazardindexK[h],foodindex[i]]
                                           +0.5*sigcK[,hazardindexK[h],foodindex[i]]^2,
                                           sqrt(Vs0) ),
                                    0.05,names=FALSE,na.rm=TRUE),3)
          DF95posK <- data.frame(Quantity = paste0("Q95(chronic)"),
                                 From = paste0("pos days"),
                                 Hazard = paste0(hazardnamesusedK[h]),
                                 Food = paste0(foodnamesused[i]),
                                 # Limit = paste0(""),
                                 #Quantity = paste0("Q95 chronic ",hazardnamesusedK[h]," from pos days, ",foodnamesused[i]),
                                 Q05 = as.character(round(qu95_05,2)),
                                 Q50 = as.character(round(qu95_50,2)),
                                 Q95 = as.character(round(qu95_95,2)),
                                 stringsAsFactors=FALSE)
          
          DF95 <- rbind.data.frame(DF95,DF95allK,DF95posK)
        } # end of if hazard-food is modeled    
      }} # end of for nhused nfused
    
  } # (nhusedK>0)&(nfused>0) chemical exposures
  
  # Microbial exposures:  
  
  if((nhusedM>0)&(nfused>0)){
    # redefine dimensions if scalars were returned from BUGS:
    
    RM = matrix(NA,nf,nhM) # factors for concentration
    RM[1:nf,1:nhM] = Rall[1:nf,is.element(hazardnames,hazardnamesusedM)]
    logRM = log(RM)
    PM = matrix(NA,nf,nhM) # factors for prevalence
    PM[1:nf,1:nhM] = Pall[1:nf,is.element(hazardnames,hazardnamesusedM)]
    
    for(h in 1:nhusedM){
      for(i in 1:nfused){
        if(nexactM[hazardindexM[h],foodindex[i]]>0){ # this hazard-food is modeled
          
          # numerical results:
          
          # chronic exposure (hazard i, food j) over all days, all servings (including zeros):
          V <- 5000 # variability simulations
          logitpconsume <- matrix(NA,nf,V)
          pconsume <- matrix(NA,nf,V)
          cmeanpos <- numeric()
          qutotal95 <- numeric()
          punderlimitposM <- numeric()
          punderlimitallM <- numeric()
          
          Vs <- numeric() # variances
          Vs0 <- numeric() # variances
          p0 <- exp(logitp0)/(1+exp(logitp0)) 
          
          for(u in 1:(n_sim)){ 
            # simulate variability for V individuals, 
            # per each uncertain parameter:
            Vs[u] <- Ss[u,foodindex[i],foodindex[i]]
            Vs0[u] <- Ss0[u,foodindex[i],foodindex[i]]
            
            if(input_modelchoice=="Independent days"){
              if(input_modelchoice2 =="Yes"){
                logitpconsume[foodindex[i],1:V] <- rnorm(V,logitp0[u,foodindex[i]],sqrt(Sp[u,foodindex[i],foodindex[i]]))
              }
              if(input_modelchoice2 == "No"){
                logitpconsume[foodindex[i],1:V] <- rep(logitp0[u,foodindex[i]],V)   
              }
              pconsume[foodindex[i],1:V] <- exp(logitpconsume[foodindex[i],1:V])/(1+exp(logitpconsume[foodindex[i],1:V]))  
            }
            if(input_modelchoice=="Dependent days"){
              pconsume[foodindex[i],1:V] <- rep(p0[u,foodindex[i]],V)
            }
            
            meanM <- mean(pconsume[foodindex[i],1:V])
            
            # evaluate the 95% quantile of the exposure distribution including all days 
            # (not only positively contaminated consumptions), i.e. zero inflated distribution
            PPOSM <- meanM*
              pM[u,hazardindexM[h],foodindex[i]]*
              PM[foodindex[i],hazardindexM[h]]
            if(0.95<=(1-PPOSM)){qutotal95[u]<-0}
            if(0.95>(1-PPOSM)){
              qutotal95[u]<- qlnorm((0.95-1+PPOSM)/PPOSM,logRM[foodindex[i],hazardindexM[h]]
                                    +mus0[u,foodindex[i]]
                                    +mucM[u,hazardindexM[h],foodindex[i]]
                                    +muw[u],
                                    sqrt(Vs[u]
                                         +Vs0[u]
                                         +sigcM[u,hazardindexM[h],foodindex[i]]^2
                                         +sigw[u]^2))
            }
            
            plnormM <- plnorm(limitexpoM[h],logRM[foodindex[i],hazardindexM[h]]
                              +mus0[u,foodindex[i]]
                              +mucM[u,hazardindexM[h],foodindex[i]]
                              +muw[u],
                              sqrt(Vs[u]
                                   +Vs0[u] 
                                   +sigcM[u,hazardindexM[h],foodindex[i]]^2
                                   +sigw[u]^2))
            
            # Monte Carlo estimate for P(exposure < limit) over variability dimension:
            # all days: (two possible cases: P(no contamination)*1 + P(contamination)*F(limit))
            punderlimitallM[u] <- 
              (1-meanM*
                 PM[foodindex[i],hazardindexM[h]]*
                 pM[u,hazardindexM[h],foodindex[i]])*1+
              meanM*
              PM[foodindex[i],hazardindexM[h]]*
              pM[u,hazardindexM[h],foodindex[i]]*
              plnormM 
            # contaminated consumptions: (one possible case: F(limit) cumulative prob.)
            # from lnorm-variability of single microbial exposures ('poisson means') 
            punderlimitposM[u] <- plnormM 
            
          } # end of for u
          qu95_05 <- round(quantile(qlnorm(0.95,logRM[foodindex[i],hazardindexM[h]]
                                           +mus0[,foodindex[i]]
                                           +mucM[,hazardindexM[h],foodindex[i]]
                                           +muw,
                                           sqrt(Vs
                                                +Vs0
                                                +sigcM[,hazardindexM[h],foodindex[i]]^2
                                                +sigw^2)),0.05,names=FALSE,na.rm=TRUE),3)
          qu95_50 <- round(quantile(qlnorm(0.95,logRM[foodindex[i],hazardindexM[h]]
                                           +mus0[,foodindex[i]]
                                           +mucM[,hazardindexM[h],foodindex[i]]
                                           +muw,
                                           sqrt(Vs
                                                +Vs0
                                                +sigcM[,hazardindexM[h],foodindex[i]]^2
                                                +sigw^2)),0.5,names=FALSE,na.rm=TRUE),3)
          qu95_95 <- round(quantile(qlnorm(0.95,logRM[foodindex[i],hazardindexM[h]]
                                           +mus0[,foodindex[i]]
                                           +mucM[,hazardindexM[h],foodindex[i]]
                                           +muw,
                                           sqrt(Vs
                                                +Vs0
                                                +sigcM[,hazardindexM[h],foodindex[i]]^2
                                                +sigw^2)),0.95,names=FALSE,na.rm=TRUE),3)
          
          # get the probability (of not exceeding limit) into data frame:
          DFplimposM <- data.frame(Quantity = paste0("P(acute<",limitexpoM[h], ")"),
                                   From = paste0("pos days"),
                                   Hazard = paste0(hazardnamesusedM[h]),
                                   Food = paste0(foodnamesused[i]),
                                   # Limit = paste0("<",limitexpoM[h]),
                                   #Quantity = paste0("P(acute ",hazardnamesusedM[h]," from pos days, ",foodnamesused[i],", <",limitexpoM[h],")"),
                                   Q05 = as.character(round(quantile(punderlimitposM,c(0.05),names=FALSE,na.rm=TRUE),2)),
                                   Q50 = as.character(round(quantile(punderlimitposM,c(0.5),names=FALSE,na.rm=TRUE),2)),
                                   Q95 = as.character(round(quantile(punderlimitposM,c(0.95),names=FALSE,na.rm=TRUE),2)),
                                   stringsAsFactors=FALSE)
          DFplimallM <- data.frame(Quantity = paste0("P(acute<",limitexpoM[h], ")"),
                                   From = paste0("all days"),
                                   Hazard = paste0(hazardnamesusedM[h]),
                                   Food = paste0(foodnamesused[i]),
                                   # Limit = paste0("<",limitexpoM[h]),
                                   #Quantity = paste0("P(acute ",hazardnamesusedM[h]," from all days, ",foodnamesused[i],", <",limitexpoM[h],")"),
                                   Q05 = as.character(round(quantile(punderlimitallM,c(0.05),names=FALSE,na.rm=TRUE),2)),
                                   Q50 = as.character(round(quantile(punderlimitallM,c(0.5),names=FALSE,na.rm=TRUE),2)),
                                   Q95 = as.character(round(quantile(punderlimitallM,c(0.95),names=FALSE,na.rm=TRUE),2)),
                                   stringsAsFactors=FALSE)
          DFplim <- rbind.data.frame(DFplim,DFplimallM,DFplimposM)
          
          DF95allM <- data.frame(Quantity = paste0("Q95(acute)"), 
                                 From = paste0("all days"),
                                 Hazard = paste0(hazardnamesusedM[h]),
                                 Food = paste0(foodnamesused[i]),
                                 # Limit = paste0(""),
                                 #Quantity = paste0("Q95 acute ",hazardnamesusedM[h]," from all days, ",foodnamesused[i]), 
                                 Q05 = as.character(round(quantile(qutotal95,0.05,names=FALSE,na.rm=TRUE),2)),
                                 Q50 = as.character(round(quantile(qutotal95,0.5,names=FALSE,na.rm=TRUE),2)),
                                 Q95 = as.character(round(quantile(qutotal95,0.95,names=FALSE,na.rm=TRUE),2)), 
                                 stringsAsFactors=FALSE)
          
          # positive acute ('poisson mean') exposures, 
          # posterior quantiles of 95% variability quantile:
          DF95posM <- data.frame(Quantity = paste0("Q95(acute)"), 
                                 From = paste0("pos days"),
                                 Hazard = paste0(hazardnamesusedM[h]),
                                 Food = paste0(foodnamesused[i]),
                                 # Limit = paste0(""),
                                 #Quantity = paste0("Q95 acute ",hazardnamesusedM[h]," from pos days, ",foodnamesused[i]), 
                                 Q05 = as.character(round(qu95_05,2)),
                                 Q50 = as.character(round(qu95_50,2)),
                                 Q95 = as.character(round(qu95_95,2)), 
                                 stringsAsFactors=FALSE)
          
          DF95 <- rbind.data.frame(DF95,DF95allM,DF95posM)
          
        } # end of if hazard-food is modeled   
      }} # end of for nhused nfused
  } # (nhusedM>0)&(nfused>0) microbial exposures
  
  rbind(DFplim,DF95) # results collected as data frame
  
  
} # end of reactive


