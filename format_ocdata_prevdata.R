# FormatData----



foodnames<-names(ocdata[4:dim(ocdata)[2]])
hazardtypes <- ocdata$hazardtypes
hazardnames <- ocdata$hazardnames
hazardnamesK <- hazardnames[hazardtypes=="K"] # names of chemical hazards
hazardnamesM <- hazardnames[hazardtypes=="M"] # names of microbiological hazards
nf <- length(foodnames)   # Calculate the number of foods
nh <- length(hazardnames) # Calculate the number of hazards
nhK <- sum(hazardtypes=="K") # number of chemical hazards
nhM <- sum(hazardtypes=="M") # number of microbiological hazards

#limitexpo <- ocdata$limitexpo



# ocdata----
if(sum(hazardtypes=="M")>0){
  # Occurrence Table of Microbials consists of marks "all" or "positives" to denote data type:
  OTM<-matrix(NA,sum(hazardtypes=="M"),length(foodnames))  
  OTM[1:sum(hazardtypes=="M"),1:length(foodnames)]<-as.matrix(ocdata[hazardtypes=="M",4:length(ocdata[1,])])  
  #limitexpoM <- numeric()
  #limitexpoM <- as.numeric(limitexpo[hazardtypes=="M"])  
  
} 
if(sum(hazardtypes=="K")>0){
  # Occurrence Table of Chemicals consists of marks "all" or "positives" to denote data type:
  OTK<-matrix(NA,sum(hazardtypes=="K"),length(foodnames))  
  OTK[1:sum(hazardtypes=="K"),1:length(foodnames)]<-as.matrix(ocdata[hazardtypes=="K",4:length(ocdata[1,])]) 
  #limitexpoK <- numeric()
  #limitexpoK <- as.numeric(limitexpo[hazardtypes=="K"])  
  
}
 

# prevdata----
## prevalence data for microbials:----
nposM <- matrix(NA,sum(hazardtypes=="M"),length(foodnames))
nsamM <- matrix(NA,sum(hazardtypes=="M"),length(foodnames))
for(i in 1:dim(prevdata)[1]){
  nposM[which(prevdata[i,1]==hazardnamesM),which(prevdata[i,3]==foodnames)] <- prevdata[i,4]
  nsamM[which(prevdata[i,1]==hazardnamesM),which(prevdata[i,3]==foodnames)] <- prevdata[i,5]
}
## prevalence data for chemicals:----
nposK <- matrix(NA,sum(hazardtypes=="K"),length(foodnames))
nsamK <- matrix(NA,sum(hazardtypes=="K"),length(foodnames))
for(i in 1:dim(prevdata)[1]){
  nposK[which(prevdata[i,1]==hazardnamesK),which(prevdata[i,3]==foodnames)] <- prevdata[i,4]
  nsamK[which(prevdata[i,1]==hazardnamesK),which(prevdata[i,3]==foodnames)] <- prevdata[i,5]
}