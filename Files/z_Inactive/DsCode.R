####################################
##  Ds Calculations for PINTAILS  ##
####################################

## Calculates DS using Data from running PintailSimulation.R
## You must first source PintailSimulation.R before running this code

WEIGHT <- 0
numSex <- 2 # Calculate Cr separately for males and females
DSind <- array(0,c(num_nodes,seasons,numSex))

#Choose which type (male or female) of adult to calculate Cr
for (sex in 1:numSex){
  
  # Get Cr Values
  source("Cr_pintail.R")
  
  start <- timestep-seasons
  end <- timestep-1
  CrCount <- 1
  for(t in (start:end)){
    if(sex==1){##Females
      POP<- matrix(unlist((N[[t]]$adult_female)))+ matrix(unlist((N[[t]]$juvenile_female)))
    }
    if(sex==2){##Males
      POP<- matrix(unlist((N[[t]]$adult_male)))+ matrix(unlist((N[[t]]$juvenile_male)))
    }

    for(i in (1:num_nodes)){
      # Unweighted Ds Values Cr * Population
      DSind[i,CrCount,sex] <- CR[i,CrCount]*POP[i]
    }
    CrCount <- CrCount + 1
  }
  # Weighing factor so final counts sum to one
  WEIGHT <- WEIGHT + colSums(DSind[,,sex])
} 


for (sex in 1:numSex){ 
  for (i in (1:seasons)){
    for(j in (1:num_nodes)){
      DSind[j,i,sex] <- DSind[j,i,sex]/WEIGHT[i]
    }}}

cat("Seasonal Proportional Dependence:\n Female: \n")
cat("     Breeding   Winter   Spring Stop\n")
print(DSind[,,1])

cat("Seasonal Proportional Dependence:\n Male: \n")
cat("     Breeding   Winter   Spring Stop\n")
print(DSind[,,2])


cat("\n\n Annual Average Proportional Dependence:\n")
DS <- matrix(rowSums(DSind)/3,nrow=num_nodes)
print(DS)


