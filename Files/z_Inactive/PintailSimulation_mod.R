##################  Originally developed by Joana Bieri and Christine Sample,
## NETWORK CODE ##    converted into a function by B. Mattsson to be called by ecosystem_service_flows.r within R project 
##################
#library(XLConnect)

#########################################
### SET SPECEIS SPECIFIC NETWORK INFO ###
#########################################

### PINTAIL EXAMPLE ###

# IMPORTANT NOTE #
# Users should specify network model functions f_(i,t), p_(ij,t), and s_(ij,t) in the file:
#           NetworkFunctions.R, even if these are constants they must still be set as such.

### User specified information specific to the simulation ###

SIMNAME <- scenarioName # Specifies which baseline folder, will be a function argument, during development specify in ecosystem_service_flows.r
# This is the subfoulder where the network data, outputs, and .RData file should are saved
# This is also the subfoulder where the network input files are stored: 
                        #   "./SIMNAME/network_inputs_NETNAME.xlsx

NETNAME <- c("adult_female", "adult_male", "juvenile_female", "juvenile_male") # Give a distinct name for each class as used in input files
# Order is important here we would index [[1]] = class 1 and [[2]] = class 2 in alpha and beta.

ERR <- .01 # Error tolerance for convergence. 
# To test convergence, we compare total population of all classes in the current season
# to the matching season from the previous year.

seasons <- 3 # Number of seasons or steps in one annul cycle. 
# This must match number of spreadsheets in input files

num_nodes <- 5 # Number of nodes in the network
# This must match the number of initial conditions given in input files

tmax <- 301 # Maximum number of steps to take - assume non convergence if t=tmax

OUTPUTS <- TRUE # TRUE = Process final outputs, FALSE = Do not process just run the sumulation.

## For debugging your model equations ##
SILENT <- TRUE # TRUE = Do not print data to console - silence outputs.
                # FALSE = Print population data and network function data to the Console for debugging.




### Users should not need to interact with the code below ###

################
## SIMULATION ##
################

# Set the working directory, only needed if working outside of R project containing required folders and scripts in main folder 
    # -> may need additional troubleshooting to run outside such an R project
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

# Set location of source code if working outside R project
#netcode <- c("../NetworkCode1.1/") # identifies folder containing the code, which is in the parent of the current working directory

# Set location of source code if working in R project
netcode <- c("NetworkCode1.1/") # identifies folder containing the code, which is in the parent of the current working directory

# Clear the workspace reserving needed network input variables
base_variables <- c("seasons", "num_nodes", "NETNAME", "tmax", "SIMNAME", "ERR", "OUTPUTS", "SILENT","netcode","base_variables")
rm(list=setdiff(ls(), base_variables))

### SET UP THE NETWORK(S) ###
source(paste(netcode,"NetworkSetup.R",sep="")) # should finish running in a few seconds

### RUN THE POPULATION SIMULATION ###
print(paste("Running", SIMNAME, sep=" "))
source(paste(netcode,"NetworkSimulation.R",sep=""))

########################  
###  PROCESS OUTPUTS ###
########################

if (OUTPUTS == T){
  source(paste(netcode,"NetworkOutputs.R",sep=""))
}

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




######################
### SAVE THE DATA ####
######################

save.image(file=paste(SIMNAME,"/",SIMNAME, ".RData", sep = ""))

