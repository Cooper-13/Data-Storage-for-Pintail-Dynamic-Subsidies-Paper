##################
## NETWORK CODE ##
##################
library(XLConnect)

#########################################
### SET SPECEIS SPECIFIC NETWORK INFO ###
#########################################

### PINTAIL EXAMPLE ###

# IMPORTANT NOTE #
# Users should specify network model functions f_(i,t), p_(ij,t), and s_(ij,t) in the file:
#           NetworkFunctions.R, even if these are constants they must still be set as such.

### User specified information specific to the simulation ###

SIMNAME <- "Baseline1" # Specifies which baseline folder 
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

# Set the working directory
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Set location of source code
netcode <- c("../NetworkCode1.1/") # identifies folder containing the code, which is in the parent of the current working directory
# Clear the workspace reserving needed network input variables
base_variables <- c("seasons", "num_nodes", "NETNAME", "tmax", "SIMNAME", "ERR", "OUTPUTS", "SILENT","netcode","base_variables")
rm(list=setdiff(ls(), base_variables))

### SET UP THE NETWORK(S) ###
source(paste(netcode,"NetworkSetup.R",sep=""))

### RUN THE BASELINE SIMULATION ###
print(paste("Running", SIMNAME, sep=" "))
source(paste(netcode,"NetworkSimulation.R",sep=""))

########################  
###  PROCESS OUTPUTS ###
########################

if (OUTPUTS == T){
  source(paste(netcode,"NetworkOutputs.R",sep=""))
}

######################
### SAVE THE DATA ####
######################

save.image(file=paste(SIMNAME,"/",SIMNAME, ".RData", sep = ""))

