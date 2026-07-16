# Front matter ####
#  Originally developed by Joana Bieri and Christine Sample,
#  B. Mattsson made the following changes:  1) combined all subscripts (originally sourced) into this script
#                                             a) removed density-dependent reproduction within breeding areas via f_function
#                                             b) merged Cr_pintail.r and CR_ONEEQ.r into Cr_fn
#                                           2) converted into function then to be called by ecosystem_service_flows.r within R project 
#                                           3) accordingly updated & streamlined headings for easier navigation

# Set the working directory, only needed if working outside of R project containing required folders and scripts in main folder 
# -> may need additional troubleshooting to run outside such an R project
#this.dir <- dirname(parent.frame(2)$ofile)
#setwd(this.dir)

# clear workspace except for chosen objects and functions
#rm(list = ls()) # removes all objects
base_variables <- c("scenarioName")
rm(list = setdiff(ls(), c(base_variables, lsf.str())))

# Specify demographic functions for pintails ####
#source("SpeciesFunctions.R")

## CALCULATE f -> NODE DYNAMICS UPDATE #################

f_function <- function() {
  # TYPICAL VARIABLES USED (N,alpha,type,ind,t)
  # N - contains the population arriving at the node at all previous time steps - N[["time_step"]]$"class_name"
  # NOTE if you specify "class_name" the it will look to that specific class's population each time
  # NOTE if you set "class_name"=[[type]] it will get the numbers for the current calculation's class
  # alpha - contains all information about node dynamics - alpha[["class_number"]][["season"]]$"variable_name"
  # type - gives the class number as an integer - the class types are defined in NETNAME 
  # NOTE if NETNAME <- c("seeds","plants") then type 1 = seeds and type 2 = plants -> BM writes: wrong comment here
  # ind - gives the season number as an integer
  # t - gives the time step
  
  # Initiaize basic update information ####
  Repro <- 0*N[[t]][[type]]
  Survi <- alpha[[type]][[ind]]$S*N[[t]][[type]]
  Trans <- 0*N[[t]][[type]]
  
  # print("Pre")
  # print(paste("t=",t,"type=",type))
  # print(Repro)
  # print(Survi)
  # print(Trans)
  
  
  # Breeding Nodes ####
  if(ind==1){
    if(type==3 || type==4){ # If we are updating the Juvenile population
      
      POP <- N[[t]][[1]]+N[[t]][[2]] # Number of existing Males and Females for density dependence
      Survi_F <- alpha[[1]][[ind]]$S # Female survival Rate
      
      R <- alpha[[1]][[ind]]$R # only adult females can produce young during breeding
      
      Repro <- R*Survi_F*N[[t]][[1]]
      
    } 
  }
  
  # Winter Nodes ####
    # Density dependent node survival rates only apply to the post harvest winter-spring season
  if(ind==2){
    
    beta0 <- alpha[[type]][[ind]]$b_0
    beta1 <- alpha[[type]][[ind]]$b_1
    smax <- alpha[[type]][[ind]]$S_max
    smin <- alpha[[type]][[ind]]$S_min
    POP <- N[[t]][[1]]+N[[t]][[2]]+N[[t]][[3]]+N[[t]][[4]]
    
    Z <- beta0+beta1*POP 
    Survi <- (smin+((smax)-(smin))/(1+exp(-Z)))*N[[t]][[type]]
    
    
    # Transition Rates Juvenile -> Adult in Winter/Spring
    if(type==3 || type==4){ # All Juveniles must transition.
      Survi <- 0*N[[t]][[type]]
      Repro <- 0*N[[t]][[type]]
      Trans <- 0*N[[t]][[type]]
    }
    if(type==1){
      Survi_J <- (smin+((smax)-(smin))/(1+exp(-Z)))
      Trans <- Survi_J*N[[t]][[3]] # Transition the Juveniles to Adult Females
    }
    if(type==2){
      Survi_J <- (smin+((smax)-(smin))/(1+exp(-Z)))
      Trans <- Survi_J*N[[t]][[4]] # Transition the Juveniles to Adult Males
    }
  }
  
  f_new <- Survi + Repro + Trans # demog_list <- tibble::lst(Survi, Repro, Trans) 
  print(f_new)
  # Units of f_new should be total number of migrants leaving the node
  
  # return ####
  return(f_new)
}

## CALCULATE p -> PATH TRANSITION RATES #################
p_function <- function(){
  # TYPICAL VARIABLES USED (N,f_update,alpha,beta,type,ind,t)
  # N - contains the population arriving at the nodes at all previous time steps - N[["time_step"]]$"class_name"
  # NOTE this population count is before accounting for node dynamics
  # f_update - contains the population after node dyanamics have been applied for the current time step - f_update[["class_number"]]
  # alpha - contains all information about node dynamics - alpha[["class_number"]][["season"]]$"variable_name"
  # beta - contains all information about path dynamics - beta[["class_number"]][["season]]$"variable_name"
  # NOTE edge transition rates are found in beta[["class_number"]][["season]]$p_ij
  # type - gives the class number as an integer - the class types are defined in NETNAME 
  # for example: if NETNAME <- c("seeds","plants") then type 1 = seeds and type 2 = plants
  # ind - gives the season number as an integer
  # t - gives the time step
  # NOTE if it is required to change any variable globally then use "old_value" <<- "new_value"
  
  p <- beta[[type]][[ind]]$p_ij  #p_edge[[type]][[ind]]
  
  # density dependent transitions only apply to the spring stopover season
  if(ind==3){
    delta0 <- alpha[[type]][[ind]]$Delta_0[2]
    delta1 <- alpha[[type]][[ind]]$Delta_1[2]
    delta2 <- alpha[[type]][[ind]]$Delta_2[2]
    
    # Number of migrants who survived PR
    PRPOP <- f_update[[1]][2]+f_update[[2]][2]
    ponds <- alpha[[type]][[ind]]$P[2]
    
    Y <- delta0 + delta1*PRPOP+delta2*ponds
    
    # Original transition probabilities from PR
    psim <- alpha[[type]][[ind]]$psi_max[2]
    newpsi_leave <- (psim)*1/(1+exp(-Y))
    
    # Probabilities of leaving PR for AK or NU
    psi21 <- beta[[type]][[ind]]$psi_ij[2,1]  
    psi23 <- beta[[type]][[ind]]$psi_ij[2,3]
    
    # Update the transition probabilities
    p[2,1] <- newpsi_leave*psi21
    p[2,3] <- newpsi_leave*psi23
    p[2,2] <- 1-newpsi_leave
    
  }
  
  p_new <- p
  # p_new should be unitless and represent the edge transition probabilities for the given step
  
  return(p_new)
}

## CALCULATE s -> PATH SURVIVAL RATES #################
s_function <- function(){
  # TYPICAL VARIABLES USED (N,f_update,alpha,beta,type,ind,t)
  # N - contains the population arriving at the nodes at all previous time steps - N[["time_step"]]$"class_name"
  # NOTE this population count is before accounting for node dynamics
  # f_update - contains the population after node dyanamics have been applied for the current time step - f_update[["class_number"]]
  # alpha - contains all information about node dynamics - alpha[["class_number"]][["season"]]$"variable_name"
  # beta - contains all information about path dynamics - beta[["class_number"]][["season]]$"variable_name
  # NOTE edge survival rates are found in beta[["class_number"]][["season]]$s_ij
  # type - gives the class number as an integer - the class types are defined in NETNAME 
  # NOTE if NETNAME <- c("seeds","plants") then type 1 = seeds and type 2 = plants
  # ind - gives the season number as an integer
  # t - gives the time step
  # NOTE if it is required to change any variable globally then use "old_value" <<- "new_value"
  
  # Edge survival where k=hunting mortality
  s <- beta[[type]][[ind]]$s_ij*(1-beta[[type]][[ind]]$kappa_ij)  #s_edge[[type]][[ind]]
  
  s_new <- s
  # s_new should be unitless and represnt the edge survival for the given step
  return(s_new)
}

# specify Cr functions ####
## Shifter function  #####
# Shift vector x by n units to the left
shifter <- function(x, n = 0) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

## Coid Function   ####
# INPUTS - the pathway (in vector form), the beginning season, the breeding season, sA_node, sJ_node, R0, sA_edge, sJ_edge
Coid <- function(path, theSeason, breedSeason, sA_node, sJ_node, R0, sA_edge, sJ_edge)  
{
  a <- 1:(length(path)-1)
  seasonvec <- shifter(a, theSeason-1) #theSeason is the anniversary date
  Aoid <- 1  #initialize contribution
  Joid <- 1  
  breedOccurred <- 0  #breeding season has not yet occurred
  for (i in a){ 
    Aoid_mat <- diag(sA_node[,seasonvec[i]]) %*% as.matrix(sA_edge[,,seasonvec[i]])
    if(seasonvec[i] == breedSeason){  #Check if the breeding season occurred and define Juvenile matrix accordingly
      breedOccurred <- 1
      Joid_mat <- diag(sA_node[,seasonvec[i]]*R0[,seasonvec[i]]) %*% as.matrix(sJ_edge[,,seasonvec[i]])
    }
    else if(breedOccurred == 1){
      Joid_mat <- diag(sJ_node[,seasonvec[i]]) %*% as.matrix(sJ_edge[,,seasonvec[i]])
    }
    else {
      Joid_mat <- Aoid_mat
    }
    Aoid <- Aoid*Aoid_mat[path[i],path[i+1]]
    Joid <- Joid*Joid_mat[path[i],path[i+1]]
  }
  return(Aoid + Joid)  # OUTPUT - "path" pathway contribution 
}

## Cr function  ####
Cr_fn <- function(nSites, sA_node, sA_edge, p, breedSeason, R0, sJ_node, sJ_edge){
  # This code calculates the Cr for each node at each season
  # using the ONE EQUATION / MULTI-ANNIVERSARY APPROACH
  # Originally by Joana Bieri & Christine Sample, modified by B. Mattsson as follows:
  # 1) Convert Cr calculations to a function that can be called by PintailSimulation.r
  # 2) streamlined headings for easier navigation
  #
  #Parameters that this code requires to be defined elsewhere:
  #
  # seasons - number of time periods in a cycle
  # nSites - number of nodes
  # breedSeason - the season number during which breeding occurs
  # p - matrix of transition probabilities for each season
  # sA_edge - matrix of adult edge survival for each season
  # sJ_edge - matrix of juvenile edge survival for each season
  # sA_node - vector of adult node survival for each season
  # sJ_node - vector of juvenile node survival for each season
  # R0 - vector of reproduction (birth rate) at each node for each season
  
  ##Initialize
  CR <- matrix(0,nSites,seasons)
  COI <- array(0,c(nSites,nSites,seasons))
  a <- 1:seasons
  ones <- matrix(1,nSites,1)  #unit column vector
  
  NT <- list()
  LAMBDA <- seq(0,0,length.out=seasons)
  nSites <- num_nodes
  breedSeason <- 1
  a <- 1:seasons
  sA_node <- matrix(0,nSites,seasons)
  sJ_node <- matrix(0,nSites,seasons)
  sA_edge <- array(0,c(nSites,nSites,seasons))
  sJ_edge <- array(0,c(nSites,nSites,seasons))
  p <- array(0,c(nSites,nSites,seasons))
  R0 <- matrix(0,nSites,seasons)
  
  ##Calculate Cr
  #Calculate the CR for each season as anniversary date
  for (k in 1:seasons){ 
    
    # Find which simulation time step gives the correct start time
    # Here we augment t to allow for a full year of seasons
    starttime <- timestep - seasons + k -1
    t <- starttime
    if((t+seasons-1)>(timestep-1)){t=t-seasons}
    
    for (i in shifter(a,(k-1))){
      ind <- i
      
      # Get data for f, P, and S for each of the seasons/classes
      for (j in 1:NUMNET){
        type <- j
        f_update[[j]] <- f_function()
        p_update[[j]] <- p_function()
        s_update[[j]] <- s_function()
      }
      
      #transition probability
      p[1:nSites,1:nSites,ind]<-as.matrix(p_update[[sex]])
      
      #Adult edge survival
      sA_edge[1:nSites,1:nSites,ind]<-as.matrix(s_update[[sex]])
      
      #Juvenile edge survival
      if(ind == breedSeason){
        sJ_edge[1:nSites,1:nSites,ind]<-as.matrix(s_update[[sex+2]])
      }
      else{
        sJ_edge[1:nSites,1:nSites,ind]<-sA_edge[1:nSites,1:nSites,ind]
      }
      
      #Adult node survival and Reproduction
      for (j in 1:num_nodes){
        if((unlist(N[[t]])[(sex-1)*nSites+j]+unlist(N[[t]])[(sex+1)*nSites+j])!=0){  #if (sex-specific) node pop is not zero
          sA_node[j,ind] <- f_update[[sex]][j]/(unlist(N[[t]])[(sex-1)*nSites+j]+unlist(N[[t]])[(sex+1)*nSites+j])
        }
        if(unlist(N[[t]])[j]!=0 && ind == breedSeason){  #if adult females pop is not zero and it's the breeding season
          R0[j,ind] <- f_update[[sex+2]][j]/(unlist(N[[t]])[j]*sA_node[j,ind]) 
        }
      }
      #Juvenile node survival
      sJ_node[1:nSites,ind] <- sA_node[1:nSites,ind] 
      
      t = t+1 
    }
    
  }
  
  ## Cr for each season ####
  for (k in a-1){  
    CrprodA <- diag(nSites)  #initialze with identity matrix
    CrprodJ <- diag(nSites)  
    COIprodA <- diag(nSites)
    COIprodJ <- diag(nSites)
    seasonvec <- shifter(a, k) #start at season (k+1) as the anniversary date
    breedOccurred <- 0  #breeding season has not occurred yet
    for (i in seasonvec){  
      sA_node_mat <- diag(sA_node[,i]) # matrix with adult node survival as diagonal elements
      qA <- as.matrix(sA_edge[,,i]*p[,,i]) # Hadamard product of Adult edge survival and probability
      if(i == breedSeason){  #Check if the breeding season occurred and define Juvenile matrix accordingly
        breedOccurred <- 1
        sJ_node_mat <- diag(sA_node[,i]*R0[,i]) # matrix with adult node survival times reproduction as diagonal elements
        sJ <- as.matrix(sJ_edge[,,i]) # Juvenile edge survival
      }
      else if(breedOccurred == 1){
        sJ_node_mat <- diag(sJ_node[,i]) # matrix with juvenile node survival as diagonal elements
        sJ <- as.matrix(sJ_edge[,,i]) # Juvenile edge survival
      }
      else {
        sJ_node_mat <- sA_node_mat  # matrix with adult node survival as diagonal elements
        sJ <- as.matrix(sA_edge[,,i]) # adult edge survival
      }
      CrprodA <- CrprodA %*% sA_node_mat %*% qA  # matrix multiplication
      CrprodJ <- CrprodJ %*% sJ_node_mat %*% as.matrix(sJ*p[,,i]) 
      if (i!=seasonvec[length(seasonvec)]){  #if not the last season
        COIprodA <- COIprodA %*% sA_node_mat %*% sA_edge[,,i]
        COIprodJ <- COIprodJ %*% sJ_node_mat %*% sJ 
      }
    }
    CR[,k+1] <- (CrprodA + CrprodJ) %*% ones  #column vector of Cr values for time step k+1
    COI[,,k+1] <- as.matrix(COIprodA*(ones %*% t(sA_node_mat %*% qA %*% ones))) + as.matrix(COIprodJ*(ones %*% t(sJ_node_mat %*% as.matrix(sJ*p[,,i]) %*% ones))) #matrix of Coi values for time step k+1
  }
  return(CR)
} # end Cr_fn

# SET UP PINTAIL SPECIFIC NETWORK ####

# IMPORTANT NOTE #
# Users should specify network model functions f_(i,t), p_(ij,t), and s_(ij,t) below, even if these are constants they must still be set as such.

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

# This code sets up the network variables to be used in the Network Simulation.
# All data should be stored in .xlsx spreadsheets in the correct format.
# Spreadsheets should be located at ./SIMNAME/network_inputs_NETNAME.xlsx
# Where SIMNAME and NETNAME are defined in the SpeciesSimulation.R script

# Set location of source code if working outside R project
#netcode <- c("../NetworkCode1.1/") # identifies folder containing the code, which is in the parent of the current working directory

# Set location of source code if working in R project
netcode <- c("NetworkCode1.1/") # identifies folder containing the code, which is in the parent of the current working directory

# Clear the workspace reserving needed network input variables
base_variables <- c("seasons", "num_nodes", "NETNAME", "tmax", "SIMNAME", "ERR", "OUTPUTS", "SILENT","netcode","base_variables", "scenarioName")
#rm(list=setdiff(ls(), base_variables))

#source(paste(netcode,"NetworkSetup.R",sep="")) # should finish running in a few seconds

nodes<-list()
alpha<-list()
beta<-list()
N_initial<-list()

# Find the number of classes
NUMNET <- length(NETNAME)
type <- 1 # use network 1 (adult females) for troubleshooting

# Set the input file names
input_file_names <- matrix(0,1,NUMNET)
for (i in 1:NUMNET){
  input_file_names[i]<-c(paste(SIMNAME,"/network_inputs_",NETNAME[i],".xlsx", sep=""))
}

# Check the validity of input files
for (i in 1:NUMNET){
  CHECK_WORKBOOK <- file.exists(input_file_names[i])
  if (CHECK_WORKBOOK==F){stop(paste("The Workbook,", input_file_names[i],"could not be found. \n *** Please check that the end of the input file names match NETNAME"))}
}
for (i in 1:NUMNET){
  SPREADSHEETS <- getSheets(loadWorkbook(input_file_names[i]))
  NUMSHEETS <- length(SPREADSHEETS)
  EXPECTEDSHEETS <- seasons + 1
  if (NUMSHEETS != EXPECTEDSHEETS){stop(paste("\n The Workbook,", input_file_names[i],"has an incorrect number of sheets. \n *** It should contain one sheet for initial conditions and one sheet for each season."))}
}


## Find the number of node variables ####
DATA <- readWorksheetFromFile(input_file_names[1],sheet=2)
varname_node <-list()
i=3
j=1

test <- DATA[1,i]=="STOP"
while(test == F){
  varname_node[[j]]<-DATA[1,i]
  j = j+1
  i = i+1
  test <- DATA[1,i]=="STOP"
}
numvar_node <- length(varname_node)

## Find the number of path variables
varname_path <-list(c("p_ij"),c("s_ij"))
i=1+3*(num_nodes+3) # First location after p_ij and s_ij
j=1

test <- DATA[i,1]=="STOP"
while(test == F){
  varname_path[[j+2]]<-DATA[i,1]
  j = j+1
  i = i+num_nodes+3
  test <- DATA[i,1]=="STOP"
}
numvar_path <- length(varname_path)-2


## SET UP THE NETWORK(S) ####

for (i in 1:NUMNET){
  networkname <- NETNAME[i]
  NODE <- list()
  EDGEPROB <- list()
  EDGESURVIVE <- list()
  PATH <- list()
  
  ## Read in first worksheet
  ## The first worksheet should contain initial conditions 
  ## Data should start in cell B2
  Ninit <- readWorksheetFromFile(input_file_names[i], sheet=1, startRow=1)
  N_initial[[networkname]] <- Ninit[,2]
  n <- nrow(Ninit) #Number of Nodes
  
  if((n != num_nodes) | (any(is.na(Ninit[,2])))){
    stop(paste("\n Incorrect number of initial conditions given for species:", networkname,"\n Check you input files."))}
  
  nodes[[i]]<-n
  
  ## Read in the remaing worksheets for each season
  ## t=0 season should be worksheet 2
  ## Node data belongs on the top starting in cell C2
  ## Path data should be contained in nXn ranges below the node data with 3 rows separating
  ## Transition rates should start in cell C(n+5) where n=number of nodes
  ## Survival probabilities should start in cell C(2n+9)
  ## Other path variables should start in cell C(3n+12)
  for (k in 1:seasons){
    # READ IN THE DATA
    SR <- 2 # row in the spreadsheet where we start reading data
    SC <- 3 # column in the spreadsheet where we start reading data
    ECnode <- SC + numvar_node - 1 # column in the spreadsheet where we stop reading node data
    ECpath <- SC + n -1
    
    ###____seasonal node characteristics ____###
    NODE[[k]] <- readWorksheetFromFile(input_file_names[i], sheet=k+1, startRow=SR, endRow=SR+n, startCol=SC, endCol=ECnode)
    
    # Check that there are no empty cells in node characteristics
    if(any(is.na(NODE[[k]]))){
      stop(paste('\n Check the input file! \n *** There is at least one empty node characteristic in season', k))}
    
    ###____seasonal edge characteristics____###
    path_vars <- vector("list", length(numvar_path))
    
    SR <- (SR + n + 3)
    EDGEPROB[[k]] <- readWorksheetFromFile(input_file_names[i], sheet=k+1, startRow=SR, endRow=SR+n, startCol=SC, endCol=ECpath)
    path_vars[[1]] <- EDGEPROB[[k]]
    
    SR <- (SR + n + 3)
    EDGESURVIVE[[k]] <- readWorksheetFromFile(input_file_names[i], sheet=k+1, startRow=SR, endRow=SR+n, startCol=SC, endCol=ECpath)
    path_vars[[2]] <- EDGESURVIVE[[k]]
    
    # Check that there are no empty cells in edge transition and survival probabilities
    if((any(is.na(EDGEPROB[[k]]))) | (any(is.na(EDGESURVIVE[[k]])))){
      stop(paste('\n Check the input file! \n *** There is at least one empty edge transition or survival in season', k))}
    
    # Check the sum of transition probabilities should sum to 1 or 0
    x <- unlist(lapply(EDGEPROB[k],rowSums))
    eps <- 0.000001
    if( any(x < 1-eps & x!=0) | any(x > 1+eps & x!=0) ){ stop('\n Check the input file! \n *** Edge transition probabilities must sum to 0 or 1')}
    
    if(numvar_path > 0){
      for(j in 1:numvar_path){
        SR <- (SR + n + 3)
        path_vars[[j+2]] <- readWorksheetFromFile(input_file_names[i], sheet=k+1, startRow=SR, endRow=SR+n, startCol=SC, endCol=ECpath)
      }
    }
    
    
    PATH[[k]] <- path_vars
    rm(path_vars)
    names(PATH[[k]]) <- varname_path
    
    
  }
  
  
  ## Store the node and path characteristics in lists
  alpha[[networkname]] <- NODE
  beta[[networkname]] <- PATH
  
  rm(NODE,EDGEPROB, EDGESURVIVE, PATH)
  
}


### CHECK THAT EACH NETWORK IS THE SAME SIZE ###
test1 <- nodes[[1]]
for(i in 1:NUMNET){
  test2 <- nodes[[i]]
  if( test1!=test2 ) stop('\n Check that the networks have the same number of nodes.')
}
### STORE NUMBER OF NODES ###
nSites <- test2

### CHECK THAT THE NUMBER OF PATH VARIABLES MATCHES NUMBER OF NODES ###
for (i in 1:NUMNET){
  for (j in 1:seasons){
    for (k in 1:length(varname_path)){
      check1 <- ncol(beta[[i]][[j]][[k]])
      check2 <- nrow(beta[[i]][[j]][[k]])
      if(check1 != check2){stop(paste("\n", varname_path[[k]], "for class", NUMNET[[1]], "season", j, "should be a square matrix. \n Check your input files"))}
      if(check1 != n){stop(paste("\n", varname_path[[k]], "for class", NUMNET[[1]], "season", j, "should be an nXn where n=number of nodes. \n Check you input files"))}
    }
  }
}


# Clear the workspace reserving needed network input variables and base variables
network_variables <- c("N_initial","alpha","p_edge","s_edge","beta","varname_path","varname_node","NUMNET","network_variables")
#rm(list=setdiff(ls(),c(network_variables,base_variables)))


print("Network Setup Complete")




# POPULATION NETWORK SIMULATION ####
#print(paste("Running", SIMNAME, sep=" "))
#source(paste(netcode,"NetworkSimulation.R",sep=""))

## This code runs the network simulation based on the general network model
## It first loads the user defined Newtork Functions: f_(i,t), p_(ij,t), and s_(ij,t)

N <- list()
M <- list()
WR <-list()
total_pop <- list()
f_update <- list()
p_update <- list()
s_update <- list()


## INITIALIZE NODE POPULATION
N[[1]] <- N_initial # Set initial population after movement to each node (before reproduction/survival at node)

### INITIALIZE OTHER POPULATION DATA
M[[1]] <- vector("list", length(N_initial)) # Path Population Matrix
names(M[[1]]) <- NETNAME
total_pop[[1]] <- vector("list", length(N_initial)) # total network population - sum of population at each node
names(total_pop[[1]]) <- NETNAME
WR[[1]] <- vector("list", length(N_initial)) # population distribution at each node
names(WR[[1]]) <- NETNAME


for (i in 1:NUMNET){
  total_pop[[1]][i] <- lapply(N[[1]][i], function(x) sum(x))
  test <- as.double(total_pop[[1]][i])
  if (test == 0){WR[[1]][i] <- N[[1]][i]}
  else{WR[[1]][i] <- lapply(N[[1]][i],y=unlist(total_pop[[1]][i]), function(x,y) x/y)}
}
rm(i)

if(SILENT==F){
  print("Initial Population")
  print(total_pop[[1]])
}

### INITIALIZE ERROR ###
errorstop <- 0
ERRPOP_OLD <- matrix(100,1,seasons)  

### INITIALIZE TIME STEPS ###
t <- 0

### START SIMULATION ###
### LOOP THROUGH TIME ###
while (errorstop == 0){
  t <- t+1
  if(SILENT==F){
    print("Time Step")
    print(t)
  }
  
  ind <- ((t-1)%%seasons) +1 # season number
  if(SILENT==F){
    print("Season Number")
    print(ind)
  }
  
  ## MODEL FUNCTIONS ##
  for (i in 1:NUMNET){
    type <- i
    f_update[[i]] <- f_function()   # f_function(N,alpha,i,ind,t) # Number of individuals leaving each node
  }
  for (i in 1:NUMNET){
    type <- i
    p_update[[i]] <- p_function()    # p_function(N,f_update,alpha,p_edge,beta,i,ind,t) # Path transition probability
    s_update[[i]] <- s_function()   # s_function(N,f_update,alpha,s_edge,beta,i,ind,t) # Path survival probability
  }
  rm(i)
  
  if(SILENT==F){
    print("f_(i,t)=")
    print(f_update)
    print("p_(ij,t)=")
    print(p_update)
    print("s_(ij,t)=")
    print(s_update)
  }
  
  ## MODEL EQUATION ##
  ## Create lists elements for next time step
  M[[t]] <- vector("list", length(N_initial))
  names(M[[t]]) <- NETNAME  # originally names(M[[1]]) <- NETNAME , changed by BM
  N[[t+1]] <- vector("list", length(N_initial))
  names(N[[t+1]]) <- NETNAME
  
  ## Calculate next time step; c(s_update,p_update,f_update)
  for (i in 1:NUMNET){
    # skip list if null -- added by BM during troubleshooting
    if (length(s_update[[i]]) == 0L ||
        length(p_update[[i]]) == 0L ||
        length(f_update[[i]]) == 0L) {
      next
    }
    
    M[[t]][i] <- list(s_update[[i]]*p_update[[i]]*f_update[[i]]) # Number of individuals on each path
    N[[t+1]][i] <- list(colSums(M[[t]][[i]])) # Number of individuals arriving at the nodes in the next season
  }
  rm(i)
  
  ## UPDATE TOTAL POPULATION ##
  total_pop[[t+1]] <- vector("list", length(N_initial))
  names(total_pop[[t+1]]) <- NETNAME
  
  WR[[t+1]] <- vector("list", length(N_initial))
  names(WR[[t+1]]) <- NETNAME
  
  ## Set total population data
  for (i in 1:NUMNET){
    total_pop[[t+1]][i] <- lapply(N[[t+1]][i], function(x) sum(x))
    test <- as.double(total_pop[[t+1]][i])
    if (test == 0){WR[[t+1]][i] <- N[[t+1]][i]}
    else{WR[[t+1]][i] <- lapply(N[[t+1]][i],y=unlist(total_pop[[t+1]][i]), function(x,y) x/y)}
  }
  
  if(SILENT==F){
    print(paste("Total Population in season", ind, "for time step", t))
    print(total_pop[t+1])
  }
  if(SILENT==F){
    print(paste("Node Population in season", ind, "for time step", t))
    print(N[t+1])
  }
  
  ### TEST FOR BLOW-UP OR INFINITE NUMBERS ###
  if(any(lapply(total_pop[[t+1]], function(x) is.finite(as.double(x)))=="FALSE")){
    stop("\n NaN found in population data: \n *** This means an infinite population has been reached in at least one node. \n *** Check divide by zero or missing data in model parameters. \n *** Check density dependent equations. \n *** This is likely caused by SpeciesFunctions.R")}
  
  
  ### CALCULATE THE ERROR AT EACH SEASON ###
  if(t >= seasons){
    sum_new <- 0
    sum_old <- 0
    for (i in 1:NUMNET){
      sum_new <- sum_new + unlist(total_pop[[t+1]][i])  
      sum_old <- sum_old + unlist(total_pop[[t+1-seasons]][i])
    }
    ERRPOP_OLD[1,ind] <- abs(sum_new - sum_old)
    ## Only stop if the total error is less than the allowable error across all seasons
    if(all(ERRPOP_OLD < ERR)){errorstop <- 1}
    if(t+1 >= tmax){
      errorstop <- 1 
      print("\n The simulation did not converge within the maximum time allowed")}
  }
}

# We give results for the total population at the start of season 1
# Check at which season the simulation stopped
timestep <- t+1 - ind%%seasons

# Clear the workspace reserving functions and needed network input variables and base variables and simulation variables
simulation_variables <- c("timestep","WR","total_pop","M","N","t","ind","f_update","p_update","s_update","simulation_variables")
rm(list=setdiff(ls(),c(network_variables,base_variables,simulation_variables,lsf.str())))




  
# PROCESS NETWORK OUTPUTS ####
# if (OUTPUTS == T){  source(paste(netcode,"NetworkOutputs.R",sep="")) }

## This code produces basic outputs for the Network simulation.
## 1. A graph of Population over time for each class - to show each population reaching a steady state.
## 2. A table of values showing the steady state total network populations for each season at steady state.
## 3. A single season graph of the steady state total network populations for each season at steady state.

## ggplot of annual changes in total breeding population  ####
# Helper to safely extract a single numeric component
get_component <- function(x, name) {
  if (!is.null(x[[name]]) && length(x[[name]]) >= 1) as.numeric(x[[name]][1]) else NA_real_
}

# Extract cohorts and compute total per time step
af <- sapply(total_pop, get_component, name = "adult_female")
am <- sapply(total_pop, get_component, name = "adult_male")
jf <- sapply(total_pop, get_component, name = "juvenile_female")
jm <- sapply(total_pop, get_component, name = "juvenile_male")

pop_total <- af + am + jf + jm

df_yearly <- tibble(
  t = seq_along(pop_total),
  pop_total = pop_total
) %>%
  filter(t %% seasons == 0) %>%           # keep only the last season in each year
  mutate(year = t / seasons) %>%          # convert time step to year number
  filter(year <= 300)                     # cap at 300 years (if available)

ggplot(df_yearly, aes(x = year, y = pop_total)) +
  geom_line(color = "steelblue", linewidth = 1) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Total population by year (every 3rd time step)",
    x = "Year",
    y = "Total count"
  ) +
  theme_minimal()
## probably not needed: Base-R Plot total populations over time  ####
platform <- .Platform$OS.type
if(platform == "unix") {X11()
} else{
  if(platform == "windows"){windows()
  } else{quartz()}}

time_steps <- 0:timestep/seasons
STEPwidth <- seasons
STEPstart <- 1
graphbot <- 0 
graphtop <- max(unlist(lapply(total_pop[c(seq(STEPstart, timestep+1,by=STEPwidth))], function(x) x[])))
dottype <- data.frame(matrix(0,1,NUMNET))
dottype[1] <- 20
total_pop_plot <- data.frame(matrix(0,length(N),NUMNET))

plot(c(0),c(0),type="l", 
     main=paste("Total Network Population vs. Time \n Single Annual Survey (beginning of season 1)"),
     ylim = c(graphbot,graphtop), xlim = c(min(time_steps),max(time_steps)),
     xlab="Years",ylab="Total Population")

xplot <- time_steps[seq(STEPstart, length(time_steps),by=STEPwidth)]
for(i in 1:NUMNET){
  par(new=TRUE)
  
  # head(total_pop)
  total_pop_plot[,i] <- unlist(lapply(total_pop, function(x) x[][[i]]))
  
  yplot <- total_pop_plot[seq(STEPstart, length(time_steps),by=STEPwidth),i]
  dot <- as.double(dottype[i])
  plot(xplot,yplot,type="o", lty=1, pch=dot,ylim = c(graphbot,graphtop), 
       xlim = c(min(time_steps),max(time_steps)), axes="FALSE", xlab = "", ylab = "")
  dottype[i+1] <- dottype[i] + 1
}
rm(i)

legend('right', NETNAME , lty=1, pch=dottype, bty='n', cex=.75)


## Store data as .csv for steady state annual cycle population numbers  ####  
pop_output <- data.frame(matrix(0,seasons,NUMNET))
for(i in 1:NUMNET){
  temp <- unlist(lapply(total_pop, function(x) x[][[i]]))
  pop_output[,i] <- data.frame(temp[seq(timestep-seasons+1,timestep, by=1)],row.names=NULL) 
  
}
rm(i)

colnames(pop_output) <- NETNAME
for(i in 1:seasons){
  rownames(pop_output)[i] <- paste("season",i)}
print("Total Equilibrium Population - for each season")
print(pop_output)
write.csv(pop_output,file=paste(SIMNAME,"/SteadyStatePopulation.csv",sep=""))
rm(i)


## Plot steady state annual cycle population numbers ####
platform <- .Platform$OS.type
if(platform == "unix"){X11()
} else{
  if(platform == "windows"){windows()}
  else{quartz()}}

time_steps <- 1:seasons
graphbot <- 0 
graphtop <- max(pop_output)
rm(dottype)
dottype <- data.frame(matrix(0,1,NUMNET))
dottype[1] <- 20

plot(c(0),c(0),type="l", ylim = c(graphbot,graphtop), xlim = c(min(time_steps),max(time_steps)),
     xaxt = "n", main=paste("Class Population at Steady State \n Over One Annual Cycle"),
     xlab="Season Number",ylab="Population")

axis(side = 1, at = time_steps)
for(i in 1:NUMNET){
  par(new=TRUE)
  dot <- as.double(dottype[i])
  plot(time_steps,pop_output[,i],type="o", lty=1, pch=dot, ylim = c(graphbot,graphtop), 
       xlim = c(min(time_steps),max(time_steps)), axes = FALSE, xlab = "", ylab = "")
  dottype[i+1] <- dottype[i] + 1
}
rm(i)
legend('right', NETNAME , lty=1, pch=dottype, bty='n', cex=.75)


# Clear the workspace reserving needed network input variables and base variables and simulation variables
output_variables <- c("pop_output", "output_variables")
rm(list=setdiff(ls(),c(network_variables,base_variables,simulation_variables,output_variables)))



# Calculate Ds  ####

WEIGHT <- 0
numSex <- 2 # Calculate Cr separately for males and females
DSind <- array(0,c(num_nodes,seasons,numSex))

#Choose which type (male or female) of adult to calculate Cr
for (sex in 1:numSex){
  
  # Get Cr Values
  #source("Cr_pintail.R") 
    # variables in global environment used in this function
      Cr_vars <- c(nSites, sA_node, sA_edge, p, breedSeason, R0, sJ_node, sJ_edge, 
                   seasons, num_nodes, NUMNET, f_update, p_update, s_update, f_function, p_function, s_function, sex, timestep, N)
  CR <- Cr_fn()
  
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
      DSind[i,CrCount,sex] <- CR[i,CrCount]*POP[i]   ### CR is a list derived from Cr_pintail code
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




# SAVE THE DATA and outputs ####
#save.image(file=paste(SIMNAME,"/",SIMNAME, ".RData", sep = ""))

