####################################
##  Cr  Calculations        ##
####################################


# # # # # # # # # # #
# Shifter function  #
# # # # # # # # # # #

# Shift vector x by n units to the left
# Used to shift the seasons for different anniversary dates
shifter <- function(x, n = 0) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
} 
  

# # # # # # # # # # #
#     Initialize    #
# # # # # # # # # # #

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

# # # # # # # # # # #
# Pintail Parameters  #
# # # # # # # # # # #

source("SpeciesFunctions.R")

for (k in 1:seasons){ #Calculate the CR for each season as anniversary date
  
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

source("CR_ONEEQ.R")

# # # # # # # # # # # #
# Lambda CALCULATION  #
# # # # # # # # # # # #

for (k in a-1){  #Find Cr for each season

  # Find which simulation time step gives the correct start time
  # Here we augment t to allow for a full year of seasons
  starttime <- timestep - seasons + k 
  t <- starttime
  if((t+seasons-1)>(timestep-1)){t=t-seasons}
  
  #Calculate total population size at each node
  for(j in 1:num_nodes){
    NT[[j]] <- 0
    # for(i in 1:NUMNET){
    for(i in c(sex,sex+2)){
      NT[[j]] <- NT[[j]]+ N[[starttime]][[i]][[j]]
    }
  }
  
  # Calculate growth rate
  # LAMBDA[[k+1]] <- t(diag(NT)%*%matrix(1,num_nodes,1)) %*% (CR[,k+1])/sum(unlist(N[[starttime]]))
  LAMBDA[[k+1]] <- t(diag(NT)%*%matrix(1,num_nodes,1)) %*% (CR[,k+1])/sum(unlist(N[[starttime]])[c(((sex-1)*nSites+1):(nSites*sex),(nSites*(sex+1)+1):(nSites*(sex+2)))])
  
}

# cat("Class and season specific Cr:\n")
# print(CR)
# cat("Class and season specific growth rate:\n")
# print(LAMBDA)
