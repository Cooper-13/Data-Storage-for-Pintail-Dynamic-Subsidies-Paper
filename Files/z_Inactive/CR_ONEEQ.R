######################################################################################
# This code calculates the Cr for each node at each season
# using the ONE EQUATION / MULTI-ANNIVERSARY APPROACH
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
#
#####################################################################################

# # # # # # # # # # #
# Shifter function  #
# # # # # # # # # # #

# Shift vector x by n units to the left
shifter <- function(x, n = 0) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

# # # # # # # # # # 
# Coid Function   #
# # # # # # # # # # 

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

# # # # # # # # # # 
# Cr CALCULATION  #
# # # # # # # # # # 

##Initialize
CR <- matrix(0,nSites,seasons)
COI <- array(0,c(nSites,nSites,seasons))
a <- 1:seasons
ones <- matrix(1,nSites,1)  #unit column vector

##Calculate Cr
for (k in a-1){  #Find Cr for each season
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
