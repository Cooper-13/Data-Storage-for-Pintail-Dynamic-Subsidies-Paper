#################################
##    MODEL FUNCTIONS          ##
#################################

### PINTAILS ###

############## CALCULATE f -> NODE DYNAMICS UPDATE #################

f_function <- function() {
  # TYPICAL VARIABLES USED (N,alpha,type,ind,t)
  # N - contains the population arriving at the node at all previous time steps - N[["time_step"]]$"class_name"
  # NOTE if you specify "class_name" the it will look to that specific class's population each time
  # NOTE if you set "class_name"=[[type]] it will get the numbers for the current calculation's class
  # alpha - contains all information about node dynamics - alpha[["class_number"]][["season"]]$"variable_name"
  # type - gives the class number as an integer - the class types are defined in NETNAME 
  # NOTE if NETNAME <- c("seeds","plants") then type 1 = seeds and type 2 = plants
  # ind - gives the season number as an integer
  # t - gives the time step
  
  # Initiaize basic update information
  Repro <- 0*N[[t]][[type]]
  Survi <- alpha[[type]][[ind]]$S*N[[t]][[type]]
  Trans <- 0*N[[t]][[type]]
  
  # print("Pre")
  # print(paste("t=",t,"type=",type))
  # print(Repro)
  # print(Survi)
  # print(Trans)
  
  
  # Breeding Nodes
  if(ind==1){
    if(type==3 || type==4){ # If we are updating the Juvenile population
    
      POP <- N[[t]][[1]]+N[[t]][[2]] # Number of existing Males and Females for density dependence
      Survi_F <- alpha[[1]][[ind]]$S # Female survival Rate
    
      ponds <- alpha[[type]][[ind]]$P
      alpha0 <- alpha[[type]][[ind]]$a_0
      alpha1 <- alpha[[type]][[ind]]$a_1
      alpha2 <- alpha[[type]][[ind]]$a_2
    
      R <- exp(alpha0+alpha1*POP+alpha2*ponds) # Density Dependent Reproductive Rate
    
      Repro <- R*Survi_F*N[[t]][[1]]
    
      } 
  }
  
  # Winter Nodes
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
   
  f_new <- Survi + Repro + Trans
  # Units of f_new should be total number of migrants leaving the node
  
  return(f_new)
}
########################################################################## 

############## CALCULATE p -> PATH TRANSITION RATES #################
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
##########################################################################

############## CALCULATE s -> PATH SURVIVAL RATES #################
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
##########################################################################
