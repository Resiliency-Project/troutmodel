## BROOK and BROWN TROUT IBM ##
## DEVELOPED BY SHANE FLINN ##
## ADAPTED BY KAILI GREGORY

# Script for defining temperature scenarios

# Temperature data is July Mean temperature (deg. C) by reach

# There are 3 temperature scenarios based on 3 cliamte change scenaios:
  # 1. Low change in temperature (Scenario 2.5)
  # 2. Moderate change in temperature (Scenario 3.8)
  # 3. High change in temperature (Scenario 4.8)

# Temperature data is associated with a predicted density of trout
  # this relationship is based on a regression with data from Michigan DNR Status and Trends
  # predictions of density for each reach, for 1000 simulations have been
  # pre-ran to reduce run time of the model. Those predictions are stored as
  # an Rdata file 'temp_density_predictions.RData'
  # the temperature projections by reach are also loaded in this script
  # they are stored in the file 'temp_projections.RData'.

# The density values are then translated to temperatures scores on a scale of 0-5
  # greater values indicate better temperautres for trout

#load in projected temperatures and predicted density values based on temperature projections
load(file = "Data/Habitat/temp_projections.RData")
load(file = "Data/Habitat/temp_density_predictions.RData")

#set up empty arrays for temperature scores by scenario
tempscores2.5 <- array(NA, dim=c(nrow(d2.5), yrs, sims))
tempscores3.8<- array(NA, dim=c(nrow(d3.8), yrs, sims))
tempscores4.8<- array(NA, dim=c(nrow(d4.8), yrs, sims))

#subset the density data to the number of simulations defined in 'model.R'
if(sims != dim(d2.5)[3]){
  d2.5 <- d2.5[,,1:sims]
  d3.8 <- d3.8[,,1:sims]
  d4.8 <- d4.8[,,1:sims]
}

#subset the density data to the number of years defined in 'model.R'
if(yrs != dim(d2.5)[2]){
  d2.5 <- d2.5[,((55-yrs)+1):55,]
  d3.8 <- d3.8[,((55-yrs)+1):55,]
  d4.8 <- d4.8[,((55-yrs)+1):55,]
}

#Calculate temperature scores for each scenario
  # Scores are relative within each simulation
for(k in 1:sims){
  tempscores2.5[,,k] <- (5*d2.5[,,k])/max(d2.5[,,k]) 
  tempscores3.8[,,k] <- (5*d3.8[,,k])/max(d3.8[,,k]) 
  tempscores4.8[,,k] <- (5*d4.8[,,k])/max(d4.8[,,k])
}

#Ensure all values are 0-5 
tempscores2.5[tempscores2.5 < 0] <- 0
tempscores2.5[tempscores2.5 > 5] <- 5

tempscores3.8[tempscores3.8 < 0] <- 0
tempscores3.8[tempscores3.8 > 5] <- 5

tempscores4.8[tempscores4.8 < 0] <- 0
tempscores4.8[tempscores4.8 > 5] <- 5

