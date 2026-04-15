## BROOK and BROWN TROUT IBM ##
## DEVELOPED BY SHANE FLINN ##
## ADAPTED BY KAILI GREGORY

# Script for calculating habitat suitability scores

#read in habitat data
habdata <- read.csv("Data/Habitat/habitatdata_clean.csv")
habdata <- habdata[,-1] #remove first column (errant)

#### Temperature ####
# Temperature scores are calculated in 'temp_scenarios.R' which should already have
  # been run.

#### Substrate ####
gradient <- as.vector(habdata$gradient) # isolate gradient information

# create function to categorize gradient data
substrate_scoring <- function(data) {
  if(data <= 0.0009)                      {score <- 1} #very low gradient class
  if(data > 0.0009 & data <= 0.0019)      {score <- 2} #low gradient class
  if(data > 0.0019 & data <= 0.0047)      {score <- 3} #medium gradient class
  if(data > 0.0047)                       {score <- 3} #high gradient class

return(score)
}

#apply function to gradient data
substratescores <- vapply(gradient, substrate_scoring, numeric(1))

### Low flow yield ####
  # Low flow yield data should already be loaded as 'dN_lfy'

#subset density data to number of simulations defined in 'model.R'
if(sims != dim(dN_lfy)[3]){
  dN_lfy <- dN_lfy[,,1:sims]
}

# create empty array for lfy scores
lfyscores <- array(NA, dim=c(nrow(habdata), yrs, sims))

#calculate lfy scores relative to each scenario
for(k in 1:sims){
  lfyscores[,,k] <- (5*dN_lfy[,,k])/max(dN_lfy[,,k]) 
}

# Ensure all values are 0-5
lfyscores[lfyscores < 0] <- 0
lfyscores[lfyscores > 5] <- 5

### Wood ####
#load in wood habitat data (TU sampling)
wdata <- read.csv("Data/Habitat/wood.clean.csv") %>% 
  na.omit() #remove and NAs

wdata$PercentWoodyDebris <- wdata$PercentWoodyDebris/100 #convert to decimal

#create empty array
woodpercent_3D <- array(NA, dim = c(length((habdata$medianpercentwood)), yrs, sims))

# empirical data entered as data for year 1
woodpercent_3D[,1,] <- as.vector(habdata$medianpercentwood)

# Fit a zero inflated beta distribution (from gamlss package)
  # save distribution parameters
model_bezi <- gamlss(wdata$PercentWoodyDebris ~ 1, family = BEZI)
bezi.mu <- fitted(model_bezi, what = "mu")[1]
bezi.sigma <- fitted(model_bezi, what = "sigma")[1]
bezi.nu <- fitted(model_bezi, what = "nu")[1]

# fill any NA values in year 1 with draws from the zero inflated beta distribution
for (k in 1:dim(woodpercent_3D)[3]) {
  na_indices <- is.na(woodpercent_3D[, 1, k])   # Get logical vector of which rows in column 1 are NA
  n_na <- sum(na_indices)   # Count how many NA values to fill
  
  # If there are any NAs, replace them with random values from distribution
  if (n_na > 0) {
    woodpercent_3D[na_indices, 1, k] <- rBEZI(n_na, mu = bezi.mu, sigma = bezi.sigma, nu = bezi.nu)
  }
}

# apply scenarios of wood changing though time (or not)
for(k in 1:sims){
  for(j in 1:ncol(woodpercent_3D)){
    for(i in 1:nrow(woodpercent_3D)){
      if(j>1){
        #change wood % overtime
        woodpercent_3D[i,j,k] <- woodpercent_3D[i,j-1,k] + 
          runif(1, min=wood.change$lwr, max=wood.change$upr)
        
        #make sure wood % is 0-1
        if(woodpercent_3D[i,j,k] < 0) {woodpercent_3D[i,j,k] <- 0}
        if(woodpercent_3D[i,j,k] > 1) {woodpercent_3D[i,j,k] <- 1}
      }
    }
  }
}

# If wood is being added to the river, this code will run
if(exists("wood.add")==TRUE){
  for(k in 1:sims){
    for(j in 1:ncol(woodpercent_3D)){
      for(i in 1:nrow(woodpercent_3D)){
        if(j>= wood.add.yr){
          wood.add.amt <- woodpercent_3D[i,1,k]*wood.add # increase wood in defined year
          
          #maximum allowable wood addition is 20% of channel width
            # assume 20% width is equal to area
          if(wood.add.amt > 0.2){wood.add.amt <- 0.2}
          if(wood.add.amt == 0){wood.add.amt <- wood.add.0}
          
          # update eisting object
          woodpercent_3D[i,j,k] <- woodpercent_3D[i,j,k]+ wood.add.amt
          
          #make sure wood % is 0-1
          if(woodpercent_3D[i,j,k] < 0) {woodpercent_3D[i,j,k] <- 0}
          if(woodpercent_3D[i,j,k] > 1) {woodpercent_3D[i,j,k] <- 1}
        }
      }
    }
  }
}

# Function for scoring wood based on hypothesis 1 or 2
woodscoring <- function(data) {
  if (data <= 0.1) {score <- wood.hyp[1]} 
  else if (data <= 0.2) {score <- wood.hyp[2]} 
  else if (data <= 0.3) {score <- wood.hyp[3]} 
  else if (data <= 0.4) {score <- wood.hyp[4]} 
  else if (data <= 0.5) {score <- wood.hyp[5]}
  else if (data <= 0.6) {score <- wood.hyp[6]} 
  else if (data <= 0.7) {score <- wood.hyp[7]} 
  else if (data <= 0.8) {score <- wood.hyp[8]} 
  else if (data <= 0.9) {score <- wood.hyp[9]} 
  else if (data <= 1.0) {score <- wood.hyp[10]} 

  return(score)
}

# Apply wood scoring function 
  # Flatten, apply, and reshape
woodscores <- array(
  vapply(as.vector(woodpercent_3D), woodscoring, numeric(1)),
  dim = dim(woodpercent_3D)
)

#### Stream area ####
NCarea <- data.frame(habdata$NCarea) #isolate network catchment area information

# Function for defining size classes
NCscoring <- function(data) {
  if(data > 1605.7)                   {score <- 1} #very large stream size class
  if(data < 1605.7 & data >= 466.0)   {score <- 1}  #large stream size class
  if(data < 466.0 & data >= 103.6)    {score <- 1} #medium stream size class
  if(data < 103.6 & data >= 25.0)     {score <- 2} #small stream size class
  if(data < 25.0)                     {score <- 2} #very small stream size class
  
  return(score)
}

# apply size class function
NCarea <- as.vector(habdata$NCarea)
NCscores <- vapply(NCarea, NCscoring, numeric(1))


### Combining scores ####
# Combine scores for each habitat suitability metric

# Rescale vector scores to 0-5
substratescores_resc <- rescale(substratescores, to=c(0,5), from=c(1,3))
NCscores_resc <- rescale(NCscores, to=c(0,5), from=c(1,2))

# Rescale wood 
  # (need to break into vector, then reform b/c it's an array)
wood_vector <- as.vector(woodscores) # Reshape to a vector
wood_vector_rescaled <- rescale(wood_vector, to = c(0, 5), from = c(1, 4)) # Rescale to range 0 to 5
woodscores_resc <- array(wood_vector_rescaled, dim = dim(woodscores)) # Reshape back to original dimensions

# Empty objects for reach-level habitat suitability score
reach.scBKT <- array(NA, dim=c(length(NCscores), yrs, sims))
reach.scBNT <- array(NA, dim=c(length(NCscores), yrs, sims))

# Expand substrate and NC scores to 3D arrays for vectorized combination later
substrate_3D <- array(rep(substratescores_resc, yrs * sims), 
                      dim = c(length(substratescores_resc), yrs, sims))
NC_3D <- array(rep(NCscores_resc, yrs * sims), 
               dim = c(length(NCscores_resc), yrs, sims))

# Habitat quality metric weights (expert elicitation information)
w_temp <- 0.3584      # temperature weight
w_lfy <- 0.2083       # lfy weight
w_substrate <- 0.2083 # substrate weight
w_wood <- 0.225       # wood weight
w_temp + w_lfy + w_substrate + w_wood #check it sums to 1

#Calculate reach-level habitat quality for BKT
reach.qualityBKT <-   w_temp * tempscores +
  w_substrate * substrate_3D +
  w_lfy * lfyscores +
  w_wood * woodscores_resc 

#Calculate reach-level habitat quality for BNT
reach.qualityBNT <-   w_temp * tempscores +
  w_substrate * substrate_3D +
  w_lfy * lfyscores +
  w_wood * woodscores_resc 

#Habitat quality vs quantity weights (expert elicitation information)
w_quality <- 0.55   # habitat quality
w_quantity <- 0.45  # habitat quantity
w_quality + w_quantity   #check it sums to 1

#Calculate reach-level habitat scores with quality/quantity weights
  # BKT
    reach.scBKT <- w_quality*reach.qualityBKT + w_quantity*NC_3D
  
  # BNT
    reach.scBNT <- w_quality*reach.qualityBNT + w_quantity*NC_3D

    
# Implement to make the reach score = 0 when temperature is >= 23.9 deg.C
# Subset temperature data for number of simulations
if(sims != dim(temps)[3]){
  temps <- temps[,,1:sims]
}

# Assign 0 score to reaches >= 23.9 deg.C
for(k in 1:sims){
  for(j in 1:ncol(reach.scBKT)){
    for(i in 1:nrow(reach.scBKT)){
      if(temps[i,j,k] >= 23.9) {
        reach.scBKT[i,j,k] <- 0
        reach.scBNT[i,j,k] <- 0
      }
    }
  }
}


#### Summarizing by unit ####
# Add in COMID labels (for summarizing by SEU)
reach.habscoresBKT <- array(NA, dim=c(length(NCscores), yrs+1, sims))
reach.habscoresBNT <- array(NA, dim=c(length(NCscores), yrs+1, sims))    
    
for (k in 1:sims) {
  reach.habscoresBKT[,,k] <- cbind(habdata$COMID, reach.scBKT[,,k])
    reach.habscoresBNT[,,k] <- cbind(habdata$COMID, reach.scBNT[,,k])
}
    
#load in area data information that containts the SEU for each COMIS
area.data <- read.csv("Data/Spatial/area.data.csv") %>% 
  select(unit, area.acre, COMID)

#add in COMID name
dimnames(reach.habscoresBKT) <- list(NULL, c("COMID", paste0("X", 1:yrs)), NULL)
dimnames(reach.habscoresBNT) <- list(NULL, c("COMID", paste0("X", 1:yrs)), NULL)

scoreBKT <- list()
scoreBKT.var <- list()
scoreBKT.sd <- list()

scoreBNT <- list()
scoreBNT.var <- list()
scoreBNT.sd <- list()

#subset for just units above Mio (1-19)
area.data.sub <- area.data %>% 
  filter(area.data$unit < 20)

# Summary statistics by SEU and species
for(k in 1:sims) {
#BKT
    x <- merge(area.data.sub, reach.habscoresBKT[,,k] , by="COMID")
    
    #weighted mean score 
    meanBKT <- x %>%
      group_by(unit) %>%
      summarise(
        across(
          starts_with("X"),
          list(
            weighted_mean = ~weighted.mean(.x, area.acre)),
            .names = "{.col}_{.fn}"),
            .groups = "drop")
      
    scoreBKT[[k]] <- as.matrix(meanBKT)
    
    #weighted variance   
    varBKT <- x %>%
      group_by(unit) %>%
      summarise(
        across(
          starts_with("X"),
          list(
            weighted_var  = ~sum(area.acre * (.x - weighted.mean(.x, area.acre))^2) / sum(area.acre)),
            .names = "{.col}_{.fn}"),
            .groups = "drop")
    
    scoreBKT.var[[k]] <- as.matrix(varBKT)
    
    #weighted standard deviation   
    sdBKT <- x %>%
      group_by(unit) %>%
      summarise(
        across(
          starts_with("X"),
          list(
            weighted_sd   = ~sqrt(sum(area.acre * (.x - weighted.mean(.x, area.acre))^2) / sum(area.acre))),
            .names = "{.col}_{.fn}"),
          .groups = "drop")
    
    scoreBKT.sd[[k]] <- as.matrix(sdBKT)  

#BNT
    y <- merge(area.data.sub, reach.habscoresBNT[,,k] , by="COMID")
    
    #weighted mean score 
    meanBNT <- y %>%
      group_by(unit) %>%
      summarise(
        across(
          starts_with("X"),
          list(
            weighted_mean = ~weighted.mean(.x, area.acre)),
          .names = "{.col}_{.fn}"),
        .groups = "drop")
    
    scoreBNT[[k]] <- as.matrix(meanBNT)
    
    #weighted variance   
    varBNT <- y %>%
      group_by(unit) %>%
      summarise(
        across(
          starts_with("X"),
          list(
            weighted_var  = ~sum(area.acre * (.x - weighted.mean(.x, area.acre))^2) / sum(area.acre)),
          .names = "{.col}_{.fn}"),
        .groups = "drop")
    
    scoreBNT.var[[k]] <- as.matrix(varBNT)
    
    #weighted standard deviation   
    sdBNT <- y %>%
      group_by(unit) %>%
      summarise(
        across(
          starts_with("X"),
          list(
            weighted_sd   = ~sqrt(sum(area.acre * (.x - weighted.mean(.x, area.acre))^2) / sum(area.acre))),
          .names = "{.col}_{.fn}"),
        .groups = "drop")
    
    scoreBNT.sd[[k]] <- as.matrix(sdBNT)  

      }

# Convert matrices list into array and rename columns
##BKT
    unit.scoresBKT.noburnin <- array(as.numeric(unlist(scoreBKT)), dim=c(nrow(scoreBKT[[1]]), yrs+1, sims))
    dimnames(unit.scoresBKT.noburnin) <- list(NULL,colnames(scoreBKT[[1]]), NULL)

    unit.scoresBKT.var <- array(as.numeric(unlist(scoreBKT.var)), dim=c(nrow(scoreBKT.var[[1]]), yrs+1, sims))
    dimnames(unit.scoresBKT.var) <- list(NULL,colnames(scoreBKT.var[[1]]), NULL)

    unit.scoresBKT.sd <- array(as.numeric(unlist(scoreBKT.sd)), dim=c(nrow(scoreBKT.sd[[1]]), yrs+1, sims))
    dimnames(unit.scoresBKT.sd) <- list(NULL,colnames(scoreBKT.sd[[1]]), NULL)

##BNT
    unit.scoresBNT.noburnin <- array(as.numeric(unlist(scoreBNT)), dim=c(nrow(scoreBNT[[1]]), yrs+1, sims))
    dimnames(unit.scoresBNT.noburnin) <- list(NULL,colnames(scoreBNT[[1]]), NULL)

    unit.scoresBNT.var <- array(as.numeric(unlist(scoreBNT.var)), dim=c(nrow(scoreBNT.var[[1]]), yrs+1, sims))
    dimnames(unit.scoresBNT.var) <- list(NULL,colnames(scoreBNT.var[[1]]), NULL)

    unit.scoresBNT.sd <- array(as.numeric(unlist(scoreBNT.sd)), dim=c(nrow(scoreBNT.sd[[1]]), yrs+1, sims))
    dimnames(unit.scoresBNT.sd) <- list(NULL,colnames(scoreBNT.sd[[1]]), NULL)

#### Adding burn in to beginning of habitat data ####
# Habitat scores do not change during the burn in period
# Dimensions of the original array
  n_units <- dim(unit.scoresBKT.noburnin)[1]  # 19 units
  n_years <- dim(unit.scoresBKT.noburnin)[2]  # 1 label + yrs
  n_sims  <- dim(unit.scoresBKT.noburnin)[3]  # sims

# Initialize a new array with nburn more columns 
  unit.scoresBKT <- array(NA, dim = c(n_units, n_years + nburn, n_sims))
  unit.scoresBNT <- array(NA, dim = c(n_units, n_years + nburn, n_sims))

# Copy the unit label column (column 1)
  unit.scoresBKT[, 1, ] <- unit.scoresBKT.noburnin[, 1, ]
  unit.scoresBNT[, 1, ] <- unit.scoresBNT.noburnin[, 1, ]

# Copy column 2 (first year of habitat scores) into the next nburn columns (2:nburn)
  for (i in 1:nburn) {
    unit.scoresBKT[, i + 1, ] <- unit.scoresBKT.noburnin[, 2, ]
    unit.scoresBNT[, i + 1, ] <- unit.scoresBNT.noburnin[, 2, ]

  }

# Copy the remaining habitat score columns
  unit.scoresBKT[, 7:(n_years + nburn), ] <- unit.scoresBKT.noburnin[, 2:n_years, ]
  unit.scoresBNT[, 7:(n_years + nburn), ] <- unit.scoresBNT.noburnin[, 2:n_years, ]

# Remove 1st column (SEU labels)
  unit.scoresBKT <- unit.scoresBKT[,-1,]
  unit.scoresBNT <- unit.scoresBNT[,-1,]
  
  