## BROOK and BROWN TROUT IBM ##
## DEVELOPED BY SHANE FLINN ##
## ADAPTED BY KAILI GREGORY

#Functions for brook trout model

# Initialize population with a function
inits.pop <- function(data){
  
        n.adults.bkt <- sum(initN.bkt[,j+1]) #number of bkt indivs in sim j (+1 b/c first column is unit number)
        n.adults.bnt <- sum(initN.bnt[,j+1]) #number of bkt indivs in sim j
        
        data.bkt <- array(data = 0, dim = c((n.adults.bkt), 8)) # no. of individuals and 8 characteristics
        colnames(data.bkt) <- c("length", "sex", "mature", "death", "unit","loc_score", "age", "species"); #Loc score is habitat score, age is age, species is species
        data.bnt <- array(data = 0, dim = c((n.adults.bnt), 8)) # no. of individuals and 8 characteristics
        colnames(data.bnt) <- c("length", "sex", "mature", "death", "unit","loc_score", "age", "species"); #Loc score is habitat score, age is age, species is species

        #col 1: assign lengths (NA for now)
        data.bkt[c(1:n.adults.bkt), 1] <- NA
        data.bnt[c(1:n.adults.bnt), 1] <- NA
        
        #col 2: assign sex
        data.bkt[1:n.adults.bkt, 2] <- sample(c(0,1), replace=TRUE, size=n.adults.bkt , prob=c(0.5,0.5)); # male = 0; female = 1
        data.bnt[1:n.adults.bnt, 2] <- sample(c(0,1), replace=TRUE, size=n.adults.bnt , prob=c(0.5,0.5)); # male = 0; female = 1
        
        #cols 7 and 3: assign age (7) and maturity (3)
        bkt.probabilities <- c(0.73140446, 0.23378638, 0.03162654, 0.00318261)  # Probability of being  age 0-3 (avg Au Sable age ratio, 2010-present)
        bnt.probabilities <- c(0.7678378540, 0.1531733264, 0.0473504666, 0.0175149262, 
                               0.0101152363, 0.0033002243, 0.0007079661)
        
          for(k in 1:nrow(data.bkt)) { #for each fish, assign an age and maturity
            #age (col7)
            data.bkt[k,7] <-  sample(0:3, 1, replace = TRUE, prob = bkt.probabilities)
            
            #maturity (col3)
            if(data.bkt[k,7] == 0) {data.bkt[k,3] <- 0} #if age 0 , all fish are immature (0)
            if(data.bkt[k, 7] >= 1 & data.bkt[k,2] == 0) {data.bkt[k,3] <- 1} #if age 1+ and male (0), mature (1)
            if(data.bkt[k, 7] == 1 & data.bkt[k,2] == 1) {data.bkt[k,3] <- rbinom(1, 1, 0.8)} #if age 1 and female (1), 80% are mature (1), 20% are immature (0)
            if(data.bkt[k,7] > 1 & data.bkt[k,2] == 1) {data.bkt[k,3] <- 1} #if older than 1 and female (1), mature (1)
          }
        
          for(k in 1:nrow(data.bnt)) { #for each fish, assign an age and maturity
            #age (col7)
            data.bnt[k,7] <-  sample(0:6, 1, replace = TRUE, prob = bnt.probabilities)
            
            #maturity (col3)
            if(data.bnt[k,7] == 0) {data.bnt[k,3] <- 0} #if age 0 , all fish are immature (0)
            
            #males
              if(data.bnt[k, 7] == 1 & data.bnt[k,2] == 0) {data.bnt[k,3] <- rbinom(1, 1, 0.32)} #if age 1 and male (0), 32% are mature (1) (zorn 2018,Taube 1976)
              if(data.bnt[k, 7] == 2 & data.bnt[k,2] == 0) {data.bnt[k,3] <- rbinom(1, 1, 0.79)} #if age 2 and male (0), 79% are mature (1)
              if(data.bnt[k, 7] >= 3 & data.bnt[k,2] == 0) {data.bnt[k,3] <- 1} #if age 3+ and male (0), mature (1)
            
            #females
              if(data.bnt[k, 7] == 1 & data.bnt[k,2] == 1) {data.bnt[k,3] <- rbinom(1, 1, 0.16)} #if age 1 and female (1), 16% are mature (1)
              if(data.bnt[k, 7] == 2 & data.bnt[k,2] == 1) {data.bnt[k,3] <- rbinom(1, 1, 0.77)} #if age 2 and female (1), 77% are mature (1)
              if(data.bnt[k, 7] == 3 & data.bnt[k,2] == 1) {data.bnt[k,3] <- rbinom(1, 1, 0.88)} #if age 3+ and female (1), 88% mature (1)
              if(data.bnt[k, 7] >= 4 & data.bnt[k,2] == 1) {data.bnt[k,3] <- 1} #if age 4+ and female (1), mature (1)
            }
        
        #cols 5 and 6: assign initial locations(5) and habitat scores(6)
        unit <- initN.bkt[,1]
        
        initbrook <- cumsum(initN.bkt[,j+1]) #cumulative sum of init pop size
        initbrown <- cumsum(initN.bnt[,j+1])
        
        #assign starting habitat score for that simulation
        # habitatscore.bkt <-  as.data.frame(unit.scoresBKT[,i+1,j]) #bkt habitat score in year 1, sim j (column 2 b/c 1st col is unit)
        habitatscore.bkt <-  as.data.frame(unit.scoresBKT[,i,j]) #bkt habitat score in year 1, sim j (column 2 b/c 1st col is unit)
        initN.byunit.bkt <- as.data.frame(cbind(unit, initbrook, habitatscore.bkt))

        # habitatscore.bnt <-  as.data.frame(unit.scoresBKT[,i+1,j]) #bnt habitat score in year 1, sim j (column 2 b/c 1st col is unit)
        habitatscore.bnt <-  as.data.frame(unit.scoresBKT[,i,j]) #bnt habitat score in year 1, sim j (column 2 b/c 1st col is unit)
        initN.byunit.bnt <- as.data.frame(cbind(unit, initbrown, habitatscore.bnt))
        
        #loop to give unit #s and habitat scores to fish based on how many are in each unit
          #ex. 100 fish in unit 1 -> first 100 rows of data.bkt are assigned unit 1 and the respective habitat score
            #BKT
        start.idx.bkt <- 1
        totalbkt <- nrow(data.bkt)
        
        for (z in 1:nrow(initN.byunit.bkt)) {
          end.idx.bkt <- initN.byunit.bkt[z, 2]
          
          # Clamp to the number of fish to avoid going out of bounds
          end.idx.bkt <- min(end.idx.bkt, totalbkt)
          
          if (end.idx.bkt >= start.idx.bkt) {
            data.bkt[start.idx.bkt:end.idx.bkt, 5] <- initN.byunit.bkt[z, 1]  # habitat ID
            data.bkt[start.idx.bkt:end.idx.bkt, 6] <- initN.byunit.bkt[z, 3]  # habitat score
          }
          start.idx.bkt <- end.idx.bkt + 1
        }
            
            #BNT
        start.idx.bnt <- 1
        totalbnt <- nrow(data.bnt)
        
        for (z in 1:nrow(initN.byunit.bnt)) {
          end.idx.bnt <- initN.byunit.bnt[z, 2]
          
          # Clamp to the number of fish to avoid going out of bounds
          end.idx.bnt <- min(end.idx.bnt, totalbnt)
          
          if (end.idx.bnt >= start.idx.bnt) {
            data.bnt[start.idx.bnt:end.idx.bnt, 5] <- initN.byunit.bnt[z, 1]  # habitat ID
            data.bnt[start.idx.bnt:end.idx.bnt, 6] <- initN.byunit.bnt[z, 3]  # habitat score
          }
          start.idx.bnt <- end.idx.bnt + 1
        }
        
            
        #label species as brook trout - bkt - (col 8)
        data.bkt[,8] <- "bkt"
        data.bnt[,8] <- "bnt"
        
      #combine species data
        combined.data <- as.data.frame(rbind(data.bkt, data.bnt))
        combined.data$sex <- as.numeric(combined.data$sex)
        combined.data$mature <- as.numeric(combined.data$mature)
        combined.data$death <- as.numeric(combined.data$death)
        combined.data$unit <- as.numeric(combined.data$unit)
        combined.data$loc_score <- as.numeric(combined.data$loc_score)
        combined.data$age <- as.numeric(combined.data$age)
        combined.data$species <- as.factor(combined.data$species)
        return(combined.data)
}

# Mortality functions (uses instantaneous rates of mortality)
mortrates <- read.csv("Data/Fish/mortality_rates.csv")

dt <- 182.5
delta.t <- 365

imr.age1.bkt <-  1 - exp((log(mortrates$survival[1]) * dt) / delta.t)
imr.age2.bkt <- 1 - exp((log(mortrates$survival[2]) * dt) / delta.t)
imr.age3.bkt <- 1 - exp((log(mortrates$survival[3]) * dt) / delta.t)
var.age1.bkt <- mortrates$var[1]
var.age2.bkt <- mortrates$var[2]
var.age3.bkt <- mortrates$var[3]

imr.age1.bkt.bnt <-  1 - exp((log(mortrates$survival[4]) * dt) / delta.t)
imr.age2.bkt.bnt <- 1 - exp((log(mortrates$survival[5]) * dt) / delta.t)
imr.age3.bkt.bnt <- 0
var.age1.bkt.bnt <- mortrates$var[4]
var.age2.bkt.bnt <- mortrates$var[5]

imr.age1.bnt <-  1 - exp((log(mortrates$survival[7]) * dt) / delta.t)
imr.age2.bnt <- 1 - exp((log(mortrates$survival[8]) * dt) / delta.t)
imr.age3.bnt <- 1 - exp((log(mortrates$survival[9]) * dt) / delta.t)
var.age1.bnt <- mortrates$var[7]
var.age2.bnt <- mortrates$var[8]
var.age3.bnt <- mortrates$var[9]

estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

beta.params <- list(
  age1.bkt.bnt = estBetaParams(mu = imr.age1.bkt.bnt, var = var.age1.bkt.bnt),
  age2.bkt.bnt = estBetaParams(mu = imr.age2.bkt.bnt, var = var.age2.bkt.bnt),
  age1.bkt = estBetaParams(mu = imr.age1.bkt, var = var.age1.bkt),
  age2.bkt = estBetaParams(mu = imr.age2.bkt, var = var.age2.bkt),
  age3.bkt = estBetaParams(mu = imr.age3.bkt, var = var.age3.bkt),
  age1.bnt = estBetaParams(mu = imr.age1.bnt, var = var.age1.bnt),
  age2.bnt = estBetaParams(mu = imr.age2.bnt, var = var.age2.bnt),
  age3.bnt = estBetaParams(mu = imr.age3.bnt, var = var.age3.bnt)
)

mortality <- function(data, death_col=4, unit_col=5, age_col=7, spp_col=8){
  if(nrow(data) == 0){ #if total pop (both spp) is 0, pass an array to make this an empty data frame
    newdata <- array(data = 0, dim = c(0, 8)); # #'s of individuals and 8 characteristics
    colnames(newdata) <- c("length", "sex", "mature", "death", "unit", "loc_score", "age", "species"); 
    # give colnames to reflect characteristics we are including 
  } else {
    
    #store unit, age, spp in vectors
    unit <- data[, unit_col]
    age <- data[, age_col]
    species <- as.character(data[, spp_col])
    
    # Create logical index for bnt presence in each unit
    bnt_by_unit <- unique(unit[species == "bnt"])
    bnt_in_unit <- unit %in% bnt_by_unit
    
    #create empty death vector
    death <- rep(0L, nrow(data))
    
    for(i in 1:nrow(data)) {
      species.i <- species[i]
      age.i <- age[i]
      
      if(species.i == "bkt"){
        
        #if there are bnt, bkt have reduced survival
        if(bnt_in_unit[i]) {
          
          #age 1 bkt mortality w/ bnt present
          if(age.i == 1) 
          {#p1.bkt.bnt <- estBetaParams(mu=imr.age1.bkt.bnt, var=var.age1.bkt.bnt)
            death[i] <- rbinom(n = 1, size=1, prob=rbeta(1, beta.params$age1.bkt.bnt$alpha, beta.params$age1.bkt.bnt$beta)) }
          
          #age 2 bkt mortality w/ bnt present
          else if(age.i == 2)
          {#p2.bkt.bnt <- estBetaParams(mu=imr.age2.bkt.bnt, var=var.age2.bkt.bnt)
            death[i] <- rbinom(n = 1, size=1, prob=rbeta(1, beta.params$age2.bkt.bnt$alpha, beta.params$age2.bkt.bnt$beta)) }
          
          #age3 bkt mortality w/ bnt present (100% mortality)
          else if(age.i >= 3)
          {death[i] <- 1}
        }
        
        #if there are NOT bnt, bkt have higher survival
        else {
          if(age.i == 1) 
          {#p1.bkt<- estBetaParams(mu=imr.age1.bkt, var=var.age1.bkt)
            death[i] <- rbinom(n = 1, size=1, prob=rbeta(1, beta.params$age1.bkt$alpha, beta.params$age1.bkt$beta)) }
          
          #age 2 bkt mortality w/ bnt present
          else if(age.i == 2)
          {#p2.bkt <- estBetaParams(mu=imr.age2.bkt, var=var.age2.bkt)
            death[i] <- rbinom(n = 1, size=1, prob=rbeta(1, beta.params$age2.bkt$alpha, beta.params$age2.bkt$beta)) }
          
          #age 3 bkt mortality w/ bnt present
          else if(age.i == 3)
          {#p2.bkt <- estBetaParams(mu=imr.age2.bkt, var=var.age2.bkt)
            death[i] <- rbinom(n = 1, size=1, prob=rbeta(1, beta.params$age3.bkt$alpha, beta.params$age3.bkt$beta)) }
          
          #age 4 bkt mortality w/ bnt present (100% mortality)
          else if(age.i >= 4)
          {death[i] <- 1}
        }
      }
      else
        if(species.i == "bnt") {
          #age 1 bnt mortality
          if(age.i == 1)
          {#p1.bnt <- estBetaParams(mu=imr.age1.bnt, var=var.age1.bnt)
            death[i] <- rbinom(n = 1, size=1, prob=rbeta(1, beta.params$age1.bnt$alpha, beta.params$age1.bnt$beta)) }
          
          #age 2 bnt mortality
          else if(age.i == 2)
          {#p2.bnt <- estBetaParams(mu=imr.age2.bnt, var=var.age2.bnt)
            death[i] <- rbinom(n = 1, size=1, prob=rbeta(1, beta.params$age2.bnt$alpha, beta.params$age2.bnt$beta))}
          
          #age 3, 4, 6 bnt mortality (all assumed to be the same)
          else if(age.i %in% c(3, 4, 5))
          {#p3.bnt <- estBetaParams(mu=imr.age3.bnt, var=var.age3.bnt)
            death[i] <- rbinom(n = 1, size=1, prob=rbeta(1,1, beta.params$age3.bnt$alpha, beta.params$age3.bnt$beta))}
          
          #age 6 bnt mortality (100% mortality)
          else if(age.i >= 6)
          {death[i] <- 1}
        }
    }
  }
  
  data[,death_col] <- death
  
  # Filter to keep only fish that are still alive (death == 0)
  newdata <- data[death == 0, ]
  return(newdata)
  }

# Movement Function
tbkt <- as.data.frame(apply(unit.scoresBKT, c(2,3), sum)) #out of the total river, what is the total habitat score in each year, in each sim
tbnt <- as.data.frame(apply(unit.scoresBNT, c(2,3), sum)) #out of the total river, what is the total habitat score in each year, in each sim


spawner_movement <- function(data, mature_col = 3, spp_col=8) { #inds data goes in
  
  #BKT: how many individuals are allocated to each SEU?
    total_spawners.bkt <- sum(data[,spp_col] == "bkt" & data[,mature_col] ==1) #how many bkt spawning indivs are there?
    # cell_density.bkt <- (total_spawners.bkt * (unit.scoresBKT[,i+1,j] / tbkt[i,j]))  # habitat scores (change thru time)
    cell_density.bkt <- (total_spawners.bkt * (unit.scoresBKT[,i,j] / tbkt[i,j]))  # habitat scores (change thru time)
    cell_dens.bkt <- round(cell_density.bkt)

  #BNT: how many individuals are allocated to each SEU?
    total_spawners.bnt <- sum(data[,spp_col] == "bnt" & data[,mature_col] ==1)  #how many bkt spawning indivs are there?
    # cell_density.bnt <- (total_spawners.bnt * (unit.scoresBNT[,i+1,j] / tbnt[i,j]))  # habitat scores (change thru time)
    cell_density.bnt <- (total_spawners.bnt * (unit.scoresBNT[,i,j] / tbnt[i,j]))  # habitat scores (change thru time)
    cell_dens.bnt <- round(cell_density.bnt)

#assemble output data
  unit <- c(1:n_units)
  abundance.bkt <- cell_dens.bkt
  abundance.bnt <- cell_dens.bnt
  # habitat_score.bkt <- unit.scoresBKT[,i+1,j]
  # habitat_score.bnt <- unit.scoresBNT[,i+1,j]
  habitat_score.bkt <- unit.scoresBKT[,i,j]
  habitat_score.bnt <- unit.scoresBNT[,i,j]

  
  K.bkt <- kBKT[,i,j]
  K.bnt <- kBNT[,i,j]
  
  density <- as.data.frame(cbind(unit, abundance.bkt, habitat_score.bkt,
                                 abundance.bnt, habitat_score.bnt,
                                 K.bkt, K.bnt))

  return(density)
}

update_locations <- function(data, movedata) {
  #Filter into spawners (can move) and nonspawners (stay in unit)
  spawners.df <- data[data[,3]==1, ]
  nonspawners.df <- data[data[,3]==0, ]

  #target spawners for each unit - by species and then combines
  target_bkt <- data.frame(unit = movedata[,1], target_spawners = movedata[,2], species = "bkt")
  target_bnt <- data.frame(unit = movedata[,1], target_spawners = movedata[,4], species = "bnt")
  target_spawners_df <- rbind(target_bkt, target_bnt)

  #Create an empty list to collect reassigned spawners by species
  reassigned.spawners <- list()

  #Loop over each species
  for(sp in c("bkt", "bnt")){
    sp_spawners <- spawners.df[spawners.df[,8] == sp, ] # spawners of a given species in current data
    sp_targets <- target_spawners_df[target_spawners_df$species == sp, ] # spawners to allocate to new units

    # shuffle spawners to randomize unit assignment
    shuffled_spawners <- sp_spawners[sample(nrow(sp_spawners)), ]

    #calculate cumulative inde ranges for assignment
    ntargets <- sp_targets$target_spawners
    ntargets[is.na(ntargets)] <- 0  # handle NA targets as 0s
    ends <- cumsum(ntargets) #cumulative sums to determine end indices for assignment
    starts <- c(1, head(ends, -1) + 1) #corresponding start indices

    # empty list to store spawners assigned to each unit
    assigned_units <- vector("list", length(ntargets))

    # Loop through all target units for this species
    for (i in seq_along(ntargets)) {
      if (starts[i] <= nrow(shuffled_spawners)) {     # are there any spawners to assign?

        # Define a safe index range from shuffled spawner pool
        idx_range <- starts[i] : min(ends[i], nrow(shuffled_spawners))

        # Extract the appropriate individuals
        assigned <- shuffled_spawners[idx_range, ]

        # Assign the target unit ID to their 'unit' column
        assigned$unit <- sp_targets$unit[i]

        # Store the updated individuals in the assignment list
        assigned_units[[i]] <- assigned
      }
      # If no fish are left (starts[i] > nrow()), skip that unit
    }

    # Combine all the assigned individuals for the species into one data frame
    reassigned.spawners[[sp]] <- do.call(rbind, assigned_units)
  }
  #combine unmoved juveniles with moved spawners
  newData <- rbind(nonspawners.df, do.call(rbind, reassigned.spawners))

return(newData)
}

# Recruitment function
Lin_Recruit <- function(Nt, r, Kn){
  Recruits <- Nt*((exp(r*((1-(Nt/Kn)))-1 + rnorm(1, mean = 0, sd = 1)))) #ricker function
  Recruits <- round(Recruits, digits=0)
  if(is.na(Recruits)){Recruits <- NA}
  else {Recruits <- Recruits}
  return(Recruits)
}

recruitment <- function(data){
  #spawning stock
    stock.bkt <- data[,2]
    stock.bnt <- data[,4]
  
  #carrying capacity 
    K.bkt <- data[,6]
    K.bnt <- data[,7]
    
  #recruitment habitat suitability score
    r.BKT <- r.bkt[,i,j]
    r.BNT <- r.bnt[,i,j]
    
    # Vectorized recruitment (add new columns to move)
    data[,8] <- mapply(Lin_Recruit, Nt = stock.bkt, r = r.BKT, Kn = K.bkt)
    data[,9] <- mapply(Lin_Recruit, Nt = stock.bnt, r = r.BNT, Kn = K.bnt)
    
    #assemble rec output
    rec <- data.frame(data[,1],
                      recruits.bkt= round(data[,8]),
                      recruits.bnt= round(data[,9]))

  return(rec)
}

recruit.locs <- function(data, recdata, movedata){
      #BKT recruits
      total.recs.bkt <- sum(recdata[,2]) #total number of BKT recruits
      
      if (total.recs.bkt > 0) { #if there are >0 recruits
        n_recs_bkt <- recdata[,2] # number of recs by unit
        idx_bkt <- rep(seq_along(n_recs_bkt), times = n_recs_bkt) #index for indiv recruits
        
        newinds.bkt <- data.frame(
          length     = NA,
          sex        = sample(c(0, 1), total.recs.bkt, replace = TRUE), # assume 50/50 sex ratio
          mature     = 0L,
          death      = 0L,
          unit       = movedata[idx_bkt, 1],  # unit for indiv recruits
          loc_score  = movedata[idx_bkt, 3],  # habitat score for indiv recruits
          age        = 0L,
          species    = "bkt",
          stringsAsFactors = FALSE
        )
      } else { #if there are 0 recruits, create empty matrix
        newinds.bkt <- data.frame(matrix(NA, nrow = 0, ncol = ncol(data)))
        colnames(newinds.bkt) <- colnames(data)
      }
      
      #BNT recruits
      total.recs.bnt <- sum(recdata[,3]) #total number of BKT recruits
      
      if (total.recs.bnt > 0) { #if there are >0 recruits
        n_recs_bnt <- recdata[,3] # number of recs by unit
        idx_bnt <- rep(seq_along(n_recs_bnt), times = n_recs_bnt) #index for indiv recruits
        
        newinds.bnt <- data.frame(
          length     = NA,
          sex        = sample(c(0, 1), total.recs.bnt, replace = TRUE), # assume 50/50 sex ratio
          mature     = 0L,
          death      = 0L,
          unit       = movedata[idx_bnt, 1],  # unit for indiv recruits
          loc_score  = movedata[idx_bnt, 3],  # habitat score for indiv recruits
          age        = 0L,
          species    = "bnt",
          stringsAsFactors = FALSE
        )
      } else { #if there are 0 recruits, create empty matrix
        newinds.bnt <- data.frame(matrix(NA, nrow = 0, ncol = ncol(data)))
        colnames(newinds.bnt) <- colnames(data)
      }
      
      # Combine and ensure correct format
      newData <- rbind(data, newinds.bkt, newinds.bnt)
      
      # Ensure column types match expectations
      newData$length     <- as.numeric(newData$length)
      newData$sex        <- as.integer(newData$sex)
      newData$mature     <- as.integer(newData$mature)
      newData$death      <- as.integer(newData$death)
      newData$unit       <- as.integer(newData$unit)
      newData$loc_score  <- as.numeric(newData$loc_score)
      newData$age        <- as.integer(newData$age)
      newData$species    <- as.factor(newData$species)
      
   return(newData)
      }

#von Bert growth function
vonBert <- function(Linf, k, t0, age){
  Length <- Linf * (1 - exp(-k * (age - t0)))
}

#parameters of the vonBert
length.params.bkt <- read.csv("Data/Fish/Length_data/nls_lengthparametersBKT.csv")
length.params.bkt <- length.params.bkt[,-1]

length.params.bnt <- read.csv("Data/Fish/Length_data/nls_lengthparametersBNT.csv")
length.params.bnt <- length.params.bnt[,-1]

params.bkt <- NA #empty object, used to randomly select which parameters to use
params.bnt <- NA #empty object, used to randomly select which parameters to use

growth <- function(data, length_col=1, age_col=7, spp_col=8){

  if(nrow(data) == 0){
    newData <- array(data = 0, dim = c(0, 8)); # #'s of individuals and 8 characteristics
    colnames(newData) <- c("length", "sex", "mature", "death", "unit", "loc_score", "age_col", "species"); # give colnames to reflect characteristics we are including
  } else {

    #age indivs (add one year) 
    for(i in 1:nrow(data)){
        params.bkt <- sample(1:nrow(length.params.bkt), size=1) #pick a value from 1-nrows of parameters
        params.bnt <- sample(1:nrow(length.params.bnt), size=1) #pick a value from 1-nrows of parameters
          #randomly selects bootstrapped vonBert parameters
      
      if(data[i,spp_col] == "bkt") {
        data[i,length_col] <- as.numeric(vonBert(Linf= length.params.bkt[params.bkt, 1],
                                      k= length.params.bkt[params.bkt, 2],
                                      t0= length.params.bkt[params.bkt, 3],
                                      age= as.numeric(data[i,age_col])))
      }
      else if(data[i,spp_col] == "bnt") {
        data[i,length_col] <- as.numeric(vonBert(Linf= length.params.bnt[params.bnt, 1],
                                                 k= length.params.bnt[params.bnt, 2],
                                                 t0= length.params.bnt[params.bnt, 3],
                                                 age= as.numeric(data[i,age_col])))
      }
    }
  }
  newData <- data
  return(newData)
}

age <- function(data, age_col=7) {
  if (nrow(data) == 0) {
    data5 <- array(data = 0, dim = c(0, 8))
    colnames(data5) <- c("length", "sex", "mature", "death", "unit", "loc_score", "age", "species")
    
  } else {
    # Vectorized aging (add one year)
    data[[age_col]] <- as.numeric(data[[age_col]]) + 1
  }
  
  newData <- data
  return(newData)
}

mature <- function(data, sex_col= 2, mature_col= 3, age_col= 7, spp_col=8){
      if(nrow(data) == 0){ #if there are no fish, create empty vector
        newData <- array(data = 0, dim = c(0, 8)) # #'s of individuals and 8 characteristics
        colnames(newData) <- c("length", "sex", "mature", "death", "unit","loc_score", "age", "species"); #Loc score is habitat score, recruit is age
      } else {
        #store values
        sex     <- as.numeric(data[, sex_col])       # 0 = male, 1 = female
        age     <- as.numeric(data[, age_col])       # numeric age
        species <- as.character(data[, spp_col])     # "bkt" or "bnt"
        mature  <- as.numeric(data[, mature_col])    # 0 = immature, 1 = mature
        immature <- mature == 0                      # logical: TRUE for only immature fish
        
        is_bkt <- species == "bkt"
        is_bnt <- species == "bnt"
        
        #BKT maturation
          #males (sex 0) age ≥ 1 - all mature
          data[immature & is_bkt & age >= 1 & sex == 0, 3] <- 1
          
          #females (sex 1) age 1 - 80% chance of maturing
          idx <- immature & is_bkt & age == 1 & sex == 1
          data[idx, 3] <- rbinom(sum(idx), 1, 0.8)
          
          #females (sex 1) age > 1 - all mature
          data[immature & is_bkt & age > 1 & sex == 1, 3] <- 1
          
        #BNT maturation
          #male (sex 0)
            #age 1 - 32% chance of maturing
            idx <- immature & is_bnt & age == 1 & sex == 0
            data[idx, 3] <- rbinom(sum(idx), 1, 0.32)
            
            #age 2 - 79% chance of maturing
            idx <- immature & is_bnt & age == 2 & sex == 0
            data[idx, 3] <- rbinom(sum(idx), 1, 0.79)
            
            #age ≥ 3 - all mature mature
            data[immature & is_bnt & age >= 3 & sex == 0, 3] <- 1
          
          #female (sex 1)
            #age 1 - 16% chance of maturing
            idx <- immature & is_bnt & age == 1 & sex == 1
            data[idx, 3] <- rbinom(sum(idx), 1, 0.16)
            
            #age 2 - 77% chance of maturing
            idx <- immature & is_bnt & age == 2 & sex == 1
            data[idx, 3] <- rbinom(sum(idx), 1, 0.77)
            
            #age 3 - 88% chance of maturing
            idx <- immature & is_bnt & age == 3 & sex == 1
            data[idx, 3] <- rbinom(sum(idx), 1, 0.88)
            
            #age ≥ 4 - all mature
            data[immature & is_bnt & age >= 4 & sex == 1, 3] <- 1
        
        newData <- data
      }
  return(newData)
}
