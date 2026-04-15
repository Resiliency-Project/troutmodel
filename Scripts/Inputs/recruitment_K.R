## BROOK and BROWN TROUT IBM ##
## DEVELOPED BY SHANE FLINN ##
## ADAPTED BY KAILI GREGORY

# Script for calculating recruitment rates and carrying capacity through time

# BKT and BNT each have habitat suitability scores for recruitment. They vary
  # mainly in terms of the relative importance of factors (from expert elicited data)
  # the scores (0-5) from each of the metrics (temperature, lfy, wood, substrate)
  # are the same as the habitat suitability scores for adult movement

### BKT recruitment ####
# Habitat quality metric weights (expert elicited)
w_temp_recBKT <- 0.425        # temperature weight
w_lfy_recBKT <- 0.27          # lfy weight
w_substrate_recBKT <- 0.162   # substrate weight
w_wood_recBKT <- 0.143        # wood weight
w_temp_recBKT + w_lfy_recBKT + w_substrate_recBKT + w_wood_recBKT #check they sum to 1
    
# Reach-level habitat quality BKT
reach.quality_recBKT <-   w_temp_recBKT * tempscores +
  w_substrate_recBKT * substrate_3D +
  w_lfy_recBKT * lfyscores +
  w_wood_recBKT * woodscores_resc 

# Habitat quality vs quantity weights (expert elicited)
w_quality_recBKT <- 0.625
w_quantity_recBKT <- 0.375
w_quality_recBKT + w_quantity_recBKT   #check they sum to 1

# Calculate reach-level habitat scores with quality/quantity weights
reach.sc_recBKT <- w_quality_recBKT*reach.quality_recBKT + w_quantity_recBKT*NC_3D

# Empty object for reach scores
reach.habscores_recBKT <- array(NA, dim=c(length(NCscores), yrs+1, sims))

# add in COMID labels
for (k in 1:sims) {
  reach.habscores_recBKT[,,k] <- cbind(habdata$COMID, reach.sc_recBKT[,,k])
}

# add in COMID name
dimnames(reach.habscores_recBKT) <- list(NULL, c("COMID", paste0("X", 1:yrs)), NULL)

score_recBKT <- list()
score_var_recBKT <- list()
score_sd_recBKT <- list()

#subset for just units above Mio (1-19)
area.data.sub <- area.data %>% 
  filter(area.data$unit < 20)

# Summary statistics for BKT recruitment score by SEU
for(k in 1:sims) {
  #BKT
  x <- merge(area.data.sub, reach.habscores_recBKT[,,k] , by="COMID")
  
  #weighted mean score 
  mean_recBKT <- x %>%
    group_by(unit) %>%
    summarise(
      across(
        starts_with("X"),
        list(
          weighted_mean = ~weighted.mean(.x, area.acre)),
        .names = "{.col}_{.fn}"),
      .groups = "drop")
  
  score_recBKT[[k]] <- as.matrix(mean_recBKT)
  
  #weighted variance   
  var_recBKT <- x %>%
    group_by(unit) %>%
    summarise(
      across(
        starts_with("X"),
        list(
          weighted_var  = ~sum(area.acre * (.x - weighted.mean(.x, area.acre))^2) / sum(area.acre)),
        .names = "{.col}_{.fn}"),
      .groups = "drop")
  
  score_var_recBKT[[k]] <- as.matrix(var_recBKT)
  
  #weighted standard deviation
  sd_recBKT <- x %>%
    group_by(unit) %>%
    summarise(
      across(
        starts_with("X"),
        list(
          weighted_sd   = ~sqrt(sum(area.acre * (.x - weighted.mean(.x, area.acre))^2) / sum(area.acre))),
        .names = "{.col}_{.fn}"),
      .groups = "drop")

  score_sd_recBKT[[k]] <- as.matrix(sd_recBKT)

}

#convert matrices list into array and rename columns
unit.scores_recBKT.noburnin <- array(as.numeric(unlist(score_recBKT)), dim=c(nrow(score_recBKT[[1]]), yrs+1, sims))
dimnames(unit.scores_recBKT.noburnin) <- list(NULL,colnames(score_recBKT[[1]]), NULL)

unit.scores.var_recBKT <- array(as.numeric(unlist(score_var_recBKT)), dim=c(nrow(score_var_recBKT[[1]]), yrs+1, sims))
dimnames(unit.scores.var_recBKT) <- list(NULL,colnames(score_var_recBKT[[1]]), NULL)

unit.scores.sd_recBKT <- array(as.numeric(unlist(score_sd_recBKT)), dim=c(nrow(score_sd_recBKT[[1]]), yrs+1, sims))
dimnames(unit.scores.sd_recBKT) <- list(NULL,colnames(score_sd_recBKT[[1]]), NULL)


### BNT recruitment ####
# Habitat quality metric weights (expert elicited)
w_temp_recBNT <- 0.375        # temperature weight
w_lfy_recBNT <- 0.22          # lfy weight
w_substrate_recBNT <- 0.2416  # substrate weight 
w_wood_recBNT <- 0.1634       # wood weight
w_temp_recBNT + w_lfy_recBNT + w_substrate_recBNT + w_wood_recBNT  #check they sum to 1

# Reach-level habitat quality BNT
reach.quality_recBNT <-   w_temp_recBNT * tempscores +
  w_substrate_recBNT * substrate_3D +
  w_lfy_recBNT * lfyscores +
  w_wood_recBNT * woodscores_resc 

# Habitat quality vs quantity weights (expert elicited)
w_quality_recBNT <- 0.54
w_quantity_recBNT <- 0.46
w_quality_recBNT + w_quantity_recBNT    #check they sum to 1

# Calculate reach-level habitat scores with quality/quantity weights
reach.sc_recBNT <- w_quality_recBNT*reach.quality_recBNT + w_quantity_recBNT*NC_3D

# Empty object for reach-level habitat suitability score
reach.habscores_recBNT <- array(NA, dim=c(length(NCscores), yrs+1, sims))

#add in COMID labels
for (k in 1:sims) {
  reach.habscores_recBNT[,,k] <- cbind(habdata$COMID, reach.sc_recBNT[,,k])
}

#add in COMID name
dimnames(reach.habscores_recBNT) <- list(NULL, c("COMID", paste0("X", 1:yrs)), NULL)

score_recBNT <- list()
score_var_recBNT <- list()
score_sd_recBNT <- list()

#subset for just units above Mio (1-19)
area.data.sub <- area.data %>% 
  filter(area.data$unit < 20)

# Summary statistics for BNT recruitment habitat score
for(k in 1:sims) {
  #BNT
  x <- merge(area.data.sub, reach.habscores_recBNT[,,k] , by="COMID")
  
  #weighted mean score 
  mean_recBNT <- x %>%
    group_by(unit) %>%
    summarise(
      across(
        starts_with("X"),
        list(
          weighted_mean = ~weighted.mean(.x, area.acre)),
        .names = "{.col}_{.fn}"),
      .groups = "drop")
  
  score_recBNT[[k]] <- as.matrix(mean_recBNT)
  
  #weighted variance   
  var_recBNT <- x %>%
    group_by(unit) %>%
    summarise(
      across(
        starts_with("X"),
        list(
          weighted_var  = ~sum(area.acre * (.x - weighted.mean(.x, area.acre))^2) / sum(area.acre)),
        .names = "{.col}_{.fn}"),
      .groups = "drop")
  
  score_var_recBNT[[k]] <- as.matrix(var_recBNT)
  
  #weighted standard deviation
  sd_recBNT <- x %>%
    group_by(unit) %>%
    summarise(
      across(
        starts_with("X"),
        list(
          weighted_sd   = ~sqrt(sum(area.acre * (.x - weighted.mean(.x, area.acre))^2) / sum(area.acre))),
        .names = "{.col}_{.fn}"),
      .groups = "drop")
  
  score_sd_recBNT[[k]] <- as.matrix(sd_recBNT)
  
}

#convert matrices list into array and rename columns
unit.scores_recBNT.noburnin <- array(as.numeric(unlist(score_recBNT)), dim=c(nrow(score_recBNT[[1]]), yrs+1, sims))
dimnames(unit.scores_recBNT.noburnin) <- list(NULL,colnames(score_recBNT[[1]]), NULL)

unit.scores.var_recBNT <- array(as.numeric(unlist(score_var_recBNT)), dim=c(nrow(score_var_recBNT[[1]]), yrs+1, sims))
dimnames(unit.scores.var_recBNT) <- list(NULL,colnames(score_var_recBNT[[1]]), NULL)

unit.scores.sd_recBNT <- array(as.numeric(unlist(score_sd_recBNT)), dim=c(nrow(score_sd_recBNT[[1]]), yrs+1, sims))
dimnames(unit.scores.sd_recBNT) <- list(NULL,colnames(score_sd_recBNT[[1]]), NULL)


#### Calculating pop growth rate (r) and carrying capacity (K) ###
unit.scores_recBKT.noburnin <- unit.scores_recBKT.noburnin[,-1,]
unit.scores_recBNT.noburnin <- unit.scores_recBNT.noburnin[,-1,]

# Define initial r
r.bkt.noburnin <- array(NA, dim=dim(unit.scores_recBKT.noburnin))
r.bnt.noburnin <- array(NA, dim=dim(unit.scores_recBNT.noburnin))

for(k in 1:sims){
    for(i in 1:nrow(unit.scores_recBKT.noburnin)) {
      r.bkt.noburnin[i,,k] <- rtnorm(1, mean=1.6, sd=0.2, lower=0)
      r.bnt.noburnin[i,,k] <- rtnorm(1, mean=1.6, sd=0.2, lower=0)

    }
}

# Calculate initial carrying capacity (K)
  unitK <- area.data %>% 
      group_by(unit) %>% 
      summarize(unit.area = sum(area.acre))
    
# 90th quantile of age 0 bkt and bnt density from 2010-present
q90.bkt <- 619.2628
q90.bnt <- 717.6219
    
unitK$K.bkt <- unitK$unit.area*q90.bkt
unitK$K.bnt <- unitK$unit.area*q90.bnt
    
unitK <- unitK %>% #just units above Mio
  filter(unitK$unit < 20)

# Calculate annual change in recruitment habitat score
changeBKT <- array(0, dim=c(nrow(score_recBNT[[1]]), yrs, sims))
      
changeBNT <- array(0, dim=c(nrow(score_recBNT[[1]]), yrs, sims))
      
for(k in 1:sims){
   for(j in 1:ncol(unit.scores_recBKT.noburnin)){
      for(i in 1:nrow(unit.scores_recBKT.noburnin)){
         if(j>1){
          #how much change in score is there from previous year to next year
          changeBKT[i,j,k] <- (unit.scores_recBKT.noburnin[i,j,k]  - unit.scores_recBKT.noburnin[i,j-1,k])/unit.scores_recBKT.noburnin[i,j-1,k]
          changeBNT[i,j,k] <- (unit.scores_recBNT.noburnin[i,j,k] - unit.scores_recBNT.noburnin[i,j-1,k])/unit.scores_recBNT.noburnin[i,j-1,k]
          }
        }
      }
}


#object of constant K
kBKT.noburnin <- array(unitK$K.bkt, dim=c(nrow(score_recBKT[[1]]), yrs, sims))
kBNT.noburnin <- array(unitK$K.bnt, dim=c(nrow(score_recBNT[[1]]), yrs, sims))


#change K and r based on rate of change of habitat scores
for(k in 1:sims){
  for(j in 2:ncol(changeBKT)){ #start in year 2
    for(i in 1:nrow(unit.scores_recBKT.noburnin)){
      if(j>1){
        # Apply proportional change using previous year's value
        
        # K
        kBKT.noburnin[i, j, k] <- kBKT.noburnin[i, j - 1, k] * (1 + changeBKT[i, j, k])
        kBNT.noburnin[i, j, k] <- kBNT.noburnin[i, j - 1, k] * (1 + changeBNT[i, j, k])
        
        # r
        r.bkt.noburnin[i, j, k] <- r.bkt.noburnin[i, j - 1, k] * (1 + changeBKT[i, j, k])
        r.bnt.noburnin[i, j, k] <- r.bnt.noburnin[i, j - 1, k] * (1 + changeBNT[i, j, k])
      }
    }
  }
}

#### Adding burn in to BKT, BNT recruitment, r, and K data ####
# Dimensions of the original array
n_units <- dim(unit.scores_recBKT.noburnin)[1]  # 19 units
n_years <- dim(unit.scores_recBKT.noburnin)[2]  # 1 label + yrs
n_sims  <- dim(unit.scores_recBKT.noburnin)[3]  # sims

# Initialize new arrays with 5 more columns
unit.scores_recBKT <- array(NA, dim = c(n_units, n_years + nburn, n_sims))
unit.scores_recBNT <- array(NA, dim = c(n_units, n_years + nburn, n_sims))

r.bkt <- array(NA, dim = c(n_units, n_years + nburn, n_sims))
r.bnt <- array(NA, dim = c(n_units, n_years + nburn, n_sims))

kBKT <- array(NA, dim = c(n_units, n_years + nburn, n_sims))
kBNT <- array(NA, dim = c(n_units, n_years + nburn, n_sims))

# Copy column 2 (first year of habitat scores) into the next 5 columns (2:6)
for (i in 1:nburn) {
  unit.scores_recBKT[,i, ] <- unit.scores_recBKT.noburnin[,1, ]
  unit.scores_recBNT[,i, ] <- unit.scores_recBNT.noburnin[,1, ]
  
  r.bkt[,i, ] <- r.bkt.noburnin[,1, ]
  r.bnt[,i, ] <- r.bnt.noburnin[,1, ]
  
  kBKT[,i, ] <- kBKT.noburnin[,1, ]
  kBNT[,i, ] <- kBNT.noburnin[,1, ]
}

# Copy the remaining habitat score columns (columns 2 to 56 in original) to columns 7 to 61
unit.scores_recBKT[, 6:(n_years + nburn), ] <- unit.scores_recBKT.noburnin[, 1:n_years, ]
unit.scores_recBNT[, 6:(n_years + nburn), ] <- unit.scores_recBNT.noburnin[, 1:n_years, ]

r.bkt[, 6:(n_years + nburn), ] <- r.bkt.noburnin[, 1:n_years, ]
r.bnt[, 6:(n_years + nburn), ] <- r.bnt.noburnin[, 1:n_years, ]

kBKT[, 6:(n_years + nburn), ] <- kBKT.noburnin[, 1:n_years, ]
kBNT[, 6:(n_years + nburn), ] <- kBNT.noburnin[, 1:n_years, ]

