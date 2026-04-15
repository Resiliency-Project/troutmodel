##Script for calculating and exporting habitat suitability metrics by SEU
# will need to have run input scripts of model for this to work

#### Habitat quality ####
#input data required: habitat data by reach
# reach.qualityBKT, tempscores, substrate_3D, lfyscores, OR woodscores_resc
inputdata <- reach.qualityBKT

#add in COMID labels
reach.habscoresBKT <- array(NA, dim=c(length(NCscores), yrs+1, sims))
reach.habscoresBNT <- array(NA, dim=c(length(NCscores), yrs+1, sims))    

for (k in 1:sims) {
  reach.habscoresBKT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
  reach.habscoresBNT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
}


##Summarizing by unit
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

#convert matrices list into array and rename columns
# habitat suitability
unit.habqual.BKT <- array(as.numeric(unlist(scoreBKT)), dim=c(nrow(scoreBKT[[1]]), yrs+1, sims))
dimnames(unit.habqual.BKT) <- list(NULL,colnames(scoreBKT[[1]]), NULL)


#### Temperature ####
#input data required: habitat data by reach
# reach.qualityBKT, tempscores, substrate_3D, lfyscores, OR woodscores_resc
inputdata <- tempscores

#add in COMID labels
reach.habscoresBKT <- array(NA, dim=c(length(NCscores), yrs+1, sims))
reach.habscoresBNT <- array(NA, dim=c(length(NCscores), yrs+1, sims))    

for (k in 1:sims) {
  reach.habscoresBKT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
  reach.habscoresBNT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
}

##Summarizing by unit ##
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

  unit.habtemps.BKT <- array(as.numeric(unlist(scoreBKT)), dim=c(nrow(scoreBKT[[1]]), yrs+1, sims))
  dimnames(unit.habqual.BKT) <- list(NULL,colnames(scoreBKT[[1]]), NULL)

#### Substrate ####
  #input data required: habitat data by reach
  inputdata <- substrate_3D
  
  #add in COMID labels
  reach.habscoresBKT <- array(NA, dim=c(length(NCscores), yrs+1, sims))
  reach.habscoresBNT <- array(NA, dim=c(length(NCscores), yrs+1, sims))    
  
  for (k in 1:sims) {
    reach.habscoresBKT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
    reach.habscoresBNT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
  }
  
  ##Summarizing by unit ##
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
  
  unit.habsub.BKT <- array(as.numeric(unlist(scoreBKT)), dim=c(nrow(scoreBKT[[1]]), yrs+1, sims))
  dimnames(unit.habsub.BKT) <- list(NULL,colnames(scoreBKT[[1]]), NULL)
  
#### LFY ####
  #input data required: habitat data by reach
  inputdata <- lfyscores
  
  #add in COMID labels
  reach.habscoresBKT <- array(NA, dim=c(length(NCscores), yrs+1, sims))
  reach.habscoresBNT <- array(NA, dim=c(length(NCscores), yrs+1, sims))    
  
  for (k in 1:sims) {
    reach.habscoresBKT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
    reach.habscoresBNT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
  }
  
  ##Summarizing by unit ##
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

  unit.hablfy.BKT <-  array(as.numeric(unlist(scoreBKT)), dim=c(nrow(scoreBKT[[1]]), yrs+1, sims))
  dimnames(unit.hablfy.BKT) <- list(NULL,colnames(scoreBKT[[1]]), NULL)


#### Wood ####
  #input data required: habitat data by reach
  inputdata <- woodscores_resc
  
  #add in COMID labels
  reach.habscoresBKT <- array(NA, dim=c(length(NCscores), yrs+1, sims))
  reach.habscoresBNT <- array(NA, dim=c(length(NCscores), yrs+1, sims))    
  
  for (k in 1:sims) {
    reach.habscoresBKT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
    reach.habscoresBNT[,,k] <- cbind(habdata$COMID, inputdata[,,k])
  }
  
  ##Summarizing by unit ##
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
  
  unit.habwood.BKT <-  array(as.numeric(unlist(scoreBKT)), dim=c(nrow(scoreBKT[[1]]), yrs+1, sims))
  dimnames(unit.habwood.BKT) <- list(NULL,colnames(scoreBKT[[1]]), NULL)
  


#### Collating and exporting summary stats #####
# habitat suitability 
  # *unit.scores already saved in model run
  # column 7 is year 1 after burn in
mean.hsi <- round(apply(unit.scoresBKT[,7,], c(1), mean), 2)
sd.hsi <- round(apply(unit.scoresBKT[,7,], c(1), sd), 2)
hsi <- paste(as.character(mean.hsi), as.character(sd.hsi), sep=", ")

# habitat quantity
  # unit.habqual is calculated above
mean.hqual <- round(apply(unit.habqual.BKT[,2,], c(1), mean), 2)
sd.hqual <- round(apply(unit.habqual.BKT[,2,], c(1), sd), 2)
hqual <- paste(as.character(mean.hqual), as.character(sd.hqual), sep=", ")

# temperature  
  # unit.habtemps is calculated above
mean.htemp <- round(apply(unit.habtemps.BKT[,2,], c(1), mean), 2)
sd.htemp <- round(apply(unit.habtemps.BKT[,2,], c(1), sd), 2)
htemp <- paste(as.character(mean.htemp), as.character(sd.htemp), sep=", ")

# substrate
# unit.habtemps is calculated above
mean.hsub <- round(apply(unit.habsub.BKT[,2,], c(1), mean), 2)
sd.htemp <- round(apply(unit.habsub.BKT[,2,], c(1), sd), 2)
hsub <- paste(as.character(mean.hsub), as.character(sd.htemp), sep=", ")

# low flow yield
mean.hlfy <- round(apply(unit.hablfy.BKT[,2,], c(1), mean), 2)
sd.hlfy <- round(apply(unit.hablfy.BKT[,2,], c(1), sd), 2)
hlfy <- paste(as.character(mean.hlfy), as.character(sd.hlfy), sep=", ")

# wood
mean.hwood <- round(apply(unit.habwood.BKT[,2,], c(1), mean), 2)
sd.hwood <- round(apply(unit.habwood.BKT[,2,], c(1), sd), 2)
hwood <- paste(as.character(mean.hwood), as.character(sd.hwood), sep=", ")


data <- data.frame(unit=c(1:19),
                   Habitat_suitability = hsi,
                   Habitat_quality = hqual,
                   Temperature = htemp,
                   Substrate_composition = hsub,
                   Low_flow_yield = hlfy,
                   Wood = hwood)

write.csv(data, "Results/Habitat_suitability/habitatscore.csv")


#### Habitat scores for recruitment ####
#### BKT ##
inputdata.bkt <- reach.quality_recBKT
inputdata.bnt <- reach.quality_recBNT


#add in COMID labels
reach.habscoresBKT <- array(NA, dim=c(length(NCscores), yrs+1, sims))
reach.habscoresBNT <- array(NA, dim=c(length(NCscores), yrs+1, sims))    

for (k in 1:sims) {
  reach.habscoresBKT[,,k] <- cbind(habdata$COMID, inputdata.bkt[,,k])
  reach.habscoresBNT[,,k] <- cbind(habdata$COMID, inputdata.bnt[,,k])
}


##Summarizing by unit
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

#convert matrices list into array and rename columns
# habitat suitability
unit.habqual_recBKT <- array(as.numeric(unlist(scoreBKT)), 
                             dim=c(nrow(scoreBKT[[1]]), yrs+1, sims))
dimnames(unit.habqual_recBKT) <- list(NULL,colnames(scoreBKT[[1]]), NULL)

unit.habqual_recBNT <- array(as.numeric(unlist(scoreBNT)), 
                             dim=c(nrow(scoreBNT[[1]]), yrs+1, sims))
dimnames(unit.habqual_recBNT) <- list(NULL,colnames(scoreBNT[[1]]), NULL)


#### Collating and exporting summary stats #####
# habitat suitability 
# *unit.scores already saved in model run
# column 7 is year 1 after burn in
mean.hsi_recBKT <- round(apply(unit.scores_recBKT[,7,], c(1), mean), 2)
sd.hsi_recBKT <- round(apply(unit.scores_recBKT[,7,], c(1), sd), 2)
hsi_recBKT <- paste(as.character(mean.hsi_recBKT), as.character(sd.hsi_recBKT), sep=", ")

mean.hsi_recBNT <- round(apply(unit.scores_recBNT[,7,], c(1), mean), 2)
sd.hsi_recBNT <- round(apply(unit.scores_recBNT[,7,], c(1), sd), 2)
hsi_recBNT <- paste(as.character(mean.hsi_recBNT), as.character(sd.hsi_recBNT), sep=", ")


# habitat quantity
# unit.habqual is calculated above
mean.hqual_recBKT <- round(apply(unit.habqual_recBKT[,2,], c(1), mean), 2)
sd.hqual_recBKT <- round(apply(unit.habqual_recBKT[,2,], c(1), sd), 2)
hqual_recBKT <- paste(as.character(mean.hqual_recBKT), as.character(sd.hqual_recBKT), sep=", ")

mean.hqual_recBNT <- round(apply(unit.habqual_recBNT[,2,], c(1), mean), 2)
sd.hqual_recBNT <- round(apply(unit.habqual_recBNT[,2,], c(1), sd), 2)
hqual_recBNT <- paste(as.character(mean.hqual_recBNT), as.character(sd.hqual_recBNT), sep=", ")


data_rec <- data.frame(unit=c(1:19),
                   Habitat_suitability_recBKT = hsi_recBKT,
                   Habitat_quality_recBKT = hqual_recBKT,
                   Habitat_suitability_recBNT = hsi_recBNT,
                   Habitat_quality_recBNT = hqual_recBNT)

write.csv(data_rec, "Results/Habitat_suitability/habitatscore_recruitment.csv")


#### Heatmaps of average habitat scores ####
library(sf)
library(ggpubr)
reach.units <- read_sf("Data/Spatial/participatory_mapping/reaches_with_units/reach_units.shp")

map.lims <- c(0,5)

# by SEU
seu.data <- data.frame(unit=c(1:19),
                       Habitat_suitability = mean.hsi,
                       Habitat_quality = mean.hqual,
                       Temperature = mean.htemp,
                       Substrate_composition = mean.hsub,
                       Low_flow_yield = mean.hlfy,
                       Wood = mean.hwood)
seu.mapdata <- merge(reach.units, seu.data, by="unit", all.y=T)
  
hsi.byseu <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.mapdata, aes(color=Habitat_suitability), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "viridis", direction= -1) +
  labs(title= "Habitat suitability by SEU",
       color="Habitat\nsuitability") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
         axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
         axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
         axis.ticks = element_blank(),  # Removes axis tick marks
         axis.line = element_blank()    # Removes the axis line itself
  )

hqual.byseu <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.mapdata, aes(color=Habitat_quality), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "viridis", direction= -1) +
  labs(title= "Habitat quality by SEU",
       color="Habitat\nquality") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

temp.byseu <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.mapdata, aes(color=Temperature), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "plasma", direction= -1) +
  labs(title= "a) Temperature",
       color="Score\n[0,5]") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

sub.byseu <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.mapdata, aes(color=Substrate_composition), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "plasma", direction= -1) +
  labs(title= "b) Substrate composition") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

lfy.byseu <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.mapdata, aes(color=Low_flow_yield), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "plasma", direction= -1) +
  labs(title= "c) Low flow yield") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

wood.byseu <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.mapdata, aes(color=Wood), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "plasma", direction= -1) +
  labs(title= "d) Wood") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

x <- ggarrange(temp.byseu, sub.byseu, lfy.byseu, wood.byseu,
          common.legend=T, legend="right")
scores.byseu <- annotate_figure(x,top = text_grob("Scores by SEU", size = 16))

# Recruitment by species scores

# by SEU
seu.rec.data <- data.frame(unit=c(1:19),
                       Habitat_suitability_recBKT = mean.hsi_recBKT,
                       Habitat_quality_recBKT = mean.hqual_recBNT,
                       Habitat_suitability_recBNT = mean.hsi_recBNT,
                       Habitat_quality_recBNT = mean.hqual_recBNT)

seu.rec.mapdata <- merge(reach.units, seu.rec.data, by="unit", all.y=T)

hsi.byseu.recBKT <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.rec.mapdata, aes(color=Habitat_suitability_recBKT), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "viridis", direction= -1) +
  labs(title= "Brook trout recruitment",
       color="Habitat\nsuitability") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

hsi.byseu.recBNT <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.rec.mapdata, aes(color=Habitat_suitability_recBNT), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "viridis", direction= -1) +
  labs(title= "Brown trout recruitment",
       color="Habitat\nsuitability") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

hqual.byseu.recBKT <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.rec.mapdata, aes(color=Habitat_quality_recBKT), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "viridis", direction= -1) +
  labs(title= "Brook trout recruitment",
       color="Habitat\nquality") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

hqual.byseu.recBNT <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="grey") +
  geom_sf(data=seu.rec.mapdata, aes(color=Habitat_quality_recBNT), lwd=0.75) +
  scale_color_viridis_c(limits=map.lims, option = "viridis", direction= -1) +
  labs(title= "Brown trout recruitment",
       color="Habitat\nquality") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
        axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
        axis.ticks = element_blank(),  # Removes axis tick marks
        axis.line = element_blank()    # Removes the axis line itself
  )

hsi.rec.byspp <- ggarrange(hsi.byseu.recBKT, hsi.byseu.recBNT, nrow=1, 
          common.legend = TRUE, legend = "bottom")

habquality.rec.byspp <- ggarrange(hqual.byseu.recBKT, hqual.byseu.recBNT, nrow=1, 
          common.legend = TRUE, legend = "bottom")

#### export figures 
ggsave(filename = "hsi.by.seu.jpeg", plot = hsi.byseu, 
       path = "Results/Habitat_suitability/", 
       width = 6, height = 4, dpi = 300)

ggsave(filename = "habquality.by.seu.jpeg", plot = hqual.byseu, 
       path = "Results/Habitat_suitability/", 
       width = 6, height = 4, dpi = 300)

ggsave(filename = "qualityscores.by.seu.jpeg", plot = scores.byseu, 
       path = "Results/Habitat_suitability/", 
       width = 6, height = 4, dpi = 300)

ggsave(filename = "hsi.rec.byspp.jpeg", plot = hsi.rec.byspp, 
       path = "Results/Habitat_suitability/", 
       width = 6, height = 4, dpi = 300)

ggsave(filename = "habquality.rec.byspp.jpeg", plot = habquality.rec.byspp, 
       path = "Results/Habitat_suitability/", 
       width = 6, height = 4, dpi = 300)








