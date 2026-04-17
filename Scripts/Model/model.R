## BROOK and BROWN TROUT IBM ##
## DEVELOPED BY SHANE FLINN ##
## ADAPTED BY KAILI GREGORY

# Model script

#read in libraries
library(tidyverse)
library(sf)
library(beepr)
library(ggplot2)
library(gamlss)
library(scales)
library(msm)
select <- dplyr::select

sims <- 10 # number of simulations (minimum of 10, maximum of 1000)
current.yr <- 2027
yrs <- 2080 - current.yr # number of years into the future to simulate (max to 2080)
nburn <- 5 # number of of years of burn in

set.seed(20016) #

start.time <- Sys.time() # Set timer
start.time #start time of model

#### Scenarios ####
source("Scripts/Inputs/temp_scenarios.R")

# for each of the scenario categories (Temperature, Wood, Wood hypothesis, flows)
  # only run the lines of code that correspond to your selected scenario
  # comment out the scenarios that you do NOT pick


# PICK ONE TEMPERATURE SCENARIO
  # Low change (2.5)
    # tempscores <- tempscores2.5
    # temps <- temps2.5

  # Moderate change (3.8)
    tempscores <- tempscores3.8
    temps <- temps3.8

  # High change (4.8)
    # tempscores <- tempscores4.8
    # temps <- temps4.8

    
# PICK ONE WOOD SCENARIO
  # Baseline assumed rate of decay
    # wood.change <- data.frame(lwr = -.015, upr = 0.005)
    
   # Wood added at rate of decay (current levels maintained)
    wood.change <- data.frame(lwr = 0, upr = 0)
    
  # Increase wood by 25% in year X, then add at rate of decay/maintain levels
    # wood.change <- data.frame(lwr = 0, upr = 0) # for maintaining wood levels
    # wood.add <- 0.25 # how much wood to add (% expressed as a decimal)
    # wood.add.0 <- 0.05 # how much wood to add with areas with 0 wood (% expressed as a decimal)
    # wood.add.yr <- 3 # what year to add wood (1- 2025, 2=2026, 3=2027 etc)

  # Increase wood by 50% in year X, then add at rate of decay/maintain levels
    # wood.change <- data.frame(lwr = 0, upr = 0) # for maintaining wood levels
    # wood.add <- 0.5 # how much wood to add (% expressed as a decimal)
    # wood.add.0 <- 0.05 # how much wood to add with areas with 0 wood (% expressed as a decimal)
    # wood.add.yr <- 3 # what year to add wood (1- 2025, 2=2026, 3=2027 etc)
    
    
# PICK WOOD HYPOTHESIS
  # Hypothesis1: More wood is good
    wood.hyp <- c(1.00, 1.83, 2.42, 3.00, 3.42, 3.50, 4.00, 4.00, 4.00, 4.00)
    
  # Hypothesis 2: Diminishing returns
    # wood.hyp <- c(1.00, 1.83, 2.58, 3.17, 3.58, 3.50, 3.33, 2.83, 2.33, 2.00)

    
# PICK A FLOW SCENARIO
  # No change in flows
    load(file = "Data/Habitat/lfy_density_projections.RData")
    
  # Maximum allowable reduction in flow
    #load(file = "Troutmodel/Data/Habitat/lfy_reduced_density_projections.RData")
    
### Collate inputs ####
source("Scripts/Inputs/habitatscoring.R") # Habitat suitability scores

source("Scripts/Inputs/initialN.R") # Initial population size

source("Scripts/Inputs/recruitment_K.R") # Recruitment and carrying capacity parameters

# Reduce number of individuals to lessen model run time (1 indiv = 50 indivs)
  # Carrying capacity
    kBKT <- round(kBKT/50)
    kBNT <- round(kBNT/50)

  # Initial abundance
    initN.bkt <- round(cbind(initN.bkt[,1], initN.bkt[,2:ncol(initN.bkt)]/50))
    initN.bnt <- round(cbind(initN.bnt[,1], initN.bnt[,2:ncol(initN.bnt)]/50))

# NO change scenario
    #source("Scripts/Inputs/nochange_scenario.R")

### Functions and empty objects ####
source("Scripts/Model/functions_bothspp.R")  # load functions
n.units <- 19                                           # number of SEUs
source("Scripts/Model/model_objects.R")      # empty objects for model


#### MODEL ####
yrs <- yrs + nburn #total number of years to run the model for

start.time <- Sys.time() # Set timer
start.time #when the model started

for(j in 1:sims){
  for(i in 1:yrs){
    if (i == 1){ #year 1
    # set up starting population
      inds <- inits.pop(data = data) # this is the total population in year 1
      #inds <- growth(data = inds) #add in lengths (inits.pop doesn't lengths), commented out to speed up run time

    # start year 1 life history
      inds <- mortality(data = inds) #pre spawn mortality
      move <- spawner_movement(data = inds) #how many spawners need to move to where
      inds <- update_locations(data=inds, movedata=move) #move spawners to new units (juveniles don't move)
      #no recruitment function in first year b/c it is already estimated in initial pop size
      inds <- mortality(data = inds) # post-spawn mortality of mature fish
      inds <- age(data=inds) # age +1
      #inds <- growth(data = inds) # growth, commented out to speed up run time
      inds <- mature(data = inds) # determine maturity of fish
    }
  else{
    if (i > 1 & i <= nburn){ #burn in period
    # regular life history, without storage of outputs
      inds <- mortality(data = inds) #pre spawn mortality
      move <- spawner_movement(data = inds) #how many spawners need to move to where
      inds <- update_locations(data=inds, movedata=move) #move spawners to new units (juveniles don't move)
      rec <- recruitment(data = move) # calculate recruitment for each unit
      inds <- mortality(data = inds) # post-spawn mortality of mature fish
      inds <- recruit.locs(data = inds, recdata=rec, movedata=move) # place new recruits in units in the population
      inds <- age(data=inds) # age +1
      #inds <- growth(data = inds) # growth
      inds <- mature(data = inds) # determine maturity of fish
      
      #recording abundances after burn-in period
        if(i == nburn) { 
          #recruit abundances
              #both species
              i.recruits.hist[j] <- sum(rec[,2]) + sum(rec[,3]) #total no. recruits each year, sim
              i.recruits.units[j,] <- rec[,2] + rec[,3]
              
              #bkt
              i.recruits.hist.bkt[j] <- sum(rec[,2]) #total no. recruits each year, sim
              i.recruits.units.bkt[j,] <- rec[,2]
              
              #bnt
              i.recruits.hist.bnt[j] <- sum(rec[,3]) #total no. recruits each year, sim
              i.recruits.units.bnt[j,] <- rec[,3]
          
          #track spawner abundances
              #both species
              i.spawners.hist[j] <- sum(inds[,3][inds[,3] == 1]) #total no. spawners each year, sim
              for(k in 1:n.units){
                nspawns <- as.data.frame(inds) %>% group_by(unit) %>% summarize(sum(mature)) #mature=1, so you can just take sum
                nspawns <- as.data.frame(nspawns)
                i.spawners.units[j,k] <- nspawns[k,2]
              } #no. spawners each year, sim, in each unit
              
              #bkt
              i.spawners.hist.bkt[j] <- sum(inds[,3][ inds[,3] == 1 & inds[,8]=="bkt"]) #total no. spawners each year, sim
              for(k in 1:n.units){
                nspawns <- as.data.frame(inds) %>% filter(species=="bkt") %>%
                  group_by(unit) %>% summarize(sum(mature)) #mature=1, so you can just take sum
                nspawns <- as.data.frame(nspawns)
                i.spawners.units.bkt[j,k] <- nspawns[k,2]
              } #no. BKT spawners each year, sim, in each unit
              
              #bnt
              i.spawners.hist.bnt[j] <- sum(inds[,3][ inds[,3] == 1 & inds[,8]=="bnt"]) #total no. spawners each year, sim
              for(k in 1:n.units){
                nspawns <- as.data.frame(inds) %>% filter(species=="bnt") %>%
                  group_by(unit) %>% summarize(sum(mature)) #mature=1, so you can just take sum
                nspawns <- as.data.frame(nspawns)
                i.spawners.units.bnt[j,k] <- nspawns[k,2]
              } #no. bnt spawners each year, sim, in each unit
          
          #track total abundances
              #both species
              i.abundance.hist[j] <- nrow(inds) # total population
              nabun <- as.data.frame(inds) %>% group_by(unit) %>% summarize(n())
              nabun <- as.data.frame(nabun)
              i.abundance.units[j,] <- nabun[,2]
              
              #bkt
              i.abundance.hist.bkt[j] <- sum(inds$species == "bkt")# total population
              nabun <- as.data.frame(inds) %>%  filter(species=="bkt")  %>%
                group_by(unit) %>% summarize(n())
              nabun <- as.data.frame(nabun)
              i.abundance.units.bkt[j,] <- nabun[,2]
              
              #bnt
              i.abundance.hist.bnt[j] <- sum(inds$species == "bnt")# total population
              nabun <- as.data.frame(inds) %>%  filter(species=="bnt")  %>%
                group_by(unit) %>% summarize(n())
              nabun <- as.data.frame(nabun)
              i.abundance.units.bnt[j,] <- nabun[,2]
        }
      }
   else { #all other years
    inds <- mortality(data = inds) #pre spawn mortality
    move <- spawner_movement(data = inds) #how many spawners need to move to where
    inds <- update_locations(data=inds, movedata=move) #move spawners to new units (juveniles don't move)
    rec <- recruitment(data = move) # calculate recruitment for each unit
    inds <- mortality(data = inds) # post-spawn mortality of mature fish
    inds <- recruit.locs(data = inds, recdata=rec, movedata=move) # place new recruits in units in the population
    inds <- age(data=inds) # age +1
    #inds <- growth(data = inds) # growth, commented out to speed up run time
    inds <- mature(data = inds) # determine maturity of fish
    
    ##### store abundances ####
     # recruit abundances
           # both species
           recruits.hist[i-nburn,j] <- sum(rec[,2]) + sum(rec[,3]) #total no. recruits each year, sim
           recruits.units[i-nburn,j,] <- rec[,2] + rec[,3]
           
           # bkt
           recruits.hist.bkt[i-nburn,j] <- sum(rec[,2]) #total no. recruits each year, sim
           recruits.units.bkt[i-nburn,j,] <- rec[,2]
           
           # bnt
           recruits.hist.bnt[i-nburn,j] <- sum(rec[,3]) #total no. recruits each year, sim
           recruits.units.bnt[i-nburn,j,] <- rec[,3]   
   
      # spawner abundances
           # both species
           spawners.hist[i-nburn,j] <- sum(inds[,3][inds[,3] == 1]) #total no. spawners each year, sim
           for(k in 1:n.units){
             nspawns <- as.data.frame(inds) %>% group_by(unit) %>% summarize(sum(mature)) #mature=1, so you can just take sum
             nspawns <- as.data.frame(nspawns)
             spawners.units[i-nburn,j,k] <- nspawns[k,2]
           } #no. spawners each year, sim, in each unit
           
           # bkt
           spawners.hist.bkt[i-nburn,j] <- sum(inds[,3][ inds[,3] == 1 & inds[,8]=="bkt"]) #total no. spawners each year, sim
           for(k in 1:n.units){
             nspawns <- as.data.frame(inds) %>% filter(species=="bkt") %>%
               group_by(unit) %>% summarize(sum(mature)) #mature=1, so you can just take sum
             nspawns <- as.data.frame(nspawns)
             spawners.units.bkt[i-nburn,j,k] <- nspawns[k,2]
           } #no. BKT spawners each year, sim, in each unit
           
           #bnt
           spawners.hist.bnt[i-nburn,j] <- sum(inds[,3][ inds[,3] == 1 & inds[,8]=="bnt"]) #total no. spawners each year, sim
           for(k in 1:n.units){
             nspawns <- as.data.frame(inds) %>% filter(species=="bnt") %>%
               group_by(unit) %>% summarize(sum(mature)) #mature=1, so you can just take sum
             nspawns <- as.data.frame(nspawns)
             spawners.units.bnt[i-nburn,j,k] <- nspawns[k,2]
           } #no. bnt spawners each year, sim, in each unit
   
        #total abundances
            # both species
           abundance.hist[i - nburn, j] <- nrow(inds) # Assign total abundance
            nabun <- as.data.frame(inds) %>%
              count(unit) %>%
              complete(unit = 1:19, fill = list(n = 0)) %>%
              arrange(unit)  # Make sure the order is correct
            abundance.units[i - nburn, j, ] <- nabun$n # Assign abundance per unit (should now be length 19)
  
            # bkt
            abundance.hist.bkt[i - nburn, j] <- nrow(inds[inds$species == "bkt", ]) # Assign total abundance
            nabun.bkt <- as.data.frame(inds) %>%
              filter(species == "bkt") %>%
              count(unit) %>%
              complete(unit = 1:19, fill = list(n = 0)) %>%
              arrange(unit)  # Make sure the order is correct
            abundance.units.bkt[i - nburn, j, ] <- nabun.bkt$n # Assign abundance per unit (should now be length 19)
  
            # bnt
            abundance.hist.bnt[i - nburn, j] <- nrow(inds[inds$species == "bnt", ]) # Assign total abundance
            nabun.bnt <- as.data.frame(inds) %>%
              filter(species == "bnt") %>%
              count(unit) %>%
              complete(unit = 1:19, fill = list(n = 0)) %>%
              arrange(unit)  # Make sure the order is correct
            abundance.units.bnt[i - nburn, j, ] <- nabun.bnt$n # Assign abundance per unit (should now be length 19)
      
   }
  }
  }
  # Report the simulation the model is on 
  if(j %in% seq(1,sims, by=1)) { 
    cat('Just finished simulation ',j, '/', sims,'\n\n', sep='')}
}
 
end.time <- Sys.time() # end timer
elapsed.time <- round(difftime(end.time, start.time, units='mins'), digits = 2)
cat('The model ran in ', elapsed.time, ' minutes\n\n', sep='') # display text message of model run time
beep(5) #sound when model is done

### Save results ####
# Define file path for saving model results
  # file = "Results/____.Rdata".
  # Complete the blank with your chosen file name, ensure it ends with R.data
  # This file path will be used for summary and visualization of results
save(abundance.hist, recruits.hist, spawners.hist,
     abundance.hist.bkt, recruits.hist.bkt, spawners.hist.bkt,
     abundance.hist.bnt, recruits.hist.bnt, spawners.hist.bnt,
     abundance.units, recruits.units, spawners.units,
     abundance.units.bkt, recruits.units.bkt, spawners.units.bkt,
     abundance.units.bnt, recruits.units.bnt, spawners.units.bnt,
     file = "Results/S1.Rdata")


#initial abundances (this only needs to be ran once)
# save(i.abundance.hist, i.recruits.hist, i.spawners.hist,
#      i.abundance.hist.bkt, i.recruits.hist.bkt, i.spawners.hist.bkt,
#      i.abundance.hist.bnt, i.recruits.hist.bnt, i.spawners.hist.bnt,
#      i.abundance.units, i.recruits.units, i.spawners.units,
#      i.abundance.units.bkt, i.recruits.units.bkt, i.spawners.units.bkt,
#      i.abundance.units.bnt, i.recruits.units.bnt, i.spawners.units.bnt,
#      file = "Results/S0_initialvalues.Rdata")


