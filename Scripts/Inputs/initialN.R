## BROOK and BROWN TROUT IBM ##
## DEVELOPED BY SHANE FLINN ##
## ADAPTED BY KAILI GREGORY

# Script for calculating initial population size

#load in available trout density data from the Au Sable
bktdens <- read.csv("Data/Fish/Density_data/density_2010presentBKT.csv")
bntdens <- read.csv("Data/Fish/Density_data/density_2010presentBNT.csv")

#### Custom likelihood function to weight sites equally ####  
# Custom negative binomial log likelihood function
weighted_nb_loglik <- function(params, data, weights) {
  mu <- params[1]
  size <- params[2]
  if (mu <= 0 || size <= 0) return(Inf)
  -sum(weights * dnbinom(data, mu = mu, size = size, log = TRUE))
}

# Function to estimate parameters for the distribution
bootstrap_weighted_nb <- function(data,n_boot, data_weight) 
  { 
  init_params <- c(mu = mean(data$n), size = 1)
  
  fit <- optim(par = init_params,
               fn = weighted_nb_loglik,
               data = round(data$n, 0),
               weights = data_weight,
               method = "L-BFGS-B",
               lower = c(1e-5, 1e-5))
  
  fitted_mu <- fit$par[1]
  fitted_size <- fit$par[2]
  
# Empty object for bootstrapped parameter estimates
  boot_params <- data.frame(mu = numeric(n_boot), size = numeric(n_boot))
  
  for (i in 1:n_boot) {
    #Simulate new densities from fitted NB distribution
    sim_densities <- rnbinom(n = nrow(data), mu = fitted_mu, size = fitted_size)

    #Create simulated data set and assign equal site weights
    sim_data <- data #original data
    sim_data$density <- sim_densities

    # Refit NB model to simulated data
    sim_fit <-  optim(par = c(mean(sim_data$density), 1),
                      fn = weighted_nb_loglik,
                      data = sim_data$density,
                      weights = data_weight,
                      method = "L-BFGS-B",
                      lower = c(1e-5, 1e-5))

    #S tore bootstrapped parameters
    boot_params$mu[i] <- sim_fit$par[1]
    boot_params$size[i] <- sim_fit$par[2]
  }

  return(boot_params)
}

# Generate 1000 bootsrapped parameters for both species
bkt.nb.params <- bootstrap_weighted_nb(data=bktdens, n_boot=1000, data_weight = bktdens$site_weights)
bnt.nb.params <- bootstrap_weighted_nb(data=bntdens, n_boot=1000, data_weight = bntdens$site_weights)

#### Calculate initial N by reach (COMID) and SEU ####
bktdensity <- matrix(NA, nrow=nrow(area.data), ncol = sims)
bntdensity <- matrix(NA, nrow=nrow(area.data), ncol = sims)

for(j in 1:ncol(bktdensity)){
  for(i in 1:nrow(bktdensity)) {
    
    # Randomly pick an index number from number of bootstrapped values
    index <- sample(1:sims, 1)
    
    # equal weight across sites (site)
      bkt.params <- bkt.nb.params[index,] #select those parameters
      bnt.params <- bnt.nb.params[index,] #select those parameters
      
    # draw a density value from NB distribution
      bktdensity[i,j] <- rnbinom(n=1, size=bkt.params[1,2], mu=bkt.params[1,1]) 
      bntdensity[i,j] <- rnbinom(n=1, size=bnt.params[1,2], mu=bnt.params[1,1])
  }
}

# Subset for SEU numbers and COMID area
area <- data.frame(area.data$unit, area.data$area.acre)

# initial N by COMID
initN.bkt.reach <- cbind(area, as.data.frame(bktdensity*area$area.data.area.acre))
initN.bnt.reach <- cbind(area, as.data.frame(bntdensity*area$area.data.area.acre))

#initial N by SEU
    initN.bkt <- initN.bkt.reach[,-2]
    initN.bkt <- initN.bkt %>% 
      group_by(area.data.unit) %>% 
      mutate(across(.cols = starts_with("V"), .fns=sum)) %>% 
      unique()
    
    initN.bnt <- initN.bnt.reach[,-2]
    initN.bnt <- initN.bnt %>% 
      group_by(area.data.unit) %>% 
      mutate(across(.cols = starts_with("V"), .fns=sum)) %>% 
      unique()
    
    initN.bkt$area.data.unit <- as.integer(initN.bkt$area.data.unit)
    initN.bkt <- initN.bkt %>% #subset for units above Mio
      filter(area.data.unit <= 19)
    initN.bkt <- initN.bkt[order(initN.bkt$area.data.unit),] #order by the units
    initN.bkt <- round(initN.bkt)
    
    initN.bnt$area.data.unit <- as.integer(initN.bnt$area.data.unit)
    initN.bnt <- initN.bnt %>% #subset for units above Mio
      filter(area.data.unit <= 19)
    initN.bnt <- initN.bnt[order(initN.bnt$area.data.unit),] #order by the units
    initN.bnt <- round(initN.bnt)
    

# Total N 
  totalN.bkt <- as.data.frame(colSums(initN.bkt[,-1])) #sum all columns except units
  totalN.bnt <- as.data.frame(colSums(initN.bnt[,-1])) #sum all column except units
   
  