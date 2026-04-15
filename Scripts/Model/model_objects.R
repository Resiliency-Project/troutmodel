## BROOK ands BROWN TROUT IBM ##
## DEVELOPED BY SHANE FLINN ##
## ADAPTED BY KAILI GREGORY

# Empty model objects
#create empty matrices and arrays for model to populate

#### both species ####
spawners.hist <- matrix(data = NA, nrow = yrs , ncol = sims)
recruits.hist <- matrix(data = NA, nrow = yrs , ncol = sims)
abundance.hist <- matrix(data = NA, nrow = yrs , ncol = sims)

spawners.units <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))
recruits.units <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))
abundance.units <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))

    # storing initial population size (after burn in)
    i.spawners.hist <- matrix(data = NA, nrow = sims, ncol = 1)
    i.recruits.hist <- matrix(data = NA, nrow = sims, ncol = 1)
    i.abundance.hist <- matrix(data = NA, nrow = sims, ncol = 1)
    
    i.spawners.units <- matrix(data=NA, nrow=sims, ncol=n.units)
    i.recruits.units <-  matrix(data=NA, nrow=sims, ncol=n.units)
    i.abundance.units <-  matrix(data=NA, nrow=sims, ncol=n.units)

### bkt ####
spawners.hist.bkt <- matrix(data = NA, nrow = yrs , ncol = sims)
recruits.hist.bkt <- matrix(data = NA, nrow = yrs , ncol = sims)
abundance.hist.bkt <- matrix(data = NA, nrow = yrs , ncol = sims)

spawners.units.bkt <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))
recruits.units.bkt <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))
abundance.units.bkt <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))

    # storing initial population size (after burn in)
    i.spawners.hist.bkt <- matrix(data = NA, nrow = sims, ncol = 1)
    i.recruits.hist.bkt <- matrix(data = NA, nrow = sims, ncol = 1)
    i.abundance.hist.bkt <- matrix(data = NA, nrow = sims, ncol = 1)
    
    i.spawners.units.bkt <- matrix(data=NA, nrow=sims, ncol=n.units)
    i.recruits.units.bkt <-  matrix(data=NA, nrow=sims, ncol=n.units)
    i.abundance.units.bkt <-  matrix(data=NA, nrow=sims, ncol=n.units)

#### bnt ####
spawners.hist.bnt <- matrix(data = NA, nrow = yrs , ncol = sims)
recruits.hist.bnt <- matrix(data = NA, nrow = yrs , ncol = sims)
abundance.hist.bnt <- matrix(data = NA, nrow = yrs , ncol = sims)

spawners.units.bnt <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))
recruits.units.bnt <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))
abundance.units.bnt <- array(data=NA, dim=c(nrow=yrs , ncol=sims, nmat=n.units))

    # storing initial population size (after burn in)
    i.spawners.hist.bnt <- matrix(data = NA, nrow = sims, ncol = 1)
    i.recruits.hist.bnt <- matrix(data = NA, nrow = sims, ncol = 1)
    i.abundance.hist.bnt <- matrix(data = NA, nrow = sims, ncol = 1)
    
    i.spawners.units.bnt <- matrix(data=NA, nrow=sims, ncol=n.units)
    i.recruits.units.bnt <-  matrix(data=NA, nrow=sims, ncol=n.units)
    i.abundance.units.bnt <-  matrix(data=NA, nrow=sims, ncol=n.units)

