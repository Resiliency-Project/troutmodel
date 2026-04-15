#model presentation visualizations

library(ggplot2)
library(sf)
library(ggpubr)

# re-enter number of years and simulations if the environment has been cleared
  # between running the model and this script
yrs <- 55
sims <- 10

# Load in the results of interest using the file path saved at the
  # end of model.R
load("Results/S1.Rdata")

#### time vs % change ####
delta.N <- matrix(NA, nrow=yrs, ncol=sims) 
delta.N.bkt <- matrix(NA, nrow=yrs, ncol=sims) 
delta.N.bnt <- matrix(NA, nrow=yrs, ncol=sims) 

delta.N.rec <- matrix(NA, nrow=yrs, ncol=sims) 
delta.N.bkt.rec <- matrix(NA, nrow=yrs, ncol=sims) 
delta.N.bnt.rec <- matrix(NA, nrow=yrs, ncol=sims) 

delta.N.spawners <- matrix(NA, nrow=yrs, ncol=sims) 
delta.N.bkt.spawners <- matrix(NA, nrow=yrs, ncol=sims) 
delta.N.bnt.spawners <- matrix(NA, nrow=yrs, ncol=sims) 

for(i in 1:(yrs)){
  for(j in 1:sims){
    # total abundance
    delta.N[i,j] <- ((abundance.hist[i,j] - abundance.hist[1,j])/abundance.hist[1,j]) *100
    delta.N.bkt[i,j] <- ((abundance.hist.bkt[i,j] - abundance.hist.bkt[1,j])/abundance.hist.bkt[1,j]) *100
    delta.N.bnt[i,j] <- ((abundance.hist.bnt[i,j] - abundance.hist.bnt[1,j])/abundance.hist.bnt[1,j]) *100
    
    # recruits
    delta.N.rec[i,j] <- ((recruits.hist[i,j] - recruits.hist[1,j])/recruits.hist[1,j]) *100
    delta.N.bkt.rec[i,j] <- ((recruits.hist.bkt[i,j] - recruits.hist.bkt[1,j])/recruits.hist.bkt[1,j]) *100
    delta.N.bnt.rec[i,j] <- ((recruits.hist.bnt[i,j] - recruits.hist.bnt[1,j])/recruits.hist.bnt[1,j]) *100
    
    # spawners
    delta.N.spawners[i,j] <- ((spawners.hist[i,j] - spawners.hist[1,j])/spawners.hist[1,j]) *100
    delta.N.bkt.spawners[i,j] <- ((spawners.hist.bkt[i,j] - spawners.hist.bkt[1,j])/spawners.hist.bkt[1,j]) *100
    delta.N.bnt.spawners[i,j] <- ((spawners.hist.bnt[i,j] - spawners.hist.bnt[1,j])/spawners.hist.bnt[1,j]) *100

    }
}

#abundance
    delta.N.mean <- apply(delta.N, 1, mean)
    delta.N.sd <- apply(delta.N, 1, sd)
    delta.N.se <- delta.N.sd/sqrt(sims)
    delta.N.data <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, 
                               sd=delta.N.sd, se=delta.N.se, group=c("Both species"))
    
    delta.N.bkt.mean <- apply(delta.N.bkt, 1, mean)
    delta.N.bkt.sd <- apply(delta.N.bkt, 1, sd)
    delta.N.bkt.se <- delta.N.bkt.sd/sqrt(sims)
    delta.N.bkt.data <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.bkt.mean, 
                               sd=delta.N.bkt.se, se=delta.N.bkt.se, group=c("Brook trout"))
    
    delta.N.bnt.mean <- apply(delta.N.bnt, 1, mean)
    delta.N.bnt.sd <- apply(delta.N.bnt, 1, sd)
    delta.N.bnt.se <- delta.N.bnt.sd/sqrt(sims)
    delta.N.bnt.data <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.bnt.mean, 
                                   sd=delta.N.bnt.sd, se=delta.N.bnt.se, group=c("Brown trout"))
    
    delta.N.data.comb <- as.data.frame(rbind(delta.N.data, delta.N.bkt.data, delta.N.bnt.data))
    delta.N.data.comb$group <- as.factor(delta.N.data.comb$group)

# recruits
    delta.N.mean.rec <- apply(delta.N.rec, 1, mean)
    delta.N.sd.rec <- apply(delta.N.rec, 1, sd)
    delta.N.se.rec <- delta.N.sd.rec/sqrt(sims)
    delta.N.data.rec <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean.rec, 
                               sd=delta.N.sd.rec, se=delta.N.se.rec, group=c("Both species recruits"))
    
    delta.N.bkt.mean.rec <- apply(delta.N.bkt.rec, 1, mean)
    delta.N.bkt.sd.rec <- apply(delta.N.bkt.rec, 1, sd)
    delta.N.bkt.se.rec <- delta.N.bkt.sd.rec/sqrt(sims)
    delta.N.bkt.data.rec <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.bkt.mean.rec, 
                                   sd=delta.N.bkt.se.rec, se=delta.N.bkt.se.rec, group=c("Brook trout recruits"))
    
    delta.N.bnt.mean.rec <- apply(delta.N.bnt.rec, 1, mean)
    delta.N.bnt.sd.rec <- apply(delta.N.bnt.rec, 1, sd)
    delta.N.bnt.se.rec <- delta.N.bnt.sd.rec/sqrt(sims)
    delta.N.bnt.data.rec <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.bnt.mean.rec, 
                                   sd=delta.N.bnt.sd.rec, se=delta.N.bnt.se.rec, group=c("Brown trout recruits"))
    
# spawners
    delta.N.mean.spawners <- apply(delta.N.spawners, 1, mean)
    delta.N.sd.spawners <- apply(delta.N.spawners, 1, sd)
    delta.N.se.spawners <- delta.N.sd.spawners/sqrt(sims)
    delta.N.data.spawners <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean.spawners, 
                                   sd=delta.N.sd.spawners, se=delta.N.se.spawners, group=c("Both species spawner"))
    
    delta.N.bkt.mean.spawners <- apply(delta.N.bkt.spawners, 1, mean)
    delta.N.bkt.sd.spawners <- apply(delta.N.bkt.spawners, 1, sd)
    delta.N.bkt.se.spawners <- delta.N.bkt.sd.spawners/sqrt(sims)
    delta.N.bkt.data.spawners <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.bkt.mean.spawners, 
                                       sd=delta.N.bkt.se.spawners, se=delta.N.bkt.se.spawners, group=c("Brook trout spawners"))
    
    delta.N.bnt.mean.spawners <- apply(delta.N.bnt.spawners, 1, mean)
    delta.N.bnt.sd.spawners <- apply(delta.N.bnt.spawners, 1, sd)
    delta.N.bnt.se.spawners <- delta.N.bnt.sd.spawners/sqrt(sims)
    delta.N.bnt.data.spawners <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.bnt.mean.spawners, 
                                       sd=delta.N.bnt.sd.spawners, se=delta.N.bnt.se.spawners, group=c("Brown trout spawners"))

    delta.N.data.comb.lifestage <- as.data.frame(rbind(delta.N.data.rec, delta.N.bkt.data.rec, delta.N.bnt.data.rec,
      delta.N.data.spawners, delta.N.bkt.data.spawners, delta.N.bnt.data.spawners))
    delta.N.data.comb.lifestage$group <- as.factor(delta.N.data.comb.lifestage$group)


### plots of change overtime ####
# abundance
    change_overtime <- ggplot(data=delta.N.data) +
      geom_ribbon(aes(x=year, ymin=mean-1.96*se, ymax=mean+1.96*se),
                  , fill="dodgerblue", alpha=0.3)+
      geom_line(aes(x=year, y=mean),  col="dodgerblue", lwd=0.75)+
      geom_hline(yintercept=0, lty="dashed")+
      theme_bw() +
      labs(title="Average difference from initial abundance (95% CIs)", 
           x="Year", y="% difference in abundance(N) from\ninitial abundance(N1)") +
      ylim(c(-60, 30))
    
    change_overtime_byspp <- ggplot(data=delta.N.data.comb %>% 
             filter(delta.N.data.comb$group == "Brook trout" |
                    delta.N.data.comb$group == "Brown trout")) +
      geom_ribbon(aes(x=year, ymin=mean-1.96*se, ymax=mean+1.96*se, fill=group), alpha=0.3)+
      geom_line(aes(x=year, y=mean, col=group))+
      geom_hline(yintercept=0, lty="dashed")+
      scale_fill_manual(values=c("Brook trout" = "chartreuse4",
                                 "Brown trout" = "chocolate4"))+
      scale_color_manual(values=c("Brook trout" = "chartreuse4",
                                 "Brown trout" = "chocolate4"))+
      theme_bw() +
      theme(legend.position = "right") +
      labs(title="Average difference from initial abundance (95% CIs)\nby species", 
           x="Year", y="% difference in abundance(N) from\ninitial abundance(N1)",
           color="Species", fill="Species") +
      ylim(c(-60, 30))

#### % change time snapshots ####
# Create a data frame with named columns
deltaN.table <- data.frame(yr2040 = rep(NA,9), yr2060 = rep(NA,9), 
                           yr2080 = rep(NA,9))
rownames(deltaN.table) <- c("Brook trout", "Brown trout", "Both species",
                            "Brook trout recruits", "Brown trout recruits", "Both species recruits",
                            "Brook trout adults", "Brown trout adults", "Both species adults")
  
#abundance
  #BKT
  deltaN.table[1,1] <- paste(round(delta.N.bkt.data$mean[15],3),
                             round(delta.N.bkt.data$se[15],3),
                             sep=", ")
  deltaN.table[1,2] <- paste(round(delta.N.bkt.data$mean[35],3),
                             round(delta.N.bkt.data$se[35],3),
                             sep=", ")
  deltaN.table[1,3] <- paste(round(delta.N.bkt.data$mean[55],3),
                             round(delta.N.bkt.data$se[55],3),
                             sep=", ")
  
  #BNT
  deltaN.table[2,1] <- paste(round(delta.N.bnt.data$mean[15],3),
                             round(delta.N.bnt.data$se[15],3),
                             sep=", ")
  deltaN.table[2,2] <- paste(round(delta.N.bnt.data$mean[35],3),
                             round(delta.N.bnt.data$se[35],3),
                             sep=", ")
  deltaN.table[2,3] <- paste(round(delta.N.bnt.data$mean[55],3),
                             round(delta.N.bnt.data$se[55],3),
                             sep=", ")
  
  #both species
  deltaN.table[3,1] <- paste(round(delta.N.data$mean[15],3),
                             round(delta.N.data$se[15],3),
                             sep=", ")
  deltaN.table[3,2] <- paste(round(delta.N.data$mean[35],3),
                             round(delta.N.data$se[35],3),
                             sep=", ")
  deltaN.table[3,3] <- paste(round(delta.N.data$mean[55],3),
                             round(delta.N.data$se[55],3),
                             sep=", ")
  
#recruits
  # BKT
  deltaN.table[4,1] <- paste(round(delta.N.bkt.data.rec$mean[15],3),
                             round(delta.N.bkt.data.rec$se[15],3),
                             sep=", ")
  deltaN.table[4,2] <- paste(round(delta.N.bkt.data.rec$mean[35],3),
                             round(delta.N.bkt.data.rec$se[35],3),
                             sep=", ")
  deltaN.table[4,3] <- paste(round(delta.N.bkt.data.rec$mean[55],3),
                             round(delta.N.bkt.data.rec$se[55],3),
                             sep=", ")
  
  #BNT
  deltaN.table[5,1] <- paste(round(delta.N.bnt.data.rec$mean[15],3),
                             round(delta.N.bnt.data.rec$se[15],3),
                             sep=", ")
  deltaN.table[5,2] <- paste(round(delta.N.bnt.data.rec$mean[35],3),
                             round(delta.N.bnt.data.rec$se[35],3),
                             sep=", ")
  deltaN.table[5,3] <- paste(round(delta.N.bnt.data.rec$mean[55],3),
                             round(delta.N.bnt.data.rec$se[55],3),
                             sep=", ")
  
  #both species
  deltaN.table[6,1] <- paste(round(delta.N.data.rec$mean[15],3),
                             round(delta.N.data.rec$se[15],3),
                             sep=", ")
  deltaN.table[6,2] <- paste(round(delta.N.data.rec$mean[35],3),
                             round(delta.N.data.rec$se[35],3),
                             sep=", ")
  deltaN.table[6,3] <- paste(round(delta.N.data.rec$mean[55],3),
                             round(delta.N.data.rec$se[55],3),
                             sep=", ")

#spawners
  # BKT
  deltaN.table[7,1] <- paste(round(delta.N.bkt.data.spawners$mean[15],3),
                             round(delta.N.bkt.data.spawners$se[15],3),
                             sep=", ")
  deltaN.table[7,2] <- paste(round(delta.N.bkt.data.spawners$mean[35],3),
                             round(delta.N.bkt.data.spawners$se[35],3),
                             sep=", ")
  deltaN.table[7,3] <- paste(round(delta.N.bkt.data.spawners$mean[55],3),
                             round(delta.N.bkt.data.spawners$se[55],3),
                             sep=", ")
  
  #BNT
  deltaN.table[8,1] <- paste(round(delta.N.bnt.data.spawners$mean[15],3),
                             round(delta.N.bnt.data.spawners$se[15],3),
                             sep=", ")
  deltaN.table[8,2] <- paste(round(delta.N.bnt.data.spawners$mean[35],3),
                             round(delta.N.bnt.data.spawners$se[35],3),
                             sep=", ")
  deltaN.table[8,3] <- paste(round(delta.N.bnt.data.spawners$mean[55],3),
                             round(delta.N.bnt.data.spawners$se[55],3),
                             sep=", ")
  
  #both species
  deltaN.table[9,1] <- paste(round(delta.N.data.spawners$mean[15],3),
                             round(delta.N.data.spawners$se[15],3),
                             sep=", ")
  deltaN.table[9,2] <- paste(round(delta.N.data.spawners$mean[35],3),
                             round(delta.N.data.spawners$se[35],3),
                             sep=", ")
  deltaN.table[9,3] <- paste(round(delta.N.data.spawners$mean[55],3),
                             round(delta.N.data.spawners$se[55],3),
                             sep=", ")
  

#### % change by spatial unit snapshots ####
#BKT
nunits <- 19

deltaN.unit <- array(NA, dim=dim(abundance.units))
deltaN.bkt.unit <- array(NA, dim=dim(abundance.units.bkt))
deltaN.bnt.unit <- array(NA, dim=dim(abundance.units.bnt))

for(k in 1:nunits){
  for(i in 1:(yrs)){
    for(j in 1:sims){
      deltaN.unit[i,j,k] <- ((abundance.units[i,j,k] - abundance.units[1,j,k])/ abundance.units[1,j,k])*100
      deltaN.bkt.unit[i,j,k] <- ((abundance.units.bkt[i,j,k] - abundance.units.bkt[1,j,k])/ abundance.units.bkt[1,j,k])*100
      deltaN.bnt.unit[i,j,k] <- ((abundance.units.bnt[i,j,k] - abundance.units.bnt[1,j,k])/ abundance.units.bnt[1,j,k])*100
      
   }
  }
}

probs <- c(0.25, 0.5, 0.75)

unit.quants <- apply(deltaN.unit, c(1,3), function(x) quantile(x, probs=probs))
unit.quants.bkt <- apply(deltaN.bkt.unit, c(1,3), function(x) quantile(x, probs=probs))
unit.quants.bnt <- apply(deltaN.bnt.unit, c(1,3), function(x) quantile(x, probs=probs))

#dimensions: 
#unit.quants[3 probs,yrs,nunits]

# both species
deltaN.table.unit <- data.frame(unit=c(1:19), group="Both species",
                                yr2040 = round(unit.quants[2,15,],3),
                                yr2060 = round(unit.quants[2,35,],3),
                                yr2080 = round(unit.quants[2,55,],3))

deltaN.table.unit.ch <- data.frame(unit=c(1:19), group="Both species",
                                   yr2040 = paste0(round(unit.quants[2,15,],2),
                                                   " (",round(unit.quants[1,15,],2),
                                                   ", ", round(unit.quants[3,15,],2),
                                                   ")"),
                                   yr2060 = paste0(round(unit.quants[2,35,],2),
                                                   " (",round(unit.quants[1,35,],2),
                                                   ", ", round(unit.quants[3,35,],2),
                                                   ")"),
                                   yr2080 = paste0(round(unit.quants[2,55,],2),
                                                   " (",round(unit.quants[1,55,],2),
                                                   ", ", round(unit.quants[3,55,],2),
                                                   ")"))
# BKT
deltaN.table.unit.bkt <- data.frame(unit=c(1:19), group="Brook trout",
                                yr2040 = round(unit.quants.bkt[2,15,],3),
                                yr2060 = round(unit.quants.bkt[2,35,],3),
                                yr2080 = round(unit.quants.bkt[2,55,],3))

deltaN.table.unit.bkt.ch <- data.frame(unit=c(1:19), group="Brook trout",
                                   yr2040 = paste0(round(unit.quants.bkt[2,15,],2),
                                                   " (",round(unit.quants.bkt[1,15,],2),
                                                   ", ", round(unit.quants.bkt[3,15,],2),
                                                   ")"),
                                   yr2060 = paste0(round(unit.quants.bkt[2,35,],2),
                                                   " (",round(unit.quants.bkt[1,35,],2),
                                                   ", ", round(unit.quants.bkt[3,35,],2),
                                                   ")"),
                                   yr2080 = paste0(round(unit.quants.bkt[2,55,],2),
                                                   " (",round(unit.quants.bkt[1,55,],2),
                                                   ", ", round(unit.quants.bkt[3,55,],2),
                                                   ")"))                                 
# BNT
deltaN.table.unit.bnt <- data.frame(unit=c(1:19), group="Brown trout",
                                    yr2040 = round(unit.quants.bnt[2,15,],3),
                                    yr2060 = round(unit.quants.bnt[2,35,],3),
                                    yr2080 = round(unit.quants.bnt[2,55,],3))

deltaN.table.unit.bnt.ch <- data.frame(unit=c(1:19), group="Brown trout",
                                       yr2040 = paste0(round(unit.quants.bnt[2,15,],2),
                                                       " (",round(unit.quants.bnt[1,15,],2),
                                                       ", ", round(unit.quants.bnt[3,15,],2),
                                                       ")"),
                                       yr2060 = paste0(round(unit.quants.bnt[2,35,],2),
                                                       " (",round(unit.quants.bnt[1,35,],2),
                                                       ", ", round(unit.quants.bnt[3,35,],2),
                                                       ")"),
                                       yr2080 = paste0(round(unit.quants.bnt[2,55,],2),
                                                       " (",round(unit.quants.bnt[1,55,],2),
                                                       ", ", round(unit.quants.bnt[3,55,],2),
                                                       ")"))  


deltaN.table.unit.ch.all <- rbind(deltaN.table.unit.ch, deltaN.table.unit.bkt.ch, deltaN.table.unit.bnt.ch)


#map of habitat % change in 2040, 2060, 2080
reach.units <- read_sf("Data/Spatial/Untitled/reach_units.shp")

map.lims <- c(-81,30)

# Both species
 deltaN.unit.sf <- merge(reach.units, deltaN.table.unit, by="unit", all.x=F)
      
      map2040 <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.sf, aes(color=yr2040), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title = "2040",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      map2060 <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.sf, aes(color=yr2060), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title = "2060",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      
      map2080 <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.sf, aes(color=yr2080), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title = "2080",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      fig.bothspp <- ggarrange(map2040, map2060, map2080, ncol=3, nrow=1, 
                           common.legend = TRUE, legend="bottom")
      
      fig.bothspp <- annotate_figure(fig.bothspp, top = text_grob("Both species", 
                                      color = "black", 
                                      face = "bold", 
                                      size = 16))
      

# BKT
      deltaN.unit.bkt.sf <- merge(reach.units, deltaN.table.unit.bkt, by="unit", all.x=F)
      
      map2040.bkt <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.bkt.sf, aes(color=yr2040), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title= "2040",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      map2060.bkt <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.bkt.sf, aes(color=yr2060), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title= "2060",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      
      map2080.bkt <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.bkt.sf, aes(color=yr2080), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title= "2080",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      fig.bkt <- ggarrange(map2040.bkt, map2060.bkt, map2080.bkt, ncol=3, nrow=1, 
                common.legend = TRUE, legend="bottom")
      fig.bkt <- annotate_figure(fig.bkt, top = text_grob("Brook trout", 
                                                                  color = "black", 
                                                                  face = "bold", 
                                                                  size = 16))
      

# BNT
      deltaN.unit.bnt.sf <- merge(reach.units, deltaN.table.unit.bnt, by="unit", all.x=F)
      
      map2040.bnt <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.bnt.sf, aes(color=yr2040), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title= "2040",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      map2060.bnt <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.bnt.sf, aes(color=yr2060), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title= "2060",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      
      map2080.bnt <- ggplot()+
        geom_sf(data=reach.units["Join_Count"], col="black") +
        geom_sf(data=deltaN.unit.bnt.sf, aes(color=yr2080), lwd=0.75) +
        scale_color_viridis_c(limits=map.lims, option = "plasma", direction=-1) +
        labs(title= "2080",
             color="% difference from\ninitial abundance") +
        coord_sf(xlim = c(-84.8, -84.05)) +
        theme_bw() +
        theme( plot.title = element_text(hjust = 0.5),
               axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
               axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
               axis.ticks = element_blank(),  # Removes axis tick marks
               axis.line = element_blank()    # Removes the axis line itself
        )
      
      fig.bnt <- ggarrange(map2040.bnt, map2060.bnt, map2080.bnt, ncol=3, nrow=1, 
                           common.legend = TRUE, legend="bottom")
      fig.bnt <- annotate_figure(fig.bnt, top = text_grob("Brown trout", 
                                                                  color = "black", 
                                                                  face = "bold", 
                                                                  size = 16))


#### Exporting tables and plots ####
      
#tables
write.csv(deltaN.table, "Results/S10_1.5wood_h1/S10deltaN.table.csv")
write.csv(deltaN.table.unit.ch.all, "Results/S10_1.5wood_h1/S10deltaN.table.byunit.csv")

#figures
ggsave(filename = "S10change_overtime.jpeg", plot = change_overtime, 
       path = "Results/S10_1.5wood_h1/", width = 6, height = 5, dpi = 300)

ggsave(filename = "S10change_overtime_byspp.jpeg", plot = change_overtime_byspp, 
       path = "Results/S10_1.5wood_h1/", width = 6, height = 4, dpi = 300)

ggsave(filename = "S10change_byunit.jpeg", plot = fig.bothspp, 
       path = "Results/S10_1.5wood_h1/", width = 6, height = 3.5, dpi = 300)

ggsave(filename = "S10change_byunit_bkt.jpeg", plot = fig.bkt, 
       path = "Results/S10_1.5wood_h1/", width = 6, height = 3.5, dpi = 300)

ggsave(filename = "S10change_byunit_bnt.jpeg", plot = fig.bnt, 
       path = "Results/S10_1.5wood_h1/", width = 6, height = 3.5, dpi = 300)

