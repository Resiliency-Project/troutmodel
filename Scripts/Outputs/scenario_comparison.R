
load("Results/S0_initialvalues/S0_initialvalues.Rdata")

library(ggplot2)
library(sf)
library(viridis)

yrs <- 55
sims <- 500

#scenario 1 ####
load("Results/S1_nochange/S1results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.1 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                           sd=delta.N.sd, se=delta.N.se, Scenario="1")

#scenario 2 ####
load("Results/S2_lowclimatechange/S2results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims) 
delta.rec <- matrix(NA, nrow=yrs, ncol=sims) 
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims) 

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.2 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                           sd=delta.N.sd, se=delta.N.se, Scenario="2")

#scenario 3 ####
load("Results/S3_midclimatechange/S3results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.3 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                           sd=delta.N.sd, se=delta.N.se, Scenario="3")

#scenario 4 ####
load("Results/S4_highclimatechange/S4results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.4 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                             sd=delta.N.sd, se=delta.N.se, Scenario="4")

#scenario 5 ####
load("Results/S5_nochangewood_h1/S5results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.5 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                             sd=delta.N.sd, se=delta.N.se, Scenario="5")

#scenario 6 ####
load("Results/S6_nochangewood_h2/S6results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.6 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                             sd=delta.N.sd, se=delta.N.se, Scenario="6")

#scenario 7 ####
load("Results/S7_maxflowreduction/S7results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.7 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                             sd=delta.N.sd, se=delta.N.se, Scenario="7")


#scenario 8 ####
load("Results/S8_1.25wood_h1/S8results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.8 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                             sd=delta.N.sd, se=delta.N.se, Scenario="8")

#scenario 9 ####
load("Results/S9_1.25wood_h2/S9results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.9 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                             sd=delta.N.sd, se=delta.N.se, Scenario="9")



#scenario 10 ####
load("Results/S10_1.5wood_h1/S10results.Rdata")
delta.N <- matrix(NA, nrow=yrs, ncol=sims)
delta.rec <- matrix(NA, nrow=yrs, ncol=sims)
delta.spawn <- matrix(NA, nrow=yrs, ncol=sims)

for(i in 1:(yrs)){
  for(j in 1:sims){
    delta.N[i,j] <- ((abundance.hist[i,j]/i.abundance.hist[j]) -1)*100
    delta.rec[i,j] <- ((recruits.hist[i,j]/i.recruits.hist[j]) -1)*100
    delta.spawn[i,j] <- ((spawners.hist[i,j]/i.spawners.hist[j]) -1)*100
  }
}

delta.N.mean <- apply(delta.N, 1, mean)
delta.N.med <- apply(delta.N, 1, median)

delta.N.sd <- apply(delta.N, 1, sd)
delta.N.se <- delta.N.sd/sqrt(sims)
delta.N.data.10 <- data.frame(year= c(2026:(2025+yrs)), mean=delta.N.mean, median=delta.N.med,
                             sd=delta.N.sd, se=delta.N.se, Scenario="10")



# combining ####
delta.N.data <- rbind(delta.N.data.1,
                      delta.N.data.2,
                      delta.N.data.3,
                      delta.N.data.4,
                      delta.N.data.5,
                      delta.N.data.6,
                      delta.N.data.7,
                      delta.N.data.8,
                      delta.N.data.9,
                      delta.N.data.10)

delta.N.data$Scenario <- as.factor(delta.N.data$Scenario)

#all scenarios
all.scens <- ggplot(data=delta.N.data) +
  geom_ribbon(aes(x=year, ymin=mean-1.96*se, ymax=mean+1.96*se, 
                  color=Scenario, fill=Scenario),  alpha=0.3, lty=0)+
  geom_line(aes(x=year, y=mean, color=Scenario))+
  theme_bw() +
  labs(title="Average difference from initial abundance (95% CIs)", 
       x="Year", y="% difference in abundance(N) from\ninitial abundance(N1)")+
  geom_hline(yintercept = 0, lty=2, lwd=0.5) +
  scale_fill_viridis(discrete=T)+
  scale_color_viridis(discrete=T) +
  ylim(c(-60, 20))


ggsave(filename = "all_scenarios.jpeg", plot = all.scens, 
       path = "Results/Scenario_comparison/", 
       width = 6, height = 4, dpi = 300)

#comparing climate scenarios
clim.scens <- ggplot(data=delta.N.data %>% 
         filter(delta.N.data$Scenario == "1" |
                delta.N.data$Scenario == "2" | 
                delta.N.data$Scenario == "3" |
                delta.N.data$Scenario == "4" )) +
  geom_ribbon(aes(x=year, ymin=mean-1.96*se, ymax=mean+1.96*se, 
                  color=Scenario, fill=Scenario),  alpha=0.3, lty=0)+
  geom_line(aes(x=year, y=mean, color=Scenario))+
  theme_bw() +
  labs(title="Average difference from initial abundance (95% CIs)", 
       x="Year", y="% difference in abundance(N) from\ninitial abundance(N1)")+
  geom_hline(yintercept = 0, lty=2, lwd=0.5) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ylim(c(-60, 20))

ggsave(filename = "climate_scenarios.jpeg", plot = clim.scens, 
       path = "Results/Scenario_comparison/", 
       width = 6, height = 4, dpi = 300)

#comparing wood scenarios
wood.scens <- ggplot(data=delta.N.data %>% 
                       filter(delta.N.data$Scenario == "2" |
                                delta.N.data$Scenario == "5" | 
                                delta.N.data$Scenario == "6" |
                                delta.N.data$Scenario == "8" |
                                delta.N.data$Scenario == "9" |
                                delta.N.data$Scenario == "10")) +
  geom_ribbon(aes(x=year, ymin=mean-1.96*se, ymax=mean+1.96*se, 
                  color=Scenario, fill=Scenario),  alpha=0.3, lty=0)+
  geom_line(aes(x=year, y=mean, color=Scenario))+
  theme_bw() +
  labs(title="Average difference from initial abundance (95% CIs)", 
       x="Year", y="% difference in abundance(N) from\ninitial abundance(N1)")+
  geom_hline(yintercept = 0, lty=2, lwd=0.5) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ylim(c(-60, 20))

ggsave(filename = "wood_scenarios.jpeg", plot = wood.scens, 
       path = "Results/Scenario_comparison/", 
       width = 6, height = 4, dpi = 300)

#comparing flow scenarios
lfy.scens <- ggplot(data=delta.N.data %>% 
                       filter(delta.N.data$Scenario == "2" |
                                delta.N.data$Scenario == "7")) +
  geom_ribbon(aes(x=year, ymin=mean-1.96*se, ymax=mean+1.96*se, 
                  color=Scenario, fill=Scenario),  alpha=0.3, lty=0)+
  geom_line(aes(x=year, y=mean, color=Scenario))+
  theme_bw() +
  labs(title="Average difference from initial abundance (95% CIs)", 
       x="Year", y="% difference in abundance(N) from\ninitial abundance(N1)")+
  geom_hline(yintercept = 0, lty=2, lwd=0.5) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  ylim(c(-60, 20))

ggsave(filename = "lfy_scenarios.jpeg", plot = lfy.scens, 
       path = "Results/Scenario_comparison/", 
       width = 6, height = 4, dpi = 300)






