### initial relative pop size map ####
load("Results/S0_initialvalues/S0_initialvalues.Rdata")

rowsum <- apply(i.abundance.units, 1, sum)
prop <- i.abundance.units/rowsum
meanperc <- round((apply(prop, 2, mean))*100, 2)
sdperc <- round((apply(prop, 2, sd))*100, 2)

initNperc <- data.frame(unit= c(1:19), perc = meanperc, sd = sdperc)
reach.units <- read_sf("Data/Spatial/participatory_mapping/reaches_with_units/reach_units.shp")

initNperc.sf <- merge(reach.units, initNperc, by="unit", all.y=F)

initNperc.map <- ggplot()+
  geom_sf(data=reach.units["Join_Count"], col="black") +
  geom_sf(data=initNperc.sf, aes(color=perc), lwd=0.75) +
  scale_color_viridis_c(limits=c(3,8), option = "viridis", direction=-1) +
  labs(title = "Relative proportion of initial abundance",
       color="% of total\npopulation") +
  coord_sf(xlim = c(-84.8, -84.05)) +
  theme_bw() +
  theme( plot.title = element_text(hjust = 0.5),
         axis.title = element_blank(),  # Removes axis titles (e.g., "wt", "mpg")
         axis.text = element_blank(),   # Removes axis tick labels (e.g., 20, 30)
         axis.ticks = element_blank(),  # Removes axis tick marks
         axis.line = element_blank()    # Removes the axis line itself
  )

ggsave(filename = "initialrelativeN.jpeg", plot = initNperc.map, 
       path = "Results/S0_initialvalues/", width = 6, height = 5, dpi = 300)

write.csv(initNperc, "Results/S0_initialvalues/initialrelativeN.csv")

