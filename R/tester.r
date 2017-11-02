# tester.r

haul %>%
  filter(vessel == "KW172") %>% 
  filter(trip=="2017009") %>% 
  mutate(vesseltrip = paste(vessel, trip, sep=" trip:")) %>% 
  ggplot(aes(plotlon, plotlat)) + 
  theme_publication() +
  theme(strip.background = element_rect(colour=NA, fill = "#f0f0f0"),
        panel.border     = element_rect(colour="gray" , size=0.2),
        legend.title=element_blank() ) +
  
  # coord_quickmap(xlim=c(-6,-2) , ylim=c(58,60)) +
  coord_quickmap(xlim=c(-7.5,0) , ylim=c(55,59.6)) +
  geom_polygon(data=world.europe.df, aes(long, lat, group=group),
               fill = "lightgray", colour=NA) +
  geom_polygon(data=fao27.df,   aes(long, lat, group=group),
               fill = NA, size=0.3, colour="black") +
  geom_polygon(data=areas, aes(long, lat, group=area), colour="red", size=0.2,fill=NA) +
  geom_jitter(aes(colour=trip), alpha = 0.6, size=2) +
  geom_text(aes(label=haul), position=position_jitter(width=0.1,height=0.1)) +
  labs(x = NULL, y = NULL) +
  facet_wrap(~vesseltrip, ncol=1)
