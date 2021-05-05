ggplot(pois, aes(x=x)) +
  geom_density(aes(group=Lambda, color=Lambda, fill=Lambda),
               adjust=4, alpha=1/2) +
  scale_color_discrete() + scale_fill_discrete() +
  ggtitle("Probability Mass Function")


summary(FromUSA$CompTotal)

##ANOVA
##Post Hawk? Turkey 


#median
PlotAvgPayPerEthnicity <- ggplot(FromUSA, aes(x=WorkWeekHrs, y=CompTotal)) + 
  geom_point(aes(col=Ethnicity), size=1) +
  geom_smooth(method="auto", col="firebrick") +
  labs(title="Average Pay Per Ethnicity", subtitle="Average Annual Pay in USD",
       y="CompTotal", x="WorkWeekHrs", caption="Figure 1. Average Pay") +       
  scale_x_continuous(breaks=seq(0, 100, 10))+
  scale_y_continuous(breaks=seq(0, 6000000, 300000))+
  coord_cartesian(xlim=c(0, 100), ylim=c(0, 6000000))

plot(PlotAvgPayPerEthnicity)