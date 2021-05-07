ggplot(FromUSA, aes(x=WorkWeekHrs, y=CompTotal)) + 
  geom_point(aes(col=Ethnicity), size=1) +
  geom_smooth(method="lm") +
  scale_x_continuous(breaks=seq(0, 100, 10))+
  scale_y_continuous(breaks=seq(0, 1600000, 300000))+
  coord_cartesian(xlim=c(0, 100), ylim=c(0, 1600000))


RegressionForPayLM <- lm(WorkWeekHrs ~ CompTotal, data=FromUSA)
RegressionForPayLM
summary(RegressionForPayLM)

stargazer(FromUSA, type = 'html')
stargazer(RegressionForPayLM, type = "html", align=TRUE)

#Anova

set.seed(1234)
Test <- DevTypeVsEthnicity %>% sample_n_by(Ethnicity,DevType, size = 1)

options(scipen=999)
#DF (Ethnicity, DevType, AveragePay)
DevType_Ethnicity_Pay<- DevTypeVsEthnicity %>%
  select(Ethnicity, DevType, AveragePay)
#DF (Ethnicity, DevType,AverageYearsExperience)
DevType_Ethnicity_Exp <- DevTypeVsEthnicity %>%
  select(Ethnicity, DevType,AverageYearsExperience)

#Summary of mean pay Group by DevType, Ethnicity
AnovaDevtypeEthPay <- DevType_Ethnicity_Pay %>% 
  group_by(DevType, Ethnicity) %>%
  get_summary_stats(AveragePay, type = "mean")
#Summary of mean Experience Group by DevType, Ethnicity
AnovaDevtypeEthExp <- DevType_Ethnicity_Exp %>% 
  facet_wrap(~DevType) %>%
  group_by(DevType, Ethnicity) %>% 
    get_summary_stats(AverageYearsExperience, type = "mean")

#p-Value = 0.273 (Pay to DevType)
model <- aov(mean~DevType, data=AnovaDevtypeEthPay)

summary(model)
#turkey for model (Pay to DevType)
Turkey<-TukeyHSD(model, conf.level=.95)
#Turkey plot (Pay to DevType)
TurkeyPlot <- plot(Turkey, las=2, alighn= "left")

stargazer(Turkey$model, type = "html")
#p-Value = .00864 (Experience to DevType)
model2 <- aov(mean~DevType, data=AnovaDevtypeEthExp)

summary(model2)
#turkey for model 2(Experience to DevType)
Turkey2<-TukeyHSD(model2, conf.level=.95)
#Turkey plot (Experience to DevType)
plot(Turkey2, las=2)

#p-Value = .28 (Pay to Ethnicity)
model3 <- aov(mean~Ethnicity, data=AnovaDevtypeEthPay)

summary(model3)
#turkey for model 3(Pay to Ethnicity)
Turkey3<-TukeyHSD(model3, conf.level=.95)
#Turkey plot (Pay to Ethnicity)
plot(Turkey3, las=2)

#p-Value = .339 (Experience to Ethnicity)
model4 <- aov(mean~Ethnicity, data=AnovaDevtypeEthExp)

summary(model4)
#turkey for model 4(Experience to Ethnicity)
Turkey4<-TukeyHSD(model4, conf.level=.95)
#Turkey plot (Experience to Ethnicity)
plot(Turkey4,las=2)




ggboxplot(AnovaDevtypeEthPay, x= "DevType", y = "mean", fill = "DevType") +
  labs(y = "Average Pay")  +
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I', 'J', 'K', 'L', 'M', 'N'))

ggboxplot(AnovaDevtypeEthPay, x= "Ethnicity", y = "mean", fill = "Ethnicity") +
  labs(y = "Average Pay")  +
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I'))

ggplot(AnovaDevtypeEthPay, aes(x=Ethnicity, y=mean)) + 
  geom_col(aes(col=Ethnicity), size=1) +
  labs(title="Average Pay Per Ethnicity", subtitle="Average Annual Pay in USD",
       y="Average Pay", x="Ethnicity", caption="Figure 1. Average Pay")+
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I'))

ggboxplot(AnovaDevtypeEthExp, x= "DevType", y = "mean", fill = "DevType") +
  labs(y = "Experence")  +
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I', 'J', 'K', 'L', 'M', 'N'))

ggboxplot(AnovaDevtypeEthExp, x= "Ethnicity", y = "mean", fill = "Ethnicity") +
  labs(y = "Experence")  +
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I'))

ggplot(AnovaDevtypeEthExp, aes(x=Ethnicity, y=mean)) + 
  geom_col(aes(col=Ethnicity), size=1) +
  labs(title="Average Experence Per Ethnicity", subtitle="Average Annual Pay in USD",
       y="Average Experence", x="Ethnicity", caption="Figure 1. Average Experence")+
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I'))

ggplot(AnovaDevtypeEthExp, aes(x=DevType, y=mean)) + 
  geom_col(aes(col=DevType), size=1) +
  labs(title="Average Experence Per DevType", subtitle="Average Annual Pay in USD",
       y="Average Experence", x="DevType", caption="Figure 1. Average Experence")+
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I', 'J', 'K', 'L', 'M', 'N'))

AnovaDevtypeEthExp %>% levene_test(mean ~ DevType)

Pairwise <- AnovaDevtypeEthExp %>% tukey_hsd(mean ~ DevType)

AnovaDevtypeEthExp %>% tukey_hsd()




AnovaBoAD <- AnovaDevtypeEthPay %>% 
  filter(Ethnicity == "Black or of African descent") %>%
  group_by(DevType)

AnovaBiracial <- AnovaDevtypeEthPay %>% 
  filter(Ethnicity == "Biracial")%>%
group_by(DevType)
AnovaEastAsian <- AnovaDevtypeEthPay %>% 
  filter(Ethnicity == "East Asian")%>%
  group_by(DevType)
AnovaHispanic <- AnovaDevtypeEthPay %>% 
  filter(Ethnicity == "Hispanic or Latino/Latina")%>%
  group_by(DevType)
AnovaMiddleEastern <- AnovaDevtypeEthPay %>% 
  filter(Ethnicity == "Middle Eastern")%>%
  group_by(DevType)
AnovaNative <- AnovaDevtypeEthPay %>% 
  filter(Ethnicity == "Native American, Pacific Islander, or Indigenous Australian")%>%
  group_by(DevType)
AnovaSouthAsian <- AnovaDevtypeEthPay %>% 
  filter(Ethnicity == "South Asian")%>%
  group_by(DevType)
AnovaWhiteorEuropeandescent <- AnovaDevtypeEthPay %>% 
  filter(Ethnicity == "White or of European descent")%>%
  group_by(DevType)




