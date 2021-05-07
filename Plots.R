#Table Creation

############################################################################################
EthnicCount <- FromUSA %>% 
  select( c(Ethnicity, CompTotal, WorkWeekHrs, Age)) %>%
  group_by(Ethnicity) %>%
  dplyr::summarise(NumberOfPeople = n(), AveragePay = mean(CompTotal), AverageHrsPerWeek = mean(WorkWeekHrs), 
            AverageAge = mean(Age))
############################################################################################

DevType <- FromUSA %>% 
  select(c(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age)) %>%
  group_by(DevType) %>%
  dplyr::summarise(NumberOfPeople = n(), AveragePay = mean(CompTotal), AverageHrsPerWeek = mean(WorkWeekHrs), 
            AverageAge = mean(Age))


DevTypeVsEthnicity <- FromUSA %>% 
  select(c(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode)) %>%
  group_by(DevType, Ethnicity) %>%
  dplyr::summarize(NumberOfPeople = n(), AveragePay = mean(CompTotal), AverageHrsPerWeek = mean(WorkWeekHrs), 
            AverageAge = mean(Age), AverageYearsExperience = mean(YearsCode)) 

###################################################################################
#Plots/Graphs   
options(scipen=999)

DevTypeAVGPayPerEthnicity<-ggplot(DevTypeVsEthnicity, aes(x = Ethnicity, 
                                                          y = AveragePay, fill = Ethnicity)) + 
  geom_col() + facet_wrap(~DevType)+
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I'))


plot(DevTypeAVGPayPerEthnicity) 
#######################################################################################

#############################################################################################
PayPerEth <- ggplot(EthnicCount, aes(x=Ethnicity, y=AveragePay, fill = Ethnicity)) +
  geom_col() +
  labs(title="Average Pay Per Ethnicity", subtitle="Average Annual Pay in USD",
       y="Average Pay", x="Ethnicity", caption="Figure 1. Average Pay") +
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I'))
####################################################################################
plot(PayPerEth)

ggplot(FromUSA, aes(y = CompTotal, x = Respondent)) +
geom_point() + geom_smooth() +
facet_wrap(~DevType + Ethnicity) +
scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I'))
#########################################################################################################
NumPeoplePerEthnicity <- ggplot(EthnicCount, aes(x=Ethnicity, y = NumberOfPeople, fill = Ethnicity)) +
  geom_col() +
  labs(title="Number of People Per Ethnicity",
       y="Number of people", x="Ethnicity", caption="Figure 1. Average Pay") +
  scale_x_discrete(labels = c('A','B','C','D','E','F','G','H','I'))+
  scale_y_continuous(breaks=seq(0, 10250, 750))
#####################################################################################################
plot(NumPeoplePerEthnicity)





Biracial <- FromUSA %>%
  select(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode) %>%
  filter(Ethnicity == 'Biracial')

BlackorofAfricandescent <- FromUSA %>%
  select(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode) %>%
  filter(Ethnicity == 'Black or of African descent')


EastAsian <- FromUSA %>%
  select(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode) %>%
  filter(Ethnicity == 'East Asian')%>%


Hispanic <- FromUSA %>%
  select(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode) %>%
  filter(Ethnicity == 'Hispanic or Latino/Latina')



MiddleEastern <- FromUSA %>%
  select(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode) %>%
  filter(Ethnicity == 'Middle Eastern')

Native <- FromUSA %>%
  select(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode) %>%
  filter(Ethnicity == 'Native American, Pacific Islander, or Indigenous Australian')


SouthAsian <- FromUSA %>%
  select(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode) %>%
  filter(Ethnicity == 'South Asian') 

WhiteorEuropeandescent <- FromUSA %>%
  select(DevType, Ethnicity, WorkWeekHrs, CompTotal, Age, YearsCode) %>%
  filter(Ethnicity == 'White or of European descent')
