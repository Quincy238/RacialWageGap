mywd <- "C:/Users/Quinc/OneDrive/Murray State/Spring 2021/CIS 361"
setwd(mywd)
getwd()

myfile <- "survey_results_public.csv"

Alldata <- read.csv(file = myfile, header = TRUE, sep = ",")

head(Alldata)
tail(Alldata)


install.packages("flexdashboard")
install.packages("plyr")
install.packages("dplyr")
install.packages("vctrs")
install.packages("ggplot2")
install.packages("rmarkdown")
install.packages("knitr")
install.packages("stargazer")
install.packages('tidyverse')
install.packages('ggpubr')
install.packages('rstatix')
library(tidyverse)
library(ggpubr)
library(rstatix)
library(rmarkdown)
library(knitr)
library(stargazer)
require(ggplot2)
require(vctrs)
require(plyr)
require(dplyr)

#Pulling the Columns I want to see.
SmallData <- select(Alldata, Respondent, Employment, Country, Ethnicity, DevType
                    , YearsCode, CompTotal, WorkWeekHrs, Age, Gender)
SmallData <-na.omit(SmallData)
#Filtering columns to get the right Data.
FromUSA <- SmallData %>% filter(Country == 'United States', Employment == 'Employed full-time')

#Remove rows with null values
FromUSA <- na.omit(FromUSA)

#Narrowing the scope of DevTypes
FromUSA[] <- FromUSA %>% 
  group_by(DevType) %>%
  arrange(DevType)
i <- 0
while (i <= 533){
  FromUSA$DevType[i] <- "Academic researcher"
  i = i + 1
}
while (i <= 1490){
  FromUSA$DevType[i] <- "Data or business analyst"
  i = i + 1
}
while (i <= 2000){
  FromUSA$DevType[i] <- "Data scientist or machine learning specialist"
  i = i + 1
}
while (i <= 2943){
  FromUSA$DevType[i] <- "Database administrator"
  i = i + 1
}
while (i <= 3620){
  FromUSA$DevType[i] <- "Designer"
  i = i + 1
}
while (i <= 12083 ){
  FromUSA$DevType[i] <- "Developer"
  i = i + 1
} 
while (i <= 12200 ){
  FromUSA$DevType[i] <- "DevOps specialist"
  i = i + 1
} 
while (i <= 12221 ){
  FromUSA$DevType[i] <- "Educator"
  i = i + 1
}
while (i <= 12433  ){
  FromUSA$DevType[i] <- "Engineer"
  i = i + 1
}
while (i <= 12465 ){
  FromUSA$DevType[i] <- "Product manager"
  i = i + 1
}
while (i <= 12488  ){
  FromUSA$DevType[i] <- "Scientist"
  i = i + 1
}
while (i <= 12537 ){
  FromUSA$DevType[i] <- "Senior executive/VP"
  i = i + 1
}
while (i <= 12541 ){
  FromUSA$DevType[i] <- "Student"
  i = i + 1
} 

FromUSA$YearsCode[grepl("More than 50 years",FromUSA$YearsCode)]<-'55'
FromUSA$YearsCode[grepl("Less than 1 year",FromUSA$YearsCode)]<-'1'
FromUSA$YearsCode <- as.numeric(FromUSA$YearsCode)

## Tables

EthnicCount <- FromUSA %>% 
  select( c(Ethnicity, CompTotal, WorkWeekHrs, Age)) %>%
  group_by(Ethnicity) %>%
  dplyr::summarise(NumberOfPeople = n(), AveragePay = mean(CompTotal), AverageHrsPerWeek = mean(WorkWeekHrs), 
                   AverageAge = mean(Age))

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

##Anova
set.seed(1234)
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
  group_by(DevType, Ethnicity) %>% 
  get_summary_stats(AverageYearsExperience, type = "mean")


#p-Value = 0.273 (Pay to DevType)
model <- aov(mean~DevType, data=AnovaDevtypeEthPay)
summary(model)
Turkey<-TukeyHSD(model, conf.level=.95)
TurkeyPlot <- plot(Turkey, las=2, alighn= "left")

#p-Value = .00864 (Experience to DevType)
model2 <- aov(mean~DevType, data=AnovaDevtypeEthExp)
summary(model2)
Turkey2<-TukeyHSD(model2, conf.level=.95)
plot(Turkey2, las=2)

#p-Value = .28 (Pay to Ethnicity)
model3 <- aov(mean~Ethnicity, data=AnovaDevtypeEthPay)
summary(model3)
Turkey3<-TukeyHSD(model3, conf.level=.95)
plot(Turkey3, las=2)

#p-Value = .339 (Experience to Ethnicity)
model4 <- aov(mean~Ethnicity, data=AnovaDevtypeEthExp)
summary(model4)
Turkey4<-TukeyHSD(model4, conf.level=.95)
plot(Turkey4,las=2)