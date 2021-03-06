## Quincy Troutman
## CIS361
## Racial Wage Gap
 
##Retrieving my data file

mywd <- "C:/Users/Quinc/OneDrive/Murray State/Spring 2021/CIS 361"
setwd(mywd)
getwd()

myfile <- "survey_results_public.csv"

Alldata <- read.csv(file = myfile, header = TRUE, sep = ",")

head(Alldata)
tail(Alldata)



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

  