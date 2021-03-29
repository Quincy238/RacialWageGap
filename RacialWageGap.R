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

#Remove rows with null values
na.omit(Alldata)

install.packages("plyr")
install.packages("dplyr")


require(plyr)
require(dplyr)

#Pulling the Columns I want to see.
SmallData <- select(Alldata, Respondent, Employment, Country, Ethnicity, DevType
                    , YearsCode, CompTotal, CompFreq, WorkWeekHrs, Age, Gender)

#Filtering columns to get the right Data.
FromUSA <- SmallData %>% filter(Country == 'United States', Employment == 'Employed full-time')

FromUSA <- na.omit(FromUSA)

EthnicCount <- FromUSA %>% 
  select(Ethnicity, YearsCode, CompTotal, WorkWeekHrs, Age) %>%
  group_by(Ethnicity) %>%
  count() #%>%
  #group_by(YearsCode) %>%
  #count() %>%
  #summarize( AveragePay = mean(CompTotal), AverageHrsPerWeek = mean(WorkWeekHrs), AverageAge = mean(Age))
        

        
        
        
        
        
        