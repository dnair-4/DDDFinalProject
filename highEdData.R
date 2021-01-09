#Data taken from IPUMS 
#SESTAT samples include data from three surveys: the National Survey of College Graduates (NSCG), Survey of Doctorate Recipients (SDR), and the National Survey of Recent College Graduates (NSRCG).
library (readr)
library(tidyverse)
library (magrittr)
originalData <- read_csv("/Users/divyanair/Desktop/DDD-I21/highered_00001.csv.gz")
print (dim(originalData)) #gives number of rows by number of columns
highEdData <- originalData
colnames(highEdData) <- c("personID","year","studyWeight","sampleID","surveyID","age","gender","race","citizenStatus","numberOfChildren","highestDegree","jobSatisfaction","salarySatisfaction","socialSatisfaction")
highEdDataCols <- colnames(highEdData) #gets column/variable names
print (highEdDataCols) 
highEdDataRows <- rownames(highEdData) #gets row names/ observations
colClass <- lapply (highEdData, class) #gets class for all columns
print (colClass)
colType <- lapply (highEdData,typeof) #gets class for all columns
print (colType)
missingVals <- lapply (highEdData,is.na) #gets missing values for all columns. In this data, all skips are logical and are denoted by a 98
print (missingVals) #none should appear

#Gender: 1 = Female, 2 = Male
#Race/Ethnicity: 1 = Asian, 2 = White, 3 = Under-represented minority, 4 = other
#Citizenship: 0 = yes, 1 = no
#Degree: 1 = BA, 2 = MA, 3 = Doctorate, 4 = Professional
#All Satisfactions: 1 = very satisfied, 2 = satisfied, 3 = dissatisfied, 4 = very dissatisfied

# Creating Binary Flags to clean data
highEdData$naChildren <- ifelse(highEdData$numberOfChildren == 98,1,0) #gives a 1 for people who didn't respond/have no children
highEdData$naJobSat <- ifelse(highEdData$jobSatisfaction == 98,1,0) #gives a 1 for people who didn't respond
highEdData$naCitizenship <- ifelse(highEdData$citizenStatus == 98,1,0) #gives a 1 for people who didn't respond/ may be undocumented
highEdData$naSalarySat <- ifelse(highEdData$salarySatisfaction == 98,1,0) #gives a 1 for people who didn't respond
highEdData$naSocialSat <- ifelse(highEdData$socialSatisfaction == 98,1,0) #gives a 1 for people who didn't respond

#Cleaning data by removing all observations with no response for one or more questions
cleanedData <- highEdData[!(highEdData$naChildren == 1 | 
                              highEdData$naJobSat == 1 | 
                              highEdData$naCitizenship == 1 | 
                              highEdData$naSalarySat == 1 | 
                              highEdData$naSocialSat == 1),]

#### Examining relationships between variables
#Gender and Highest Degree 
genderAndDegree <- highEdData %>% 
  dplyr::select(gender , highestDegree) %>% 
  dplyr::arrange(gender)
femaleDegree <- genderAndDegree[genderAndDegree$gender == 1, ]
unique (femaleDegree$gender) #should be 1
avgFemDegree <- mean(femaleDegree$highestDegree) #average degree is 1.942707. Rounded to 2, females have a Masters Degree as highest degree
maleDegree <- genderAndDegree[genderAndDegree$gender == 2, ] 
avgMaleDegree <- mean(maleDegree$highestDegree) #average degree is 2.007282. Higher than females. Rounded to 2, males have a Masters Degree as highest degree
unique (maleDegree$gender) #should be 2
#creates a dataframe with degree breakdown by gender
femDegreeBreakdown <- femaleDegree %>% 
  dplyr::group_by(gender,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = breakdown / sum(breakdown))
maleDegreeBreakdown <- maleDegree %>% 
  dplyr::group_by(gender,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = breakdown / sum(breakdown))
# Findings: Males and females have similar % of Professional degrees, 
#males have a significantly higher % of Doctorate Degrees, 
#females have significantly higher % Masters, BA % relatively similar

#Race and Highest Degree
raceAndDegree <- highEdData %>% 
  dplyr::select(race , highestDegree) %>% 
  dplyr::arrange(race)
asianDegree <- raceAndDegree[raceAndDegree$race == 1, ]
avgAsianDegree <- mean(asianDegree$highestDegree) #average Asian degree: 2.069207
whiteDegree <- raceAndDegree[raceAndDegree$race == 2, ]
avgWhiteDegree <- mean(whiteDegree$highestDegree) #average Asian degree: 1.991291
underRepMinDegree <- raceAndDegree[raceAndDegree$race == 3, ]
avgUnderRepMinDegree <- mean(underRepMinDegree$highestDegree) #average underepresented minority degree: 1.86711
#Findings of below will be best described in a graph
asianBreakdown <- asianDegree %>% 
  dplyr::group_by(race,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = breakdown / sum(breakdown))
whiteBreakdown <- whiteDegree %>% 
  dplyr::group_by(race,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = breakdown / sum(breakdown))
underRepMinBreakdown <- underRepMinDegree %>% 
  dplyr::group_by(race,highestDegree) %>% 
  dplyr::summarise(breakdown = n()) %>% 
  dplyr::mutate(percentDegree = breakdown / sum(breakdown))

#Degree and Job/Salary/Social Satisfaction

