#Data Management
#Assignment 1
#Vasileios Gounaris Bampaletsos - 403148403

#combine datasets with dplyr
#Data quality analysis 
#i fix the data issues both in R and SQL

#set working directory
setwd("/Users/basilisgounarismpampaletsos/Desktop/PROJECTS/04:12 data man 1")

#load the packages
library(readxl)
library(dplyr)
library(psych)
library(ggplot2)

#import the tables of the database
customers <- read_excel("customer.xlsx")
health_policies <- read_excel("health_policies.xlsx")
motor_policies <- read_excel("motor_policies.xlsx")
travel_policies <- read_excel("travel_policies.xlsx")

#summarise the tables
summary(customers)
summary(health_policies)
summary(motor_policies)
summary(travel_policies)

###########
#JOIN THE TABLES
###########


#change the names of the keys
#in customers table the first letter of the primaries keys of the other 3 tables were in capital letter
#i fix this problem because else, i didnt join the tables toghether
colnames(health_policies)[which(names(health_policies) == "healthID")] <- "HealthID"
colnames(motor_policies)[which(names(motor_policies) == "motorID")] <- "MotorID"
colnames(travel_policies)[which(names(travel_policies) == "travelID")] <- "TravelID"


#i made left join to create the analytics base table
JOIN <- left_join(left_join(left_join(customers, travel_policies, by = 'TravelID'),
                   motor_policies, by = 'MotorID'), 
        health_policies, by = 'HealthID')

#keep the variable i need
#create the final analytics base table
ABT <- select(JOIN, CreditCardType, Occupation, Gender, Age, Location, PrefChannel, HealthType, HealthDependentsAdults, DependentsKids,
              veh_value, MotorType, clm, exposure, veh_body, veh_age, TravelType)
head(ABT)

######################
#DATA QUALITY
#DATA UNDERSTANDING
######################
summary(ABT$Age)
summary(ABT$Gender)
describeBy(ABT$Age, ABT$Gender)

summary(ABT$Location)

table(ABT$DependentsKids)
table(ABT$HealthDependentsAdults)

summary(ABT$PrefChannel)
describeBy(ABT$Age, ABT$PrefChannel)

summary(ABT$veh_age)
summary(ABT$veh_value)
describeBy(ABT$veh_value, ABT$veh_age)

summary(ABT$exposure)
describeBy(ABT$exposure, ABT$veh_body)

#build some visualisations to find the outliers
#bar chart to check the age for outliers
ggplot(ABT, aes(Age)) +
  geom_bar(colour="black", mapping = aes(fill = Gender))

#test with scatterplot and mean
bchart <- ggplot(ABT, aes(PrefChannel, Age))
bchart + geom_boxplot() + labs(x = "Preferred Channel", y = "Age")

#barchart to find errors in veh_value
ggplot(ABT, aes(veh_value)) +
  geom_bar(colour="black", mapping = aes(fill = clm)) +
  labs(title = "Vehicle value per claim", x = "vehicle value", y = "count")

########################
#FIX DATA ISSUES
########################

#fix data issues
ABT$Age[ABT$Age>90] <- NA
ABT$Age[ABT$Age<18] <- NA
ABT$DependentsKids[ABT$DependentsKids>3] <- NA
ABT$veh_value[ABT$veh_value == 0.00] <- NA

ABT$PrefChannel[ABT$PrefChannel =="E"] <- "Email"
ABT$PrefChannel[ABT$PrefChannel =="P"] <- "Phone"
ABT$PrefChannel[ABT$PrefChannel =="S"] <- "SMS"
ABT$Gender[ABT$Gender == "m"] <- "male"
ABT$Gender[ABT$Gender == "f"] <- "female"

summary(ABT)

ABT$CreditCardType <- as.factor(ABT$CreditCardType)
ABT$Gender <- as.factor(ABT$Gender)
ABT$Location <- as.factor(ABT$Location)
ABT$PrefChannel <- as.factor(ABT$PrefChannel)
ABT$HealthType <- as.factor(ABT$HealthType)
ABT$MotorType <- as.factor(ABT$MotorType)
ABT$veh_body <- as.factor(ABT$veh_body)
ABT$veh_age <- as.factor(ABT$veh_age)
ABT$TravelType <- as.factor(ABT$TravelType)
ABT$clm <- as.factor(ABT$clm)
ABT$DependentsKids <- as.factor(ABT$DependentsKids)
ABT$HealthDependentsAdults <- as.factor(ABT$HealthDependentsAdults)

table(ABT)

#sort the data by age
arrange(ABT, -Age)
#filter the dataset
older.ABT <- filter(ABT, Age > 50)
