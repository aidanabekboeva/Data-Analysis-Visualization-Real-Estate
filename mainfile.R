#Homework 1
#-----------------------------------Problem 1----------------------------------

install.packages("tidyverse")
install.packages("ggplot2")
install.packages("car")
library(tidyverse)
library(ggplot2)
library(car)
library(openxlsx)
library(readxl)
library(dplyr)
library(stringr)

options(scipen=999)

bronx <- read.xlsx("C:\\Users\\Aidana Bekboeva\\Documents\\HW1_F22\\main.xlsx", 1, startRow = 5)
brooklyn <- read.xlsx("C:\\Users\\Aidana Bekboeva\\Documents\\HW1_F22\\main.xlsx", 2, startRow = 5)
manhattan <- read.xlsx("C:\\Users\\Aidana Bekboeva\\Documents\\HW1_F22\\main.xlsx", 3, startRow = 5)
queens <- read.xlsx("C:\\Users\\Aidana Bekboeva\\Documents\\HW1_F22\\main.xlsx", 4, startRow = 5)
statenisland <- read.xlsx("C:\\Users\\Aidana Bekboeva\\Documents\\HW1_F22\\main.xlsx", 5, startRow = 5)

#Merging the spreadsheets
df <- rbind(bronx, brooklyn, manhattan, queens, statenisland)

#Modifying data types
glimpse(df)
df$SALE.DATE <- openxlsx::convertToDateTime(df$SALE.DATE)

#Investigating empty columns and dropping them
df$`EASE-MENT`
df = subset(df, select = -c(`EASE-MENT`) )
glimpse(df)

#Transferring important, yet rare information in the column Apartment Number

df$APART.MENT.NUMBER
df$ADDRESS <- paste(df$ADDRESS, df$APART.MENT.NUMBER)
df$ADDRESS
df = subset(df, select = -c(APART.MENT.NUMBER) )

#Adjusting not descriptive titles
df$BOROUGH <- gsub("1", "1. MANHATTAN", df$BOROUGH)
df$BOROUGH <- gsub("2", "2. BRONX", df$BOROUGH)
df$BOROUGH <- gsub("3", "3. BROOKLYN", df$BOROUGH)
df$BOROUGH <- gsub("4", "4. QUEENS", df$BOROUGH)
df$BOROUGH <- gsub("5", "5. STATEN ISLAND", df$BOROUGH)
df$BOROUGH

#Formatting missing values where 0 is inexplicable 
df$YEAR.BUILT[df$YEAR.BUILT == 0] <- NA 
df$LAND.SQUARE.FEET[df$LAND.SQUARE.FEET == 0] <- NA
df$GROSS.SQUARE.FEET[df$GROSS.SQUARE.FEET == 0] <- NA
df$SALE.PRICE[df$SALE.PRICE == 0] <- NA
df$ZIP.CODE[df$ZIP.CODE == 0] <- NA

#Removing the invalid entries
df <- df[-c(which(df$SALE.PRICE < 10000)), ]
df <- df[-c(which(df$LAND.SQUARE.FEET < 70)), ]
df <- df[-c(which(df$GROSS.SQUARE.FEET < 70)), ]
df <- df[-c(which(df$TOTAL.UNITS == 0)), ]

#Detecting outliers
summary(df)
df <- df[-c(which(df$RESIDENTIAL.UNITS > 1500)), ]

hist(df$RESIDENTIAL.UNITS, xlab = "# of Residential Units", main = "Frequency of # of Residential Units")
hist(df$COMMERCIAL.UNITS, xlab = "# of Commerical Units", main = "Frequency of # of Commercial Units")
hist(df$TOTAL.UNITS, xlab = "# of Total Units", main = "Frequency of # of Total Units")
hist(df$LAND.SQUARE.FEET, xlab = "Land Sq. Feet", main = "Histogram on Land Sq. Feet")
hist(df$GROSS.SQUARE.FEET, xlab = "Gross Sq. Feet", main = "Histogram on Gross Sq. Feet")
hist(df$YEAR.BUILT, xlab = "Year built", main = "Number of buildings built each year") #the only descriptive histogram
hist(df$SALE.PRICE, xlab = "Sale Price", main = "Frequency of sale prices")

boxplot(df$RESIDENTIAL.UNITS, xlab = "# of Residential Units", main = "Boxplot. Residential Units")
boxplot(df$COMMERCIAL.UNITS, xlab = "# of Commerical Units", main = "Boxplot. Commercial Units")
boxplot(df$TOTAL.UNITS, xlab = "# of Total Units", main = "Boxplot. Total Units")
boxplot(df$LAND.SQUARE.FEET, xlab = "Land Sq. Feet", main = "Boxplot. on Land Sq. Feet")
boxplot(df$GROSS.SQUARE.FEET, xlab = "Gross Sq. Feet", main = "Boxplot. on Gross Sq. Feet")
boxplot(df$YEAR.BUILT, xlab = "Year built", main = "Boxplot. Buildings built each year") #the only descriptive histogram
boxplot(df$SALE.PRICE, xlab = "Sale Price", main = "Boxplot. Frequency of sale prices")

typeof(df$BUILDING.CLASS.CATEGORY)
unique(df$BUILDING.CLASS.CATEGORY)

BUILDING.CLASS.CATEGORY.PLOT <- df%>%filter(str_detect(df$BUILDING.CLASS.CATEGORY, 
                  "01  ONE FAMILY HOMES                        |
                  02  TWO FAMILY HOMES                        |
                  03  THREE FAMILY HOMES                      |
                  13  CONDOS - ELEVATOR APARTMENTS            |
                  12  CONDOS - WALKUP APARTMENTS              |
                  11A CONDO-RENTALS                           |
                  09  COOPS - WALKUP APARTMENTS               |
                  10  COOPS - ELEVATOR APARTMENTS             |
                  15  CONDOS - 2-10 UNIT RESIDENTIAL          |
                  16  CONDOS - 2-10 UNIT WITH COMMERCIAL UNIT |
                  17  CONDOPS                                 "))

class(BUILDING.CLASS.CATEGORY.PLOT)
summary(BUILDING.CLASS.CATEGORY.PLOT)

plot(BUILDING.CLASS.CATEGORY.PLOT$YEAR.BUILT, BUILDING.CLASS.CATEGORY.PLOT$SALE.PRICE, main="Price and year",
     xlab="Year built", ylab="Sale Price", pch=20)

plot(BUILDING.CLASS.CATEGORY.PLOT$YEAR.BUILT, BUILDING.CLASS.CATEGORY.PLOT$GROSS.SQUARE.FEET, main="Gross sq.feet and year",
     xlab="Year built", ylab="Gross Sq Feet", pch=20)


#----------------------------------Problem 2-------------------------------

library(readr)
nyt1 <- read.csv("C:\\Users\\Aidana Bekboeva\\Documents\\HW1_F22\\nyt1.csv")
nyt2 <- read.csv("C:\\Users\\Aidana Bekboeva\\Documents\\HW1_F22\\nyt2.csv")
nyt3 <- read.csv("C:\\Users\\Aidana Bekboeva\\Documents\\HW1_F22\\nyt3.csv")

#Assigning NA to user whose age is 0 to avoid confusion
nyt1$Age[nyt1$Age == 0] <- NA 
nyt2$Age[nyt2$Age == 0] <- NA 
nyt3$Age[nyt3$Age == 0] <- NA 

#Create a new variable, age_group, that categorizes users

nyt1$Age_Group <- cut(nyt1$Age,
                 breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 120),
                 labels = c("<20", "20-29", "30-39",
                             "40-49", "50-59", "60-69", "70+"),
                 right=FALSE)

nyt2$Age_Group <- cut(nyt2$Age,
                      breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 120),
                      labels = c("<20", "20-29", "30-39",
                                 "40-49", "50-59", "60-69", "70+"),
                      right=FALSE)

nyt3$Age_Group <- cut(nyt3$Age,
                      breaks = c(-Inf, 20, 30, 40, 50, 60, 70, 120),
                      labels = c("<20", "20-29", "30-39",
                                 "40-49", "50-59", "60-69", "70+"),
                      right=FALSE)

glimpse(nyt1)
nyt1$Click_Through_Rate <- (nyt1$Clicks/nyt1$Impressions)
summary(nyt2)
nyt2$Click_Through_Rate <- (nyt2$Clicks/nyt2$Impressions)
nyt3$Click_Through_Rate <- (nyt3$Clicks/nyt3$Impressions)

#Plotting distribution of click-through-rate

plot(nyt1$Age_Group, nyt1$Click_Through_Rate, main="Click Through Rate & Age groups. Day 1",
     xlab="Age group", ylab="Click Through Rate", pch=20)

plot(nyt2$Age_Group, nyt2$Click_Through_Rate, main="Click Through Rate & Age groups. Day 2",
     xlab="Age group", ylab="Click Through Rate", pch=20)

plot(nyt3$Age_Group, nyt3$Click_Through_Rate, main="Click Through Rate & Age groups. Day 3",
     xlab="Age group", ylab="Click Through Rate", pch=20)

#Plotting distribution of impressions

plot(nyt1$Age_Group, nyt1$Impressions, main="Impressions & Age groups. Day 1",
     xlab="Age group", ylab="Impressions", pch=20)

plot(nyt2$Age_Group, nyt2$Impressions, main="Impressions & Age groups. Day 2",
     xlab="Age group", ylab="Impressions", pch=20)

plot(nyt3$Age_Group, nyt3$Impressions, main="Impressions & Age groups. Day 3",
     xlab="Age group", ylab="Impressions", pch=20)

#Categorizing users based on their click behavior

summary(nyt1$Clicks)
unique(nyt1$Clicks)

nyt1$User_Type <- cut(nyt1$Clicks,
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("Passive", "Somewhat passive",
                                 "Moderately active", "Intensly active", "Extremely active"),
                      right=FALSE)

nyt2$User_Type <- cut(nyt2$Clicks,
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("Passive", "Somewhat passive",
                                 "Moderately active", "Intensly active", "Extremely active"),
                      right=FALSE)

nyt3$User_Type <- cut(nyt3$Clicks,
                      breaks = c(0, 1, 2, 3, 4, 5),
                      labels = c("Passive", "Somewhat passive",
                                 "Moderately active", "Intensly active", "Extremely active"),
                      right=FALSE)
  
#Exploring the data and making comparisons
#Day1
femaleusers <- subset(nyt1, Gender==0, select=Age:Age_Group)
maleusers <- subset(nyt1, Gender==1, select = Age:Age_Group)
glimpse(femaleusers)

scatterplot(Impressions ~ Age, data = femaleusers, main="Female users' Age and Impressions D1")
scatterplot(Impressions ~ Age, data = maleusers, main="Male users' Age and Impressions D1")

summary(femaleusers$Impressions)

MaxImpressions_F_D1 <- subset(femaleusers, Impressions > 13)
plot(MaxImpressions_F_D1$Age, MaxImpressions_F_D1$Impressions, main = "Female Maximum Impressions Age D1",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)

summary(maleusers$Impressions)

MaxImpressions_M_D1 <- subset(maleusers, Impressions > 13)
plot(MaxImpressions_M_D1$Age, MaxImpressions_M_D1$Impressions, main = "Male Maximum Impressions Age D1",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)

#Day 2

femaleusers2 <- subset(nyt2, Gender==0, select=Age:Age_Group)
maleusers2 <- subset(nyt2, Gender==1, select = Age:Age_Group)

scatterplot(Impressions ~ Age, data = femaleusers2, main="Female users' Age and Impressions D2")
scatterplot(Impressions ~ Age, data = maleusers2, main="Male users' Age and Impressions D2")

MaxImpressions_F_D2 <- subset(femaleusers2, Impressions > 13)
plot(MaxImpressions_F_D2$Age, MaxImpressions_F_D2$Impressions, main = "Female Maximum Impressions Age D2",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)

MaxImpressions_M_D2 <- subset(maleusers2, Impressions > 13)
plot(MaxImpressions_M_D2$Age, MaxImpressions_M_D2$Impressions, main = "Male Maximum Impressions Age D2",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)


#Day3
femaleusers3 <- subset(nyt3, Gender==0, select=Age:Age_Group)
maleusers3 <- subset(nyt3, Gender==1, select = Age:Age_Group)

scatterplot(Impressions ~ Age, data = femaleusers3, main="Female users' Age and Impressions D3")
scatterplot(Impressions ~ Age, data = maleusers3, main="Male users' Age and Impressions D3")

MaxImpressions_F_D3 <- subset(femaleusers3, Impressions > 13)
plot(MaxImpressions_F_D3$Age, MaxImpressions_F_D3$Impressions, main = "Female Maximum Impressions Age D3",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)

MaxImpressions_M_D3 <- subset(maleusers3, Impressions > 13)
plot(MaxImpressions_M_D3$Age, MaxImpressions_M_D3$Impressions, main = "Male Maximum Impressions Age D3",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)




