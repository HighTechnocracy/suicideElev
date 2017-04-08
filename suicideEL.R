library(tidyverse)
library(stringr)
library(extrafont) 
font_import()
loadfonts()

## Get Deaths Dataset
data1 <- read.delim("~/Desktop/Underlying Cause of Death, 1999-2015.txt")

## Get Elevation Dataset
data2 <- read.delim("~/Desktop/NationalFile_20170201.txt", sep = "|")

## Removing unnecessary rows and columns from elevations dataset
data3 <- filter(data2, FEATURE_CLASS == "Civil")
data3 <- data3[grep("County", data3$FEATURE_NAME),]
data3 <- select(data3, 5, 6, 7, 17)

## Create "County.Code" variable for merge. Change "County.Code" in 
## Deaths dataset to character to match
data3$COUNTY_NUMERIC <- str_pad(data3$COUNTY_NUMERIC, 3, pad = "0")
data3$STATE_NUMERIC <- as.character(data3$STATE_NUMERIC)
data3 <- mutate(data3, County.Code = paste(STATE_NUMERIC, COUNTY_NUMERIC, sep=""))
data1$County.Code <- as.character(data1$County.Code)

## Merge the two datasets and get rid of extraneous variables
deathEval <- right_join(data1, data3) %>%
        select(2, 3, 6, 13)

## Convert "suppressed" and "unreliable" to "NA"
deathEval$Deaths[deathEval$Deaths=="Suppressed"] <- NA
deathEval$Crude.Rate[deathEval$Crude.Rate=="Suppressed"] <- NA
deathEval$Crude.Rate[deathEval$Crude.Rate=="Unreliable"] <- NA

## Remove Missing Cases
deathEval <- na.omit(deathEval)

rm(data1, data2)

## Convert factor variables to numeric
deathEval$Deaths <- as.numeric(as.character(deathEval$Deaths))
deathEval$Population <- as.numeric(as.character(deathEval$Population))
deathEval$Crude.Rate <- as.numeric(as.character(deathEval$Crude.Rate))

## Run and plot regression
png("~//R/SuicideElev/Images/suicide_elevation.png" )
ggplot(deathEval, aes(ELEV_IN_FT, Crude.Rate, label=County)) +
  geom_point(colour = "red", size = 2, alpha = 0.4) +
  geom_smooth(method=lm) +
  geom_text(data=subset(deathEval, Crude.Rate > 40), size = 3, nudge_x = 1500) +
  ggtitle("Suicide as a Function of Elevation") +
  labs(x="Elevation (feet)",y="Crude Suicide Rate (per 100,000)") +
  theme(plot.title = element_text(family = "Trebuchet MS", color="#666666", 
                                  face="bold", size=18, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="#666666", 
                                  face="bold", size=10))
dev.off()

## Summary of Regression
x <- lm(deathEval$Crude.Rate ~ deathEval$ELEV_IN_FT)
summary(x)


