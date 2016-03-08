# Work directory set by the connection to Github, must post more detailed
# location information when importing data below

# List relevant packages, so their commands are recognized
library("dplyr")
library("ggplot2")
library("lubridate")
library("reshape2")

# Import data files, print and summarize
stocks <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/yoy_standingstocks.csv", header=TRUE, sep=",")
summary(stocks)

# Ensure all variables are being read correctly
stocks$year <- as.factor(stocks$year)
stocks$week <- as.factor(stocks$week)
stocks$site.02 <- as.numeric(stocks$site.02)
stocks$site.03 <- as.numeric(stocks$site.03)
stocks$site.04 <- as.numeric(stocks$site.04)
stocks$site.05 <- as.numeric(stocks$site.05)
summary(stocks)
sapply(stocks[,4:39], mean)
stocks$id <- paste (stocks$year,stocks$week, sep="_")
stocks$id <- as.factor(stocks$id)

# Use reshape to get data into longitudinal format
stocks <- reshape(stocks, direction = "long", 
        idvar = "id", timevar = "site", 
        varying = 4:39)

# Rename site and row.names variable
names(stocks)[names(stocks) == 'site'] <- 'stock'
summary(stocks)

# Write dataset into a csv file
write.csv(stocks,"C:/Users/brian/Desktop/Masters Thesis/Data Files/yoy_standingstocks_long.csv")

# Use clipboard to check dataset
write.table(stocks, "clipboard-250", sep=",", row.names=TRUE)
