# List relevant packages, so their commands are recognized
library("dplyr")
library("ggplot2")
library("reshape2")
library("corrgram")

# Import data, print and summarize
agelength <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/utilities_agelengthkey.csv", header=TRUE, sep=",")
summary(agelength)

# Add new id variable, as year_size class
agelength$id <- paste (agelength$year,agelength$size.class, sep="_")
names(agelength)[names(agelength) == 'number.aged'] <- 'frequency'

# Use reshape to get data into longitudinal format
agelength_long <- reshape(agelength, direction = "long", 
                  idvar = "id", timevar = "age", 
                  varying = 5:13)

# Remove the redundant age variable and rename size class as age
agelength_long$age <- NULL
names(agelength_long)[names(agelength_long) == 'size.class'] <- 'age'
names(agelength_long)[names(agelength_long) == 'number.aged'] <- 'frequency'
summary(agelength_long)

# Create new id that includes age
agelength_long$id2 <- paste (agelength_long$id,agelength_long$age, sep="_")
agelength_long$id <- NULL

# Write agelength_long as a table
write.table(agelength_long, "clipboard-5000", sep=",", row.names=TRUE)

# Import data, print and summarize
agelength_long <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/utilities_agelengthkey_long.csv", header=TRUE, sep=",")
summary(agelength_long)

# Make sure length is numeric
agelength_long$size.class <- as.numeric(agelength_long$size.class)
summary(agelength_long)

# Add new column that is the product of each length and frequency
agelength_long$product <- frequency*size.class

