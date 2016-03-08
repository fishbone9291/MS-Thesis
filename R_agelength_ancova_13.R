# Work directory set by the connection to Github, must post more detailed
# location information when importing data below

# List relevant packages, so their commands are recognized
library("dplyr")
library("ggplot2")
library("lubridate")
library("reshape2")

# Import data files, print and summarize
all_sizes <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/yoy_agelength_13.csv", header=TRUE, sep=",")
summary(all_sizes)

# Run ANCOVA using aov function
model <- aov(length~age*group, data=all_sizes)
summary(model)