# List relevant packages, so their commands are recognized
library("dplyr")
library("ggplot2")
library("reshape2")
library("nlme")
library("corrgram")
library("AICcmodavg")

# Import data, print and summarize
contingents <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/yoy_full.csv", header=TRUE, sep=",")
summary(contingents)

# Ensure all variables are being read correctly
contingents$site.02 <- as.numeric(contingents$site.02)
contingents$site.03 <- as.numeric(contingents$site.03)
contingents$site.04 <- as.numeric(contingents$site.04)
contingents$site.05 <- as.numeric(contingents$site.05)
summary(contingents)
sapply(contingents[,4:39], mean)
sapply(contingents[,4:39], median)

# Create new ID variable (with _ separator, not . separator)
contingents$id <- paste (contingents$year,contingents$week, sep="_")
contingents$id <- as.factor(contingents$id)

# Use reshape to get data into longitudinal format
contingents.long <- reshape(contingents, direction = "long", 
                  idvar = "id", timevar = "site", 
                  varying = 4:39)

# Rename site variable to signify standing stocks
names(contingents.long)[names(contingents.long) == 'site'] <- 'stock'
summary(contingents.long)

# Import salt front data, print and summarize
salt <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/hudson_saltfront.csv", header=TRUE, sep=",")
summary(salt)

# Create new ID variable (with _ separator, not . separator)
salt$id <- paste(salt$year,salt$week, sep="_")
salt$id <- as.factor(salt$id)
summary(salt)

# Get group means by using aggregate, then rename variables
weekmeans <- aggregate(salt[, 7], list(salt$id), mean)
names(weekmeans)[names(weekmeans) == 'Group.1'] <- 'id'
names(weekmeans)[names(weekmeans) == 'x'] <- 'salt.front'
summary(weekmeans)

# Merge weekmeans and contingents.long by id
combined <- merge(weekmeans,contingents.long,by="id")

# Create new variable using if statement and another binary variable
combined$salt.diff <- (combined$salt.front - combined$max)
combined$salty <- ifelse(combined$salt.diff >= 0, 1, 0)

# Sum standing stocks by year, week, and by salty
contingent.sums <- aggregate(combined[, 6], list(combined$year, combined$week, combined$salty), sum)

# Rename variables
names(contingent.sums)[names(contingent.sums) == 'Group.1'] <- 'year'
names(contingent.sums)[names(contingent.sums) == 'Group.2'] <- 'week'
names(contingent.sums)[names(contingent.sums) == 'Group.3'] <- 'salt'
names(contingent.sums)[names(contingent.sums) == 'x'] <- 'stock'
summary(contingent.sums)

# Ensure that salt is considered a factor
contingent.sums$salt <- as.factor(contingent.sums$salt)
summary(contingent.sums)

# Once again, create new ID variable (with _ separator, not . separator)
contingent.sums$id <- paste(contingent.sums$year,contingent.sums$week, sep="_")
contingent.sums$id <- as.factor(contingent.sums$id)
summary(contingent.sums)

# Import flow data, print and summarize
flow <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/hudson_flow.csv", header=TRUE, sep=",")
summary(flow)

# Import temperature data, print and summarize
temperature <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/hudson_temperature.csv", header=TRUE, sep=",")
summary(temperature)

# Convert both temperature and flow to weekly means
flow <- aggregate(flow[, 1], list(flow$year, flow$week), mean)
temperature <- aggregate(temperature[, 1], list(temperature$year, temperature$week), mean)
summary(flow)
summary(temperature)

# Rename variables for both temperature and flow
names(flow)[names(flow) == 'Group.1'] <- 'year'
names(flow)[names(flow) == 'Group.2'] <- 'week'
names(flow)[names(flow) == 'x'] <- 'flow'
summary(flow)

names(temperature)[names(temperature) == 'Group.1'] <- 'year'
names(temperature)[names(temperature) == 'Group.2'] <- 'week'
names(temperature)[names(temperature) == 'x'] <- 'temperature'
summary(temperature)

# For temp and flow, create new ID variable (with _ separator, not . separator)
flow$id <- paste(flow$year,flow$week, sep="_")
flow$id <- as.factor(flow$id)
summary(flow)

temperature$id <- paste(temperature$year,temperature$week, sep="_")
temperature$id <- as.factor(temperature$id)
summary(temperature)

# Merge temp and flow datasets
tempflow <- merge(flow, temperature, by="id")
summary(tempflow)

# Drop week and year variables
tempflow$year.x <- NULL
tempflow$week.x <- NULL
tempflow$year.y <- NULL
tempflow$week.y <- NULL
summary(tempflow)

# Merge tempflow and contingent sums by ID
fulldata <- merge(contingent.sums, tempflow, by="id")
summary(fulldata)

# Import annual metrics
annual <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/contingent_annual.csv", header=TRUE, sep=",")
summary(annual)

# Merge fulldata and annual by year
fulldata <- merge(fulldata, annual, by="year")
summary(fulldata)

# Remove flow outliers (hurricane/storm events)
fulldata <- fulldata[ which(fulldata$flow<50000),]
summary(fulldata)

# Write full dataset into a .csv file
write.csv(fulldata,"C:/Users/brian/Desktop/Masters Thesis/Data Files/contingent_mixed_data.csv")

# Use clipboard to check datasets
write.table(salt, "clipboard-500", sep=",", row.names=TRUE)

# Make a casement plot for our initial weekly and annual variables
corrgram(fulldata[1:7], upper.panel = panel.pts, diag.panel = panel.density, main ="Weekly Variables- before transformation")
corrgram(fulldata[8:20], upper.panel = panel.pts, diag.panel = panel.density, main ="Annual Variables- before transformation")

# Make histograms for stock, flow and temperature
hist(fulldata$stock, col="green", breaks=350)
hist(fulldata$flow, col="cornflowerblue")
hist(fulldata$temperature, col="red")

# Apply natural-log transformation to annual variables and re-check casement plot
fulldata[,9:10] <- log(fulldata[,9:10])
fulldata[,12:14] <- log(fulldata[,12:14])
corrgram(fulldata[8:20], upper.panel = panel.pts, diag.panel = panel.density, main ="Annual Variables- after transformation")

# Remove flow outliers and do ln(n+1) transformation on standing stocks and re-check casement plot
transformed_data <- fulldata
transformed_data$stock <- log(transformed_data$stock + 1)
transformed_data$flow <- log(transformed_data$flow)
corrgram(transformed_data[1:7], upper.panel = panel.pts, diag.panel = panel.density, main ="Weekly Variables- after transformation")
summary(transformed_data)

# Specify week and year as categorical
as.factor <- fulldata$week
as.factor <- fulldata$year
summary(fulldata)

# Use gls function on fulldata to run a kitchen sink model with no random effects
base_model <- gls(stock ~ week * year * salt * flow * temperature * temp.sum * flow.sum, 
                  data=fulldata)
summary(base_model)
plot(base_model)

# Test residuals against each variable using a casement plot
base_residuals <- residuals(base_model)
fulldata$residuals <- base_residuals
vars <- c("week","salt","year","flow","temperature","temp.sum","flow.sum","residuals")
corrgram(fulldata[vars], upper.panel = panel.pts, diag.panel = panel.density, main ="Residauls vs predictors")

# Test a kitchen sink model with VarIdent
v1 <- varIdent(form=~1|week)
v2 <- varFixed(~flow)
v3 <- varComb(v1,v2)
var1 <- lme(stock ~ week * year * flow * temperature, 
                  random= list(~1|salt), 
                  weights=v3, 
                  data=fulldata)
summary(var1)
plot(var1)

# Test a kitchen sink model with year as a random intercept factor


# Test a kitchen sink model with year as a random slope factor


# Test a kitchen sink model with week as a random intercept factor


# Test a kitchen sink model with week as a random slope factor


# Test a kitchen sink model with contingent as a random intercept factor


# Test a kitchen sink model with contingent as a random slope factor


# Compare random effect structures using AIC, aggregated with the anova function




