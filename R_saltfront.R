# Import data files, print and summarize
salt <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/hudson_saltfront.csv", header=TRUE, sep=",")
summary(salt)

# Ensure all variables are being read correctly
salt$year <- as.factor(salt$year)
salt$month <- as.factor(salt$month)
salt$day <- as.factor(salt$day)
salt$wint.year <- as.factor(salt$wint.year)
salt$week <- as.factor(salt$week)
salt$site <- as.factor(salt$site)
summary(salt)
sapply(salt, mean)

# Create new year.week variable
salt$id <- paste(salt$year,salt$week, sep="_")

# Get group means by using aggregate, then rename variables
weekmeans <- aggregate(salt[, 7], list(salt$id), mean)
names(weekmeans)[names(weekmeans) == 'Group.1'] <- 'id'
names(weekmeans)[names(weekmeans) == 'x'] <- 'salt.front'
summary(weekmeans)

# Merge two datasets by id
combined <- merge(weekmeans,long_stocks,by="id")

# Create new variable using if statement and another binary variable
combined$salt.diff <- (combined$salt.front - combined$max)
combined$salty <- ifelse(combined$salt.diff >= 0, 1, 0)

# Sum standing stocks by year, week, and by salty
aggregate(combined[, 7], list(combined$year, combined$week, combined$salty), sum)

# Make new data frame from sums and rename variables
sums <- aggregate(combined[, 7], list(combined$year, combined$week, combined$salty), sum)
names(sums)[names(sums) == 'Group.1'] <- 'year'
names(sums)[names(sums) == 'Group.2'] <- 'week'
names(sums)[names(sums) == 'Group.3'] <- 'salt'
names(sums)[names(sums) == 'x'] <- 'stock'
aggregate(sums[,4], list(sums$year, sums$salt), mean)

# Alternatively, use melt and cast functions to get means
melt(salt, na.rm = FALSE, value.name = "value")
weekmeans <- dcast(salt, week ~ year, mean, value = salt.front)

# Use clipboard to check dataset when needed
write.table(combined, "clipboard-5000", sep=",", row.names=TRUE)

