# Import salt front data, print and summarize
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

# Create new year.week variable
salt$id <- paste(salt$year,salt$week, sep="_")

# Get group means by using aggregate, then rename variables
weekmeans <- aggregate(salt[, 7], list(salt$id), mean)
names(weekmeans)[names(weekmeans) == 'Group.1'] <- 'id'
names(weekmeans)[names(weekmeans) == 'x'] <- 'salt.front'
summary(weekmeans)

# Import littoral habitat data


# Merge two datasets by id
combined <- merge(weekmeans,long_stocks,by="id")