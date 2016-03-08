# Import data
long_stocks <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/yoy_standingstocks_long.csv", header=TRUE, sep=",")
summary(long_stocks)

# Make sure all variables are being read correctly
long_stocks$year <- as.factor(long_stocks$year)
long_stocks$week <- as.factor(long_stocks$week)
long_stocks$X <- as.factor(long_stocks$X)
long_stocks$id <- as.factor(long_stocks$id)
summary(long_stocks)
sapply(long_stocks, mean)

# Use clipboard to check dataset when needed
write.table(long_stocks, "clipboard-250", sep=",", row.names=TRUE)
