# Install packages
install.packages('corrgram')
install.packages('ggplot2')
install.packages('ggvis')
install.packages('dplyr')
install.packages('reshape2')
install.packages('MASS')
install.packages('MVN')
install.packages('mvnormtest')
library(corrgram)
library(ggplot2)
library(ggvis)
library(dplyr)
library(reshape2)
library(MASS)
library(MVN)
library(mvnormtest)

# Import data files, print and summarize
trimmed <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/yoy_pca_trimmed.csv", header=TRUE, sep=",")
summary(trimmed)
sapply(trimmed, mean)

# Center and scale each variable (x-mean/SD) then check new variables
trimmed[,2:21] <- scale(trimmed[,2:21], center = T, scale = T)
trimmed[,2:21] <- round(trimmed[,2:21], digits = 4)

# Test multivariate normality assumption before transformations
initialmatrix <- as.matrix(trimmed[,2:21])
mshapiro.test(initialmatrix)

# Import data files, print and summarize
trimmed <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/yoy_pca_trimmed.csv", header=TRUE, sep=",")
summary(trimmed)
sapply(trimmed, mean)

# Look at casement plots before transformations
corrgram(trimmed[2:14], upper.panel = panel.pts, diag.panel = panel.density, main ="Biotic Variables- Before Transformation")

# Apply natural-log transformation to select variables
trimmed[,2:3] <- log(trimmed[,2:3])
trimmed[,9:11] <- log(trimmed[,9:11])
trimmed[,13:14] <- log(trimmed[,13:14])

# Center and scale each variable (x-mean/SD) then check new variables
trimmed[,2:21] <- scale(trimmed[,2:21], center = T, scale = T)
trimmed[,2:21] <- round(trimmed[,2:21], digits = 4)
summary(trimmed)
sapply(trimmed, mean)

# Make casement plot
corrgram(trimmed[2:14], upper.panel = panel.pts, diag.panel = panel.density, main ="Biotic Variables- After Transformation")
corrgram(trimmed[15:21], upper.panel = panel.pts, diag.panel = panel.density, main ="Environmental Variables")

# If necessary, remove salt.diff and brackish biotic variables
trimmed$salt.diff <- NULL
trimmed$chl.salt <- NULL
trimmed$cope.salt <- NULL
trimmed$clad.salt <- NULL

# Test multivariate normality assumption
trimmatrix <- as.matrix(trimmed[,2:17])
mshapiro.test(trimmatrix)

# If test fails, try inverting the matrix
invtrim <- t(trimmatrix)
mshapiro.test(invtrim)
mshapiro.test(invtrim[,2:17])

# Run PCA with new dataset
trimmed_pca <- prcomp(trimmed[,2:17])
print(trimmed_pca)

# Summarize, make skree plot of eigenvalues
summary (trimmed_pca)
plot(trimmed_pca$sdev^2)

# Loadings (by year)
head(trimmed_pca$rotation)
loadings <- data.frame(trimmed_pca$rotation)

# Scores (by variable)
head(trimmed_pca$s)
scores <- data.frame(trimmed_pca$x)

# Use clipboard to export scores and loadings
write.table(scores, "clipboard-5000", sep=",", row.names=TRUE)
write.table(loadings, "clipboard-5000", sep=",", row.names=TRUE)

# Make bar plot of PC1, PC2 and PC3 loadings, adjusting x-axis
melted <- cbind(melt(trimmed_pca$rotation[,1:3]))
barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value), stat="identity") +
  facet_wrap(~Var2) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(barplot)

# Add years to scores dataframe
x <- trimmed$year
scores$year <- x

# Make biplot of PC1 and PC2 loadings
arrows <- data.frame(x1=0, y1 =0, x2 = loadings$PC1, y2 = loadings$PC2)

ggplot(data=loadings, aes(x=PC1, y=PC2, label = row.names(loadings)))+
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "black", alpha = 0.8, size = 5) +
  ggtitle("PC1 vs PC2") +
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "tomato")

# Make biplot of PC1 and PC3 loadings
arrows <- data.frame(x1=0, y1 =0, x2 = loadings$PC1, y2 = loadings$PC3)

ggplot(data=loadings, aes(x=PC1, y=PC3, label = row.names(loadings)))+
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "black", alpha = 0.8, size = 5) +
  ggtitle("PC1 vs PC3") +
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "tomato")

# Make biplot of PC2 and PC3 loadings
arrows <- data.frame(x1=0, y1 =0, x2 = loadings$PC2, y2 = loadings$PC3)

ggplot(data=loadings, aes(x=PC2, y=PC3, label = row.names(loadings)))+
  geom_hline(yintercept = 0, colour = "gray65") +
  geom_vline(xintercept = 0, colour = "gray65") +
  geom_text(colour = "black", alpha = 0.8, size = 5) +
  ggtitle("PC2 vs PC3") +
  geom_segment(data = arrows, aes(x = x1, y = y1, xend = x2, yend = y2), colour = "tomato")

# Make plots regressing scores over time
plot(scores$year, scores$PC1)
cor(scores$year,scores$PC1)

plot(scores$year, scores$PC2)
cor(scores$year,scores$PC2)

plot(scores$year, scores$PC3)
cor(scores$year,scores$PC3)
