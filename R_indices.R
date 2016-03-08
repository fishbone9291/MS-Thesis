# No work directory set

# List relevant packages, so their commands are recognized
library("dplyr")
library("ggplot2")
library("reshape2")
library("ggvis")
library("corrgram")

# Import data files, print and summarize
indices <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/wp_indices.csv", header=TRUE, sep=",")
summary(indices)
sapply(indices, mean)

# Ensure variables are being read correctly
indices$year <- as.factor(indices$year)
summary(indices)

# Center and scale each variable (x-mean/SD) then check new variables
indices[,2:15] <- scale(indices[,2:15], center = T, scale = T)
indices[,2:15] <- round(indices[,2:15], digits = 4)
summary(indices)

# Make casement plot
corrgram(indices[8:15], upper.panel = panel.pts, diag.panel = panel.density, main ="WP Indices")

# Run PCA with new dataset
index_pca <- prcomp(indices[,8:15])
print(index_pca)

# Summarize, make skree plot
summary (index_pca)
plot(index_pca$sdev)

# Loadings (by year)
head(index_pca$rotation)
loadings <- data.frame(index_pca$rotation)

# Scores (by variable)
head(index_pca$s)
scores <- data.frame(index_pca$x)

# Use clipboard to export scores and loadings
write.table(scores, "clipboard-5000", sep=",", row.names=TRUE)
write.table(loadings, "clipboard-5000", sep=",", row.names=TRUE)

# Make biplot of PC1 and PC2 loadings, adjusting x-axis
melted <- cbind(melt(index_pca$rotation[,1:3]))
barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value), stat="identity") +
  facet_wrap(~Var2) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(barplot)
