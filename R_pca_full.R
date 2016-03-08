# Import data files, print and summarize
full <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/yoy_pca_full.csv", header=TRUE, sep=",")
summary(full)
sapply(full, mean)

# Center and scale each variable (x-mean/SD) then check new variables
full[,2:30] <- scale(full[,2:30], center = T, scale = T)
full[,2:30] <- round(full[,2:30], digits = 4)
summary(full)
sapply(full, mean)

# Plot histograms of all variables to check for skewness
ggplot(full, aes(nao.sum)) + geom_histogram(fill=NA, color="black") + theme_bw()

# Make casement plot
install.packages('corrgram')
library(corrgram)
corrgram(full[2:12], upper.panel = panel.pts, diag.panel = panel.density, main ="Biotic Variables")
corrgram(full[13:24], upper.panel = panel.pts, diag.panel = panel.density, main ="Environmental Variables")
corrgram(full[25:30], upper.panel = panel.pts, diag.panel = panel.density, main ="Climate Variables")

# Run PCA with new dataset
full_pca <- prcomp(full[,13:30])
print(full_pca)

# Summarize, make skree plot
summary (full_pca)
plot(full_pca$sdev)

# Loadings (by year)
head(full_pca$rotation)
loadings <- data.frame(full_pca$rotation)

# Scores (by variable)
head(full_pca$s)
scores <- data.frame(full_pca$x)

# Use clipboard to export scores and loadings
write.table(scores, "clipboard-5000", sep=",", row.names=TRUE)
write.table(loadings, "clipboard-5000", sep=",", row.names=TRUE)

# Make biplot of PC1 and PC2 loadings, adjusting x-axis
melted <- cbind(melt(full_pca$rotation[,1:3]))
barplot <- ggplot(data=melted) +
  geom_bar(aes(x=Var1, y=value), stat="identity") +
  facet_wrap(~Var2) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
plot(barplot)

