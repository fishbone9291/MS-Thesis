# Install packages
install.packages('corrgram')
install.packages('ggplot2')
install.packages('ggvis')
install.packages('dplyr')
install.packages('reshape2')
install.packages('AICcmodavg')
library(corrgram)
library(ggplot2)
library(ggvis)
library(dplyr)
library(reshape2)
library(AICcmodavg)

# Import data files, print and summarize
# Temperature and flow are means from weeks 19-28
SR <- read.csv("C:/Users/brian/Desktop/Masters Thesis/Data Files/stock_recruit.csv", header=TRUE, sep=",")
summary(SR)
sapply(SR, mean)

# Remove unnecessary variables
SR$X <- NULL

# Create two new variables, ln(R/S) and ln(YOY/PYSL)
SR$lnR.S <- log(SR$yoy/SR$adult)
SR$lnYOY.PYSL <- log(SR$yoy/SR$pysl)

# Run S-R model 1: DV=S
model1 <- lm(lnR.S ~ adult, data=SR)
summary(model1)
AIC(model1)
AICc(model1, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run S-R model 2: DV=S,Temp
model2 <- lm(lnR.S ~ adult + temp, data=SR)
summary(model2)
AIC(model2)
AICc(model2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run S-R model 3: DV=S,Flow
model3 <- lm(lnR.S ~ adult + flow, data=SR)
summary(model3)
AIC(model3)
AICc(model3, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run S-R model 4: DV=S,Filtration
model4 <- lm(lnR.S ~ adult + filtration, data=SR)
summary(model4)
AIC(model4)
AICc(model4, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run S-R model 5: DV=S,Temp,Flow
model5 <- lm(lnR.S ~ adult + temp + flow, data=SR)
summary(model5)
AIC(model5)
AICc(model5, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run S-R model 6: DV=S,Filtration,Flow
model6 <- lm(lnR.S ~ adult + filtration + flow, data=SR)
summary(model6)
AIC(model6)
AICc(model6, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run S-R model 7: DV=S,Temp,Flow,Filtration
model7 <- lm(lnR.S ~ adult + temp + flow + filtration, data=SR)
summary(model7)
AIC(model7)
AICc(model7, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run S-R model 8: DV=Temp,Flow,Filtration
model8 <- lm(lnR.S ~ temp + flow + filtration, data=SR)
summary(model8)
AIC(model8)
AICc(model8, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run YOY/PYSL model 1: DV=PYSL
model1.2 <- lm(lnYOY.PYSL ~ pysl, data=SR)
summary(model1.2)
AIC(model1.2)
AICc(model1.2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run YOY/PYSL model 2: DV=PYSL,Temp
model2.2 <- lm(lnYOY.PYSL ~ pysl + temp, data=SR)
summary(model2.2)
AIC(model2.2)
AICc(model2.2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run YOY/PYSL model 3: DV=PYSL,Flow
model3.2 <- lm(lnYOY.PYSL ~ pysl + flow, data=SR)
summary(model3.2)
AIC(model3.2)
AICc(model3.2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run YOY/PYSL model 4: DV=PYSL,Filtration
model4.2 <- lm(lnYOY.PYSL ~ pysl + filtration, data=SR)
summary(model4.2)
AIC(model4.2)
AICc(model4.2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run YOY/PYSL model 5: DV=PYSL,Temp,Flow
model5.2 <- lm(lnYOY.PYSL ~ pysl + temp + flow, data=SR)
summary(model5.2)
AIC(model5.2)
AICc(model5.2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run YOY/PYSL model 6: DV=PYSL,Filtration,Flow
model6.2 <- lm(lnYOY.PYSL ~ pysl + filtration + flow, data=SR)
summary(model6.2)
AIC(model6.2)
AICc(model6.2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run YOY/PYSL model 7: DV=PYSL,Temp,Flow,Filtration
model7.2 <- lm(lnYOY.PYSL ~ pysl + temp + flow + filtration, data=SR)
summary(model7.2)
AIC(model7.2)
AICc(model7.2, return.K = FALSE, second.ord = TRUE, nobs = NULL)

# Run YOY/PYSL model 8.2: DV=Temp,Flow,Filtration
model8.2 <- lm(lnYOY.PYSL ~ temp + flow + filtration, data=SR)
summary(model8.2)
AIC(model8.2)
AICc(model8.2, return.K = FALSE, second.ord = TRUE, nobs = NULL)


