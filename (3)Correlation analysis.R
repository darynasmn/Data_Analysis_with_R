library(dplyr)
library(tidyverse)
library(gridExtra) 


#download the updated data set
RE_train <- read.csv("lisbon.csv", header = TRUE)

RE_train <- subset(train, select = -c(Country, District, Municipality,  Longitude,Latitude, AreaGross))
# writing to the file 
write.csv(RE_train, file = "RE_train.CSV", row.names = FALSE)

str(RE_train)


#change char types to factor
RE_train$Condition <- as.factor(RE_train$Condition)
RE_train$PropertySubType <- as.factor(RE_train$PropertySubType) 
RE_train$PropertyType <- as.factor(RE_train$PropertyType)
RE_train$Parking <- as.factor(RE_train$Parking)
RE_train$Parish <- as.factor(RE_train$Parish)

RE_train$Price <- as.integer(RE_train$Price)
RE_train$Price[(RE_train$Price)>2200000] <- RE_train$Price-2000000

RE_train$Price <- as.integer(RE_train$Price)
RE_train$Price[(RE_train$Price)<0] <- RE_train$Price+2000000
str(RE_train)


glimpse(RE_train)
summary((RE_train))
#-------------------------------------------------------------------------------
#FIRST TASK
#(A1)

### Quick correlation analysis
# and graph the numeric attributes to get an idea
# about how they can relate to each other.

# convert non-numeric types to numeric ones
RE_train <- RE_train %>% mutate_if(is.factor,  as.numeric)

RE_train <- subset(RE_train, select = -c(Id))
round(cor(RE_train), 2)
corrplot::corrplot(cor(RE_train, use="pairwise.complete.obs"), addCoef.col = "grey")

RE_train_1 <- data.frame(RE_train[1:246,])

round(cor(RE_train_1), 2)
corrplot::corrplot(cor(RE_train_1), addCoef.col = "grey")

#-------------------------------------------------------------------------------
#(B1)
library(MASS)

plot(RE_train$Price ~ RE_train$AreaNet)

X <- c(RE_train$AreaNet)
Y <- c(RE_train$Price)
plot(X, Y, xlab = "Number of square kilometers", ylab = "Price")

n <- length(X)
n

# Average values
meanX <- mean(X)
meanY <- mean(Y)
meanX
meanY


# Variation Х
varX <- 0
for(i in 1:n){
  varX = varX + (X[i] - meanX)^2
}
varX = varX / n
varX

# Variation У
varY <- 0
for(i in 1:n){
  varY = varY + (Y[i] - meanY)^2
}
varY = varY / n
varY
# Covariance
covXY <- 0
for(i in 1:n){
  covXY = covXY + (X[i] - meanX)*(Y[i] - meanY)
}
covXY = covXY / n
covXY

#-------------------------------------------------------------------------------
#(C1)
# calculate sample means of AreaNet and scores
avg_Area <- mean(RE_train$AreaNet)
avg_Price <- mean(RE_train$Price)


# calculate sample standard deviations of STR and score
sd_Area <- sd(RE_train$AreaNet)
sd_Price <- sd(RE_train$Price)


quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
quant_Area <- quantile(RE_train$Area, quantiles)
quant_Price <- quantile(RE_train$Price, quantiles)

DistributionSummary <- data.frame(Average = c(avg_Area, avg_Price), 
                                  StandardDeviation = c(sd_Area, sd_Price), 
                                  quantile = rbind(quant_Area, quant_Price))

DistributionSummary

quant_Area
quant_Price

cor(RE_train$AreaNet, RE_train$Price)


library(globals)
attach(RE_train)

# Parameter b
b <- covXY / varX
b
# Parameter a
a <- meanY - b*meanX
a

# Using functions
var(X)*(n-1)/n
var(Y)*(n-1)/n
cov(X, Y)*(n-1)/n
var(X)

b1 <- (cov(X, Y)*(n-1)/n)/(var(X)*(n-1)/n)
a1 <- meanY - b1*meanX

b1
a1
#-------------------------------------------------------------------------------
#(D1)

plot(RE_train$Price ~ RE_train$AreaNet)

#The line that shows the regression
abline(a= 31056, b = 4675 )

################################################################################
# SECOND TASK
#-------------------------------------------------------------------------------
#(A2)
linear_model <- lm(Price ~ AreaNet, data = RE_train)

# output the standard output of the evaluated lm object to the console
linear_model
#-------------------------------------------------------------------------------
#(C2)

summary(RE_train$Price)

plot(RE_train$Price ~ RE_train$AreaNet, 
     main = "Scatterplot of Price and AreaNet", 
     xlab = "AreaNet (X)",
     ylab = "Price (Y)",
     xlim = c(23, 573),
     ylim = c(85000, 2200000))

# add the regression line
abline(linear_model, col = 3) 




#-------------------------------------------------------------------------------
#
#---------------------------Research for B2----------------------------------
RE_train <- data.frame(AREA = rep(RE_train$AreaNet + 1, nrow(RE_train)), RE_train[,])

library(MASS)
plot(RE_train$Price ~ RE_train$AREA)

X <- c(RE_train$AREA)
Y <- c(RE_train$Price)
plot(X, Y, xlab = "Number of square kilometers", ylab = "Price")

n <- length(X)
n

# Average values
meanX <- mean(X)
meanY <- mean(Y)
meanX
meanY



# Variation of X
varX <- 0
for(i in 1:n){
  varX = varX + (X[i] - meanX)^2
}
varX = varX / n
varX

# Variation of Y
varY <- 0
for(i in 1:n){
  varY = varY + (Y[i] - meanY)^2
}
varY = varY / n
varY
# Covariance
covXY <- 0
for(i in 1:n){
  covXY = covXY + (X[i] - meanX)*(Y[i] - meanY)
}
covXY = covXY / n
covXY

cor(RE_train$AreaNet, RE_train$Price)


library(globals)
attach(RE_train)

# Parameter b
b <- covXY / varX
b
# Parameter a
a <- meanY - b*meanX
a



# Using functions
var(X)*(n-1)/n 
var(Y)*(n-1)/n
cov(X, Y)*(n-1)/n
var(X) 

b1 <- (cov(X, Y)*(n-1)/n)/(var(X)*(n-1)/n)
a1 <- meanY - b1*meanX

b1
a1

plot(RE_train$Price ~ RE_train$AREA, 
     main = "Scatterplot of Price and AREA", 
     xlab = "AREA (X)",
     ylab = "Price (Y)",
     xlim = c(23, 573),
     ylim = c(85000, 2200000))

# add the regression line
abline(linear_model, col = 3) 

RE_train <- subset(RE_train, select = -c(AREA))


