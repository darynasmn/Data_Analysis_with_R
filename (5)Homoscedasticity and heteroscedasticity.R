################################################################################
#------------------------------------------------------------------------------#
#-------------------- Homoscedasticity and heteroscedasticity------------------#
#------------------------------------------------------------------------------#
################################################################################
#library, which are needed for the laboratory
library(dplyr)# data manipulation
library(tidyverse)
library(gridExtra)
library(MASS)
library(AER)

str(RE_train)
data(RE_train)
# correlation of y from x
RE_train <- RE_train %>% mutate_if(is.factor, as.numeric)
round(cor(RE_train), 2)
corrplot::corrplot(cor(RE_train, use="pairwise.complete.obs"), addCoef.col = "grey")

RE_train_1 <- data.frame(RE_train[1:246,])

round(cor(RE_train_1), 2)
corrplot::corrplot(cor(RE_train_1), addCoef.col = "grey")

attach(RE_train)

#-------------------------------------------------------------------------------
#------------------------------- Task 1 ------------------------------------

##--------------------------------------m1--------------------------------------

y1 <- Price
x1 <- AreaNet


m1 <- lm(y1 ~ x1, data = RE_train)
summ1<-summary(m1)
summary(m1)

plot(x1, y1, xlab="x", ylab="y")
abline(m1, col=2)
summ1$r.squared #!
sum(summ1$residuals^2)
var(x1)
var(y1)
plot(summ1$residuals)
mean(summ1$residuals)
var(summ1$residuals)
hist(summ1$residuals)

plot(x1, y1, xlab = "x", ylab = "y", xlim = c(-1000,1000), ylim = c(-2500000, 2500000))
hist(x1)
hist(y1)


##--------------------------------------m2--------------------------------------

y2 <- Price
x2 <- Bedrooms

m2 <- lm(y2 ~ x2, data = RE_train)
summ2<-summary(m2)
summary(m2)

plot(x2, y2, xlab="x", ylab="y")
abline(m2, col=2)
summ2$r.squared #!
sum(summ2$residuals^2)
var(x2)
var(y2)
plot(summ2$residuals)
mean(summ2$residuals)
var(summ2$residuals)
hist(summ2$residuals)

plot(x2, y2, xlab = "x", ylab = "y", xlim = c(-15, 15), ylim = c(-2000000, 2000000))
hist(x2)
hist(y2)


##--------------------------------------m3--------------------------------------

y3 <- Price
x3 <- Bathrooms


m3 <- lm(y3 ~ x3, data = RE_train)

plot(x3, y3, xlab = "x", ylab = "y")
abline(coef = m3$coefficients, col=2)

hist(x3)
hist(y3)

summ3<-summary(m3)
summ3$r.squared
sum(summ3$residuals^2)
plot(summ3$residuals) #!
var(x3)
var(y3)
mean(summ3$residuals)
var(summ3$residuals)
hist(summ3$residuals)

plot(x3, y3, xlab = "x", ylab = "y", xlim = c(-15, 15), ylim = c(-2000000, 2000000))

##--------------------------------------m4--------------------------------------

y4 <- Price
x4 <- PropertySubType

m4 <- lm(y4 ~ x4, data = RE_train)

plot(x4, y4, xlab = "x", ylab = "y")
abline(coef = m4$coefficients, col=2)

summ4<-summary(m4)
summ4$r.squared
sum(summ4$residuals^2)
plot(summ4$residuals)
mean(summ4$residuals)
var(summ4$residuals)
hist(summ4$residuals)

var(x4)
var(y4)
plot(x4, y4, xlab = "x", ylab = "y", xlim = c(-10, 10), ylim = c(-2000000, 2000000))

##--------------------------------------m5--------------------------------------


y5 <- Price
x5 <- Parish

m5 <- lm(y5 ~ x5, data = RE_train)

plot(x5, y5, xlab = "x", ylab = "y")
abline(coef = m5$coefficients, col=2)

summ5<-summary(m5)
summ5$r.squared
sum(summ5$residuals^2)
plot(summ5$residuals)
mean(summ5$residuals)
var(summ5$residuals)
hist(summ5$residuals)

var(x5)
var(y5)
plot(x5, y5, xlab = "x", ylab = "y", xlim = c(0, 30), ylim = c(-2000000, 2000000))


#------------------------------ Task 2 -------------------------------------

#(A2)
library(AER)
library(stargazer)

mod <- lm(Price ~ AreaNet, data = RE_train) 
mult.mod <- lm(Price ~ AreaNet + Parish + PropertySubType + Bathrooms + Bedrooms, data = RE_train)
mod
mult.mod


#(B2)

summary(mod)
summary(mult.mod)

sqrt(sum(mult.mod$residuals^2) / mult.mod$df.residual)

# compare two models
car::compareCoefs(mod, mult.mod)

#(C2)


mult.mod1 <- lm(Price ~ AreaNet + PropertySubType + Bathrooms + Bedrooms, data = RE_train)
mod
mult.mod1
summary(mult.mod1)


#mult.mod2 <- lm(Price ~ AreaNet + Bathrooms + Bedrooms, data = RE_train)
#mod
#mult.mod2
#summary(mult.mod2)

#(D2)

summary(mult.mod)
summary(mult.mod1)


#(E2)

alpha <- 0.05
n <- length(RE_train$AreaNet)
p <- 4
t_k <- qt(p = 1 - alpha / 2, df = n - p - 1)
t_k
#[1] 1.965669



#-------------------------------------------------------------------------------

#(G2)
summary(mult.mod)

#-----------H_0 for AreaNet---------------------------

# Build a standard normal based on [-7,7]
t <- seq(-7, 7, 0.01)

par(mfrow = c(1, 1))

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = "2", 
     yaxes = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=6.27"), 
     cex.lab = 0.7,
     cex.main = 1)

# -------------- tact for STR ------
tact <- coeftest(mult.mod)[2, 3]

axis(1, at = c(0, -t_k, t_k, -tact, tact, -7, 7), cex.axis = 0.7)

# zatten critical areas with the help of polygon():

# critical area in the left tail
polygon(x = c(-7, seq(-7, -t_k, 0.01), -t_k),
        y = c(0, dnorm(seq(-7, -t_k, 0.01)), 0), 
        col = 'orange')

# critical area in the right tail

polygon(x = c(t_k, seq(t_k, 7, 0.01), 7),
        y = c(0, dnorm(seq(t_k, 7, 0.01)), 0), 
        col = 'orange')

# Add arrows and text that indicate critical areas and p-values
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, tact, 0, length = 0.1)
arrows(5, 0.16, -tact, 0, length = 0.1)

text(-3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)

text(-5, 0.18, 
     labels = expression(paste("-|",t[act],"|"), 
     cex = 0.7))
text(5, 0.18, 
     labels = expression(paste("|",t[act],"|"), 
     cex = 0.7))


#-------------------------------------------------------------------------------
#-----------H_0 for Parish---------------------------

#Build the standard normal based on [-7,7]
t <- seq(-7, 7, 0.01)

par(mfrow = c(1, 1))

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = "2", 
     yaxes = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=6.27"), 
     cex.lab = 0.7,
     cex.main = 1)

# -------------- tact for STR ------
tact <- coeftest(mult.mod)[3, 3]

axis(1, at = c(0, -t_k, t_k, -tact, tact, -7, 7), cex.axis = 0.7)

# zatten critical areas with the help of polygon():

# critical area in the left tail
polygon(x = c(-7, seq(-7, -t_k, 0.01), -t_k),
        y = c(0, dnorm(seq(-7, -t_k, 0.01)), 0), 
        col = 'orange')

# critical area in the right tail

polygon(x = c(t_k, seq(t_k, 7, 0.01), 7),
        y = c(0, dnorm(seq(t_k, 7, 0.01)), 0), 
        col = 'orange')

# Add arrows and text that indicate critical areas and p-values
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, tact, 0, length = 0.1)
arrows(5, 0.16, -tact, 0, length = 0.1)

text(-3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)

text(-5, 0.18, 
     labels = expression(paste("-|",t[act],"|"), 
                         cex = 0.7))
text(5, 0.18, 
     labels = expression(paste("|",t[act],"|"), 
                              cex = 0.7))
          
# Add the check marks which indicate critical values at 0.05, t^act and -t^act 
rug(c(-t_k, t_k), ticksize = 0.8, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")
          
text(-5, 0.3, 
      labels = expression(paste(H[1])), 
               cex = 2)
text(5, 0.3, 
      labels = expression(paste(H[1])),
               cex = 2)
          
text(0, 0.3, 
      labels = expression(paste(H[0])),
               cex = 2)
          
          
# Add the check marks which indicate critical values at 0.05, t^act and -t^act 
rug(c(-t_k, t_k), ticksize = 0.8, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")
          
text(-5, 0.3, 
       labels = expression(paste(H[1])),
               cex = 2)
text(5, 0.3, 
        labels = expression(paste(H[1])),
               cex = 2)
          
text(0, 0.3, 
        labels = expression(paste(H[0])), 
               cex = 2)

#-------------------------------------------------------------------------------

#-----------H_0 for PropertySubType---------------------------

# Build a standard normal based on [-7,7]
t <- seq(-7, 7, 0.01)

par(mfrow = c(1, 1))

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = "2", 
     yaxes = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=6.27"), 
     cex.lab = 0.7,
     cex.main = 1)

# -------------- tact for STR ------
tact <- coeftest(mult.mod)[4, 3]

axis(1, at = c(0, -t_k, t_k, -tact, tact, -7, 7), cex.axis = 0.7)

# zatten critical areas with the help of polygon():

# critical area in the left tail
polygon(x = c(-7, seq(-7, -t_k, 0.01), -t_k),
        y = c(0, dnorm(seq(-7, -t_k, 0.01)), 0), 
        col = 'orange')

# critical area in the right tail

polygon(x = c(t_k, seq(t_k, 7, 0.01), 7),
        y = c(0, dnorm(seq(t_k, 7, 0.01)), 0), 
        col = 'orange')

# Add arrows and text that indicate critical areas and p-values
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, tact, 0, length = 0.1)
arrows(5, 0.16, -tact, 0, length = 0.1)

text(-3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)

text(-5, 0.18, 
     labels = expression(paste("-|",t[act],"|"), 
     cex = 0.7))
text(5, 0.18, 
     labels = expression(paste("|",t[act],"|"), 
     cex = 0.7))

# Add the check marks which indicate critical values at 0.05, t^act and -t^act 
rug(c(-t_k, t_k), ticksize = 0.8, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")

text(-5, 0.3, 
     labels = expression(paste(H[1])), 
     cex = 2)
text(5, 0.3, 
     labels = expression(paste(H[1])), 
     cex = 2)

text(0, 0.3, 
     labels = expression(paste(H[0])), 
     cex = 2)



#-------------------------------------------------------------------------------

summary(mult.mod)
#-----------H_0 for Bathrooms---------------------------

# Build the standard normal based on [-7,7]
t <- seq(-7, 7, 0.01)

par(mfrow = c(1, 1))

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = "2", 
     yaxes = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=6.27"), 
     cex.lab = 0.7,
     cex.main = 1)

# -------------- tact for Bathrooms ------
tact <- coeftest(mult.mod)[5, 3]

axis(1, at = c(0, -t_k, t_k, -tact, tact, -7, 7), cex.axis = 0.7)

# zatten critical areas with the help of polygon():

# critical area in the left tail
polygon(x = c(-7, seq(-7, -t_k, 0.01), -t_k),
        y = c(0, dnorm(seq(-7, -t_k, 0.01)), 0), 
        col = 'orange')

# critical area in the right tail

polygon(x = c(t_k, seq(t_k, 7, 0.01), 7),
        y = c(0, dnorm(seq(t_k, 7, 0.01)), 0), 
        col = 'orange')

# Add arrows and text that indicate critical areas and p-values
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, tact, 0, length = 0.1)
arrows(5, 0.16, -tact, 0, length = 0.1)

text(-3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)

text(-5, 0.18, 
     labels = expression(paste("-|",t[act],"|"), 
                         cex = 0.7))
text(5, 0.18, 
      labels = expression(paste("|",t[act],"|"), 
                              cex = 0.7))
          
# Add the check marks which indicate critical values at 0.05, t^act and -t^act 
rug(c(-t_k, t_k), ticksize = 0.8, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")
          
text(-5, 0.3, 
      labels = expression(paste(H[1])), 
               cex = 2)
text(5, 0.3, 
      labels = expression(paste(H[1])), 
               cex = 2)
          
text(0, 0.3, 
      labels = expression(paste(H[0])), 
               cex = 2)
          

#-------------------------------------------------------------------------------

#-----------H_0 for Bedrooms---------------------------

# Construct a standard normal based on [-7,7]
t <- seq(-7, 7, 0.01)

par(mfrow = c(1, 1))

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     yaxs = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "=6.27"), 
     cex.lab = 0.7,
     cex.main = 1)

# -------------- tact for STR ------
tact <- coeftest(mult.mod)[6, 3]

axis(1, at = c(0, -t_k, t_k, -tact, tact, -7, 7), cex.axis = 0.7)

# Shade critical areas with polygon():

# critical area in the left tail
polygon(x = c(-7, seq(-7, -t_k, 0.01), -t_k),
        y = c(0, dnorm(seq(-7, -t_k, 0.01)), 0), 
        col = 'orange')

# critical area in the right tail

polygon(x = c(t_k, seq(t_k, 7, 0.01), 7),
        y = c(0, dnorm(seq(t_k, 7, 0.01)), 0), 
        col = 'orange')

# Add arrows and text to indicate critical areas and p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, tact, 0, length = 0.1)
arrows(5, 0.16, -tact, 0, length = 0.1)

text(-3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)
text(3.5, 0.22, 
     labels = expression("0.025"~"="~over(alpha, 2)),
     cex = 0.7)

text(-5, 0.18, 
     labels = expression(paste("-|",t[act],"|")), 
     cex = 0.7)
text(5, 0.18, 
     labels = expression(paste("|",t[act],"|")), 
     cex = 0.7)

# Add check marks indicating critical values at 0.05, t^act and -t^act 
rug(c(-t_k, t_k), ticksize = 0.8, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")

text(-5, 0.3, 
     labels = expression(paste(H[1])), 
     cex = 2)
text(5, 0.3, 
     labels = expression(paste(H[1])), 
     cex = 2)

text(0, 0.3, 
     labels = expression(paste(H[0])), 
     cex = 2)

#-------------------------------------------------------------------------------

#(H2)


#confidence intervals for coefficients with 90% reliability level
confint(mult.mod)

#confidence intervals for coefficients with 95% reliability level
confint(mult.mod, level = 0.9)

#confidence intervals for coefficients with a reliability level of 99%
confint(mult.mod, level = 0.99)


#(I2)

attach(RE_train_Cen)
RE_train_Cen <- data.frame(RE_train)
RE_train_Cen$AreaNet <- NULL
RE_train_Cen$Bedrooms <- NULL
RE_train_Cen$Bathrooms <- NULL
RE_train_Cen$PropertySubType <- NULL

RE_train_Cen <- data.frame(scale(RE_train_Cen, center = TRUE, scale = FALSE))

# Regression with centered response and predictors
mod_1 <- lm(Price ~ AreaNet + Bedrooms + Bathrooms + PropertySubType, data = RE_train_Cen)

# summary
summary(mod_1)
summary(mult.mod)


# scale costs to thousands of dollars
RE_train$Price <- RE_train$Price/1000

# evaluate the model
model2 <- lm(Price ~ AreaNet + PropertySubType + Parish + Bedrooms + Bathrooms, data = RE_train)
coeftest(mult.mod, vcov. = vcovHC, type = "HC1")

RE_train$Price <- RE_train$Price*1000
summary(mult.mod)

car::compareCoefs(model2, mult.mod)


#------------------------------ Assignment 3 -------------------------------------

#(A3)


# define components
n <- nrow(RE_train) # number of observations (rows)
k <- 2 # number of regressors

#> 
y_mean <- mean(RE_train$Price) # mean for average test-prices

SSR <- sum((fitted(mult.mod) - RE_train$Price)^2) # sum of squares
SST <- sum((RE_train$Price - y_mean )^2) # total sum of squares
SSE <- sum((fitted(mult.mod) - y_mean)^2) # the total sum of squares


# calculates the statistics

SER <- sqrt(1/(n-k-1) * SSR) # standard error of the regression
Rsq <- 1 - (SSR / SST) # R^2
SSE/SST
adj_Rsq <- 1 - (n-1)/(n-k-1) * SSR/SST # adj. R^2

cof_F <- (SSE/k)/(SSR/(n-k-1)) # (Rsq/k)/((1-Rsq)/(n-k-1))

# druk statistic to console
c("SER" = SER, "R2" = Rsq, "Adj.R2" = adj_Rsq)
#> SER R2 Adj.R2 
#> 14.4644831 0.4264315 0.4236805

summary(mult.mod)


#-------------------------------------------------------------------------------

#(B3)

summary(mult.mod)$fstatistic
summary(mult.mod)


#(C3)

spec1 <- lm(Price ~ AreaNet + Bathrooms, data = RE_train)
spec2 <- lm(Price ~ AreaNet + Bathrooms + Bedrooms, data = RE_train)
spec3 <- lm(Price ~ AreaNet + Bathrooms +Bedrooms +PropertySubType , data = RE_train)
spec4 <- lm(Price ~ AreaNet + Bathrooms +Bedrooms+PropertySubType + Parish, data = RE_train)
summary(spec1)$adj
summary(spec2)$adj
summary(spec3)$adj
summary(spec4)$adj
