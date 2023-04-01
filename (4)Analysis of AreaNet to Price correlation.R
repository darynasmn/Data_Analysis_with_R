################ Analysis of AreaNet to Price correlation ######################
#------------------------------------------------------------------------------#

#------------------------------- Task 1 ----------------------------------------

library(AER)
library(MASS)
library(ggplot2)
#show graph and regression line
plot(RE_train$Price ~ RE_train$AreaNet)
# Line that shows the regression
abline(a = 31056, b = 4675 , col = 3)
summary(AreaNet)
#(A1)

#Calculating the average values 
avg_Area <- mean(RE_train$AreaNet) 
avg_Price <- mean(RE_train$Price)
avg_Area
avg_Price

#(B1)

# calculation of standard values
sd_Area <- sd(RE_train$AreaNet) 
sd_Price <- sd(RE_train$Price)
sd_Area
sd_Price
#(C1)

#setting up the percentile vector and calculating quantiles
quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9)
quant_Area <- quantile(RE_train$AreaNet, quantiles)
quant_Price <- quantile(RE_train$Price, quantiles)



#(D1)

# Calculate the correlation coefficient
DistributionSummary <- data.frame(Average = c(avg_Area, avg_Price), 
                                  StandardDeviation = c(sd_Area, sd_Price), 
                                  quantile = rbind(quant_Area, quant_Price))

DistributionSummary

quant_Area
quant_Price

cor(RE_train$AreaNet, RE_train$Price)

attach(RE_train)
beta_1 <- sum(( AreaNet - mean(AreaNet)) * (Price - mean(Price))) / sum((AreaNet - mean(AreaNet))^2)
beta_1
# calculate beta_0_hat
beta_0 <- mean(Price) - beta_1 * mean(AreaNet)
beta_0


linear_model <- lm(Price ~ AreaNet, data = RE_train)
linear_model

mod_summary <- summary(linear_model)
mod_summary

#(E1)

# definition of SSR and TSS
SSR <- sum(mod_summary$residuals^2)
TSS <- sum((Price - mean(Price))^2)
SSR
TSS

#(F1)

# calculate R^2 manually
R2 <- 1 - SSR/TSS
R2

#(G1)

# calculate SER manually
n <- nrow(RE_train)
SER <- sqrt(SSR / (n-2))

# display the value on the console
SER



#--------------------------- Task 2 ----------------------------------------

#(A2 - F2)
summary(linear_model)$coefficients


# Build a standard normal based on [-6,6]
t <- seq(-30, 30, 0.01)

par(mfrow = c(1, 1))

plot(x = t, 
     y = dnorm(t, 0, 1), 
     type = "l", 
     col = "steelblue", 
     lwd = 2, 
     yaxs = "i", 
     axes = F, 
     ylab = "", 
     main = expression("Calculating the p-value of a Two-sided Test when" ~ t^act ~ "= 24.90"), 
     cex.lab = 0.7,
     cex.main = 1)

tact <- 24.90

axis(1, at = c(0, -1.96, 1.96, -tact, tact), cex.axis = 0.7)
# Shade critical areas with polygon():

# critical area in the left tail
polygon(x = c(-6, seq(-6, -1.96, 0.01), -1.96),
        y = c(0, dnorm(seq(-6, -1.96, 0.01)), 0), 
        col = 'orange')

# critical area in the right tail

polygon(x = c(1.96, seq(1.96, 6, 0.01), 6),
        y = c(0, dnorm(seq(1.96, 6, 0.01)), 0), 
        col = 'orange')

# Add arrows and text to indicate critical areas and p-value
arrows(-3.5, 0.2, -2.5, 0.02, length = 0.1)
arrows(3.5, 0.2, 2.5, 0.02, length = 0.1)

arrows(-5, 0.16, -4.75, 0, length = 0.1)
arrows(5, 0.16, 4.75, 0, length = 0.1)

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
rug(c(-1.96, 1.96), ticksize = 0.145, lwd = 2, col = "darkred")
rug(c(-tact, tact), ticksize = -0.0451, lwd = 2, col = "darkgreen")


#(G2)

# determine the residual degrees of freedom
linear_model$df.residual
#> [1] 418

2 * pt(-24.08738536, df = 244) # Pr(>|t|) for AreaNet
#> [1] 

2 * pt(-1.249561, df = 244) # Pr(>|t|) for Price


#(H2)
# calculate 95% confidence interval for coefficients in 'linear_model'
confint(linear_model)
#> 2.5 % 97.5 %
#> (Intercept)-17898.975 80011.507
#> AreaNet 4292.661 5057.245

# manually calculate 95% confidence interval for coefficients in 'linear_model'
lm_summ <- summary(linear_model)

c("lower" = lm_summ$coef[2,1] - qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2],
  "upper" = lm_summ$coef[2,1] + qt(0.975, df = lm_summ$df[2]) * lm_summ$coef[2, 2])
str(RE_train)


#--------------------------- Task 3 ----------------------------------------


#(A3)
summary(RE_train$AreaNet)
# Mean <- 109.66

# Create a dummy variable as defined 
# so, we take 110 as the average value
RE_train$D <- RE_train$AreaNet < 110


#(B3)
# Plot the data
plot(RE_train$D, RE_train$Price, # provide data for plotting
     pch = 110, # use filled circles as plot symbols
     cex = 0.5, # set the size of the plot symbols to 0.5
     col = "Steelblue", # set the color of the symbols to "Steelblue"
     xlab = expression(D[i]), # Set the name and labels of the axis
     ylab = "Price",
     main = "Dummy Regression")

# evaluate the dummy regression model
dummy_model <- lm(Price ~ D, data = RE_train)
summary(dummy_model)

#(C3)

# predictions for a specific group
points(x = RE_train$D, 
       y = predict(dummy_model), 
       col = "red", 
       pch = 110)

#(D3)

# confidence intervals for the coefficients in the dummy regression model
confint(dummy_model)


#--------------------------- Task 4 ----------------------------------------

#(A4)

#show the graph and regression line
plot(RE_train$Price ~ RE_train$AreaNet)
#The line that shows the regression
abline(a = 31056, b = 4675 , col = 3)


#(B4 - C4)
attach(RE_train)
labor_model <- lm(Price ~ AreaNet)

# plot observations and add a regression line
plot(AreaNet, 
     Price, 
     ylim = c(0, 2500000))

abline(labor_model, 
       col = "steelblue", 
       lwd = 2)

# display the contents of labor_model on the console
labor_model


# calculate the 95% confidence interval for the coefficients in the model
confint(labor_model)

# Save model summary in 'model'
model <- summary(labor_model)

# Extract the standard error of the regression from the model summary
SER <- model$sigma

# Calculate var in AreaNet
V <- (nrow(RE_train)-1) * var(AreaNet)
V

# Calculate the standard error of the slope parameter estimate and print it
SE.beta_1.hat <- sqrt(SER^2/V)
SE.beta_1.hat
#> [1] 0.06978281

# Use Boolean operators to see if the manually calculated value matches the one provided 
# in the mod$coefficients. Round estimates to four decimal places
round(model$coefficients[2, 2], 4) == round(SE.beta_1.hat, 4)
#> [1] TRUE

# calculate heteroscedasticity-robust standard errors
vcov <- vcovHC(linear_model, type = "HC1")
vcov

# calculate the square root of the diagonal elements in vcov
robust_se <- sqrt(diag(vcov))
robust_se

#----------------------------------------------

# we call the `coeftest()` function for our model
#coeftest(linear_model)
coeftest(linear_model, vcov. = vcov)

