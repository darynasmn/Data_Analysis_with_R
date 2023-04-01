########################## Different types of models ###########################
#------------------------------------------------------------------------------#
################################################################################


library(AER)
library(ggplot2)
library(MASS)

attach(RE_train)

linear_model<- lm(Price ~ AreaNet, data = RE_train)
summary(linear_model)

par(mfrow = c(1, 1))

# build observations
plot(RE_train$AreaNet, RE_train$Price,
     col = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     cex.main = 0.9,
     main = "Test Score vs. District Income and a Linear OLS Regression Function")

# add a regression line to the graph
abline(linear_model,
       col = "red",
       lwd = 2)

#------------------------------ quadratic ------------------- ------------------
quadratic_model <- lm(Price ~ AreaNet + I(AreaNet^2), data = RE_train)
summary(quadratic_model)
# get model summary
coeftest(quadratic_model, vcov. = vcovHC, type = "HC1")

#draw a scatterplot of the observations for income and test scores
plot(AreaNet, Price,
     col = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the graph
abline(linear_model, col = "black", lwd = 2)

# add a quadratic function to the graph
order_id <- order(AreaNet) # sort before building!

lines(x = AreaNet[order_id],
      y = fitted(quadratic_model)[order_id],
      col = "red",
      lwd = 2)

#----------------- 2 tasks for the quadratic model -----------------------------
quadriatic_model <- lm(Price ~ AreaNet + I(AreaNet^2), data = RE_train)

# configure data for forecasting
new_data <- data.frame(AreaNet = c(70, 71))

# make predictions
Y_hat <- predict(quadriatic_model, newdata = new_data)
Y_hat
# calculate the difference
diff(Y_hat)

exp(5734.984)

#-------------------------- larger values --------------------------------------

# configure data for forecasting
new_data <- data.frame(AreaNet = c(220, 240))

# make predictions
Y_hat <- predict(quadriatic_model, newdata = new_data)
Y_hat

# calculate the difference
diff(Y_hat)



#-------------------------------- reverse --------------------------------------

reverse <- lm(Price ~ I(1/AreaNet), data = RE_train)
summary (reverse)
# get model summary
coeftest(reverse, vcov. = vcovHC, type = "HC1")

#draw a scatterplot of the observations for income and test scores
plot(AreaNet, Price,
     col = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the graph
abline(linear_model, col = "black", lwd = 2)

# add a quadratic function to the graph
order_id <- order(AreaNet) # sort before building!

lines(x = AreaNet[order_id],
      y = fitted(reverse)[order_id],
      col = "red",
      lwd = 2)



#------------------- 2 tasks for the inverse model -----------------------------
reverse <- lm(Price ~ I(1/AreaNet), data = RE_train)
# configure data for forecasting
new_data <- data.frame(AreaNet = c(70, 80))

# make predictions
Y_hat <- predict(reverse, newdata = new_data)
Y_hat
# calculate the difference
diff(Y_hat)


#--------------------------- larger values -------------------------------------

# configure data for forecasting
new_data <- data.frame(AreaNet = c(220, 240))

# make predictions
Y_hat <- predict(reverse, newdata = new_data)
Y_hat

# calculate the difference
diff(Y_hat)


#--------------------------- exponential ----------------------------------------
#𝑦=𝑏0𝑏1^x
LogLinear_model <- lm(log(Price) ~ AreaNet, data = RE_train)
summary(LogLinear_model)

coeftest(LogLinear_model,
         vcov = vcovHC, type = "HC1")

#--------------------------- power model ---------------------------------------

LogLog_model<-lm(log(Price) ~ log(AreaNet),data=RE_train)
summary(LogLog_model)

coeftest(LogLog_model,
         vcov = vcovHC, type = "HC1")
#draw a scatterplot of the observations for income and test scores
plot(AreaNet, Price,
     col = "steelblue",
     pch = 20,
     xlab = "District Income (thousands of dollars)",
     ylab = "Test Score",
     main = "Estimated Linear and Quadratic Regression Functions")

# add a linear function to the graph
abline(linear_model, col = "black", lwd = 2)

# add a quadratic function to the graph
order_id <- order(AreaNet) # sort before building!
exp(8.54134)

lines(x = AreaNet[order_id],
      y = 5122.204*AreaNet[order_id]^0.98058 ,
      col = "red",
      lwd = 2)





#------------------------------------------------ ------------------------------
#-------------------------- 2 Tasks --------------------------------------------
# the best model is quadratic
summary(quadratic_model)

confint(quadratic_model)

alpha <-0.05
n <- length(RE_train$AreaNet)
p<-4
t_k <- qt(p = 1 - alpha / 2, df = n - p - 1)
t_k