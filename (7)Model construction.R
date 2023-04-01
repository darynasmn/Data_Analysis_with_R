################################################################################
#---------------------------- Model construction ------------------------------#
#------------------------------------------------------------------------------#
################################################################################

library(AER)
library(ggplot2)
library(MASS)

attach(RE_train)
summary.data.frame(RE_train)

#------------------------------------------------ ------------------------------
#------------------------------- 1 Task ----------------------------------------

RE_train$Bedrooms_binary <- as.numeric(RE_train$Bedrooms<4)
RE_train$Bathrooms_binary <- as.numeric(RE_train$Bathrooms<2)


plot(Bedrooms, Price,
     pch = 20,
     col = 3,
     main = "")

plot(Bedrooms, Price,
     pch = 20,
     col = 1+ RE_train$Bedrooms_binary,
     main = "")


plot(Bathrooms, Price,
     pch = 20,
     col = 3,
     main = "")

plot(Bathrooms, Price,
     pch = 20,
     col = 1+ RE_train$Bathrooms_binary,
     main = "")


# estimate a model with a binary interaction term
bi_model <- lm(Price ~ Bedrooms_binary + Bathrooms_binary, data = RE_train)
bi_model_1 <- lm(Price ~ Bedrooms_binar + Bathrooms_binar + (Bedrooms_binar:Bathrooms_binar), data = RE_train)
bi_model_2 <- lm(Price ~ Bedrooms_binar:Bathrooms_binar, data = RE_train)

summary(bi_model_2)

summary(bi_model)
# print summaries of coefficients
coeftest(bi_model, vcov. = vcovHC, type = "HC1")



summary(bi_model_1)
# print summaries of coefficients
coeftest(bi_model_1, vcov. = vcovHC, type = "HC1")



# estimate averages for all combinations of Bedrooms_binar and Bathrooms_binar

#1.
predict(bi_model_1, newdata = data.frame("Bedrooms_binar" = 0, "Bathrooms_binar" = 0))

# 2.
predict(bi_model_1, newdata = data.frame("Bedrooms_binar" = 0, "Bathrooms_binar" = 1))

#3.
predict(bi_model_1, newdata = data.frame("Bedrooms_binar" = 1, "Bathrooms_binar" = 0))

# 4.
predict(bi_model_1, newdata = data.frame("Bedrooms_binar" = 1, "Bathrooms_binar" = 1))
#------------------------------------------------ ------------------------------
#------------------------------- 2 Tasks ---------------------------------------

# evaluate the model
#1
bci_model_a <- lm(Price ~ AreaNet + Bedrooms_binary, data = RE_train)
summary(bci_model_a)

#2
bci_model_b <- lm(Price ~ AreaNet + Bedrooms_binar +(AreaNet:Bedrooms_binar), data = RE_train)
summary(bci_model_b)

#3
bci_model_c <- lm(Price ~ AreaNet +(AreaNet:Bedrooms_binar), data = RE_train)
summary(bci_model_c)

#4
bci_model_d <- lm(Price ~ I(log(AreaNet)) + (I(log(AreaNet)):Bedrooms_binar), data = RE_train)
summary(bci_model_d)

#5
bci_model_e <- lm(I(log(Price)) ~ I(log(AreaNet)) + (I(log(AreaNet)):Bedrooms_binar), data = RE_train)
summary(bci_model_e)


#------------------------------------------------ ------------------------------
#(C)
summary(bci_model_c)

confint(bci_model_c)

median_AreaNet <- median(AreaNet)

# data to use with predict() when Bedrooms_binar =1
new_data <- data.frame(AreaNet = mediana_AreaNet, Bedrooms_binar = 1)
fitted <- predict(bci_model_c, newdata = new_data, interval = "confidence")
fitted

fitted <- predict(bci_model_c, newdata = new_data, interval = "prediction")
fitted

new_data <- data.frame(AreaNet = mediana_AreaNet, Bedrooms_binar = 0)
fitted <- predict(bci_model_c, newdata = new_data, interval = "confidence")
fitted

fitted <- predict(bci_model_c, newdata = new_data, interval = "prediction")
fitted


new_data <- data.frame(AreaNet = mediana_AreaNet, Bedrooms_binar = c(0,1))
y_hat <- predict(bci_model_c, newdata = new_data, interval = "prediction" )
diff(y_hat)

#------------------------------------------------ ------------------------------
#----------------------------- Model construction -------------- --------------

# model a
id <- RE_train$Bedrooms_binary
par(mfrow = c(1, 1))
# plot observations with Bedrooms_binar = 0 as red dots
plot(RE_train$AreaNet[id==0], RE_train$Price[id==0],
     pch = 20,
     col = "red",
     main = "",
     xlim = c(0.600),
     xlab = "Bedrooms_binary",
     ylab = "Price")

# plot observations with Bedrooms_binar = 1 as green dots
points(RE_train$AreaNet[id==1], RE_train$Price[id==1],
       pch = 20,
       col = "green")

# reads the evaluated coefficients
coefs <- bci_model_a$coefficients

# draw the estimated regression line for Bedrooms_binar = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# draw the predicted regression line for Bedrooms_binar = 1
abline(coef = c(coefs[1] + coefs[3], coefs[2]),
       col = "green",
       lwd = 1.5 )

# add a legend to the plot
legend("topright",
       pch = c(20, 20),
       col = c("red", "green"),
       legend = c("HiEL = 0", "HiEL = 1"))





# model b
id <- RE_train$Bedrooms_binary
par(mfrow = c(1, 1))
# plot observations with Bedrooms_binar = 0 as red dots
plot(RE_train$AreaNet[id==0], RE_train$Price[id==0],
     pch = 20,
     col = "red",
     xlim = c(0.600),
     main = "",
     xlab = "Bedrooms_binary",
     ylab = "Price")

# plot observations with Bedrooms_binar = 1 as green dots
points(RE_train$AreaNet[id==1], RE_train$Price[id==1],
       pch = 20,
       col = "green")

# reads the evaluated coefficients
coefs <- bci_model_b$coefficients

# draw the estimated regression line for Bedrooms_binar = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# draw the predicted regression line for Bedrooms_binar = 1
abline(coef = c(coefs[1] + coefs[3], coefs[2] + coefs[4]),
       col = "green",
       lwd = 1.5 )


# add a legend to the plot
legend("topright",
       pch = c(20, 20),
       col = c("red", "green"),
       legend = c("HiEL = 0", "HiEL = 1"))






# model c
par(mfrow = c(1, 1))
# plot observations with Bedrooms_binar = 0 as red dots
plot(RE_train$AreaNet[id==0], RE_train$Price[id==0],
     pch = 20,
     col = "red",
     xlim = c(0.600),
     main = "",
     xlab = "Bedrooms_binary",
     ylab = "Price")

# plot observations with Bedrooms_binar = 1 as green dots
points(RE_train$AreaNet[id==1], RE_train$Price[id==1],
       pch = 20,
       col = "green")

# reads the evaluated coefficients
coefs <- bci_model_c$coefficients

# draw the estimated regression line for Bedrooms_binar = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# draw the predicted regression line for Bedrooms_binar = 1
abline(coef = c(coefs[1], coefs[2] + coefs[3]),
       col = "green",
       lwd = 1.5 )


# add a legend to the plot
legend("topright",
       pch = c(20, 20),
       col = c("red", "green"),
       legend = c("HiEL = 0", "HiEL = 1"))




# model d
par(mfrow = c(1, 1))
# plot observations with Bedrooms_binar = 0 as red dots
plot(log(RE_train$AreaNet[id==0]), RE_train$Price[id==0],
     pch = 20,
     col = "red",
     xlim = c(2,7),
     main = "",
     xlab = "Bedrooms_binary",
     ylab = "Price")

# plot observations with Bedrooms_binar = 1 as green dots
points(log(RE_train$AreaNet[id==1]), RE_train$Price[id==1],
       pch = 20,
       col = "green")

# reads the evaluated coefficients
coefs <- bci_model_d$coefficients

# draw the estimated regression line for Bedrooms_binar = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# draw the predicted regression line for Bedrooms_binar = 1
abline(coef = c(coefs[1], coefs[2] + coefs[3]),
       col = "green",
       lwd = 1.5 )

# add a legend to the plot
legend("topright",
       pch = c(20, 20),
       col = c("red", "green"),
       legend = c("HiEL = 0", "HiEL = 1"))





# model e
par(mfrow = c(1, 1))
# plot observations with Bedrooms_binar = 0 as red dots
plot(log(RE_train$AreaNet[id==0]), log(RE_train$Price[id==0]),
     pch = 20,
     col = "red",
     xlim = c(2,7),
     main = "",
     xlab = "Bedrooms_binary",
     ylab = "Price")

# plot observations with Bedrooms_binar = 1 as green dots
points(log(RE_train$AreaNet[id==1]), log(RE_train$Price[id==1]),
       pch = 20,
       col = "green")

# reads the evaluated coefficients
coefs <- bci_model_e$coefficients

# draw the estimated regression line for Bedrooms_binar = 0
abline(coef = c(coefs[1], coefs[2]),
       col = "red",
       lwd = 1.5)

# draw the predicted regression line for Bedrooms_binar = 1
abline(coef = c(coefs[1], coefs[2] + coefs[3]),
       col = "green",
       lwd = 1.5 )

# add a legend to the plot
legend("topright",
       pch = c(20, 20),
       col = c("red", "green"),
       legend = c("HiEL = 0", "HiEL = 1"))



#-------------------------------------------------------------------------------
#------------------------------ 3 Task ----------------------------------------- 

Price_mod1 <- lm(Price ~ AreaNet + PropertySubType, data = RE_train)
summary(Price_mod1)

Price_mod2 <- lm(Price ~ AreaNet + PropertySubType + (AreaNet:PropertySubType), data = RE_train)
summary(Price_mod2)

Price_mod3 <- lm(Price ~ AreaNet + I(AreaNet^2) + PropertySubType + AreaNet:PropertySubType + I(AreaNet^2): PropertySubType, data = RE_train)
summary(Price_mod3)

Price_mod4 <- lm(Price ~ I(log(AreaNet)) + PropertySubType + I(log(AreaNet)):I(log(PropertySubType)), data = RE_train)
summary(Price_mod4)

Price_mod5 <- lm(I(log(Price)) ~ I(log(AreaNet)) + PropertySubType + I(log(AreaNet)):I(log(PropertySubType)), data = RE_train)
summary(Price_mod5)
