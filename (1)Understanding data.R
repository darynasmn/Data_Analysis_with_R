
##########################################################################
# The task is to create a model based on "train" data                      #
# and predict prices                                                     #
##########################################################################
# 1. Understanding data                                                     #
##########################################################################


### Loading the library 
library(ggplot2)


### Loading raw train
train <- read.csv("lisbon.csv", header = TRUE)


# Check the Id, type INT, we can check the total count and the number of unique values.
# If they are equal and both are equal to the total number of records, it means there is
# are unique and have no missing values.
length(train$Id)
length(unique(train$Id))


length(unique(train$PropertySubType))


# Checking the "Price" attribute
table(train$Price)

# Checking the "Bedrooms" attribute
table(train$Bedrooms)


# Checking the "Bathrooms" attribute
table(train$Bathrooms)


# Distribution of prices by the number of bedrooms
table(train$Bedrooms, train$Price)



# Distribution of prices by the number of bathrooms
table(train$Bathrooms, train$Price)



# Distribution of prices by district where the property is located 
table(train$District, train$Price)



# Distribution of prices by district 
table(train$Parish, train$Price)


# Distribution of prices for the price per square meter 
table(train$Price.M2, train$Price)


# Distribution of prices by real estate condition 
table(train$Condition, train$Price)


# Distribution of prices by types of houses
table(train$PropertyType, train$Price)
table(train$PropertySubType, train$Price)


################################################################################

###Analysis of all char:

# Analysis the "Parish" attribute 
str(train$Parish)
summary(train$Parish)

# Analysis the "Condition" attribute
str(train$Condition)
summary(train$Condition)

# Analysis the "District" attribute
str(train$District)
summary(train$District)

# Analysis the "PropertyType" attribute
str(train$PropertyType)
summary(train$PropertyType)

# Analysis the "PropertySubType" attribute
str(train$PropertySubType)
summary(train$PropertySubType)

# Analysis the "Country" attribute
str(train$Country)
summary(train$Country)

# Analysis the "Municipality" attribute
str(train$Municipality)
summary(train$Municipality)


################################################################################


#Table of price dependence on ID (does not affect)
plot(train$Id, train$Price,
    type = "p",
     main = "Dependence of price on ID",
     xlab="Id",
     ylab="price",
     pch=15)


# A line that shows the effect of the variable (does not affect)
a <- lm(train$Price~train$Id)
abline(a, 
       col = "darkred",
       lwd = 1.5)

# Analysis the "ID" attribute
str(train$Id)
summary(train$Id)



# Table of price dependence on the number of bathrooms and rooms
plot(train$Bathrooms, train$Price,
     type = "p",
     main = "The price depends on the number of bathrooms",
     xlab="Bathrooms",
     ylab="price",
     pch=15)

#A line that shows the influence of the variable (affects)
a <- lm(train$Price~train$Bathrooms)
abline(a, 
       col = "darkred",
       lwd = 1.5)

# Analysis the "Bathrooms" attribute
str(train$Bathrooms)
summary(train$Bathrooms)



#Table of the dependence of the price on the number of bedrooms 
plot(train$Bedrooms, train$Price,
     type = "p",
     main = "The price depends on the number of rooms",
     xlab="Bedrooms",
     ylab="price",
     pch=15)



#The line that shows the influence of the variable (affects)
a <- lm(train$Price~train$Bedrooms)
abline(a, 
       col = "darkred",
       lwd = 1.5)

# Analysis the "Bedrooms" attribute
str(train$Bedrooms)
summary(train$Bedrooms)



#Table of the dependence of the price on the residential part of the premises
plot(train$AreaNet, train$Price,
     type = "p",
     main = "Dependence of the price on the residential part of the premises",
     xlab="AreaNet",
     ylab="price",
     pch=15)

#A line that shows the influence of the variable (affects)
a <- lm(train$Price~train$AreaNet)
abline(a, 
       col = "darkred",
       lwd = 1.5)

#Analysis the "AreaNet" attribute 
str(train$AreaNet)
summary(train$AreaNet)



#Table of the dependence of the price on the total number of square meters of construction space.
plot(train$AreaGross, train$Price,
     type = "p",
     main = "Dependence of the price on the total number of square meters of construction space",
     xlab="AreaGross",
     ylab="price",
     pch=15) 

#The line that shows the influence of the variable (affects)
a <- lm(train$Price~train$AreaGross)
abline(a, 
       col = "darkred",
       lwd = 1.5)

#Analysis the "AreaGross" attribute
str(train$AreaGross)
summary(train$AreaGross)



#Table of the dependence of the price on the number of parking spaces
plot(train$Parking, train$Price,
     type = "p",
     main = "The price depends on the number of parking spaces",
     xlab="Parking",
     ylab="price",
     pch=15)

#A line that shows the effect of the variable (does not affect)
a <- lm(train$Price~train$Parking)
abline(a, 
       col = "darkred",
       lwd = 1.5)

#Analysis the "Parking" attribute
str(train$Parking)
summary(train$Parking)



#Table of dependence of price on geographical latitude
plot(train$Latitude, train$Price,
     type = "p",
     main = "Dependence of the price on geographical latitude",
     xlab="Latitude",
     ylab="price",
     pch=15)

#A line that shows the effect of the variable (does not affect)
a <- lm(train$Price~train$Latitude)
abline(a, 
       col = "darkred",
       lwd = 1.5)

#Analysis the "Latitude" attribute
str(train$Latitude)
summary(train$Latitude)



#Table of dependence of price on geographic longitude.
plot(train$Longitude, train$Price,
     type = "p",
     main = "Dependence of the price on the geographical longitude",
     xlab="Longitude",
     ylab="price",
     pch=15)

#Лінія, яка показує вплив змінної(не впливає)
a <- lm(train$Price~train$Longitude)
abline(a, 
       col = "darkred",
       lwd = 1.5)

#Analysis the "Longitude" attribute
str(train$Longitude)
summary(train$Longitude)

summary(train$Parking)


#Table of the dependence of the price on the price per m² in the location of the house
plot(train$Price.M2, train$Price,
     type = "p",
     main = "Dependence of the price on the price per m² in the location of the house.",
     xlab="Price.M2",
     ylab="price",
     pch=15)

#The line that shows the effect of the variable (does not affect)
a <- lm(train$Price~train$Price.M2)
abline(a, 
       col = "darkred",
       lwd = 1.5)

#Analysis the "Price.M2" attribute
str(train$Price.M2)
summary(train$Price.M2)

#Output of the entire table 
summary.data.frame(train)

#General output of the entire table
str(train)

