library(tibble)
library(gridExtra)
library(dplyr)
library(ggplot2)
library(rpart)
train <- read.csv("Lisbon", header = TRUE)
summary.data.frame(train)

str(train)

#Attributes that are represented in an unacceptable type and will be changed:
str(train$PropertySubType)
str(train$PropertyType)
str(train$Condition)
str(train$Municipality)
str(train$Parking)
str(train$Country)
str(train$District)
str(train$Parish)
str(train$Id)


#A table that provides information about empty values in attribute columns 

missing_vars <- function(x) {
  var <- 0
  missing <- 0
  missing_prop <- 0
  for (i in 1:length(names(x))) {
    var[i] <- names(x)[i]
    missing[i] <- sum(is.na(x[, i])|x[, i] =="" )
    missing_prop[i] <- missing[i] / nrow(x)
  }
  # order   
  missing_data <- data.frame(var = var, missing = missing, missing_prop = missing_prop) %>%
    arrange(desc(missing_prop))
  # print out
  missing_data
}
#Table output
missing_vars(train)


### Assumptions regarding the grouping of attributes:
### Need to research "AreaGross" and "AreaNet"
plot(train$AreaNet, train$AreaGross,
     type = "p",
     main = ". ",
     xlab="AreaNet",
     ylab="AreaGross",
     pch=15)
### This table shows the dependence of the variable "AreaNet" on "AreaGross"
### From the specified values, we can say that the "AreaGross" values are exactly twice as large as all AreaNet variables, and therefore we can discard one unnecessary attribute


### Performing type substitution in attributes with an unacceptable type 

###Id "Attribute" Conversion: Replace incorrect order with correct order
train$Id <- train$Id -100

#To view the distribution, it makes sense to change the "Condition" type to Factor
train$Condition <- as.factor(train$Condition)
summary(as.factor(train$Condition))

#To view the distribution, it makes sense to change the "PropertyType" to Factor
train$PropertyType <- as.factor(train$PropertyType)
summary(as.factor(train$PropertyType))

#To view the distribution, it makes sense to change the "PropertySubType" to Factor
train$PropertySubType <- as.factor(train$PropertySubType)
summary(as.factor(train$PropertySubType))

#To view the distribution, it makes sense to change the "Parish" type to Factor
train$Parish <- as.factor(train$Parish)
summary(as.factor(train$Parish))


#To view the distribution, it makes sense to change the "Parish" type to Factor
train$Parking <- as.factor(train$Parking)
summary(as.factor(train$Parking))


#To view the distribution, it makes sense to change the "Country" type to Factor (after analyzing the graph, the attribute will be deleted)
train$Country <- as.factor(train$Country)
summary(as.factor(train$Country))

#To view the distribution, it makes sense to change the "District" type to Factor (after analyzing the graph, the attribute will be deleted)
train$District <- as.factor(train$District)
summary(as.factor(train$District))

#To view the distribution, it makes sense to change the "Municipality" type to Factor (after analyzing the graph, the attribute will be deleted)
train$Municipality <- as.factor(train$Municipality)
summary(as.factor(train$Municipality))


### You need to make a factor variable from the "Price" attribute to create percentage tables
train$Price
Price_labels <- c('84000 - 250000','250000 - 500000', '500000 - 1000000', '1000000 - 2000000', '2000000-4150000')
# Purpose of the label
train$Price_labels <- cut(train$Price, c(84000, 250000, 500000, 1000000, 2000000, 4150000), include.highest=TRUE, labels= Price_labels)



###Research for variable "Id." A percentage schedule is not needed, because it will not give anything
plot(train$Id, train$Price,
     type = "p",
     main = "Dependence of price on Id",
     xlab="Id",
     ylab="Price",
     pch=15)


#Graph for the "Condition" variable
p1 <- train %>%
  ggplot(aes(x = Condition, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively Condition")

# Graph of influence on "Price" in percentage
p2 <- train %>%
  ggplot(aes(x = Condition, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Condition", y = "Percent") +
  ggtitle("As a percentage of Price on Condition")

grid.arrange(p1, p2, ncol = 2)



# Graph for "PropertyType" variable
p1 <- train %>%
  ggplot(aes(x = PropertyType, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively PropertyType")

# Graph of influence on "Price" in percentage
p2 <- train %>%
  ggplot(aes(x = PropertyType, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "PropertyType", y = "Percent") +
  ggtitle("As a percentage of Price on PropertyType")

grid.arrange(p1, p2, ncol = 2)



# Graph for a variable "PropertySubType"
p1 <- train %>%
  ggplot(aes(x = PropertySubType, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively PropertySubType")

# Graph of influence on "Price" in percentage
p2 <- train %>%
  ggplot(aes(x = PropertySubType, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "PropertySubType", y = "Percent") +
  ggtitle("As percentage Price on PropertySubType")

grid.arrange(p1, p2, ncol = 2)



# Plot for the variable "Parish"
p1 <- train %>%
  ggplot(aes(x = Parish, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively Parish")

# Graph of influence on "Price" in percentage
p2 <- train %>%
  ggplot(aes(x = Parish, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Parish", y = "Percent") +
  ggtitle("As percentage Price on Parish")

grid.arrange(p1, p2, ncol = 2)



# Graph for the variable number of Bedrooms
p1 <- train %>%
  ggplot(aes(x = Bedrooms, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of Bedrooms")

# Graph of influence on "Price" in percentage
p2 <- train %>%
  ggplot(aes(x = Bedrooms, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Bedrooms", y = "Percent") +
  ggtitle("As percentage Price on number of Bedrooms")

grid.arrange(p1, p2, ncol = 2)

# Graph for the number of "Bathrooms" variable
p1 <- train %>%
  ggplot(aes(x = Bathrooms, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of Bathrooms")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = Bathrooms, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Bedrooms", y = "Percent") +
  ggtitle("As percentage Price on number of Bedrooms")

grid.arrange(p1, p2, ncol = 2)



### You need to make a factorial variable from the "AreaNet" attribute to create percentage tables 
train$AreaNet
AreaNet_labels <- c('23 - 100','100 - 200', '200 - 350', '350 - 550', '550-600')
# Purpose of the label
train$AreaNet_labels <- cut(train$AreaNet, c(22, 100, 200, 350, 550, 600), include.highest=TRUE, labels= AreaNet_labels)


# Plot for the "AreaNet" variable
p1 <- train %>%
  ggplot(aes(x = AreaNet_labels, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of AreaNet")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = AreaNet_labels, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "AreaNet_labels", y = "Percent") +
  ggtitle("As percentage Price on number of AreaNet")

grid.arrange(p1, p2, ncol = 2)


### From the "AreaGross" attribute, you need to make a factorial variable to create percentage tables 
train$AreaGross
AreaGross_labels <- c('46 - 200','200 - 500', '500 - 750', '750 - 950', '950-1200')
# Purpose of the label
train$AreaGross_labels <- cut(train$AreaGross, c(45, 200, 500, 750, 950, 1200), include.highest=TRUE, labels= AreaGross_labels)


# Plot for the "AreaGross" variable
p1 <- train %>%
  ggplot(aes(x = AreaGross_labels, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of AreaGross")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = AreaGross_labels, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "AreaGross_labels", y = "Percent") +
  ggtitle("As percentage Price on number of AreaGross")

grid.arrange(p1, p2, ncol = 2)



# Graph for the number of "Parking" variable
p1 <- train %>%
  ggplot(aes(x = Parking, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of Parking")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = Parking, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Parking", y = "Percent") +
  ggtitle("As percentage Price on number of Parking")

grid.arrange(p1, p2, ncol = 2)




### The "Latitude" attribute needs to be made into a factorial variable to create percentage tables 
train$Latitude
Latitude_labels <- c('38.70 - 38.72','38.72 - 38.75 ', '38.75 - 38.77', '38.77 - 38.79')
# Purpose of the label
train$Latitude_labels <- cut(train$Latitude, c(38.69, 38.72, 38.75, 38.77, 38.79), include.highest=TRUE, labels= Latitude_labels)


# Plot for the "AreaGross" variable
p1 <- train %>%
  ggplot(aes(x = Latitude_labels, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of Latitude")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = Latitude_labels, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Latitude_labels", y = "Percent") +
  ggtitle("As percentage Price on number of Latitude")

grid.arrange(p1, p2, ncol = 2)



### The "Latitude" attribute needs to be made into a factorial variable to create percentage tables 
train$Longitude
  Longitude_labels <- c('-9.224 - (-9.175)', '-9.175 - (-9.125)', '-9.125 - (-9.094)' )
# Purpose of the label
train$Longitude_labels <- cut(train$Longitude, c(-9.225, -9.175, -9.125, - 9.093), include.highest=TRUE, labels= Longitude_labels)
                        


# Graph for the "Longitude" variable
p1 <- train %>%
  ggplot(aes(x = Longitude_labels, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of Longitude")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = Longitude_labels, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Longitude_labels", y = "Percent") +
  ggtitle("As percentage Price on number of Longitude")

grid.arrange(p1, p2, ncol = 2)



# Graph for the number of "Country" variable
p1 <- train %>%
  ggplot(aes(x = Country, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of Country")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = Country, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Country", y = "Percent") +
  ggtitle("As percentage Price on number of Country")

grid.arrange(p1, p2, ncol = 2)


# Graph for the variable number of "District"
p1 <- train %>%
  ggplot(aes(x = District, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of District")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = District, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "District", y = "Percent") +
  ggtitle("As percentage Price on number of District")

grid.arrange(p1, p2, ncol = 2)


# Graph for the variable number of "Municipality"
p1 <- train %>%
  ggplot(aes(x = Municipality, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of Municipality")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = Municipality, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 1, 0.1)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Municipality", y = "Percent") +
  ggtitle("As percentage Price on number of District")

grid.arrange(p1, p2, ncol = 2)



### From the "Price.M2" attribute, you need to make a factor variable to create percentage tables
train$Price.M2
Price.M2_labels <- c('2393 - 3500', '3500-4500', '4500 - 5340')
# Purpose of the label
train$Price.M2_labels <- cut(train$Price.M2, c(2392,3500,4500,5340), include.highest=TRUE, labels= Price.M2_labels)


# Graph for "Price.M2" variable
p1 <- train %>%
  ggplot(aes(x = Price.M2_labels, y = ..count.., fill = factor(Price_labels))) +
  geom_bar() +
  scale_fill_discrete(name = "Price") +
  ggtitle("Price comparatively number of Price.M2")

# Graph of influence on price in percentage
p2 <- train %>%
  ggplot(aes(x = Price.M2_labels, fill = factor(Price_labels))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent, breaks = seq(0, 10, 0.5)) +
  scale_fill_discrete(name = "Price") +
  labs(x = "Price.M2_labels", y = "Percent") +
  ggtitle("As percentage Price on number of Price.M2")

grid.arrange(p1, p2, ncol = 2)


RE_train <- subset(train, select = -c(Country, District, Municipality, Longitude_labels, Longitude, AreaNet_labels, Latitude_labels, Latitude, AreaGross_labels,Bedrooms_labels,Price.M2_labels, Price_labels, AreaGross))
# Writing to a file
write.csv(RE_train, file = "RE_train.CSV", row.names = FALSE)


str(RE_train)

