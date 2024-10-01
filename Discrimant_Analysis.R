#####Linear Discriminant Analysis#####

library(MASS)
#setwd
read.csv("car.data")

#Defining predictor and target variables

variables <- c("buying", "maint","doors","persons", "lug_boot","safety", "class")

#data we will be using

car.data.converted <- read.csv("car.data",header = FALSE, col.names = variables)
head(car.data.converted)
nrow(car.data.converted)
# Our cardata has both predictor and target variable being categorical,
#DA requires data to have numerical predictor variables and categorical response vairable

# We are converting the data by applying Ordinal Encoding. 
## CAUTION: the scale for variables are not the same

# Converting Column 1
car.data.converted[,1] <- replace(car.data.converted[,1], car.data.converted[,1] == "vhigh", 4)
car.data.converted[,1] <- replace(car.data.converted[,1], car.data.converted[,1] == "high", 3)
car.data.converted[,1] <- replace(car.data.converted[,1], car.data.converted[,1] == "med", 2)
car.data.converted[,1] <- replace(car.data.converted[,1], car.data.converted[,1] == "low", 1)

# Converting Column 2
car.data.converted[,2] <- replace(car.data.converted[,2], car.data.converted[,2] == "vhigh", 4)
car.data.converted[,2] <- replace(car.data.converted[,2], car.data.converted[,2] == "high", 3)
car.data.converted[,2] <- replace(car.data.converted[,2], car.data.converted[,2] == "med", 2)
car.data.converted[,2] <- replace(car.data.converted[,2], car.data.converted[,2] == "low", 1)

# Converting Column 3
car.data.converted[,3] <- replace(car.data.converted[,3], car.data.converted[,3] == "5more", 5)

# Converting Column 4
car.data.converted[,4] <- replace(car.data.converted[,4], car.data.converted[,4] == "more", 6)

# Converting Column 5
car.data.converted[,5] <- replace(car.data.converted[,5], car.data.converted[,5] == "small", 1)
car.data.converted[,5] <- replace(car.data.converted[,5], car.data.converted[,5] == "med", 2)
car.data.converted[,5] <- replace(car.data.converted[,5], car.data.converted[,5] == "big", 3)

# Converting Column 6
car.data.converted[,6] <- replace(car.data.converted[,6], car.data.converted[,6] == "low", 1)
car.data.converted[,6] <- replace(car.data.converted[,6], car.data.converted[,6] == "med", 2)
car.data.converted[,6] <- replace(car.data.converted[,6], car.data.converted[,6] == "high", 3)

# Converting Column 7
car.data.converted[,7] <- replace(car.data.converted[,7], car.data.converted[,7] == "unacc", 1)
car.data.converted[,7] <- replace(car.data.converted[,7], car.data.converted[,7] == "acc", 2)
car.data.converted[,7] <- replace(car.data.converted[,7], car.data.converted[,7] == "good", 3)
car.data.converted[,7] <- replace(car.data.converted[,7], car.data.converted[,7] == "vgood", 4)

head(car.data.converted)
class(car.data.converted)
str(car.data.converted)
summary(car.data.converted)

#car.data.numeric <- as.numeric(factor(car.data.converted))
#converting class to numeric
for(i in 1:7){
  car.data.converted[,i] <- as.numeric(car.data.converted[,i])
}

summary(car.data.converted)
class(car.data.converted[,2])

#LDA, Ml method, using MASS package 
#With equal prior probabilities

###### we consider only the variable buying #######

x <- car.data.converted[,"buying"]
x

#we set our class column
pi.true <- car.data.converted[,7]
pi.true

n <- length(pi.true) 
n

#LDA assumes that the four classes follow a normal distribution 
#and have equal covariance matrices/variances

car.lda <- lda(car.data.converted[,7]~x, prior = c(1,1,1,1)/4)
car.lda

car.lda.predict <- predict(car.lda, as.data.frame(car.data.converted[,"buying"]))
car.lda.predict$class

mean(pi.true!=car.lda.predict$class) #measure the misclassification rate

#compare true and predicted category

cbind(pi.true, car.lda.predict$class)

#Comparing true and predicted in a table

table(pi.true,car.lda.predict$class)

###### we consider all the variables #######

x.full <- as.data.frame(car.data.converted[,1:6]) # all predictor the variables
pi.true <- car.data.converted[,7] # class response variable
pi.true

car.lda.full <- lda(pi.true~., data = x.full, prior = c(1,1,1,1)/4)
car.lda.full

car.lda.predict.full <- predict(car.lda.full, x.full)
car.lda.predict.full$class

mean(pi.true!=car.lda.predict.full$class) #measure the misclassification rate

#compare true and predicted category

cbind(pi.true, car.lda.predict.full$class)

#Comparing true and predicted in a table

table(pi.true,car.lda.predict.full$class)


