###### * Project: Car Data set (FA) * ######

# car.c45-names contains info only about target variable.
# car.names is the file that describes the entire data set. Use this to understand the data set.
# car.data is the actual data set.

# Defining column names
variables <- c('buying', 'maint', 'doors', 'persons', 'lug_boot' , 'safety', 'class')

# Data set we will be using
car.data <- read.csv("car.data", header = T, col.names = variables)

# The data contained inside is not numeric data. Hence, it's not really readable by R!

car.data.converted <- car.data  # Copy of the original data

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

class(car.data.converted[,1]) # We are working with 'character' class

# Converting to numeric class
for(i in 1:7){
  car.data.converted[,i] <- as.numeric(car.data.converted[,i])
}

# performing FA
# First we compute the Degrees of freedom d with k factor, 

p <- ncol(car.data.converted)
k <- 3
d <- 0.5*(p-k)^2 - 0.5*(p+k)
d

#our d > 0 when k=1,2,3
#we select k = 3 factors
#we extract factors using Maximum Likelihood Method

fa.ath <- factanal(~.,
                   factors=k,
                   rotation="none",
                   scores="regression",
                   data=car.data.converted)
print(fa.ath)

#we test whether 3 factors are sufficient
#The chi square statistic is 51.92 on 3 degrees of freedom.
#The p-value is 3.11e-11 

pchisq(51.93, df = d, lower.tail = FALSE)

3.099752e-11 #is  significantly less than 0.05, we reject the null hypothesis, 
#k factors are not sufficient to explain variance in our data

#we perform varimax rotation to gain better interpretability

fa.ath.var <- factanal(~.,
                   factors=k,
                   rotation="varimax",
                   scores="regression",
                   data=car.data.converted)
print(fa.ath.var)

# we plot unrotated against rotated factors for better visualization

par(mfrow=c(1,2))
plot(cbind(cos((0:360)/180*pi),sin((0:360)/180*pi)),
     type="l",
     lty="dotted",
     xlab = "Factor 1",
     ylab = "Factor 2",
     main="car.data.converted (unrotated)")
abline(h = 0)
abline(v = 0)
text(fa.ath$loadings[,1:2],
     labels=colnames(car.data.converted),
     col="black")
plot(cbind(cos((0:360)/180*pi),sin((0:360)/180*pi)),
     type="l",
     lty="dotted",
     xlab = "Factor 1",
     ylab = "Factor 2",
          main="car.data.converted (Varimax)")
abline(h = 0)
abline(v = 0)
text(fa.ath.var$loadings[,1:2],
     labels=colnames(car.data.converted),
     col="black")


#comparing loadings before and after rotation 
cbind(fa.ath$loadings[,1], fa.ath.var$loadings[,1])

## RESULT: 
  #First three factors explain only 47% of total variance in our data
  #chi-square test shows that the first three factors are not sufficient
  #rotation does not improve interpretability of our factors
  #factor analysis is not suitable for our analysis 