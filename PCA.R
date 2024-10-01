###### * Project: Car Data set (PCA) * ######

# car.c45-names contains info only about target variable.
# car.names is the file that describes the entire data set. Use this to understand the data set.
# car.data is the actual data set.

# Defining column names
variables <- c('buying', 'maint', 'doors', 'persons', 'lug_boot' , 'safety', 'class')

# Data set we will be using
car.data <- read.csv("car.data", header = F, col.names = variables)

# The data contained inside is not numeric data. Hence, it's not really readable by R!
# Luckily, the data for both features and target is ordered data.

# Note: 
    ## PCA uses covariance matrix of X. Hence X should be a numeric. 
    ## The way we assign numeric values is going to influence the covariance matrix!

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

# Performing PCA
car_pca <- prcomp(car.data.converted, scale. = T)
car_pca

# Looking at cumulative proportion
tau <- cumsum(car_pca$sdev^2)/sum(car_pca$sdev^2)  # cumulative proportion
tau

# Scree Plot
pdf("Scree_plot_pca.pdf", width = 5, height = 4)
plot(tau, pch = 19, col = "blue", xlab = "Principal Component", ylab = "cumulative variance", main = "Scree Plot")
dev.off()

## RESULT: even the first 4 PCs explain only 67% of total variation in our data. 
## This shows that PCs are not suitable here.