######## Applying Multidimensional Scaling ########

setwd("C:/Users/singh/Desktop/TUD (All Semesters)/Courses - Semester 4 (TU Dresden)/Applied Multivariate Statistics/Project")

# Defining column names
variables <- c('buying', 'maint', 'doors', 'persons', 'lug_boot' , 'safety', 'class')

# Data set we will be using
car.data <- read.csv("car.data", header = F, col.names = variables)

# Creating a distance matrix using encoding
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

# Scaling the data
sc_car.data.converted <- scale(car.data.converted)

# Calculating Euclidean distance in 7 dimensional space
dist.car <- dist(sc_car.data.converted, upper = T, diag = T, method = "euclidean")

# Applying MDS on the distance matrix
car.mds <- cmdscale(dist.car) 
head(car.mds) # The information is reduced into 2 dimensions

# Order of "class" in our data matrix
unique(car.data.converted[,7])

# Plotting 
pdf("car_data_mds.pdf", width = 5, height = 4)
plot(car.mds,
     type="p",
     pch = 20,
     ylab="",
     xlab="",
     col = c("red", "blue", "green", "orange")[car.data.converted[,7]])
title("MDS: Car Evaluation")
legend("topright",
       legend= c("unaccep", "accep", "vgood", "good"),
       text.col=c("red", "blue", "green", "orange"))
dev.off()