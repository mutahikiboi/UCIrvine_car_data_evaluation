## -------------- Correspondence Analysis -------------- ##

setwd("C:/Users/singh/Desktop/TUD (All Semesters)/Courses - Semester 4 (TU Dresden)/Applied Multivariate Statistics/Project")

# Defining column names (7 variables in total)
variables <- c('buying', 'maint', 'doors', 'persons', 'lug_boot' , 'safety', 'class')

# Data set we will be using
car.data <- read.csv("car.data", header = F, col.names = variables)

# Variable in Focus (target variable in the original study)
unique(car.data$class) # unacc << acc << good << vgood

  ## We need to create multiple contingency tables, since we only want to study relationship
  ## of class with other categorical variables.


# TABLE 1: buying price vs class (4x4 contingency table)
tb1 <- table(car.data[,7], car.data[,1]) # columns are buying price
# The matrix should be reduced to a 3 dimensional subspace. 

# install.packages("FactoMineR")
library(FactoMineR)

ca.mod1 <- CA(tb1, graph = F)
summary(ca.mod1, nbelements = Inf, graph = F) 
    # p-value (Pearson Chi-square test) ~ 0, we reject H0 (i.e. independence hypothesis)
    # dependency exists, but we don't know the direction and strength of dependency yet

ca.mod1$svd # eigenvalues + left/right side eigenvectors
ca.mod1$eig # The first 2 eigenvectors are more than sufficient

ca.mod1$col$inertia*1000 # Inertia is high for price: "vhigh", "high", low"
ca.mod1$row$inertia*1000 # Inertia is high for evaluation: "good", "vgood"
pdf("factor_map_1.pdf", width = 5, height = 4)
plot(ca.mod1, title = "buying price and evaluation (factor map)")
dev.off()
    # Plot using the first two row/column factors
    # high price has strong relationship lower evaluation/acceptability
    # low price shows relationship with higher acceptability

# Variance along column factor 1
ca.mod1$col$contrib[,1] # low has significant contribution to the variance of s1

# Variance along row factor 1
ca.mod1$row$contrib[,1] # good and vgood have significant contribution to the variance of r1

# TABLE 2: price of the maintenance vs class (4x4 contingency table)
tb2 <- table(car.data[,7], car.data[,2]) # columns are price of the maintenance
# The matrix should be reduced to a 3 dimensional subspace. 

ca.mod2 <- CA(tb2, graph = F)
summary(ca.mod2, nbelements = Inf, graph = F) 
# p-value (Pearson Chi-square test) ~ 0, we reject H0 (i.e. independence hypothesis)
# dependency exists, but we don't know the direction and strength of dependency yet

ca.mod2$svd # eigenvalues + left/right side eigenvectors
ca.mod2$eig # The first 2 eigenvectors are more than sufficient

ca.mod2$col$inertia*1000 # Inertia is high for maintenance price: "vhigh", low"
ca.mod2$row$inertia*1000 # Inertia is high for evaluation: "good"
pdf("factor_map_2.pdf", width = 5, height = 4)
plot(ca.mod2, title = "price of the maintenance and evaluation (factor map)")
dev.off()
# Plot using the first two row/column factors
# high and vhigh price has strong relationship with lower evaluation/acceptability
# low price shows relationship with higher acceptability

# Variance along column factor 1
ca.mod2$col$contrib[,1] # low and vhigh have significant contribution to the variance of s1

# Variance along row factor 1
ca.mod2$row$contrib[,1] # good has significant contribution to the variance of r1

# TABLE 3: doors vs class (4x4 contingency table)
tb3 <- table(car.data[,7], car.data[,3]) # columns are number of doors
# The matrix should be reduced to a 3 dimensional subspace. 

ca.mod3 <- CA(tb3, graph = F)
summary(ca.mod3, nbelements = Inf, graph = F) 
# p-value (Pearson Chi-square test) ~ 0.32, we can't reject H0 (i.e. independence hypothesis)
# dependency may/ may not exist

ca.mod3$svd # eigenvalues + left/right side eigenvectors
ca.mod3$eig # The first 2 eigenvectors are more than sufficient

ca.mod3$col$inertia*1000 # Inertia is high for 2 doors
ca.mod3$row$inertia*1000 # Inertia is high for evaluation: "vgood"
pdf("factor_map_3.pdf", width = 5, height = 4)
plot(ca.mod3, title = "number of doors and evaluation (factor map)")
dev.off()
# Plot using the first two row/column factors
# more doors has relationship with better evaluation/acceptability

# Variance along column factor 1
ca.mod3$col$contrib[,1] # 2 doors has significant contribution to the variance of s1

# Variance along row factor 1
ca.mod3$row$contrib[,1] # vgood, acc, unacc have significant contribution to the variance of r1

# TABLE 4: person capacity vs class (4x3 contingency table)
tb4 <- table(car.data[,7], car.data[,4]) # columns are number of people that can be carried
# The matrix should be reduced to a 2 dimensional subspace. 

ca.mod4 <- CA(tb4, graph = F)
summary(ca.mod4, nbelements = Inf, graph = F) 
# p-value (Pearson Chi-square test) ~ 0, we can reject H0 (i.e. independence hypothesis)
# dependency exists, but direction isn't known yet

ca.mod4$svd # eigenvalues + left/right side eigenvectors
ca.mod4$eig # The first 2 eigenvectors are more than sufficient

ca.mod4$col$inertia*1000 # Inertia is high for person capacity = 2 
ca.mod4$row$inertia*1000 # Inertia is high for evaluation: "acc"
pdf("factor_map_4.pdf", width = 5, height = 4)
plot(ca.mod4, title = "person capacity and evaluation (factor map)")
dev.off()
# Plot using the first two row/column factors
# lower person capacity has relationship with poor evaluation
# more person capacity has relationship with better evaluation/acceptability

# Variance along column factor 1
ca.mod4$col$contrib[,1] # 2 person capacity has significant contribution to the variance of s1

# Variance along row factor 1
ca.mod4$row$contrib[,1] # acc has significant contribution to the variance of r1

# TABLE 5: boot size vs class (4x3 contingency table)
tb5 <- table(car.data[,7], car.data[,5]) # columns are size of lugguage-boot size
# The matrix should be reduced to a 2 dimensional subspace. 

ca.mod5 <- CA(tb5, graph = F)
summary(ca.mod5, nbelements = Inf, graph = F) 
# p-value (Pearson Chi-square test) ~ 0, we can reject H0 (i.e. independence hypothesis)
# dependency exists, but direction isn't known yet

ca.mod5$svd # eigenvalues + left/right side eigenvectors
ca.mod5$eig # The first 2 eigenvectors are more than sufficient

ca.mod5$col$inertia*1000 # Inertia is high for small boot size
ca.mod5$row$inertia*1000 # Inertia is high for evaluation: "vgood"
pdf("factor_map_5.pdf", width = 5, height = 4)
plot(ca.mod5, title = "lugguage-boot size and evaluation (factor map)")
dev.off()
# Plot using the first two row/column factors
# small boot size has relationship with poor evaluation
# med and big boot size have relationship with better evaluation/acceptability

# Variance along column factor 1
ca.mod5$col$contrib[,1] # small boot size has significant contribution to the variance of s1

# Variance along row factor 1
ca.mod5$row$contrib[,1] # vgood has significant contribution to the variance of r1

# TABLE 6: estimated safety vs class (4x3 contingency table)
tb6 <- table(car.data[,7], car.data[,6]) # columns are safety level
# The matrix should be reduced to a 2 dimensional subspace. 

ca.mod6 <- CA(tb6, graph = F)
summary(ca.mod6, nbelements = Inf, graph = F) 
# p-value (Pearson Chi-square test) ~ 0, we can reject H0 (i.e. independence hypothesis)
# dependency exists, but direction isn't known yet

ca.mod6$svd # eigenvalues + left/right side eigenvectors
ca.mod6$eig # The first 2 eigenvectors are more than sufficient

ca.mod6$col$inertia*1000 # Inertia is high for low safety
ca.mod6$row$inertia*1000 # Inertia is high for evaluation: "acc"
pdf("factor_map_6.pdf", width = 5, height = 4)
plot(ca.mod6, title = "safety level and evaluation (factor map)")
dev.off()
# Plot using the first two row/column factors
# low safety has relationship with poor evaluation
# high safety is closer to better evaluation/acceptability

# Variance along column factor 1
ca.mod6$col$contrib[,1] # low safety has significant contribution to the variance of s1

# Variance along row factor 1
ca.mod6$row$contrib[,1] # acc has significant contribution to the variance of r1
