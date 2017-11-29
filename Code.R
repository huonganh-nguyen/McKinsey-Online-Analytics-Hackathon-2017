########## Student name: Huong Anh Nguyen ##########
########## Student email: huonganh.nguyen@duke.edu ##########


### Load important libraries and packages ###
source("installpackages.R")
source("DataAnalyticsFunctions.R")

### Read the train data into R ###
# The train data set loaded into R below has been edited in Excel to extract the year, month, day, 
# and hour from the DateTime column of the original train date set.
df <- read.csv("train_aWnotuB.csv")
df <- data.frame(df)

### Exploratory Data Analysis ###
summary(df)
nrow(df) #48,120
sum(complete.cases(df)) #48,120
sum(is.na(df$DateTime)) #0
sum(is.na(df$Junction)) #0
sum(is.na(df$Vehicles)) #0
sum(is.na(df$ID)) #0
# All rows in the data set are completed. There are no missing values in the data set.
df$Year <- format(as.Date(df$DateTime), "%Y")
df$Month <- format(as.Date(df$DateTime), "%m")
df$Day <- format(as.Date(df$DateTime), "%d")
df$Time <- strptime(df$DateTime, "%Y-%m-%d %H:%M:%S")
df$Hour <- format(df$Time, "%H")
head(df, 10)
# Split the data set into data frames for each of the junctions.
df1 <- df[df$Junction == 1, ] #Junction 1
df2 <- df[df$Junction == 2, ] #Junction 2
df3 <- df[df$Junction == 3, ] #Junction 3
df4 <- df[df$Junction == 4, ] #Junction 4
nrow(df1) #14,592
nrow(df2) #14,592
nrow(df3) #14,592
nrow(df4) #4,344
# See the distribution of the number of vehicles in each junction
hist(df1$Vehicles, main = "Histogram of Vehicles in Junction 1", xlab = "Number of Vehicles")
hist(df2$Vehicles, main = "Histogram of Vehicles in Junction 2", xlab = "Number of Vehicles")
hist(df3$Vehicles, main = "Histogram of Vehicles in Junction 3", xlab = "Number of Vehicles")
hist(df4$Vehicles, main = "Histogram of Vehicles in Junction 4", xlab = "Number of Vehicles")
# The distribution of vehicles in each junction is skewed with extreme outliers
# Check for outliers in number of vehicles in each junction
boxplot(Vehicles ~ Junction, data = df, main = "Boxplot of Vehicles in Each Junction", 
        xlab = "Junction Type", ylab = "Number of Vehicles")
# There are a lot of extreme outliers in all 4 junctions
# Use log transformation on the Vehicles column to reduce the impact of extreme values
df$logVehicles <- log(df$Vehicles)
summary(df)
# Split the data set into data frames for each of the junctions after log transformation
df1 <- df[df$Junction == 1, ] #Junction 1
df2 <- df[df$Junction == 2, ] #Junction 2
df3 <- df[df$Junction == 3, ] #Junction 3
df4 <- df[df$Junction == 4, ] #Junction 4
# See the distribution of the number of vehicles in each junction after log transformation
hist(df1$logVehicles, main = "Histogram of Vehicles in Junction 1", xlab = "Log Number of Vehicles")
hist(df2$logVehicles, main = "Histogram of Vehicles in Junction 2", xlab = "Log Number of Vehicles")
hist(df3$logVehicles, main = "Histogram of Vehicles in Junction 3", xlab = "Log Number of Vehicles")
hist(df4$logVehicles, main = "Histogram of Vehicles in Junction 4", xlab = "Log Number of Vehicles")
# The distribution of vehicles in each junction is now normal
# See the distribution of vehicles at every hour at each junction
boxplot(Vehicles ~ Hour, data = df1, main = "Boxplot of Vehicles at Every Hour at Junction 1",
        xlab = "Hour", ylab = "Number of Vehicles")
boxplot(Vehicles ~ Hour, data = df2, main = "Boxplot of Vehicles at Every Hour at Junction 2",
        xlab = "Hour", ylab = "Number of Vehicles")
boxplot(Vehicles ~ Hour, data = df3, main = "Boxplot of Vehicles at Every Hour at Junction 3",
        xlab = "Hour", ylab = "Number of Vehicles")
boxplot(Vehicles ~ Hour, data = df4, main = "Boxplot of Vehicles at Every Hour at Junction 4",
        xlab = "Hour", ylab = "Number of Vehicles")
# See the distribution of ID in each junction
hist(df1$ID, main = "Histogram of ID in Junction 1", xlab = "ID")
hist(df2$ID, main = "Histogram of ID in Junction 2", xlab = "ID")
hist(df3$ID, main = "Histogram of ID in Junction 3", xlab = "ID")
hist(df4$ID, main = "Histogram of ID in Junction 4", xlab = "ID")
# Check for outliers in number of vehicles in each junction after log transformation
boxplot(logVehicles ~ Junction, data = df, main = "Boxplot of logVehicles in Each Junction", 
        xlab = "Junction Type", ylab = "Log Number of Vehicles")
# There are fewer extreme outliers in all 4 junctions now

### Check for linearity between number of vehicles (dependent variable) and independent variable ###
plot(df1$logVehicles ~ df1$DateTime, xlab = "Date and Time", ylab = "Log Number of Vehicles", 
     main = "Junction 1")
plot(df2$logVehicles ~ df2$DateTime, xlab = "Date and Time", ylab = "Log Number of Vehicles", 
     main = "Junction 2")
plot(df3$logVehicles ~ df3$DateTime, xlab = "Date and Time", ylab = "Log Number of Vehicles", 
     main = "Junction 3")
plot(df4$logVehicles ~ df4$DateTime, xlab = "Date and Time", ylab = "Log Number of Vehicles", 
     main = "Junction 4")
plot(df$logVehicles ~ df$Junction, xlab = "Junction Type", ylab = "Log Number of Vehicles",
     main = "Number of Vehicles in Each Junction")
# What is the highest number of vehicles in each junction?
max(df1$Vehicles) #156 in junction 1
max(df2$Vehicles) #48 in junction 2
max(df3$Vehicles) #180 in junction 3
max(df4$Vehicles) #36 in junction 4

### Convert Junction variable into a categorical variable ###
df$JunctionFactor <- as.factor(df$Junction)
summary(df)

### Model selection ###
# Build a linear regression model without interaction terms
variables.no.interactions <- c("Month", "Day", "Hour", "JunctionFactor", "ID", "logVehicles")
df.no.interactions <- df[, (names(df) %in% variables.no.interactions)]
summary(df.no.interactions)
lm <- lm(logVehicles ~ ., data = df.no.interactions)
summary(lm)
# Use the linear regression model without interaction terms to predict number of vehicles
df.no.interactions$logVehiclesFitted <- fitted(lm)
df.no.interactions$VehiclesFitted <- exp(df.no.interactions$logVehiclesFitted)
df.no.interactions$Vehicles <- df$Vehicles
df.no.interactions$Residuals <- df.no.interactions$Vehicles - df.no.interactions$VehiclesFitted
summary(df.no.interactions)
hist(df.no.interactions$Residuals) 
# Residuals are normally distributed, with a lot of values distributed around 0, suggesting that the 
# prediction was accurate.
# Build a linear regression model with interaction terms
variables.interactions <- c("Month", "Day", "Hour", "JunctionFactor", "ID", "logVehicles")
df.interactions <- df[, (names(df) %in% variables.interactions)]
summary(df.interactions)
lm.interactions <- lm(logVehicles ~ .^2, data = df.interactions)
summary(lm.interactions)
# Use the linear regression model with interaction terms to predict number of vehicles
df.interactions$logVehiclesFitted <- fitted(lm.interactions)
df.interactions$VehiclesFitted <- exp(df.interactions$logVehiclesFitted)
df.interactions$Vehicles <- df$Vehicles
df.interactions$Residuals <- df.interactions$Vehicles - df.interactions$VehiclesFitted
summary(df.interactions)
hist(df.interactions$Residuals)
# Residuals are normally distributed, with a lot of values distributed around 0, suggesting that the 
# prediction was accurate.
summary(df.no.interactions)
summary(df.interactions)
# Adjusted R-squared of lm is 0.8074, while the adjusted R-squared of lm.interactions is 0.8678.
# Although the linear regression model with interaction terms has a higher adjusted R-squared than the 
# linear regression model without interaction terms, the model without interaction terms is more
# appropriate to use for prediction because the test data set only covers July-October 2017. Many
# interaction terms that involves the months other than July-October in the past would not be relevant
# for the test data set.

### Predict the traffic patterns in the test data set ###
# The test data set has been edited in Excel to extract the year, month, day, and hour from the 
# DateTime column of the original test date set.
# Read the test data set into R
df.test <- read.csv("test_BdBKkAj.csv")
summary(df.test)
# Convert Junction variable into factors
df.test$JunctionFactor <- as.factor(df.test$Junction)
summary(df.test)
# Split DateTime into separate columns
df.test$Month <- format(as.Date(df.test$DateTime), "%m")
df.test$Day <- format(as.Date(df.test$DateTime), "%d")
df.test$Time <- strptime(df.test$DateTime, "%Y-%m-%d %H:%M:%S")
df.test$Hour <- format(df.test$Time, "%H")
head(df.test, 10)
# Predict the traffic patterns
df.test$logVehiclesPredicted <- predict(lm, new = df.test)
df.test$VehiclesPredicted <- exp(df.test$logVehiclesPredicted)
summary(df.test)

### Create the submission file
variables.submit <- c("ID", "VehiclesPredicted")
df.submit <- df.test[, (names(df.test) %in% variables.submit)]
df.submit$Vehicles <- round(df.submit$VehiclesPredicted)
variables.submit.final <- c("ID", "Vehicles")
df.submit.final <- df.submit[, (names(df.submit) %in% variables.submit.final)]
summary(df.submit.final)
head(df.submit.final)
write.csv(df.submit.final, file = "Version 2 - Output.csv")