####################################
# File: TA Problem Set 1 Guiding Solution
# Course: Machine Learning for BI 1 
# TA: Camille Pedersen
# Aarhus University, Fall 2022
####################################


### 1. INSTALLING PACKAGES INTO R
# 1.1 
install.packages("caret")
install.packages("e1071")
# ...
# Or, all together
install.packages("caret", "e1071", "caTools", "dplyr", "tidyverse")

library(caret)
library(e1071)
library(caTools)
library(dplyr)
library(tidyverse)

### 2. IMPORTING DATA INTO R
# 2.1 
setwd("~/Documents/Phd TA og undervisning/Machine Learning for BI/Tutorials/Tutorial 1")

# 2.2
# save WestRoxbury.csv file in the same folder as your working directory
housing.df <- read.csv("~/Documents/Phd TA og undervisning/Machine Learning for BI/Tutorials/Tutorial 1/WestRoxbury.csv")


### 3. BASIC DATA EXPLORATION
# 3.1
dim(housing.df)  # find the dimension of data frame
head(housing.df)  # show the first six rows
View(housing.df)  # show all the data in a new tab

# 3.2
housing.df[1:10, 1]  # show the first 10 rows of the first column only

# 3.3
housing.df[1:10, ]  # show the first 10 rows of each of the columns 

# 3.4
housing.df[5, 1:10]  # show the fifth row of the first 10 columns

# 3.5
housing.df[, 1]  # show the whole first column
housing.df$TOTAL.VALUE  # a different way to show the whole first column

# 3,6
length(housing.df$TOTAL.VALUE)  # find the length of the first column

# 3.7
mean(housing.df$TOTAL.VALUE)  # find the mean of the first column

# 3.8
summary(housing.df)  # find summary statistics for each column     

# 3.9
s <- sample(row.names(housing.df), 5) # random sample of 5 observations
s
housing.df[s,]

# 3.10
names(housing.df)

# 3.11
t(t(names(housing.df)))  

# 3.12
colnames(housing.df)[1] <- c("TOTAL.VALUE.NEW")
t(t(names(housing.df))) #Take a look at the new name for the column 

# 3.13
str(housing.df)
# Notice that REMODEL is the only factor variable.

# 3.14
housing.df$REMODEL= as.factor(housing.df$REMODEL)
levels(housing.df$REMODEL)


# 3.15
# If the data consists of numbers incl. decimals, then we call it NUMERIC DATA. 
# If the data consists only of whole numbers, it is called as INTEGER.
# A FACTOR refers to a statistical data type used to store categorical variables. 
# Categorical variables belong to a limited number of categories (e.g., women/male, grades A, B, C, D, E, F, etc.). 
# Continuous variables, on the other hand, can correspond to an infinite number of values.

# 3.16
# You can export the data from R into a variety of forms: 
# a tab-delimited file using write.table(), as .csv file using write.csv (), e.g.,
write.csv(housing.df, "housing.df_new.csv")



### DIVIDING A DATAFILE
# 4.2
# Random split using rsample package
library(rsample)
set.seed(123)  # for reproducibility
split_1  <- initial_split(housing.df, prop = 0.6)
train <- training(split_1)
test <- testing(split_1)

# Or use an alternative way to split into a training and test set.


### MERGING TWO DATAFILES
# 5.1
total <- rbind(train, test)


