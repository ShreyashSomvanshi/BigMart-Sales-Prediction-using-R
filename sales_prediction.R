# Big Mart Sales Prediction using R

# Loading Packages
library(data.table) # used for reading and manipulation of data
library(dplyr ) # used for data manipulation and joining
library(ggplot2) # used for plotting
library(caret) # used for modelling
library(corrplot) # used for making correlation plot
library(xgboost) # used for building xgboost model
library(cowplot) # used for combining multiple plots

# Read Datasets
train = fread("/home/shreyash/R/BigMart Sales Prediction AnalyticsVidhya/train_v9rqX0R.csv")
test = fread("/home/shreyash/R/BigMart Sales Prediction AnalyticsVidhya/test_AbJTz2l.csv")
submission = fread("/home/shreyash/R/BigMart Sales Prediction AnalyticsVidhya/sample_submission_8RXa3c6.csv")

# Check Dimensions of our data
dim(train)
dim(test)

# Feature names of datasets
names(train)
names(test)

# Structure of Data: str() gives summary of all features present in dataframe.
str(train)
str(test)

# Combine Train and Test

test[, Item_Outlet_Sales := NA]
combi = rbind(train, test) #combining train and test datasets
dim(combi)

# Exploratory Data Analysis(EDA) : 1.)Univariate Analysis     2.)Bivariate Analysis

# 1. Univariate Analysis: It involves exploring variables individually
# target variable visualizing:
ggplot(train)+geom_histogram(aes(train$Item_Outlet_Sales), binwidth = 100, fill = "darkgreen") + xlab("Item_Outlet_Sales")
# Its a right skewed variable and need some data transformations to ttreat its skewness

#independent variables(numeric) visualizing:
p1 = ggplot(combi)+geom_histogram(aes(Item_Weight), binwidth = 0.5, fill = "blue")
p2 = ggplot(combi)+geom_histogram(aes(Item_Visibility), binwidth = 0.005, fill = "blue")
p3 = ggplot(combi)+geom_histogram(aes(Item_MRP), binwidth = 1, fill = "blue")
plot_grid(p1, p2, p3, nrow = 1) # from cowplot package
#Insights from these visualizations
#  1. There seems to be no clear cut pattern in Item_Weight
#  2. Item_Visibility is right skewed and should be transformed to curb its skewness
#  3. We can clearly see 4 different distributions for Item_MRP. Its interesting.

#independent variables(Categorical) visualizing:
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")
# In fig, 'LF','low fat', 'Low Fat', are the same category and can be combined into one. 
# Similarly we can combine 'reg' and 'Regular'
# after combining we'll plot the same fig again
combi$Item_Fat_Content[combi$Item_Fat_Content=="LF"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="low fat"] = "Low Fat"
combi$Item_Fat_Content[combi$Item_Fat_Content=="reg"] = "Regular"
ggplot(combi %>% group_by(Item_Fat_Content) %>% summarise(Count = n())) + geom_bar(aes(Item_Fat_Content, Count), stat = "identity", fill = "coral1")

# now check other categorical variables
# plot for Item_Type 
p4 = ggplot(combi %>% group_by(Item_Type) %>% summarise(Count = n())) + geom_bar(aes(Item_Type, Count), stat = "identity", fill = "coral1") + xlab("")+geom_label(aes(Item_Type, Count, label = Count), vjust = 0.5)+theme(axis.text.x = element_text(angle = 45, hjust = 1))+ggtitle("Item_Type")

#plot for Outlet_Identifier
p5 = ggplot(combi %>% group_by(Outlet_Identifier) %>% summarise(Count = n())) + geom_bar(aes(Outlet_Identifier, Count), stat = "identity", fill = "coral1") + geom_label(aes(Outlet_Identifier, Count, label = Count), vjust = 0.5)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

# plot for Outlet_Size
p6 = ggplot(combi %>% group_by(Outlet_Size) %>% summarise(Count = n())) + geom_bar(aes(Outlet_Size, Count), stat = "identity", fill = "coral1") + geom_label(aes(Outlet_Size, Count, label = Count), vjust = 0.5)+theme(axis.text.x = element_text(angle = 45, hjust = 1))

second_row = plot_grid(p5, p6, nrow = 1)
plot_grid(p4, second_row, ncol = 1)
# In Outlet_Size's plot for 4016 observations Outlet_Size is blank or missing. We will check for this in bivariate analysis to 
# substitute th missing values in Outlet_Size

# We'll check for remaining categorical variables

# Plot for Outlet_Establishment_Year
p7 = ggplot(combi %>% group_by(Outlet_Establishment_Year) %>% summarise(Count = n())) + geom_bar(aes(factor(Outlet_Establishment_Year), Count), stat = "identity", fill = "coral1") + geom_label(aes(factor(Outlet_Establishment_Year), Count, label = Count), vjust = 0.5) + xlab("Outlet_Establishment_Year") + theme(axis.text.x = element_text(size = 8.5))

# Plot for Outlet_Type
p8 = ggplot(combi %>% group_by(Outlet_Type) %>% summarise(Count = n())) + geom_bar(aes(Outlet_Type, Count), stat = "identity", fill = "coral1") + geom_label(aes(factor(Outlet_Type), Count, label = Count), vjust = 0.5) + theme(axis.text.x = element_text(size = 8.5))

# Plotting both graphs together
plot_grid(p7, p8, ncol = 2)

# Observations from this fig:
# 1. Less no. of observations in data for outlets established in 1981 as compared to others
# 2. Supermarket Type1 was most popular

# 2. Bivariate Analysis: It involves exploring variables individually

