### ******************************************* R Project ************************************************* ###
# Group Memebers: Teo Boon Long (WQD180085), Law Chen Cha (WQD180087), Xavier Tay Yi Han (WQD180109), Tan Chee Siang (WQD180117)
# Project Title : Analysing Census-Income of countries

#### ********** Details of Dataset ********** ####

# loading the required library
install.packages("dplyr")
library(dplyr)
library(readr)

# loading dataset into R
data  <- read_csv("adult.data", col_names = FALSE)

#data = adult
# assign column name to each column
names(data) = c("age","workclass","fnlwgt","education","education_num","marital_status","occupation","relationship",
                "race","sex","capital_gain","capital_loss","hours_per_week","native_country","income_group")

write.csv(data,"raw_data.csv")

# checking the whole dataset, dimension, structure, summary, first 6 and last 6 observations of dataset
View(data)
dim(data)
str(data)
summary(data)
head(data)
tail(data)

#### ********** Data Cleaning ********** ####
# remove insignificant column
data = data[c(-3,-5)]
View(data)
str(data)

#--------- Imputation ---------#
# table each of the attribute and sort to find missing values
# impute missing values using mode of the column of attribute
age_table = sort(table(data$age))
age_table

workclass_table = sort(table(data$workclass),decreasing = T)
workclass_table
workclassUnknown = which(data$workclass == "?")
workclassUnknown
data$workclass[workclassUnknown] = names(workclass_table[1])
workclass_table = sort(table(data$workclass),decreasing = T)
workclass_table

education_table = sort(table(data$education),decreasing = T)
education_table

marital_table = sort(table(data$marital_status),decreasing = T)
marital_table

occupation_table = sort(table(data$occupation),decreasing = T)
occupation_table
occupationUnknown = which(data$occupation == "?")
occupationUnknown
data$occupation[occupationUnknown] = names(occupation_table[1])
occupation_table = sort(table(data$occupation),decreasing = T)
occupation_table

relationship_table = sort(table(data$relationship),decreasing = T)
relationship_table

race_table = sort(table(data$race),decreasing = T)
race_table

sex_table = sort(table(data$sex),decreasing = T)
sex_table

capitalGain_table = sort(table(data$capital_gain),decreasing = T)
capitalGain_table

capitalLoss_table = sort(table(data$capital_loss),decreasing = T)
capitalLoss_table

hourPerWeek_table = sort(table(data$hours_per_week),decreasing = T)
hourPerWeek_table

country_table = sort(table(data$native_country),decreasing = T)
country_table
countryUnknown = which(data$native_country == "?")
countryUnknown
data$native_country[countryUnknown] = names(country_table[1])
country_table = sort(table(data$native_country),decreasing = T)
country_table

incomeGroup_table = sort(table(data$income_group),decreasing = T)
incomeGroup_table
#--------- End of Imputation ---------#

# plotting a new boxplot to detect outliers
boxplot(data$age)
boxplot(data$capital_gain)
boxplot(data$capital_loss)
boxplot(data$hours_per_week)

# adding one new column 'ethnicity' to replace race and sex column attribute
data <- data %>% mutate(ethnicity=paste(race,sex))
data = data[c(-7,-8)]
View(data)
#re-arrange the order of each column attribute
data = data[,c(11,10,12,2,3,4:6,1,7:9)]
str(data)

#### ********** End of Data Cleaning ********** ####

#### ********** Data Exploration ********** ####
# exporting the clean dataset to 'data_clean.csv'
write.csv(data,"data_clean.csv")
# importing the clean dataset and view the structure of dataset
data <- read.csv("data_clean.csv", row.names = 1)
View(data)
str(data)

# install required package and library
install.packages("tidyverse")
install.packages("funModeling")
install.packages("Hmisc")
install.packages("corrplot")
install.packages("plotly")
library(funModeling)
library(tidyverse) 
library(Hmisc)
library(corrplot)
library(ggplot2)
library(plotly)

# exploring all variable and plot graph pattern of dataset
View(data)

glimpse(data)
df_status(data)
freq(data)
profiling_num(data)
plot_num(data) 
describe(data)

# Interactive graph representation
#g1<-ggplot(data, aes(workclass, capital_gain, color=factor(income_group)))+geom_point()
#g2<-ggplot(data, aes(ethnicity, capital_gain, color=factor(income_group)))+geom_point()

# See both in the same screen
#p=subplot( ggplotly(g1), ggplotly(g2) )
#p

#### ********** End of Data Exploration ********** ####

#### **********Correlation Analysis********** ####

# encoding variables
names(incomeGroup_table)
names(country_table)
names(table(data$ethnicity))
names(workclass_table)
names(education_table)
names(marital_table)
names(occupation_table)
names(relationship_table)
data$income_group = factor(data$income_group,
                       levels = c("<=50K",">50K"),
                       labels = c(1,2))
data$native_country = factor(data$native_country,
                            levels = names(country_table),
                            labels = c(1:41))
data$ethnicity = factor(data$ethnicity,
                            levels = names(table(data$ethnicity)),
                            labels = c(1:10))
data$workclass = factor(data$workclass,
                            levels = names(workclass_table),
                            labels = c(1:8))
data$education = factor(data$education,
                        levels = c("Preschool","1st-4th","5th-6th","7th-8th",
                                   "9th","10th","11th","12th",
                                   "HS-grad","Some-college","Assoc-voc",
                                   "Assoc-acdm","Bachelors","Masters",
                                   "Prof-school","Doctorate"),
                        labels = c(1:16))
data$marital_status = factor(data$marital_status,
                            levels = names(marital_table),
                            labels = c(1:7))
data$occupation = factor(data$occupation,
                            levels = names(occupation_table),
                            labels = c(1:14))
data$relationship = factor(data$relationship,
                            levels = names(relationship_table),
                            labels = c(1:6))
str(data)
View(data)

#convert column attributes from income group to relationship into integer
data = mutate_each(data,funs(as.integer),data$income_group:data$relationship)
str(data)

#correlation analysis
library(GoodmanKruskal)
data1 <- data[,1:8]
GKmatrix1<- GKtauDataframe(data1)
GKmatrix1
plot(GKmatrix1, corrColors = "blue")

data2 <- data[,c(1,9:12)]
cor_matrix = cor(data2)
cor_matrix
corrplot(cor_matrix)
#### ********** End of Correlation Analysis ********** ####

#### ********** Modeling ********** ####
# levels the objective function income group
data$income_group <- factor(data$income_group,
                 levels = c(1,2),
                 labels = c(0,1))

# splitting the data into training and testing set with a ratio of 0.8 with 0.2
library(caTools)
set.seed(123)
split = sample.split(data$income_group,SplitRatio = 0.8)
training_set = subset(data,split==TRUE)
testing_set = subset(data,split==FALSE)

# scaling feature
str(data)
training_set[c(9:12)] = scale(training_set[c(9:12)])
testing_set[c(9:12)] = scale(testing_set[c(9:12)])

#--------- Machine Learning ---------#
# 1. training a classifier that fit into each machine learning
# 2. predicting testing set result using each machine learning classifier
# Naive Bayes
install.packages('e1071')
library('e1071')
naivebayes = naiveBayes(x=training_set[-1],y=training_set$income_group)
predictive_nb = predict(naivebayes,newdata = testing_set[-1])

# Decision Trees
install.packages('rpart')
library(rpart)
decision_tree = rpart(formula = income_group~.,data = training_set)
predictive_dt = predict(decision_tree, newdata = testing_set[-1],type='class')

# Random Forest
install.packages("randomForest")
library(randomForest)
set.seed(123)
randomForest = randomForest(x=training_set[-1],y=training_set$income_group,ntree=50)
predictive_rf = predict(randomForest,newdata = testing_set[-1])
#predictive_rf

# Logistic Regression

glmfit <- glm(income_group~., data=training_set, family=binomial)
predictive_LR = round(predict(glmfit, newdata = testing_set[-1],type="response"))
predictive_LR <-as.factor(predictive_LR)
#predictive_LR

# Making confusion matrix for each algorithm
#install.packages("caret")
library(caret)
x = testing_set$income_group
#x

cmNaiveBayes = confusionMatrix(predictive_nb,x)
cmRandomForest = confusionMatrix(predictive_rf,x)
cmDecisionTree = confusionMatrix(predictive_dt,x)
cmDecisionLogistic = confusionMatrix(predictive_LR,x)
#predictive_LR <-as.factor(predictive_LR)

# Study the accuracy of each machine learning
accuracy <- rbind(Naive_Bayes = cmNaiveBayes$overall["Accuracy"],
                  Decision_Tree = cmDecisionTree$overall["Accuracy"],
                  Random_Forest = cmRandomForest$overall["Accuracy"],
                  LogisticRegression = cmDecisionLogistic$overall["Accuracy"])
#accuracy


# Study the precision of each machine learning
precision <- rbind(Naive_Bayes = cmNaiveBayes$byClass["Precision"],
                   Decision_Tree = cmDecisionTree$byClass["Precision"],
                   Random_Forest = cmRandomForest$byClass["Precision"],
                   LogisticRegression = cmDecisionLogistic$byClass["Precision"])
#precision

# Study the recall of each machine learning
recall <- rbind(Naive_Bayes = cmNaiveBayes$byClass["Recall"],
                Decision_Tree = cmDecisionTree$byClass["Recall"],
                Random_Forest = cmRandomForest$byClass["Recall"],
                LogisticRegression = cmDecisionLogistic$byClass["Recall"])
#recall

# Study the F1-score of each machine learning
F1 <- rbind(Naive_Bayes = cmNaiveBayes$byClass["F1"],
            Decision_Tree = cmDecisionTree$byClass["F1"],
            Random_Forest = cmRandomForest$byClass["F1"],
            LogisticRegression = cmDecisionLogistic$byClass["F1"])
#F1

# Combining all performance matrix by columns
cbind(accuracy,precision,recall,F1)

#--------- End of Machine Learning ---------#
#### **********End of Correlation Analysis********** ####
### ******************************************* End of R Project ************************************************* ###
