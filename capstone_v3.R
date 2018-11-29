source("utils.R")

# Load datasets
#install.packages("data.table")
library(data.table)

train.data <- fread("param_data_train.csv", header = TRUE)
head(train.data)
test.data <- fread("param_data_test.csv", header = TRUE)
head(test.data)
###########################################################################################
###################################Data Modification#######################################

#########################################
########## processing raw data ##########

# Remove blank columns
train.data <- train.data[,(13:15):= NULL]

# Change columns' name
train.data = renameTrainDataCols(train.data)
test.data = renameTestDataCols(test.data)

# Check the unique items in dataset
sapply(train.data, function(x) unique(x))
sapply(test.data, function(x) unique(x))

# convert all missing values into NAs in train and test data
train.data$MixProportion = ifelse(train.data$MixProportion=="", NA, train.data$MixProportion)
test.data$MixProportion = ifelse(test.data$MixProportion=="", NA, test.data$MixProportion)

# Convert all categorical columns into factors and create dummy variables
# for training data set
train.data$MaterialA = labelMaterialA(train.data$MaterialA)
train.data$MaterialB = labelMaterialB(train.data$MaterialB)
train.data$BrandName = labelBrandName(train.data$BrandName)
train.data$MaterialSize = labelMaterialSize(train.data$MaterialSize)
train.data$ProductNo <- as.factor(train.data$ProductNo)
train.data$MixProportion <- as.factor(train.data$MixProportion)
# for testing data set
test.data$MaterialA = labelMaterialA(test.data$MaterialA)
test.data$MaterialB = labelMaterialB(test.data$MaterialB)
test.data$BrandName = labelBrandName(test.data$BrandName)
test.data$MaterialSize = labelMaterialSize(test.data$MaterialSize)
test.data$ProductNo <- as.factor(test.data$ProductNo)
test.data$MixProportion <- as.factor(test.data$MixProportion)

# Verify data
unique(train.data$MaterialA)
unique(train.data$MaterialB)
unique(train.data$BrandName)
unique(train.data$MaterialSize)
unique(train.data$MixProportion)

unique(test.data$MaterialA)
unique(test.data$MaterialB)
unique(test.data$BrandName)
unique(test.data$MaterialSize)
unique(test.data$MixProportion)

str(train.data)
str(test.data)


########### processing raw data ##########
##########################################

##########################################
########## handle missing values #########

# Use ggplot_missing funtion to map missing values
#install.packages("reshape2")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("missForest")
library(reshape2)
library(dplyr)
library(ggplot2)
library(missForest)

# Map mising values using the function
ggplotMissingData(train.data)
sapply(train.data, function(x) sum(is.na(x)))
ggplotMissingData(test.data)
sapply(test.data,function(x) sum(is.na(x)))

# Imputation for missing values
new.train.data <- imputeMissingValues(train.data)
sapply(new.train.data, function(x) sum(is.na(x))) # recheck missing values
new.test.data <- imputeMissingValues(test.data)
sapply(new.test.data, function(x) sum(is.na(x))) # recheck missing values

# Visualization for numberic features
valName <- names(new.train.data)
drawHistogram(new.train.data$Param1, valName[6])
drawHistogram(new.train.data$Param2, valName[8])
drawHistogram(new.train.data$Param3, valName[9])
drawHistogram(new.train.data$Param4, valName[10])
drawHistogram(new.train.data$Param5, valName[11])

install.packages("psych")
library(psych)
cor(new.train.data[c("Param1", "Param2", "Param3", "Param4", "Param5")])
pairs.panels(new.train.data[c("Param1", "Param2", "Param3", "Param4", "Param5")])

# map mising values using the function
ggplotMissingData(new.train.data)
ggplotMissingData(new.test.data)

########## handle missing values ##########
###########################################

###################################Data Modification#######################################
###########################################################################################


###########################################################################################
################################### Data Preparation ######################################

# Process unmatched features between train and test sets
new.train.data1 <- featureMatch(new.train.data)
new.test.data1 <- featureMatch(new.test.data)

# create dummies variables for categorical attributes
processed.train = processNominalVars(new.train.data1)
scaled.train = normalizeData(processed.train)
summary(scaled.train)

processed.test = processNominalVars(new.test.data1)
scaled.test = normalizeData(processed.test)
summary(scaled.test)


# PARTITION FOR TRAINING DATA ONLY
# Split the training data into training set and testing set
# install.packages("caret")
library(caret)
# Set random seed for replication
set.seed(200)
# Lets do stratified sampling. Select rows to based on Class variable as strata
TrainingDataIndex <- createDataPartition(scaled.train$Label, p=0.75, list=FALSE)
# Create Training Data as subset 
splited.train1 <- scaled.train[TrainingDataIndex,]
# Everything else not in training is test data. 
splited.train2 <- scaled.train[-TrainingDataIndex,]
# END: PARTITION FOR TRAINING DATA

# Reduce dimension by performing PCA
nomvars <- c(1, 7:26)
colnames(splited.train1[,-nomvars])
splited.train1.pca <- prcomp(splited.train1[,-nomvars], center = TRUE, scale. = TRUE)

summary(splited.train1.pca)

#install.packages("devtools")
library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)
ggbiplot(splited.train1.pca)

##################################### Data Preparation ######################################
#############################################################################################

#############################################################################################
##################################### Neural Network ########################################

# Neural Network Model with h2o method
# install.packages("h2o")
library(h2o)

# Start up a 8-node H2O server on local machine, 
# and allow it to use all CPU cores and up to 2GB of memory:
h2o.init(nthreads=8, max_mem_size="2G")
h2o.removeAll() ## clean slate - just in case the cluster was already running

# split train data for validation
train.hex <- as.h2o(splited.train1)
test.hex <- as.h2o(splited.train2)

splits <- h2o.splitFrame(train.hex, 0.8, seed=777)
split.train  <- h2o.assign(splits[[1]], "train.hex") # 80%
split.valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%

dl.model <- h2o.deeplearning(x=2:23,
                             y="Label",
                             training_frame=split.train,
                             validation_frame=split.valid,
                             activation = "Tanh", 
                             hidden = c(200,200),
                             variable_importances=T)
summary(dl.model)
plot(dl.model)

dl.model.predict <- h2o.predict(dl.model, test.hex)
dl.result <- as.data.frame(dl.model.predict)
dl.result

# Measure performance of H20 DL model
perf <- h2o.performance(dl.model, test.hex)
h2o.confusionMatrix(perf)

h2o.shutdown()

# examine the dl.result
summary(dl.result)
#### Explanation
# p0 is the probability that 0 is chosen.
# p1 is the probability that 1 is chosen.
# predict: is made by applying a threshold to p1

# List the important variables
head(as.data.frame(h2o.varimp(dl.model)))

# Confusion Matrix
# install.packages("gmodels")
library(gmodels)
CrossTable(splited.train2$Label, dl.result$predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Labels', 'predicted Labels'))
# accuracy
table.NN <- table(splited.train2$Label, dl.result$predict)
nn.accuracy = round(sum(diag(table.NN)/sum(table.NN)),digits=5)
nn.accuracy

##################################### Neural Network ########################################
#############################################################################################

#############################################################################################
###################################### Naive Bayes ##########################################

splited.train1$Label <- factor(splited.train1$Label)
splited.train2$Label <- factor(splited.train2$Label)

# install.packages("e1071")
library(e1071)
nb.classifier <- naiveBayes(splited.train1, splited.train1$Label)

nb.predict <- predict(nb.classifier, splited.train2)
head(nb.predict)

# Confusion Matrix
library(gmodels)
CrossTable(nb.predict, splited.train2$Label,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# accuracy
table.NB <- table(splited.train2$Label, nb.predict)
nb.accuracy = round(sum(diag(table.NB)/sum(table.NB)),digits=5)
nb.accuracy

###################################### Naive Bayes ##########################################
#############################################################################################

#############################################################################################
#################################### Decision Tree ##########################################

# Modeling data with decision tree using c50
# install.packages("C50")
library(C50)
dt.classifier <- C5.0(splited.train1[-1], splited.train1$Label)

# generate predictions for the testing dataset
dt.predict <- predict(dt.classifier, splited.train2)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(splited.train2$Label, dt.predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

# accuracy
table.DT <- table(splited.train2$Label, dt.predict)
dt.accuracy = round(sum(diag(table.DT)/sum(table.DT)),digits=5)
dt.accuracy

#################################### Decision Tree ##########################################
#############################################################################################

#############################################################################################
######################################## SVM ################################################
# modeling the data with svm() 
library(e1071)
svm.classifier <- svm(Label~.,data=splited.train1, scale=FALSE)

# generate predictions for the testing dataset
svm.predict <- predict(svm.classifier, splited.train2)
summary(svm.predict)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(splited.train2$Label, svm.predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

# accuracy
table.svm <- table(splited.train2$Label, svm.predict)
svm.accuracy = round(sum(diag(table.svm)/sum(table.svm)),digits=5)
svm.accuracy
######################################## SVM ################################################
#############################################################################################

# Table to compare each algorithms' results
com.table <- matrix(c('Neural Network', 'Naive Bayes', 'Decision Tree', 'SVM',
                      nn.accuracy, nb.accuracy, dt.accuracy, svm.accuracy), ncol=4, byrow=TRUE)
com.table


#################################################################
#                                                               #
#                       Apply on test data                      #                                                                                                                          
#                                                               #
#################################################################

##########################################################################################
######################### Apply Naive Bayes algorithm ####################################

library(e1071)
nb.classifier.data <- naiveBayes(Label~., data=scaled.train)

nb.predict.data <- predict(nb.classifier.data, scaled.test, type="class")
summary(nb.predict.data)

# Percentage of good quality prediction
length(which(nb.predict.data=="1"))*100/length(nb.predict.data)

#########################################################################################
######################### Apply SVM algorithm ###########################################
# modeling the data with svm() 
library(e1071)
svm.classifier.data <- svm(Label~.,data=scaled.train, scale=FALSE)

# generate predictions for the testing dataset
svm.predict.data <- predict(svm.classifier.data, scaled.test)
summary(svm.predict.data)

# Percentage of good quality prediction
length(which(svm.predict.data=="1"))*100/length(svm.predict.data)

########################################################################################
######################### Apply Decision Tree algorithm ################################
# Modeling data with decision tree using c50
library(C50)
dt.classifier.data <- C5.0(scaled.train[-1], scaled.train$Label)

# generate predictions for the testing dataset
dt.predict.data <- predict(dt.classifier.data, scaled.test)
summary(dt.predict.data)

# Percentage of good quality prediction
length(which(dt.predict.data=="1"))*100/length(dt.predict.data)

##########################################################################################
######################### Apply Deep Learning Neural Network H2O #########################
library(h2o)
h2o.init(nthreads=8, max_mem_size="2G")
h2o.removeAll() ## clean slate - just in case the cluster was already running
h2o.init()

# split train data for validation
train.hex <- as.h2o(scaled.train)
test.hex <- as.h2o(scaled.test)

splits <- h2o.splitFrame(train.hex, 0.8, seed=777)
split.train  <- h2o.assign(splits[[1]], "train.hex") # 80%
split.valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%

dl.model <- h2o.deeplearning(x=2:26,
                             y="Label",
                             training_frame=split.train,
                             validation_frame=split.valid,
                             activation = "Tanh", 
                             hidden = c(200,200),
                             variable_importances=T)
summary(dl.model)
plot(dl.model)

dl.model.predict.data <- h2o.predict(dl.model, test.hex)
dl.result.data <- as.data.frame(dl.model.predict.data)
h2o.shutdown()

# examine the dl.result
summary(dl.result.data)

# Percentage of good quality prediction
length(which(dl.result.data=="1"))*100/length(dt.predict.data)