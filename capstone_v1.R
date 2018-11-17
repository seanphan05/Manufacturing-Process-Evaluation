# load dataset
install.packages("data.table")
library(data.table)
train.data <- fread("param_data_train.csv", header = TRUE)

###########################################################################################
###################################Data Modification#######################################

                      #########################################
                      ########## processing raw data ##########
# remove any blank columns
train.data <- train.data[,-c(13:15)]

# check the unique items in dataset
# in train data
unique(train.data$param1)
unique(train.data$param2)
unique(train.data$param3)
unique(train.data$param4)
unique(train.data$param5)
unique(train.data$param6)
unique(train.data$param7)
unique(train.data$param8)
unique(train.data$param9)
unique(train.data$param10)


# change parameters column' name
# in train data
names(train.data)[1]<-"ProductNo"
names(train.data)[2]<-"Labels"
names(train.data)[3]<-"MaterialsA"
names(train.data)[4]<-"MaterialsB"
names(train.data)[5]<-"BrandName"
names(train.data)[6]<-"param1"
names(train.data)[7]<-"MaterialSize"
names(train.data)[8]<-"param2"
names(train.data)[9]<-"param3"
names(train.data)[10]<-"param4"
names(train.data)[11]<-"param5"
names(train.data)[12]<-"MixProportion"


# change values of observations in train data
train.data$MaterialsA = ifelse(train.data$MaterialsA=="75a5f96063fbc3290b07b0e81c3249d0","A1",
                               ifelse(train.data$MaterialsA=="42a6854ae47630b3a32e84823d147e0b","A2",
                                      ifelse(train.data$MaterialsA=="f5c2479be5048388c45cb2e81edfbd3f","A3", "A4")))
train.data$MaterialsB = ifelse(train.data$MaterialsB=="4610065df728e0bd399446ef5fd3ea74","B1",
                               ifelse(train.data$MaterialsB=="6fc672934f2f6e2a64898efd18c24111","B2",
                                      ifelse(train.data$MaterialsB=="0e05731278221a3ac6ebaa1d795b6177","B3", 
                                             ifelse(train.data$MaterialsB=="fe68fc884ca9961c9e8ec90e7fc5ec9d","B4",
                                                    ifelse(train.data$MaterialsB=="e3d6228b4bb426d6d7004613eb04e124","B5",
                                                           ifelse(train.data$MaterialsB=="5437f1e2e5cf83c784e5dba00ee42e7a","B6",
                                                                  ifelse(train.data$MaterialsB=="9e617be991132586229e9964b8bf4463","B7","B8")))))))
train.data$BrandName = ifelse(train.data$BrandName=="29f4d775d7fd37f40a72f66f39b7453f","BrandName1","BrandName2")
train.data$MaterialSize = ifelse(train.data$MaterialSize=="0.115*600",0.115*600,
                                 ifelse(train.data$MaterialSize=="0.115*720",0.115*720,
                                        ifelse(train.data$MaterialSize=="0.115*580",0.115*580,
                                               ifelse(train.data$MaterialSize=="0.115*620",0.115*620,0.115*300))))


# convert all missing values into NAs in training data
train.data$MixProportion = ifelse(train.data$MixProportion=="",NA,train.data$MixProportion)

                      ########### processing raw data ##########
                      ##########################################


                      ##########################################
                      ########## handle missing values #########

# set categorical attributes as factor for training data
train.data$ProductNo <- as.factor(train.data$ProductNo)
train.data$MaterialsA <- as.factor(train.data$MaterialsA)
train.data$MaterialsB <- as.factor(train.data$MaterialsB)
train.data$BrandName <- as.factor(train.data$BrandName)
train.data$MixProportion <- as.factor(train.data$MixProportion)


# ggplot_missing funtion to map missing values
library(reshape2)
library(dplyr)
library(ggplot2)
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Present', 'Missing')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'Variables on Dataset', y = 'Rows / Observations')
}

# map mising values using the function
ggplot_missing(train.data)
sapply(train.data,function(x) sum(is.na(x)))


# imputation for missing values
#install.packages("missForest")
library(missForest)
# exclude first column due to missForest limitation of 53 different categorical variables
train.data.imp <- missForest(train.data[,-1])
train.data.imp$ximp
new.train.data <- data.frame(train.data$ProductNo,train.data.imp$ximp)
new.train.data
sapply(new.train.data,function(x) sum(is.na(x))) # recheck missing values

                      ########## handle missing values ##########
                      ###########################################

###################################Data Modification#######################################
###########################################################################################


###########################################################################################
################################### Data Preparation ######################################

# create dummies variables for categorical attributes
# for train data
add.train <- new.train.data
#convert Labels column into factor
add.train$Labels <- as.factor(add.train$Labels)
# 3 dummies variable for 4 type of materialsA
add.train$A1 <- ifelse(add.train$MaterialsA=="A1",1,0)
add.train$A2 <- ifelse(add.train$MaterialsA=="A2",1,0)
add.train$A3 <- ifelse(add.train$MaterialsA=="A3",1,0)
# 7 dummies variable for 8 type of materialsB
add.train$B1 <- ifelse(add.train$MaterialsB=="B1",1,0)
add.train$B2 <- ifelse(add.train$MaterialsB=="B2",1,0)
add.train$B3 <- ifelse(add.train$MaterialsB=="B3",1,0)
add.train$B4 <- ifelse(add.train$MaterialsB=="B4",1,0)
add.train$B5 <- ifelse(add.train$MaterialsB=="B5",1,0)
add.train$B6 <- ifelse(add.train$MaterialsB=="B6",1,0)
add.train$B7 <- ifelse(add.train$MaterialsB=="B7",1,0)
# 1 dummy variable for 2 type of BrandName
add.train$BN1 <- ifelse(add.train$BrandName=="BrandName1",1,0)
# 2 dummies variable for 3 type of MixProduction
add.train$Mix40.60 <- ifelse(add.train$MixProportion=="40-60",1,0)
add.train$Mix50.50 <- ifelse(add.train$MixProportion=="50-50",1,0)

# Nomalization
normalize <- function(x) {return((x - min(x)) / (max(x) - min(x)))}
nom.train <- lapply(add.train[,6:11],normalize)
scaled.train <- data.frame(add.train[2],nom.train,add.train[,13:25])


# PARTITION TRAINING DATA
# Split the training data into training and testing data
#install.packages("caret")
library(caret)
# Set random seed for replication
set.seed(200)
# Lets do stratified sampling. Select rows to based on Class variable as strata
TrainingDataIndex <- createDataPartition(scaled.train$Labels, p=0.75, list=FALSE)
# Create Training Data as subset 
splited.train1 <- scaled.train[TrainingDataIndex,]
# Everything else not in training is test data. 
splited.train2 <- scaled.train[-TrainingDataIndex,]
# END: PARTITION TRAINING DATA


##################################### Data Preparation ######################################
#############################################################################################


#############################################################################################
##################################### Neural Network ########################################

# Neural Network Model with h2o method
library(h2o)

# Start up a 8-node H2O server on local machine, 
# and allow it to use all CPU cores and up to 2GB of memory:
h2o.init(nthreads=8, max_mem_size="2G")
h2o.removeAll() ## clean slate - just in case the cluster was already running
h2o.init()

# split train data for validation
train.hex <- as.h2o(splited.train1)
test.hex <- as.h2o(splited.train2)

splits <- h2o.splitFrame(train.hex, 0.8, seed=777)
split.train  <- h2o.assign(splits[[1]], "train.hex") # 80%
split.valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%

dl.model <- h2o.deeplearning(x=2:20,
                             y="Labels",
                             training_frame=split.train,
                             validation_frame=split.valid,
                             activation = "Tanh", 
                             hidden = c(200,200),
                             variable_importances=T)
summary(dl.model)
plot(dl.model)

dl.model.predict <- h2o.predict(dl.model, test.hex)
dl.result <- as.data.frame(dl.model.predict)
h2o.shutdown()

# examine the dl.result
summary(dl.result)
#### Explanation
# p0 is the probability that 0 is chosen.
# p1 is the probability that 1 is chosen.
# predict: is made by applying a threshold to p1

# List the important variables
head(as.data.frame(h2o.varimp(dl.model)))

# Percentage of good quality prediction
length(dl.result$predict[dl.result$predict=="1"])*100/length(dl.result$predict)
################### Method: h2o #####################

dl.result
# Confusion Matrix
library(gmodels)
CrossTable(splited.train2$Labels, dl.result$predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual Labels', 'predicted Labels'))
# accuracy
table.NN <- table(splited.train2$Labels, dl.result$predict)
nn.accuracy = round(sum(diag(table.NN)/sum(table.NN)),digits=5)
nn.accuracy

##################################### Neural Network ########################################
#############################################################################################
#############################################################################################
###################################### Naive Bayes ##########################################

splited.train1$Labels <- factor(splited.train1$Labels)
splited.train2$Labels <- factor(splited.train2$Labels)

library(e1071)
nb.classifier <- naiveBayes(splited.train1, splited.train1$Labels)

nb.predict <- predict(nb.classifier, splited.train2)
head(nb.predict)

# Confusion Matrix
library(gmodels)
CrossTable(nb.predict, splited.train2$Labels,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

# accuracy
table.NB <- table(splited.train2$Labels, nb.predict)
nb.accuracy = round(sum(diag(table.NB)/sum(table.NB)),digits=5)
nb.accuracy

###################################### Naive Bayes ##########################################
#############################################################################################
#############################################################################################
#################################### Decision Tree ##########################################

# Modeling data with decision tree using c50
library(C50)
dt.classifier <- C5.0(splited.train1[-1], splited.train1$Labels)

# generate predictions for the testing dataset
dt.predict <- predict(dt.classifier, splited.train2)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(splited.train2$Labels, dt.predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

# accuracy
table.DT <- table(splited.train2$Labels, dt.predict)
dt.accuracy = round(sum(diag(table.DT)/sum(table.DT)),digits=5)
dt.accuracy

#################################### Decision Tree ##########################################
#############################################################################################
#############################################################################################
######################################## SVM ################################################

# modeling the data with svm() 
library(e1071)
svm.classifier <- svm(Labels~.,data=splited.train1)

# generate predictions for the testing dataset
svm.predict <- predict(svm.classifier, splited.train2)
summary(svm.predict)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(splited.train2$Labels, svm.predict,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual', 'predicted'))

# accuracy
table.svm <- table(splited.train2$Labels, svm.predict)
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
          #      Use Naive Bayes Algorithm to apply on test data          #                                                                                                                          
          #                                                               #
          #################################################################


# loading test data
library(data.table)
test.data <- fread("param_data_test.csv", header = TRUE)

# check unique in test data
unique(test.data$param1)
unique(test.data$param2)
unique(test.data$param3)
unique(test.data$param4)
unique(test.data$param5)
unique(test.data$param6)
unique(test.data$param7)
unique(test.data$param8)
unique(test.data$param9)
unique(test.data$param10)

# change parameters column' name
# in test data
names(test.data)[1]<-"ProductNo"
names(test.data)[2]<-"MaterialsA"
names(test.data)[3]<-"MaterialsB"
names(test.data)[4]<-"BrandName"
names(test.data)[5]<-"param1"
names(test.data)[6]<-"MaterialSize"
names(test.data)[7]<-"param2"
names(test.data)[8]<-"param3"
names(test.data)[9]<-"param4"
names(test.data)[10]<-"param5"
names(test.data)[11]<-"MixProportion"

# change values of observations in test data
test.data$MaterialsA = ifelse(test.data$MaterialsA=="75a5f96063fbc3290b07b0e81c3249d0","A1",
                              ifelse(test.data$MaterialsA=="42a6854ae47630b3a32e84823d147e0b","A2",
                                     ifelse(test.data$MaterialsA=="f5c2479be5048388c45cb2e81edfbd3f","A3", "A4")))
test.data$MaterialsB = ifelse(test.data$MaterialsB=="4610065df728e0bd399446ef5fd3ea74","B1",
                              ifelse(test.data$MaterialsB=="6fc672934f2f6e2a64898efd18c24111","B2",
                                     ifelse(test.data$MaterialsB=="0e05731278221a3ac6ebaa1d795b6177","B3", 
                                            ifelse(test.data$MaterialsB=="fe68fc884ca9961c9e8ec90e7fc5ec9d","B4",
                                                   ifelse(test.data$MaterialsB=="e3d6228b4bb426d6d7004613eb04e124","B5",
                                                          ifelse(test.data$MaterialsB=="5437f1e2e5cf83c784e5dba00ee42e7a","B6",
                                                                 ifelse(test.data$MaterialsB=="9e617be991132586229e9964b8bf4463","B7",
                                                                        ifelse(test.data$MaterialsB=="129d425bfebdb61df18ad3940118751b","B8",
                                                                               ifelse(test.data$MaterialsB=="dfac0f954eb95d09d014305d0c5dbe3f","B9",
                                                                                      ifelse(test.data$MaterialsB=="2e54a9875f82b6686db826c4ab45cb8a","B10","B11"))))))))))
test.data$BrandName = ifelse(test.data$BrandName=="29f4d775d7fd37f40a72f66f39b7453f","BrandName1","BrandName2")
test.data$MaterialSize = ifelse(test.data$MaterialSize=="0.115*600",0.115*600,
                                ifelse(test.data$MaterialSize=="0.115*580",0.115*580,
                                       ifelse(test.data$MaterialSize=="0.115*300",0.115*300,0.115*720)))

# convert all missing values into NAs
test.data$MixProportion = ifelse(test.data$MixProportion=="",NA,train.data$MixProportion)

# set categorical attributes as factor
#for test data
test.data$ProductNo <- as.factor(test.data$ProductNo)
test.data$MaterialsA <- as.factor(test.data$MaterialsA)
test.data$MaterialsB <- as.factor(test.data$MaterialsB)
test.data$BrandName <- as.factor(test.data$BrandName)
test.data$MixProportion <- as.factor(test.data$MixProportion)

# ggplot_missing funtion to map missing values
library(reshape2)
library(dplyr)
library(ggplot2)
ggplot_missing <- function(x){
  x %>% is.na %>% melt %>% ggplot(data = ., aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey (name = '', labels = c('Present', 'Missing')) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    labs(x = 'Variables on Dataset', y = 'Rows / Observations')
}

# map mising values using the function
ggplot_missing(test.data)
sapply(test.data,function(x) sum(is.na(x)))

# imputation for missing values
# install.packages("missForest")
library(missForest)
# exclude first column due to missForest limitation of 53 different categorical variables
test.data.imp <- missForest(test.data[,-1])
test.data.imp$ximp
new.test.data <- data.frame(test.data$ProductNo,test.data.imp$ximp)
new.test.data
sapply(new.test.data,function(x) sum(is.na(x))) # recheck missing values


##########################################################################################
############################# Preparing testing data #####################################

# create dummies variables for categorical attributes using one-hot encoding
add.test <- new.test.data
# 3 dummies variable for 4 type of materialsA
add.test$A1 <- ifelse(add.test$MaterialsA=="A1",1,0)
add.test$A2 <- ifelse(add.test$MaterialsA=="A2",1,0)
add.test$A3 <- ifelse(add.test$MaterialsA=="A3",1,0)
# 10 dummies variable for 11 type of materialsB
add.test$B1 <- ifelse(add.test$MaterialsB=="B1",1,0)
add.test$B2 <- ifelse(add.test$MaterialsB=="B2",1,0)
add.test$B3 <- ifelse(add.test$MaterialsB=="B3",1,0)
add.test$B4 <- ifelse(add.test$MaterialsB=="B4",1,0)
add.test$B5 <- ifelse(add.test$MaterialsB=="B5",1,0)
add.test$B6 <- ifelse(add.test$MaterialsB=="B6",1,0)
add.test$B7 <- ifelse(add.test$MaterialsB=="B7",1,0)
add.test$B8 <- ifelse(add.test$MaterialsB=="B8",1,0)
add.test$B9 <- ifelse(add.test$MaterialsB=="B9",1,0)
add.test$B10 <- ifelse(add.test$MaterialsB=="B10",1,0)
# 1 dummy variable for 2 type of BrandName
add.test$BN1 <- ifelse(add.test$BrandName=="BrandName1",1,0)
# 2 dummies variable for 3 type of MixProduction
add.test$Mix40.60 <- ifelse(add.test$MixProportion=="40-60",1,0)
add.test$Mix50.50 <- ifelse(add.test$MixProportion=="50-50",1,0)


# Nomalization
normalize <- function(x) {return((x - min(x)) / (max(x) - min(x)))}

nom.test <- lapply(add.test[5:10], normalize)
scaled.test.data <- data.frame(nom.test,add.test[,12:27])
str(scaled.test.data)

############################# Preparing testing data #####################################
##########################################################################################



######################### Apply Naive Bayes algorithm ####################################
scaled.train.data <- scaled.train
scaled.train.data$Labels <- factor(scaled.train.data$Labels)

library(e1071)
nb.classifier.data <- naiveBayes(scaled.train.data, scaled.train.data$Labels)

nb.predict.data <- predict(nb.classifier.data, scaled.test.data)
summary(nb.predict.data)

# Percentage of good quality prediction
length(which(nb.predict.data=="1"))*100/length(nb.predict.data)





######################### Apply SVM algorithm ###########################################
# modeling the data with svm() 
library(e1071)
svm.classifier.data <- svm(Labels~.,data=scaled.train.data)

# generate predictions for the testing dataset
svm.predict.data <- predict(svm.classifier.data, scaled.test.data)
summary(svm.predict.data)

# Percentage of good quality prediction
length(which(svm.predict.data=="1"))*100/length(svm.predict.data)





######################### Apply Decision Tree algorithm ################################
# Modeling data with decision tree using c50
library(C50)
dt.classifier.data <- C5.0(scaled.train.data[-1], scaled.train.data$Labels)

# generate predictions for the testing dataset
dt.predict.data <- predict(dt.classifier.data, scaled.test.data)

# Percentage of good quality prediction
length(which(dt.predict.data=="1"))*100/length(dt.predict.data)




library(h2o)
h2o.init(nthreads=8, max_mem_size="2G")
h2o.removeAll() ## clean slate - just in case the cluster was already running
h2o.init()

# split train data for validation
train.hex <- as.h2o(scaled.train.data)
test.hex <- as.h2o(scaled.test.data)

splits <- h2o.splitFrame(train.hex, 0.8, seed=777)
split.train  <- h2o.assign(splits[[1]], "train.hex") # 80%
split.valid  <- h2o.assign(splits[[2]], "valid.hex") # 20%

dl.model <- h2o.deeplearning(x=2:20,
                             y="Labels",
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
length(dl.result.data$predict[dl.result.data$predict=="1"])*100/length(dl.result.data$predict)
