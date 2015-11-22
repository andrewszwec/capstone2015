##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      AWS UBUNTU CODE
##    SUBTITLE:   Build model to predict likely star output based on bag of words
##    
##    DESCRIPTION: 
##    Uses data set of 10k records prepared in buildCluster.R to build a
##    regression model to predict the likely number of stars a user will give 
##    based on a bag of words model.
##
##    GET DATA
##    wget https://s3-ap-southeast-2.amazonaws.com/awsmlsz/dataForModelling002.csv
##    wget https://s3-ap-southeast-2.amazonaws.com/awsmlsz/awsBuildModel.R

## init
setwd("/home/ubuntu")
set.seed(1234)
data <- subset( read.csv(file="dataForModelling002.csv"), select=c( -X,-review_id ))

num.rows = 10000
# Remove the identifies and text for model building
raw_train <- data[sample(1:nrow(data), num.rows), ]
rm(data)

require(caret)
set.seed(975)
inTrain = createDataPartition(raw_train$stars, p = 0.7)[[1]]
training = raw_train[ inTrain,]     # 70% of records
testing = raw_train[-inTrain,]      # 30% of reocrds

## All subsequent models are then run in parallel :-)
require(doMC)
cores <- detectCores(all.tests = FALSE, logical = FALSE) - 1
registerDoMC(cores = cores)


set.seed(575656)
training$stars <- as.factor(training$stars)

system.time({ 
      ### Run full model!
      fit <- train(subset(training, select=c(-stars)), training$stars  
                              ,preProcess=NULL
                              ,method='rf'
                              ,allowParallel=TRUE
      )
      save(fit, file = "buildRegression002.RData")
      ### Print the final model details for the Random Forest Model
      print(fit$finalModel)
})

# user   system  elapsed 
# 1518.385    6.844  161.448 

# Call:
#       randomForest(x = x, y = y, mtry = param$mtry, allowParallel = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 66.72%
# Confusion matrix:
#       1 2 3   4    5 class.error
# 1 0 0 0 189  425   1.0000000
# 2 0 0 1 192  478   1.0000000
# 3 0 0 1 317  782   0.9990909
# 4 0 0 7 597 1618   0.7313231
# 5 0 0 4 658 1732   0.2765246

require(pROC)
pred <- predict(fit, newdata=testing)
jpeg(file="/home/ubuntu/Build_Regression_AUC_001.jpg")
auc(pred,testing$stars )
dev.off()




# ROC
require(ROCR)
pred <- predict(fit, newdata=testing)
perf <- performance(pred,"tpr","fpr")

jpeg(file="/home/ubuntu/Build_Regression_ROC_001.jpg")
plot(perf, colorize=T)
dev.off()


################################################################################
## Variable importance
################################################################################
### Look at the variable importance to the model 
varImp(classification, useModel=TRUE)

