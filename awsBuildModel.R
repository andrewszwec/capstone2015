##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      AWS UBUNTU CODE
##    SUBTITLE:   Build model to predict likely star output based on bag of words
##    
##    DESCRIPTION: 
##    Uses data set of 200k records prepared in buildCluster.R to build a
##    regression model to predict the likely number of stars a user will give 
##    based on a bag of words model.
##
##    GET DATA
##    wget https://s3-ap-southeast-2.amazonaws.com/awsmlsz/dataForModelling002.csv
##    wget https://s3-ap-southeast-2.amazonaws.com/awsmlsz/awsBuildModel.R

## init
setwd("/home/ubuntu")
set.seed(1234)
data <- subset( read.csv(file="dataForModelling002.csv"), select=c(-X))

# Remove the identifies and text for model building
raw_train <- subset(data, select=c( -review_id ))
rm(data)
raw_train$stars <- as.factor(raw_train$stars)

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
### Run full model!
classification <- train(subset(training, select=c(-stars)), training$stars  
                        ,preProcess=NULL
                        ,method='rf'
                        ,allowParallel=TRUE
)
save(classification, file = "rf_class_200k_001.RData")
### Print the final model details for the Random Forest Model
print(classification$finalModel)

# Time: 2-3hrs to build

# Call:
#       randomForest(x = x, y = y, mtry = param$mtry, allowParallel = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 11
# 
# OOB estimate of  error rate: 67.96%
# Confusion matrix:
#    1   2    3     4     5    class.error
# 1 154 174  483  4585  6756   0.9873272
# 2 161 239  451  5304  7555   0.9825675
# 3 252 298  785  8262 11885   0.9634578
# 4 523 614 1681 17387 24852   0.6141110
# 5 523 690 1745 18350 26294   0.4476283
# 

# ROC
jpeg(file="/home/ubuntu/roc.jpg")
pred <- predict(classification, newdata=testing)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize=T)
dev.off()


