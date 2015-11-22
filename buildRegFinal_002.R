##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Build model to predict likely star output based on bag of words
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    Uses data set of 955 records prepared in wordsAnalysisWin.R to build a
##    regression model to predict the likely number of stars a user will give 
##    based on a bag of words model.
##
##
##


## init
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)
data <- subset( read.csv(file="data_for_model_building.csv", stringsAsFactors=FALSE), select=c(-X))

# Remove the identifies and text for model building
raw_train <- subset(data, select=c(-review_id, -user_id, -date, -text, -business_id))

# Make it classification
#raw_train$stars <- ifelse(raw_train$stars == 5, "stars.5", ifelse( raw_train$stars == 4, "stars.4", ifelse(raw_train$stars == 3, "stars.3" , ifelse(raw_train$stars == 2, "stars.2",ifelse(raw_train$stars == 2, "stars.1","unknown") ))))

# Is five stars?
#raw_train$stars <- as.factor(raw_train$stars)
raw_train$stars <- as.factor(ifelse(raw_train$stars==5,"stars.5","stars.not.5"))
raw_train$stars <-ifelse(raw_train$stars==5,"stars.5","stars.not.5")


require(caret)
set.seed(975)
inTrain = createDataPartition(raw_train$stars, p = 0.7)[[1]]
training = raw_train[ inTrain,]     # 70% of records
testing = raw_train[-inTrain,]      # 30% of reocrds

################################################################################
## Model B
################################################################################
## All subsequent models are then run in parallel :-)
require(doMC)
cores <- detectCores(all.tests = FALSE, logical = FALSE) - 1
registerDoMC(cores = cores)

require(pROC)
# gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
#                         n.trees = (1:30)*50,
#                         shrinkage = 0.1,
#                         n.minobsinnode = 20)
# 
# nrow(gbmGrid)

fitControl <- trainControl(method = "repeatedcv",
                           number = 1,
                           repeats = 1,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)


gbmFit3 <- train(x=training[2:ncol(training)],
                 y=training$stars,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 preProcess=c('center','scale'),
                 #tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC",
                 allowParallel=TRUE
               )


################################################################################
## Model Graveyard
################################################################################

## All subsequent models are then run in parallel :-)
require(doMC)
cores <- detectCores(all.tests = FALSE, logical = FALSE) - 1
registerDoMC(cores = cores)

### Run full model!
fit1 <- train(x=training[2:ncol(training)]
                  ,y=training$stars
                  ,method='rf'
                  ,allowParallel=TRUE
                  ,preProcess=c("center", "scale")
)
save(fit1, file = "final_reg_001.RData")
### Print the final model details for the Random Forest Model
print(fit1$finalModel)


################################################################################
## Model Summary
################################################################################
# Regression
# Call:
#       randomForest(x = x, y = y, mtry = param$mtry, allowParallel = TRUE) 
# Type of random forest: regression
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# Mean of squared residuals: 1.545503
# % Var explained: 8.42

# Classification
# Call:
#       randomForest(x = x, y = y, mtry = param$mtry, allowParallel = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 64.98%
# Confusion matrix:
#       1 2 3  4   5 class.error
# 1 2 2 0 32  29   0.9692308
# 2 1 1 0 30  36   0.9852941
# 3 1 0 0 44  48   1.0000000
# 4 1 4 0 84 132   0.6199095
# 5 3 1 0 72 148   0.3392857


################################################################################
## ROCR
################################################################################
#install.packages("ROCR")
require(ROCR)
predictions <- as.numeric(predict(fit1, newdata=testing))
pred <- prediction(predictions, testing$stars )
perf <- performance(pred, measure = "tpr", x.measure = "fpr") 
plot(perf, col=rainbow(10))


pred <- prediction(fit1, newdata=testing)
perf <- performance(pred,"tpr","fpr")


jpeg(file="~/Documents/Coursera/dataScienceSpecialisation/capstone/buildRegFinal_roc_001.jpg")
plot(perf, colorize=T)
dev.off()



################################################################################
## Metrics
################################################################################
# Score validation set
pred <- predict(fit1, newdata=testing, type="raw")
z <- data.frame(pred,observation=testing$stars )

confusionMatrix(pred, testing$stars)
g <- confusionMatrix(pred, testing$stars)$table
