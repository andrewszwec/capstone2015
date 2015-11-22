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
data <- subset( read.csv(file="data_for_model_building.csv"), select=c(-X))

# Remove the identifies and text for model building
raw_train <- subset(data, select=c(-review_id, -user_id, -date, -text, -business_id))

# Make it classification
raw_train$stars <- as.factor(raw_train$stars)

require(caret)
set.seed(975)
inTrain = createDataPartition(raw_train$stars, p = 0.7)[[1]]
training = raw_train[ inTrain,]     # 70% of records
testing = raw_train[-inTrain,]      # 30% of reocrds


set.seed(575656)

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


# Score validation set
pred <- predict(fit1, newdata=testing)
z <- data.frame(pred,observation=testing$stars )

confusionMatrix(pred, testing$stars)
g <- confusionMatrix(pred, testing$stars)$table

################################################################################
## Confusion Matrix - On Test set
################################################################################
# Confusion Matrix and Statistics
# 
#                  Reference
#     Prediction     1  2  3  4  5
#                 1  0  0  0  0  0
#                 2  0  0  0  1  2
#                 3  1  0  0  0  0
#                 4 14 13 26 40 34
#                 5 12 15 13 53 60
# 
# Overall Statistics
# 
# Accuracy : 0.3521          
# 95% CI : (0.2966, 0.4107)
# No Information Rate : 0.338           
# P-Value [Acc > NIR] : 0.3284          
# 
# Kappa : 0.0306          
# Mcnemar's Test P-Value : NA              
# 
# Statistics by Class:
# 
#                      Class: 1 Class: 2 Class: 3 Class: 4 Class: 5
# Sensitivity           0.00000  0.00000 0.000000   0.4255   0.6250
# Specificity           1.00000  0.98828 0.995918   0.5421   0.5053
# Pos Pred Value            NaN  0.00000 0.000000   0.3150   0.3922
# Neg Pred Value        0.90493  0.90036 0.862191   0.6561   0.7252
# Prevalence            0.09507  0.09859 0.137324   0.3310   0.3380
# Detection Rate        0.00000  0.00000 0.000000   0.1408   0.2113
# Detection Prevalence  0.00000  0.01056 0.003521   0.4472   0.5387
# Balanced Accuracy     0.50000  0.49414 0.497959   0.4838   0.5652


tp <- sum(g * diag(nrow(g)) )
fp = 0
for (x in 1:5){ 
      fp <- fp + sum(g[ , x])-g[x, x]
}
fn = 0
for (x in 1:5){ 
      fn <- fn + sum(g[x, ])- g[x, x]   
}

F1 <- 2*tp / (2*tp + fp + fn)
F1
# 0.352
# 35.2%


