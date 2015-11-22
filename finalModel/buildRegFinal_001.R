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
pred <- predict(classification, newdata=testing)
z <- data.frame(pred,observation=testing$caters.for.dietry.req )

confusionMatrix(pred, testing$caters.for.dietry.req)
g <- confusionMatrix(pred, testing$caters.for.dietry.req)$table


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
