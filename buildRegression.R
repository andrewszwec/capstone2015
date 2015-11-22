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

require(caret)
set.seed(975)
inTrain = createDataPartition(raw_train$stars, p = 0.7)[[1]]
training = raw_train[ inTrain,]     # 70% of records
testing = raw_train[-inTrain,]      # 30% of reocrds


## Find columns with near zero variance and remove them
df_nzv <- nearZeroVar(training, saveMetrics=TRUE)
# remaining <- df_nzv[which(df_nzv$nzv==FALSE),]
#df_all_var <- subset(df , select=rownames(remaining))
# Remove Columsn with NAs
#df_rm_na <- df_all_var[ , colSums(is.na(training)) == 0]

# Find Correlated variables 
df_corr <- cor( training ) #subset(training, select=-stars))  # now look at correlation with target

library(corrgram)
corrgram(df_corr, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         col.regions=colorRampPalette(c("red","salmon","white","royalblue","navy")),       
         main="Correlation of remaining features")


set.seed(575656)

## All subsequent models are then run in parallel :-)
require(doMC)
cores <- detectCores(all.tests = FALSE, logical = FALSE) - 1
registerDoMC(cores = cores)

### Run full model!
mod_rf_full <- train(stars ~ . 
                     ,method='rf'
                     ,allowParallel=TRUE
                     ,data = training
)
save(mod_rf_full, file = "rf_regression_002.RData")
### Print the final model details for the Random Forest Model
print(mod_rf_full$finalModel)

################################################################################
## Comments
## The regression model was pretty good at predicting numer of stars
################################################################################


set.seed(575656)
training2 <- training
training2$stars <- as.factor(training2$stars)
### Run full model!
classification <- train(stars ~ . 
                     ,method='rf'
                     ,allowParallel=TRUE
                     ,data = training2
)
save(classification, file = "rf_classification_001.RData")
### Print the final model details for the Random Forest Model
print(classification$finalModel)

################################################################################
## Model Summary
################################################################################
# Call:
#       randomForest(x = x, y = y, mtry = param$mtry, allowParallel = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 61.29%
# Confusion matrix:
#   1 2 3   4   5 class.error
# 1 5 0 2  22  39   0.9264706
# 2 1 0 0  30  35   1.0000000
# 3 0 0 2  45  43   0.9777778
# 4 0 1 1 107 112   0.5158371
# 5 3 0 3  73 145   0.3526786

################################################################################
## Variable importance
################################################################################
### Look at the variable importance to the model 
varImp(classification, useModel=TRUE)
# Overall
# num.words        100.00
# text.len          99.77
# numSpecialChars   95.43
# numCapitals       88.26
# votes.useful      52.26
# it                36.51
# votes.cool        36.37
# votes.funny       33.88
# food              26.64
# great             26.20
# good              25.03
# place             22.14
# pretty            20.12
# ordered           19.57
# service           19.47
# nice              18.57
# order             18.13
# back              17.60
# time              16.45
# love              16.30


set.seed(575656)
training2 <- subset(training, select=c(-num.words,-text.len,-numSpecialChars,-numCapitals,-votes.useful, -it,-votes.cool,-votes.funny   ))
training2$stars <- as.factor(training2$stars)
### Run full model!
classification <- train(stars ~ . 
                        ,method='rf'
                        ,allowParallel=TRUE
                        ,data = training2
)
save(classification, file = "rf_classification_002.RData")
print(classification$finalModel)
################################################################################
## Variable importance for reduced attribute model
################################################################################
### Look at the variable importance to the model 
varImp(classification, useModel=TRUE)
# Overall
# great        100.00
# food          90.35
# good          88.13
# ordered       84.23
# nice          80.63
# service       77.21
# order         74.39
# place         73.01
# pretty        73.01
# back          72.26
# time          70.82
# love          61.52
# chicken       61.11
# restaurant    52.34
# menu          49.48
# table         48.73
# eat           43.80
# cheese        37.85
# delicious     20.24
# dietary.req    0.00

################################################################################
## Model Summary
################################################################################
# 
# Call:
#       randomForest(x = x, y = y, mtry = param$mtry, allowParallel = TRUE) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# 
# OOB estimate of  error rate: 61.88%
# Confusion matrix:
#       1 2 3  4   5 class.error
# 1 4 0 0 26  38   0.9411765
# 2 1 0 0 20  45   1.0000000
# 3 1 0 0 42  47   1.0000000
# 4 1 0 1 86 133   0.6108597
# 5 3 0 1 55 165   0.2633929

################################################################################
## COMMENTS
##
## The model buit only on key words is better at predicting a 5 star review
## but no as good at predicting 1's or 4's. 2's and 3's are universally poorly
## predicted.
##
################################################################################


################################################################################
## Model to predict if review is 5 star or not
################################################################################
set.seed(575656)
training2 <- subset(training, select=c(-num.words,-text.len,-numSpecialChars,-numCapitals,-votes.useful, -it,-votes.cool,-votes.funny   ))
training2$isFiveStar <- as.factor(ifelse(training2$stars == 5, 1,0 ))
training2 <- subset(training2, select=c(-stars ))

### Run full model!
classification <- train(isFiveStar ~ . 
                        ,method='rf'
                        ,allowParallel=TRUE
                        ,data = training2
)
save(classification, file = "rf_classification_003.RData")
print(classification$finalModel)

################################################################################
## COMMENTS
## Good at predicting not a five star review
##
################################################################################

################################################################################
## Model to predict if review is 5 star or not (ALL ATTRIBUTES)
################################################################################
set.seed(575656)
training2 <- training
training2$isFiveStar <- as.factor(ifelse(training2$stars == 5, 1,0 ))
training2 <- subset(training2, select=c(-stars ))

### Run full model!
classification <- train(isFiveStar ~ . 
                        ,method='rf'
                        ,allowParallel=TRUE
                        ,data = training2
)
save(classification, file = "rf_classification_005.RData")
print(classification$finalModel)

################################################################################
## COMMENTS
## 
##
################################################################################

################################################################################
## Model to predict the likelihood of a 5 star review (SVM Linear) (classification)
################################################################################
## NOT RUN
set.seed(575656)
training2 <- training
training2$isFiveStar <- as.factor(ifelse(training2$stars == 5, 1,0 ))
training2 <- subset(training2, select=c(-stars ))

### Run full model!
classification <- train(isFiveStar ~ . 
                        ,method='svmLinear'
                        ,allowParallel=TRUE
                        ,data = training2
)
save(classification, file = "rf_classification_004.RData")
print(classification$finalModel)

################################################################################
## COMMENTS
## SVM is not good
##
################################################################################

################################################################################
## Model to predict the likelihood of a 5 star review (AdaBag) (classification)
################################################################################
## NOT RUN
set.seed(575656)
training2 <- training
training2$isFiveStar <- as.factor(ifelse(training2$stars == 5, 1,0 ))
training2 <- subset(training2, select=c(-stars ))

### Run full model!
classification <- train(isFiveStar ~ . 
                        ,method='ada'
                        ,allowParallel=TRUE
                        ,data = training2
)
save(classification, file = "rf_classification_006.RData")
print(classification$finalModel)

################################################################################
## COMMENTS
## 
##
###

### Use Random Forest model to predict values on the cross-validation set
```{r cache=TRUE}
rf_full_predictions <- predict(classification, newdata = testing)
pred <- data.frame(rf_full_predictions, stars=ifelse(testing$stars == 5, 1,0 ))
correct <- nrow(pred[with( which(rf_full_predictions==classe), data=pred ),])
wrong <- nrow(pred[with( which(rf_full_predictions != classe), data=pred ),])
percent_correct = correct/nrow(pred)*100
percent_correct
percent_wrong = wrong/nrow(pred)*100
percent_wrong


### The Out of sample error is...
```{r cache = TRUE}
out_of_sample_error <- 1/(length(rf_full_predictions)) * sum( ( wrong )^2 )
out_of_sample_error
```

### Use random forest model to predict the outcome of the 20 test cases for submission
```{r}
rf_submission_outcomes <- predict(mod_rf_full, newdata = raw_test)
rf_submission_outcomes
```
