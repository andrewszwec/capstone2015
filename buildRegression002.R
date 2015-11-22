##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Build model to predict likely star output based on bag of words
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    Uses data set of 200k records prepared in buildCluster.R to build a
##    regression model to predict the likely number of stars a user will give 
##    based on a bag of words model.
##
##
##


## init
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
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


## Find columns with near zero variance and remove them
df_nzv <- nearZeroVar(training, saveMetrics=TRUE)
# remaining <- df_nzv[which(df_nzv$nzv==FALSE),]
#df_all_var <- subset(df , select=rownames(remaining))
# Remove Columsn with NAs
#df_rm_na <- df_all_var[ , colSums(is.na(training)) == 0]

# Find Correlated variables 
df_corr <- cor( raw_train ) #subset(training, select=-stars))  # now look at correlation with target

library(corrgram)
corrgram(df_corr, order=FALSE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         col.regions=colorRampPalette(c("red","salmon","white","royalblue","navy")),       
         main="Correlation of remaining features")



################################################################################
## Classification
################################################################################


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


################################################################################
## ROC - save to file
################################################################################
#install.packages("ROCR")
require(ROCR)

# ROC
jpeg(file="/home/ubuntu/roc.jpg")
pred <- predict(classification, newdata=testing)
perf <- performance(pred,"tpr","fpr")
plot(perf, colorize=T)
dev.off()




