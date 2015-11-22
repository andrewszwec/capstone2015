##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Multicore 
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    Users multicore packages
##
##

## init
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)
data <- subset( read.csv(file="data_for_model_building.csv"), select=c(-X))

# Remove the identifies and text for model building
raw_train <- subset(data, select=c(-review_id, -user_id, -date, -text, -business_id))
raw_train$stars <- as.factor(raw_train$stars)

require(caret)
set.seed(975)
inTrain = createDataPartition(raw_train$stars, p = 0.7)[[1]]
training = raw_train[ inTrain,]     # 70% of records
testing = raw_train[-inTrain,]      # 30% of reocrds

#
#       RANDOM FOREST CLASSIFICATION
#


# install.packages('foreach'); install.packages('doSNOW')
library(foreach)
library(doSNOW)
library(caret)
library(parallel)
require(randomForest)


cores <- detectCores(all.tests = FALSE, logical = FALSE) - 1
cl <- makeCluster(cores, type = "SOCK",outfile="")
registerDoSNOW(cl)


total.tree <- 2000
num.chunk <- cores
avg.tree <- ceiling(total.tree/num.chunk)

# If possible it would be best to do bagging or boosting to the data before 
# sending it to the random forest code.
# Bagging is bootstrape aggregating which takes the data and splits it
# So you can train many weak classifiers and then take their average.

system.time({
      rf_fit <- foreach(ntree = rep(avg.tree, num.chunk), .combine = combine, 
                        .packages = c("randomForest")) %dopar% {
                              randomForest(training[2:length(training)], training$stars, ntree = ntree, importance=TRUE, keep.forest)
                        }
})

stopCluster(cl)

save(rf_fit, file = "parallelRF.RData")


# Score validation set
pred <- predict(rf_fit, newdata=testing)
z <- data.frame(pred,observation=testing$stars )

confusionMatrix(pred, testing$stars)
g <- confusionMatrix(pred, testing$stars)$table

# Prediction [,1] [,2] [,3] [,4] [,5]
# 1    1    1    1    0    0
# 2    1    1    0    2    4
# 3    3    2    3    2    3
# 4   14   15   28   47   35
# 5    8    9    7   43   54

tp <- sum(g * diag(5) )
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

