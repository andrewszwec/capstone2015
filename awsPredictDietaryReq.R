##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      AWS UBUNTU CODE - PREDICT DIETARY REQUIREMENTS
##    SUBTITLE:   Build model to predict likely star output based on bag of words
##    
##    DESCRIPTION: 
##    
##
##    GET DATA
##    wget https://s3-ap-southeast-2.amazonaws.com/awsmlsz/dataForModelling002.csv
##    wget https://s3-ap-southeast-2.amazonaws.com/awsmlsz/awsBuildModel.R

## init
setwd("/home/ubuntu")
set.seed(1234)
data <- subset( read.csv(file="awsPredictDietReq_Data.csv"), select=c(-X))

# Remove the identifies and text for model building
raw_train <- subset(data, select=c( -review_id, -business_id, -stars ))
rm(data)

require(caret)
set.seed(975)
inTrain = createDataPartition(raw_train$caters.for.dietry.req, p = 0.7)[[1]]
training = raw_train[ inTrain,]     # 70% of records
testing = raw_train[-inTrain,]      # 30% of reocrds

## All subsequent models are then run in parallel :-)
require(doMC)
cores <- detectCores(all.tests = FALSE, logical = FALSE) - 1
registerDoMC(cores = cores)


set.seed(575656)

system.time({
      ### Run full model!
      classification <- train(subset(training, select=c(-caters.for.dietry.req)), training$caters.for.dietry.req  
                              ,preProcess=NULL
                              ,method='rf'
                              ,allowParallel=TRUE
      )
      save(classification, file = "rf_pred_diet_req_regression_002.RData")
})

### Print the final model details for the Random Forest Model
print(classification$finalModel)


# Score validation set
# pred <- predict(classification, newdata=testing)
# z <- data.frame(pred,observation=testing$caters.for.dietry.req )
# 
# confusionMatrix(pred, testing$caters.for.dietry.req)
# g <- confusionMatrix(pred, testing$caters.for.dietry.req)$table
# 
# write.csv(g, file="confusion_pred_diet_req.csv")
# 
# tp <- sum(g * diag(nrow(g)) )
# fp = 0
# for (x in 1:5){ 
#       fp <- fp + sum(g[ , x])-g[x, x]
# }
# fn = 0
# for (x in 1:5){ 
#       fn <- fn + sum(g[x, ])- g[x, x]   
# }
# 
# F1 <- 2*tp / (2*tp + fp + fn)
# F1
# write.csv(F1, file="F1_pred_diet_req.csv")



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


# performance, recall vs precision
# jpeg(file="/home/ubuntu/prec-rec.jpg")
# perf <- performance(pred,"prec", "rec")
# plot(perf, colorize=T)
# dev.off()
# 
# Accurarcy
# jpeg(file="/home/ubuntu/acc.jpg")
# perf <- performance(pred,"acc")
# plot(perf, col=rainbow(10))
# dev.off()




