##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      AWS Looi's Feature Matrix 
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    Builds a feature matrix with some other information on AWS
##
##    FILES:
##    jRC-biz-sample.csv
##
##
##
##


## init
setwd("/home/ubuntu")
set.seed(55984)

################################################################
## Add num special chars and num capitals to feature matrix
################################################################

#justRestAndCafe.biz <- subset(read.csv(file="justRestAndCafe_BizFeatures.csv",stringsAsFactors = FALSE ), select=c(-X))
#jRC.biz.sample <- justRestAndCafe.biz[sample(1:nrow(justRestAndCafe.biz),100000 ),]
#write.csv(jRC.biz.sample, file="jRC-biz-sample.csv", row.names=FALSE)
#jRC.biz.sample <- read.csv(file="jRC-biz-sample.csv",stringsAsFactors = FALSE )
#jRC.biz.sample <- jRC.biz.sample[sample(1:nrow(jRC.biz.sample),10000 ),]
#write.csv(jRC.biz.sample, file="jRC-biz-sample002.csv", row.names=FALSE)
jRC.biz.sample <- read.csv(file="jRC-biz-sample002.csv", stringsAsFactors=FALSE)

## PARALLEL
# library(foreach)
# library(doMC)
# cores <- detectCores(all.tests = FALSE, logical = FALSE) - 1
# registerDoMC(cores)  #change the 2 to your number of CPU cores  
# 
# foreach(i = 1:nrow(jRC.biz.sample)) %dopar% {
#       
#       #loop contents here
#       b <- strsplit(jRC.biz.sample$text[[i]], "")[[1]]        # Split text into letters
#       jRC.biz.sample$numSpecialChars[[i]] <- length(grep('[^\\w\\s]', b, ignore.case=TRUE, value=TRUE, perl=TRUE))[[1]]  # count number of special chars
#       jRC.biz.sample$numCapitals[[i]] <- length(grep('[A-Z]', b, ignore.case=FALSE, value=TRUE, perl=TRUE))[[1]]     
#       
# }

## NON PARALLEL
# for each review in matrix split into letters and count number of special characters
for (i in 1:nrow(jRC.biz.sample)) {
      b <- strsplit(jRC.biz.sample$text[[i]], "")[[1]]        # Split text into letters
      jRC.biz.sample$numSpecialChars[i] <- length(grep('[^\\w\\s]', b, ignore.case=TRUE, value=TRUE, perl=TRUE))  # count number of special chars
      jRC.biz.sample$numCapitals[i] <- length(grep('[A-Z]', b, ignore.case=FALSE, value=TRUE, perl=TRUE))      
}
write.csv(jRC.biz.sample, file="jRC-biz-sample-spec-attr.csv", row.names=FALSE)

################################################################
## Prepare data for modelling: Take random sample
################################################################
sampleSize=1000

raw_train <- jRC.biz.sample[sample(1:nrow(jRC.biz.sample),sampleSize ),]

names(raw_train) <- gsub("\\s|-",".",names(raw_train), perl=TRUE)
# Remove the identifies and text for model building
#raw_train <- subset(raw_train, select=c(-review_id, -user_id, -date, -text, -business_id, -type, -attributes.Accepts.Credit.Cards, -hours.Tuesday.close, -hours.Tuesday.open, -hours.Friday.close, -hours.Friday.open, -hours.Monday.close, -hours.Monday.open, -hours.Wednesday.close, -hours.Wednesday.open, -hours.Thursday.close, -hours.Thursday.open,  -hours.Sunday.close, -hours.Sunday.open, -hours.Saturday.close, -hours.Saturday.open ))
raw_train <- subset(raw_train, select=c(-review_id, -user_id, -date, -text, -business_id, -hours.Tuesday.close, -hours.Tuesday.open, -hours.Friday.close, -hours.Friday.open, -hours.Monday.close, -hours.Monday.open, -hours.Wednesday.close, -hours.Wednesday.open, -hours.Thursday.close, -hours.Thursday.open,  -hours.Sunday.close, -hours.Sunday.open, -hours.Saturday.close, -hours.Saturday.open ))

# Note: attributes.Accepts.Credit.Cards is a list element, get rid of it above

# Make stars a factor so this becomes classification problem
raw_train$stars <- as.factor(raw_train$stars)

raw_train[is.na(raw_train$attributes.Price.Range),"attributes.Price.Range"] <- rep(0, length(raw_train[is.na(raw_train$attributes.Price.Range),"attributes.Price.Range"])) 
# Replace NAs with "unknown" - NA is different to false
raw_train[is.na(raw_train)] <- "unknown"

# Convert to factor for randomForest
# raw_train$hours.Tuesday.close <- as.factor(raw_train$hours.Tuesday.close) 
# raw_train$hours.Tuesday.open <- as.factor(raw_train$hours.Tuesday.open) 
# raw_train$hours.Friday.close <- as.factor(raw_train$hours.Friday.close) 
# raw_train$hours.Friday.open <- as.factor(raw_train$hours.Friday.open) 
# raw_train$hours.Monday.close <- as.factor(raw_train$hours.Monday.close) 
# raw_train$hours.Monday.open <- as.factor(raw_train$hours.Monday.open) 
# raw_train$hours.Wednesday.close <- as.factor(raw_train$hours.Wednesday.close) 
# raw_train$hours.Wednesday.open <- as.factor(raw_train$hours.Wednesday.open) 
# raw_train$hours.Thursday.close <- as.factor(raw_train$hours.Thursday.close) 
# raw_train$hours.Thursday.open <- as.factor(raw_train$hours.Thursday.open) 
# raw_train$hours.Sunday.close <- as.factor(raw_train$hours.Sunday.close) 
# raw_train$hours.Sunday.open <- as.factor(raw_train$hours.Sunday.open) 
# raw_train$hours.Saturday.close <- as.factor(raw_train$hours.Saturday.close) 
# raw_train$hours.Saturday.open <- as.factor(raw_train$hours.Saturday.open) 

raw_train$attributes.Happy.Hour <- as.factor(raw_train$attributes.Happy.Hour)
raw_train$attributes.Good.For.Groups <- as.factor(raw_train$attributes.Good.For.Groups)
raw_train$attributes.Outdoor.Seating <- as.factor(raw_train$attributes.Outdoor.Seating)
raw_train$attributes.Price.Range <- as.factor(raw_train$attributes.Price.Range)
raw_train$attributes.Good.for.Kids <- as.factor(raw_train$attributes.Good.for.Kids)
raw_train$attributes.Alcohol <- as.factor(raw_train$attributes.Alcohol)
raw_train$attributes.Noise.Level <- as.factor(raw_train$attributes.Noise.Level)
raw_train$attributes.Has.TV <- as.factor(raw_train$attributes.Has.TV)
raw_train$attributes.Attire <- as.factor(raw_train$attributes.Attire)
raw_train$attributes.Good.For.Dancing <- as.factor(raw_train$attributes.Good.For.Dancing)
raw_train$attributes.Delivery <- as.factor(raw_train$attributes.Delivery)
raw_train$attributes.Coat.Check <- as.factor(raw_train$attributes.Coat.Check)
raw_train$attributes.Smoking <- as.factor(raw_train$attributes.Smoking)
raw_train$attributes.Take.out <- as.factor(raw_train$attributes.Take.out)
raw_train$attributes.Takes.Reservations <- as.factor(raw_train$attributes.Takes.Reservations)
raw_train$attributes.Waiter.Service <- as.factor(raw_train$attributes.Waiter.Service)
raw_train$attributes.Wi.Fi <- as.factor(raw_train$attributes.Wi.Fi)
raw_train$attributes.Caters <- as.factor(raw_train$attributes.Caters)
raw_train$attributes.Drive.Thru <- as.factor(raw_train$attributes.Drive.Thru)
raw_train$attributes.Wheelchair.Accessible <- as.factor(raw_train$attributes.Wheelchair.Accessible)
raw_train$attributes.BYOB <- as.factor(raw_train$attributes.BYOB)
raw_train$attributes.Corkage <- as.factor(raw_train$attributes.Corkage)
raw_train$attributes.BYOB/Corkage <- as.factor(raw_train$attributes.BYOB/Corkage)
raw_train$attributes.Good.For.Kids <- as.factor(raw_train$attributes.Good.For.Kids)
raw_train$attributes.Dogs.Allowed <- as.factor(raw_train$attributes.Dogs.Allowed)
raw_train$attributes.Open.24.Hours <- as.factor(raw_train$attributes.Open.24.Hours)
raw_train$attributes.Ambience.classy <- as.factor(raw_train$attributes.Ambience.classy)
raw_train$attributes.Ambience.trendy <- as.factor(raw_train$attributes.Ambience.trendy)
raw_train$attributes.Ambience.casual <- as.factor(raw_train$attributes.Ambience.casual)
raw_train$attributes.Good.For.latenight <- as.factor(raw_train$attributes.Good.For.latenight)
raw_train$attributes.Good.For.lunch <- as.factor(raw_train$attributes.Good.For.lunch)
raw_train$attributes.Good.For.dinner <- as.factor(raw_train$attributes.Good.For.dinner)
raw_train$attributes.Good.For.breakfast <- as.factor(raw_train$attributes.Good.For.breakfast)
raw_train$attributes.Good.For.brunch <- as.factor(raw_train$attributes.Good.For.brunch)
raw_train$attributes.Parking.garage <- as.factor(raw_train$attributes.Parking.garage)
raw_train$attributes.Parking.street <- as.factor(raw_train$attributes.Parking.street)
raw_train$attributes.Parking.lot <- as.factor(raw_train$attributes.Parking.lot)
raw_train$attributes.Parking.valet <- as.factor(raw_train$attributes.Parking.valet)
raw_train$attributes.Music.dj <- as.factor(raw_train$attributes.Music.dj)
raw_train$attributes.Music.background_music <- as.factor(raw_train$attributes.Music.background_music)
raw_train$attributes.Music.karaoke <- as.factor(raw_train$attributes.Music.karaoke)
raw_train$attributes.Music.live <- as.factor(raw_train$attributes.Music.live)
raw_train$attributes.Music.video <- as.factor(raw_train$attributes.Music.video)
raw_train$attributes.Music.jukebox <- as.factor(raw_train$attributes.Music.jukebox)

## Find columns with near zero variance and remove them
df_nzv <- nearZeroVar(raw_train, saveMetrics=TRUE)
remaining <- df_nzv[which(df_nzv$nzv==FALSE),]
raw_train <- subset(raw_train , select=rownames(remaining))

require(caret)
set.seed(975)
inTrain = createDataPartition(raw_train$stars, p = 0.7)[[1]]
training = raw_train[ inTrain,]     # 70% of records
testing = raw_train[-inTrain,]      # 30% of reocrds

################################################################################
#       RANDOM FOREST CLASSIFICATION
################################################################################
## All subsequent models are then run in parallel :-)
require(doMC)
cores <- detectCores(all.tests = FALSE, logical = FALSE) - 1
registerDoMC(cores = cores)

set.seed(575656)

system.time({
      ### Run full model!
      classification <- train(training[ ,-4 ], training$stars  
                              ,preProcess=NULL
                              ,method='ada'
                              ,allowParallel=TRUE
      )
      save(classification, file = "LooisModel_RF_20151121_001.RData")
})

### Print the final model details for the Random Forest Model
print(classification$finalModel)


################################################################################
## Variable importance
################################################################################
### Look at the variable importance to the model 
varImp(classification, useModel=TRUE)


################################################################################
#       Check output
################################################################################

# Score validation set
pred <- predict(classification, newdata=testing)
z <- data.frame(pred,observation=testing$caters.for.dietry.req )

confusionMatrix(pred, testing$caters.for.dietry.req)
g <- confusionMatrix(pred, testing$caters.for.dietry.req)$table

write.csv(g, file="confusion_looisFeatureMatrix.csv")

# ROC
require(ROCR)
pred <- predict(fit, newdata=testing)
perf <- performance(pred,"tpr","fpr")

jpeg(file="/home/ubuntu/LooisModel_ROC_001.jpg")
plot(perf, colorize=T)
dev.off()














