##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Looi's Feature Matrix 
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    Builds a feature matrix with some other information
##
##

## init
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)

justRestAndCafe <- subset(read.csv(file='justRestAndCafe.csv', stringsAsFactors=FALSE), select=c(-X))


################################################################
## Add Date based attributes
################################################################
require(lubridate)
justRestAndCafe$weekday <- wday(as.Date(justRestAndCafe$date))
justRestAndCafe$month <- month(justRestAndCafe$date)
justRestAndCafe$year <- year(justRestAndCafe$date)

# write.csv(justRestAndCafe, file="justRestAndCafe_Features.csv")


################################################################
## Get business attribtues for feature matrix
################################################################
# Load Business
require(jsonlite)
raw.business <- stream_in(file("./yelp_dataset_challenge_academic_dataset/yelp_academic_dataset_business.json"))
businessf <- flatten(raw.business)

cafesRests <- read.csv(file="Business_id-Cafe-Restaurants.csv", stringsAsFactors=FALSE)

# Get fields from businessf associated with just cafes and restaurants
cafeRests.data <- businessf[ businessf$business_id %in% cafesRests$bus_id, ]
nrow(cafeRests.data)
nrow(cafesRests)

# Find duplicate business_ids in cafeRests
require(plyr)
v <- count(cafesRests, vars="bus_id" )
v[v$freq>1, ]
v <- count(cafeRests.data, vars="bus_id" )
v[v$freq>1, ]

################################################################
## Add business attribtues to feature matrix
################################################################
bizAttr <- cafeRests.data[ ,c("business_id","hours.Tuesday.close", "hours.Tuesday.open", "hours.Friday.close", "hours.Friday.open", "hours.Monday.close", "hours.Monday.open", "hours.Wednesday.close", "hours.Wednesday.open", "hours.Thursday.close", "hours.Thursday.open", "hours.Sunday.close", "hours.Sunday.open", "hours.Saturday.close", "hours.Saturday.open", "attributes.By Appointment Only", "attributes.Happy Hour", "attributes.Accepts Credit Cards", "attributes.Good For Groups", "attributes.Outdoor Seating", "attributes.Price Range", "attributes.Good for Kids", "attributes.Alcohol", "attributes.Noise Level", "attributes.Has TV", "attributes.Attire", "attributes.Good For Dancing", "attributes.Delivery", "attributes.Coat Check", "attributes.Smoking", "attributes.Take-out", "attributes.Takes Reservations", "attributes.Waiter Service", "attributes.Wi-Fi", "attributes.Caters", "attributes.Drive-Thru", "attributes.Wheelchair Accessible", "attributes.BYOB", "attributes.Corkage", "attributes.BYOB/Corkage", "attributes.Order at Counter", "attributes.Good For Kids", "attributes.Dogs Allowed", "attributes.Open 24 Hours", "attributes.Accepts Insurance", "attributes.Ages Allowed", "attributes.Ambience.romantic", "attributes.Ambience.intimate", "attributes.Ambience.classy", "attributes.Ambience.hipster", "attributes.Ambience.divey", "attributes.Ambience.touristy", "attributes.Ambience.trendy", "attributes.Ambience.upscale", "attributes.Ambience.casual", "attributes.Good For.dessert", "attributes.Good For.latenight", "attributes.Good For.lunch", "attributes.Good For.dinner", "attributes.Good For.breakfast", "attributes.Good For.brunch", "attributes.Parking.garage", "attributes.Parking.street", "attributes.Parking.validated", "attributes.Parking.lot", "attributes.Parking.valet", "attributes.Music.dj", "attributes.Music.background_music", "attributes.Music.karaoke", "attributes.Music.live", "attributes.Music.video", "attributes.Music.jukebox", "attributes.Music.playlist", "attributes.Payment Types.amex", "attributes.Payment Types.cash_only", "attributes.Payment Types.mastercard", "attributes.Payment Types.visa", "attributes.Payment Types.discover", "attributes.Dietary Restrictions.dairy-free", "attributes.Dietary Restrictions.gluten-free", "attributes.Dietary Restrictions.vegan", "attributes.Dietary Restrictions.kosher", "attributes.Dietary Restrictions.halal", "attributes.Dietary Restrictions.soy-free", "attributes.Dietary Restrictions.vegetarian")]

justRestAndCafe.biz <- merge(justRestAndCafe, bizAttr, by="business_id", all.x=TRUE)
#save(justRestAndCafe.biz, file="justRestAndCafe_BizFeatures.RData")
load(file="justRestAndCafe_BizFeatures.RData")

# remove " " and "-" from column names
names(justRestAndCafe.biz) <- gsub("\\s|-",".",names(justRestAndCafe.biz), perl=TRUE)
justRestAndCafe.biz <- subset(justRestAndCafe.biz, select=c(-attributes.Accepts.Credit.Cards))
#write.csv(justRestAndCafe.biz, file="justRestAndCafe_BizFeatures.csv")

justRestAndCafe.biz <- read.csv(file="justRestAndCafe_BizFeatures.csv")

################################################################
## Prepare data for modelling: Take random sample
################################################################
sampleSize=1000

raw_train <- justRestAndCafe.biz[sample(1:nrow(justRestAndCafe.biz),sampleSize ),]

names(raw_train) <- gsub("\\s|-",".",names(raw_train), perl=TRUE)
# Remove the identifies and text for model building
raw_train <- subset(raw_train, select=c(-review_id, -user_id, -date, -text, -business_id, -type, -attributes.Accepts.Credit.Cards))

# Note: attributes.Accepts.Credit.Cards is a list element, get rid of it above

# Make stars a factor so this becomes classification problem
raw_train$stars <- as.factor(raw_train$stars)

raw_train[is.na(raw_train$attributes.Price.Range),"attributes.Price.Range"] <- rep(0, length(raw_train[is.na(raw_train$attributes.Price.Range),"attributes.Price.Range"])) 
# Replace NAs with "unknown" - NA is different to false
raw_train[is.na(raw_train)] <- "unknown"

# Convert to factor for randomForest
raw_train$hours.Tuesday.close <- as.factor(raw_train$hours.Tuesday.close) 
raw_train$hours.Tuesday.open <- as.factor(raw_train$hours.Tuesday.open) 
raw_train$hours.Friday.close <- as.factor(raw_train$hours.Friday.close) 
raw_train$hours.Friday.open <- as.factor(raw_train$hours.Friday.open) 
raw_train$hours.Monday.close <- as.factor(raw_train$hours.Monday.close) 
raw_train$hours.Monday.open <- as.factor(raw_train$hours.Monday.open) 
raw_train$hours.Wednesday.close <- as.factor(raw_train$hours.Wednesday.close) 
raw_train$hours.Wednesday.open <- as.factor(raw_train$hours.Wednesday.open) 
raw_train$hours.Thursday.close <- as.factor(raw_train$hours.Thursday.close) 
raw_train$hours.Thursday.open <- as.factor(raw_train$hours.Thursday.open) 
raw_train$hours.Sunday.close <- as.factor(raw_train$hours.Sunday.close) 
raw_train$hours.Sunday.open <- as.factor(raw_train$hours.Sunday.open) 
raw_train$hours.Saturday.close <- as.factor(raw_train$hours.Saturday.close) 
raw_train$hours.Saturday.open <- as.factor(raw_train$hours.Saturday.open) 

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
## Do a small scale test to see if it is working with normal train?
################################################################################

classification <- train(stars ~ . 
                        ,method='rf'
                        ,allowParallel=TRUE
                        ,data = training
)
# Score validation set
pred <- predict(classification, newdata=testing)
z <- data.frame(pred,observation=testing$stars )

confusionMatrix(pred, testing$stars)
g <- confusionMatrix(pred, testing$stars)$table



################################################################################
#       RANDOM FOREST CLASSIFICATION
################################################################################


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
                              randomForest(training[2:length(training)], training$stars, ntree = ntree, importance=TRUE)
                        }
})

stopCluster(cl)
save(rf_fit, file = "parallelRF.RData")
varImp(rf_fit)
print(rf_fit)



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







