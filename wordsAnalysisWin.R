
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)

words <- read.csv("splitWordsSample1000_with_review_id.csv" )
head(words)

# Group by rev_id, words, count(*)
require(plyr)
b <- count(words, vars=c("rev_id", "words"))
b <- b[order(-b$freq), ]

# make all words lowercase
b$words <- tolower(b$words)
names(b)[2] <- "word"

# remove stop words
stopwords <- read.table("english.stop.txt", head=FALSE, sep="", col.names=c("word") )

# get words from b not in stopwords
c <- b[ which(!(b$word %in% stopwords$word)),]

# Remove special characters from rows and in words
d <- c[grep("[a-zA-Z]{2,}", c$word, perl=TRUE, ignore.case=TRUE),]
d$word <- gsub( pattern="[^a-zA-Z]", replacement="", x=d$word , perl=TRUE, ignore.case=TRUE)
## -- 17/11/2015 -- dont need anymore since list of words now does this 

################################################################
## Find Top 20 Words to use as features
################################################################
# Most common words from all sample reviews
f <- aggregate(d$freq, by=list(d$word), FUN="sum")
names(f) <- c("word", "count")
f <- f[order(-f$count), ]
# get the top 20 words to use as features
top20Words <- head(f, n=20L)

################################################################
## BUILDING FEATURE VECTOR
##
## Go through matrix d and identify whether each review has
## any of the top 20 words and how many they have?
################################################################

# Output should look like

#     review_id               |   food    |     good  |     place | .....
#     ---------------------------------------------------------------------
#     qm88AS_MaUpfbT6GSeSRJA  |   3       |     0     |     1     | .....  
#     JfB8O-7f7CtfAvPwEYD7RQ  |   2       |     1     |     0     | .....  

wordsByReview <- b[order(b$rev_id), ]

# Wash words against top 20 and only keep those in top20
wordsByReview <- wordsByReview[wordsByReview$word %in% top20Words$word, ]

# Pivot matrix to look like above
require(reshape)
featureMatrix <- cast(wordsByReview, rev_id ~ word, value="freq")
nrow(featureMatrix)
# 955, this means 46 reviews dropped out as a result of the washing with top20Word process (Started with 1001 unique rev_id)
# write.csv(featureMatrix, file="featureMatrix.csv")
featureMatrix <- subset(read.csv(file="featureMatrix.csv",stringsAsFactors=FALSE), select=c(-X))

## UNIT TEST
# wordsByReview
#                       rev_id    word freq
# 123 _9rdXonA2Fn_xK_AV1W_9w      it    3
# 82  _9rdXonA2Fn_xK_AV1W_9w chicken    1
# 102 _9rdXonA2Fn_xK_AV1W_9w    food    1
# 109 _9rdXonA2Fn_xK_AV1W_9w   great    1
# 124 _9rdXonA2Fn_xK_AV1W_9w      it    1
# 133 _9rdXonA2Fn_xK_AV1W_9w    love    1
# 155 _9rdXonA2Fn_xK_AV1W_9w  pretty    1

# featureMatrix
# rev_id                      back cheese chicken delicious eat food good great     it   love menu nice order ordered place pretty restaurant service table time
# 3 _9rdXonA2Fn_xK_AV1W_9w    0      0       1         0    0    1    0     1       4    1    0    0     0       0     0      1          0       0     0    0



################################################################
## Join Word Feature Vector onto the end of the reviews table
## extracted as a sample in .....
##
################################################################

# load justRestAndCafe again
justRestAndCafe <- read.csv("justRestAndCafe.csv")

# subset justRestAndCafe to the 955 reviews processed
jrc.sample <-  justRestAndCafe[justRestAndCafe$review_id %in% featureMatrix$rev_id,]

#write.csv(jrc.sample, file="jrc.sample.csv")
jrc.sample <- subset(read.csv(file="jrc.sample.csv",stringsAsFactors=FALSE), select=c(-X,-X.1,-type))

################################################################################
## Add Num of Special Characters to jrc.sample
################################################################################

# for each review in matrix split into letters and count number of special characters
for (i in 1:nrow(jrc.sample)) {
      b <- strsplit(jrc.sample$text[[i]], "")[[1]]        # Split text into letters
      jrc.sample$numSpecialChars[i] <- length(grep('[^\\w\\s]', b, ignore.case=TRUE, value=TRUE, perl=TRUE))  # count number of special chars
      jrc.sample$numCapitals[i] <- length(grep('[A-Z]', b, ignore.case=FALSE, value=TRUE, perl=TRUE))      
}

################################################################################
## Join feature matrix and the reviews in jrc.sample
################################################################################

data <- merge(jrc.sample, featureMatrix, by.x="review_id", by.y="rev_id", all=TRUE)
# write.csv(data, file="data_for_model_building.csv") ## You can take a sample of this to cut and paste into the presentation








# Large scale R
# https://www.kaggle.com/c/job-salary-prediction/forums/t/4076/large-scale-text-mining-in-r/80253
