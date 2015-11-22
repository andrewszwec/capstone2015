##    AUTHOR:     Andrew Szwec
##    DATE:       2015-10-07
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Split all reviews into words 
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    This is SOLUTION 1 to create a feature matrix.
##    Creates a table of review and its words. This can be pivoted and counted 
##    for each record.
##
##
##    !!.COOL.!!
##


################################################################################
## Code to split all reviews into words 
################################################################################
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)

## Load the businesses that are just cafes or restaurants
cafesRests <- read.csv("Business_id-Cafe-Restaurants.csv")
load(file="reviewf.Rda")

# Get the Res+Cafe reviews
justRestAndCafe <- reviewf[reviewf$business_id %in% cafesRests$bus_id, ]


numRecords = 100
# Take a random sample from justRestAndCafe
a <- justRestAndCafe[ sample(1:nrow(justRestAndCafe), numRecords, replace = FALSE), ]
nrow(a)

# pre allocate a matrix with this many rows and one column
numRows <- sum(a$num.words)*1.10
# 136697
# Pre allocating
words <- data.frame(rev_id=rep(NA, numRows), words=rep("", numRows),  # as many cols as you need
                     stringsAsFactors=FALSE)          # you don't know levels yet



################################################################################
## Go through each review and remove special characters
################################################################################
#justRestAndCafe$text 
a$text <- gsub( pattern="[^a-zA-Z ]", replacement="", x=a$text , perl=TRUE, ignore.case=TRUE)

################################################################################
## Split reviews into words
################################################################################
# For each row in the set of Cafes and Rest (justRestAndCafe) do the following things
# 1. Get record, split up by " " space

# Init
split <- "\\s" # string to split on
word_idx = 1
# for each review  
for (i in 1:nrow(a)){
      # i is a review
      reviewWords <- unlist( strsplit(a$text[[i]]  , split, perl=TRUE)[[1]])  
      for (j in 1:length(reviewWords)){
            # j is a word in a review
            # print(c(reviewWords[j], word_idx)) # debug
            words[word_idx, "rev_id"]  <- a$review_id[[i]]
            words[word_idx, "words"]  <- reviewWords[j]
            word_idx = word_idx + 1
      }
}

# write.csv(words, file="splitWordsSample1000.csv")
#write.csv(words, file="splitWordsSample3000_INCOMPLETE_with_review_id.csv")




require(plyr)
counts <- count(words, vars="words")