##    AUTHOR:     Andrew Szwec
##    DATE:       2015-10-07
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    This script will use text mining techniques to find the frequency of words
##    in each review.
##
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)

## Load the businesses that are just cafes or restaurants
cafesRests <- read.csv("Business_id-Cafe-Restaurants.csv")
load(file="reviewf.Rda")


# Get the Res+Cafe reviews
justRestAndCafe <- reviewf[reviewf$business_id %in% cafesRests$bus_id, ]


system.time({

numRecords = 100
# Perform techniques on a small sample
# a <- head(justRestAndCafe, n=numRecords)

# Take a random sample from justRestAndCafe
a <- justRestAndCafe[ sample(1:nrow(justRestAndCafe), numRecords, replace = FALSE), ]

################################################################################
## Do this process for each row of the justRestAndCafe df
################################################################################

require(tm)
myCorpus <- Corpus(VectorSource(a$text))
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
# keep "r" by removing it from stopwords
myStopwords <- c(stopwords('english'), "available", "via")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# requires Snowball
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect the first three ``documents"
#inspect(myCorpus[1:3])

myCorpus <- tm_map(myCorpus, stripWhitespace)

# stem completion (builds the text back up)
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

# Build your matrix
myDtm <- DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))
#inspect(myDtm[1:10,1:10])
#m <- inspect(myDtm)
#m[1:10,1:10]


# Remove words that do not occur in many documents besides one or two
sparse = 0.1
DTM.common = removeSparseTerms(myDtm, sparse)

## End system time
})


DTM.common

# if it is good do this
m <- inspect(DTM.common)

################################################################################
## There are 27 words that are commonly used. The others are very sparse. 
## Use this n x 27 matrix as a feature matrix (n = num docs)
################################################################################



################################################################################
## Check if sparse matrix can be more efficiently stored
################################################################################
#install.packages("slam")
require(slam)
#DTM.dense <- as.matrix(DTM.common)
#object.size(DTM.common)
#object.size(DTM.dense)
#rm(DTM.dense)

TDM <- TermDocumentMatrix(myCorpus)

################################################################################
## Pivot data
################################################################################
TDM.common = inspect(removeSparseTerms(TDM, 0.80))
require(reshape2)
TDM.common = melt(TDM.common, value.name = "count")

################################################################################
## Build Image
################################################################################
require(ggplot2)
ggplot(TDM.common, aes(x = Docs, y = Terms, fill = log10(count))) +
geom_tile(colour = "white") +
scale_fill_gradient(high="#FF0000" , low="#FFFFFF") +
ylab("") +
theme(panel.background = element_blank()) +
theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



