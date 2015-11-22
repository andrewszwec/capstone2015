##    AUTHOR:     Andrew Szwec
##    DATE:       2015-10-07
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Create feature matrix
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

#write.csv(justRestAndCafe, file="justRestAndCafe.csv")
#justRestAndCafe  <- read.csv("justRestAndCafe.csv")

numRecords = 100

# Take a random sample from justRestAndCafe
a <- justRestAndCafe[ sample(1:nrow(justRestAndCafe), numRecords, replace = FALSE), ]



################################################################################
## Do this process for each row of the justRestAndCafe df
################################################################################
# 16sec for 100 records

require(tm)
myCorpus <- Corpus(VectorSource(a$text))
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords
myStopwords <- c(stopwords('english'), "available", "via")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)


dictCorpus <- myCorpus
# stem words in a text document 
myCorpus <- tm_map(myCorpus, stemDocument)
# Remove white space
myCorpus <- tm_map(myCorpus, stripWhitespace)
# stem completion (builds the text back up)
myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)
# Build your matrix
myDtm <- DocumentTermMatrix(myCorpus, control = list(minWordLength = 1))

################################################################################
## Optimise the sparsity
################################################################################
flag = 0
sparseLow = 15
sparseHigh = 30
sparse = 0.1 #0.8
step = 0.1
j = 1
sparseTimeout = 10
while (flag==0){
      # Remove words that do not occur in many documents besides one or two
      DTM.common = removeSparseTerms(myDtm, sparse)
      
      n <- DTM.common$ncol
      
      print(c(sparse, DTM.common$ncol))
      
      if( (n >= sparseLow && n<= sparseHigh) | j == sparseTimeout){
            flag = 1    # drop out of loop
            
      } # END IF
      
      if( n <= sparseLow  ){
            sparse = sparse + step
      }
      if( n >= sparseHigh  ){
            sparse = sparse - step
      }
      j = j + 1
} # END WHILE



# if it is good do this
m <- inspect(DTM.common)




