##    AUTHOR:     Andrew Szwec
##    DATE:       2015-10-07
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Create feature matrix (AUTOMATED)
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    THIS SCRIPT IS AUTOMATED FOR AWS EC2 - This script will use text mining techniques to find the frequency of words
##    in each review. 
##
##    TRANSFER FILE TO EC2
##    scp -i /Users/andrewszwec/Documents/AWS/machome.pem /Users/andrewszwec/Documents/Coursera/dataScienceSpecialisation/capstone/ec2-createFeatures.R  ec2-user@ec2-52-65-14-48.ap-southeast-2.compute.amazonaws.com:/home/ec2-user 
##    scp -i /Users/andrewszwec/Documents/AWS/machome.pem /Users/andrewszwec/Documents/Coursera/dataScienceSpecialisation/capstone/justRestAndCafe-sample.csv  ec2-user@ec2-52-65-14-48.ap-southeast-2.compute.amazonaws.com:/home/ec2-user 
##    
##    
##    TO RUN TYPE
##    Rscript ec2-createFeatures.R 
##
##    TO GET FILES FROM S3
##    wget https://s3-ap-southeast-2.amazonaws.com/awsmlsz/justRestAndCafe.csv
##
##


numRecords = 5000

# Take a random sample from justRestAndCafe 
## Uncomment to run ## b <- justRestAndCafe[ sample(1:nrow(justRestAndCafe), numRecords, replace = FALSE), ]
# Remove \n new line
## Uncomment to run ## b$text <- gsub( pattern="\\n", replacement=" ", x=a$text , perl=TRUE, ignore.case=TRUE)

#write.csv(b, file="justRestAndCafe-sample.csv")
b <- read.csv(file="justRestAndCafe-sample.csv")

numRecordPreProcess = 100
a <- head(b, n=numRecordPreProcess)

################################################################################
## Do this process for each row of the justRestAndCafe df
################################################################################

cc <- Corpus(VectorSource(a$text))
cc <- tm_map(cc, tolower, lazy=TRUE)
# remove punctuation
cc <- tm_map(cc, removePunctuation, lazy=TRUE)
# remove numbers
cc <- tm_map(cc, removeNumbers, lazy=TRUE)
# remove stopwords
myStopwords <- c(stopwords('english'), "available", "via")
cc <- tm_map(cc, removeWords, myStopwords, lazy=TRUE)


dictCorpus <- cc
# stem words in a text document 
cc <- tm_map(cc, stemDocument, lazy=TRUE)
# Remove white space (got to here OK)
cc <- tm_map(cc, stripWhitespace, lazy=TRUE)
# stem completion (builds the text back up)
cc <- tm_map(cc, stemCompletion, dictionary=dictCorpus, lazy=TRUE)
# Build your matrix
myDtm <- DocumentTermMatrix(cc, control = list(minWordLength = 1), lazy=TRUE)
sparse = 0.8
DTM.common = removeSparseTerms(myDtm, sparse)
write.csv(myDtm, file="myDtm.csv")
write.csv(DTM.common, file="DTM.common.csv")


# Suggestion from net to speed up
# dtm <- DocumentTermMatrix(the_corpus, 
#                           control=list(wordLengths=c(1, Inf),
#                                        bounds=list(global=c(floor(length(the_corpus)*0.05), Inf))))


