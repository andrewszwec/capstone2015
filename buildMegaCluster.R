##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Build Kmeans and Hierarchical clustering model  
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    Build Kmeans and Hierarchical clustering model using this data:
##
##    -     words <- read.csv("splitWordsSample1000_with_review_id.csv" )
##    
##    This is all words from the 1000 reviews analysed.
##

## init
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)

################################################################
## Get justRestAndCafe and take top 1000 records and try again
################################################################
## This would reduce the number of documents caused by me 
## pre-splitting the words into a matrix
justRestAndCafe <- subset(read.csv(file='justRestAndCafe.csv', stringsAsFactors=FALSE), select=c(-X))

################################################################
## Preparation for clustering (Trying alternate path, worked 
## for 1000 reviews - trying more)
################################################################
a<- justRestAndCafe[ , c("review_id","text","stars")]  # Run overnight, then comment me out
rm(justRestAndCafe)
# Take the words matrix and use recommended methods
library(tm)  
doc.vec <- VectorSource(a$text)    # ASZ 2015-11-19 Changed from words$words to "a" to try an alternate method
doc.corpus <- Corpus(doc.vec)
doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
doc.corpus <- tm_map(doc.corpus, removePunctuation)
doc.corpus <- tm_map(doc.corpus, removeNumbers)
doc.corpus <- tm_map(doc.corpus, removeWords, stopwords("english"))

library(SnowballC)
doc.corpus <- tm_map(doc.corpus, stemDocument)
doc.corpus <- tm_map(doc.corpus, stripWhitespace)

dtm <- DocumentTermMatrix(doc.corpus)   
save(dtm, file="dtm_mega_20151120.Rda")

dtmss <- removeSparseTerms(dtm, 0.82) # This makes a matrix that is only 82% empty space, maximum.   

################################################################
## Hierarchal Clustering
################################################################
library(cluster)   
d <- dist(t(dtmss), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit  

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   


################################################################
## Kmeans Clustering
################################################################
library(fpc)   
#d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 3)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0,
         main=paste0("K-means Cluster: Review Text Sample (900K records)") ) 



################################################################
## Make feature matrix for modelling
################################################################

m <- data.frame(inspect(dtmss))
dim(m)
m$review_id <- a$review_id
m$stars <- a$stars

#write.csv(m, file="dataForModelling003_900k.csv")







