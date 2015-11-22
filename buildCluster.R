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
numSamples = 200000
a <- justRestAndCafe[sample(1:nrow(justRestAndCafe), numSamples) , c("review_id","text","stars")]


################################################################
## Working Code
################################################################

words <- read.csv("splitWordsSample1000_with_review_id.csv" )
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


################################################################
## Plot Word Frequencies
################################################################
library(ggplot2)   
p <- ggplot(subset(d, freq>5), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p <- p + ggtitle("Word frequencies in 1000 record sample (freq>5)") + theme(plot.title = element_text(lineheight=.8, face="bold"))
p <- p + theme(axis.title.x = element_text(face="bold", size=15))
p <- p + theme(axis.title.y = element_text(face="bold", size=15))
p   


################################################################
## Word Cloud
################################################################
library(wordcloud)  

#set.seed(142)   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(d$word, d$freq, max.words=100, rot.per=0.2, colors=dark2)   


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

#tdm <- TermDocumentMatrix(doc.corpus)     
#save(tdm, file="tdm_20151119.Rda")
#load(file="tdm_20151119.Rda")

dtm <- DocumentTermMatrix(doc.corpus)   
#save(dtm, file="dtm_20151119.Rda")
#save(dtm, file="dtm_20151119_002.Rda") ## 200k reviews
#load(file="dtm_20151119.Rda")

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
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0,
         main=paste0("K-means Cluster: Review Text Sample (",numSamples/1000,"K records)") ) 



################################################################
## Make feature matrix for modelling
################################################################

m <- data.frame(inspect(dtmss))
dim(m)
m$review_id <- a$review_id
m$stars <- a$stars

write.csv(m, file="dataForModelling002.csv")










