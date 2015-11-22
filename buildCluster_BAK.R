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
## Hierarchal Clustering
################################################################

#     !! RUN ME !!

library(cluster)   
require(reshape)
dtm <- cast(d, rev_id ~ word, value="freq")
write.csv(dtm, file='dtm.csv')

r <- dist(t(dtm), method="euclidian")   
fit <- hclust(d=r, method="ward")   
fit   

plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=5)   # "k=" defines the number of clusters you are using   
rect.hclust(fit, k=5, border="red") # draw dendogram with red borders around the 5 clusters   
################################################################
## Kmeans Clustering
################################################################
library(fpc)   
d <- dist(t(dtmss), method="euclidian")   
kfit <- kmeans(d, 2)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  
