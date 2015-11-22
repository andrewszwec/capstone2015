##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Model of reviews with dietary req ~ does business cater for diet req
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    Can we predict if a restaurant caters for dietary requirement just by the reviews it gets?
##
##
##


## init
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)



# Business - Cafes and Restaurants
cafesRests <- read.csv("Business_id-Cafe-Restaurants.csv")

# Business Data
load(file="businessf.Rda")
sum(businessf$caters.for.dietry.req)
# 131 businesses registered for diet req


################################################################
## Get justRestAndCafe and take top 1000 records and try again
################################################################
## This would reduce the number of documents caused by me 
## pre-splitting the words into a matrix
justRestAndCafe <- subset(read.csv(file='justRestAndCafe.csv', stringsAsFactors=FALSE), select=c(-X))

# just those reviews that mention some dietary requirment
a <- justRestAndCafe[ justRestAndCafe$dietary.req == 1, c("review_id","text","stars", "business_id")]



################################################################
## Preparation for clustering (Trying alternate path, worked 
## for 1000 reviews - trying more)
################################################################

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
save(dtm, file="dtm_diet_req_20151120.Rda")
load(file="dtm_diet_req_20151120.Rda")


dtmss <- removeSparseTerms(dtm, 0.75) # This makes a matrix that is only 82% empty space, maximum.   

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
kfit <- kmeans(d, 5)   
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0,
         main=paste0("K-means Cluster: Review Text Sample (",nrow(a)/1000,"K records)") ) 



################################################################
## Make feature matrix for modelling
################################################################

m <- data.frame(inspect(dtmss))
dim(m)
m$review_id <- a$review_id
m$stars <- a$stars
m$business_id <- a$business_id

# This is now the feature matrix. It has: key words and if restaurant caters for diet req
# get the dietary requirements pattern from earlier and merge on business_id
t <- merge( m ,businessf[ ,c("business_id","caters.for.dietry.req")], by="business_id", all.x=TRUE)
t <- t[order(t$caters.for.dietry.req), ]
################################################################
## Take a stratified sample of 50-50 diet req and not
################################################################
# install.packages("sampling")
# require(sampling)
# u <- strata(t, stratanames=c("caters.for.dietry.req"), size=c(sum(t$caters.for.dietry.req),sum(t$caters.for.dietry.req))
#        , method=c("srswor"), description=FALSE)

nonCater <- t[ t$caters.for.dietry.req==0, ]
cater <-t[t$caters.for.dietry.req==1, ]
u <- rbind( cater ,nonCater[sample(1:nrow(nonCater), nrow(cater)),  ] )
rm(cater, nonCater)

#write.csv(u, file="awsPredictDietReq_Data.csv")






















