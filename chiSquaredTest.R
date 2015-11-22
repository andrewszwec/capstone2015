##    AUTHOR:     Andrew Szwec
##    DATE:       2015-11-18
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Assess statistical significance and independance of variables
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    
##
##
##


## init
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)

#u <- subset(read.csv("awsPredictDietReq_Data.csv", stringsAsFactors=FALSE), select=c(-X))

#load(file="businessf.Rda")


#justRestAndCafe <- subset(read.csv(file='justRestAndCafe.csv', stringsAsFactors=FALSE), select=c(-X))

# just those reviews that mention some dietary requirment
a <- justRestAndCafe[ , c("review_id","text","stars", "business_id", "dietary.req")]

t <- merge( a ,businessf[ ,c("business_id","caters.for.dietry.req")], by="business_id", all.x=TRUE)

##############################################################################
## Check if dietary.req and caters.for.dietry.req are independant
##############################################################################
tbl <- table(t[,c("dietary.req","caters.for.dietry.req")])

chisq.test(tbl) 

# Pearson's Chi-squared test with Yates' continuity correction
# 
# data:  tbl
# X-squared = 1271.8, df = 1, p-value < 2.2e-16
#
# p-value is < 0.05 so we reject the null hypothesis that dietary.req and caters.for.dietry.req are independant. 
# Therefore dietary.req and caters.for.dietry.req are dependant variables 



##############################################################################
## Check if key words are independant
##############################################################################
tbl <- table(u[, c("also","back", "can")]) #,"eat","food","friend","get","good","great","just","like","love","menu","one","option","order","place","realli","restaur","servic","time","tri","vegan","vegetarian","will" )])
chisq.test(tbl)

# interestingly the freq of the keywords are not independant.
# The more words a review has the more words it has of each of the keywords and 
# hence they are related

# Cant use a t-test because these two variables are not normally distributed
hist(t$dietary.req)
hist(t$caters.for.dietry.req)

ttest <- t.test(t$dietary.req, t$caters.for.dietry.req)
ttest$statistic
ttest$p.value
