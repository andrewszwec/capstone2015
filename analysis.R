################################################################################
# STATS
################################################################################

##----------------------------------------------------------------------------##
## REVIEWS - Sample Stats
##----------------------------------------------------------------------------##

# Dates
min(review.samples$date)
"2005-04-22"
max(review.samples$date)
"2015-01-08"

# Ave length of review
a <- mean(unlist(lapply(review.samples$text, nchar) ))
# Avg review length is 675 words

# Avg number stars
mean(review.samples$stars)
# 3.74

# Type 
# is "review"

# votes.funny
mean(review.samples$votes.funny)
# 0.4778

# votes.useful
mean(review.samples$votes.useful)
# 1.05

# votes.
mean(review.samples$votes.cool)
# 0.591


#require( "plyr")
#b <- llply(a, unlist)


##----------------------------------------------------------------------------##
## BUSINESS - Sample Stats
##----------------------------------------------------------------------------##
unique(business.samples$open)
# TRUE, FALSE

# Business Categories - requires a concatenation process on the sublists
categories <- unique(unlist(unique(business.samples$categories)))

c <- lapply(head(business.samples$categories),unlist)

unique(business.samples$city)
unique(business.samples$state)

d <- merge(business.samples, stcd, by.x='state', by.y= 'ANSI', x.all)

data.frame(d$state, d$Name)
unique(as.character(d$Name))

aggregate(data.frame(d$state, d$review_count), by=list('state'), FUN=sum)

##----------------------------------------------------------------------------##
## TIPS - Sample Stats
##----------------------------------------------------------------------------##
head(tipf[which(tipf$likes!=0),])
businessf[businessf['attributes.Dietary Restrictions.gluten-free']!=NA,]



