# Question Investigation


# These are all the reviews 
# review.samples$text

#user_id, review_id, business_id, business.gf
newNames <- gsub('[ |-]','.', names(businessf))
names(businessf) <- newNames

# find people who mention dietary requirements in their reviews
# rows where they mention gluten - there are 61 in the sample that i took initially
gf <- review.samples[ grepl('gluten', review.samples$text, ignore.case=TRUE ),] 
gf.users <- gf$user_id

# What is the mean star rating for the gf restaurant?
mean(gf$stars)
# 4.081967  <- that is pretty high :-)

mean(reviewf$stars)
# 3.742656 <- on the whole ppl who mention gf in their review have a higher star rating for restaurants than the general pop

# What about when we look at the reviews of just this subset of people?
gf.users.other.reviews <- reviewf[reviewf$user_id %in% gf.users, ]
mean(gf.users.other.reviews$stars)
# 3.719516

# so of the 61 users who mentioned gf in their review
# they have done 3633 reviews - WOW!
require(plyr)
# Which users comment the most?
a <- count(gf.users.other.reviews, vars="user_id")
a[order(a$freq, decreasing=TRUE),]

# wx12_24dFiL1Pc0H_PygLw this guy has 695 reviews WOW!
# b is all the reviews for this one dude who has mentioned gf somewhere
b <- reviewf[which(reviewf$user_id == "wx12_24dFiL1Pc0H_PygLw"),]
# now remove the reviews with gf in them

rev_id <- sqldf("select user_id, review_id, stars from gf where user_id == 'wx12_24dFiL1Pc0H_PygLw' ")


sqldf("select bb.*
      from b bb
      where review_id != 'uDgNVMOrZTq8dAGJYHnPsg'"
) 

# Custom NOT IN function
'%ni%' <- Negate('%in%')
b.2 <- b[b$review_id %ni% gf$review_id, ]
nrow(b)
# b   695 -- Has gf star rating
# b.2 694 -- gf star rating removed

mean(b.2$stars)
# This dude with 650 reviews, one of which says gf 
# has a mean stars of 3.936599
# whereas his review with gf mentioned was 4 stars. Very small difference.



####################################################################################
# Do this on a larger scale
####################################################################################
gf <- reviewf[ grepl('gluten', reviewf$text, ignore.case=TRUE ),] 

## reviews from ppl who have ever mentioned gf
allreviews  <- sqldf(
      "select 
            r.*
      from reviewf r
      inner join gf g
      on r.user_id = g.user_id
")
# 433404 reviews with gf occuring in review text
length(unique(allreviews$user_id))
# 4709 user_ids with gf in their review text
mean(allreviews$stars)
# 3.813525

## Just the GF reviews
gfReviews <- sqldf("
      select *
      
      from  allreviews a
      inner join gf g
      on a.review_id = g.review_id
      
      ")
mean(gfReviews$stars)
# 4.071368
# Calculate gf star uplift
mean(gfReviews$stars) - mean(allreviews$stars)
# percent uplift
(mean(gfReviews$stars) - mean(allreviews$stars))/mean(allreviews$stars)*100


non_gfReviews <- sqldf("
      select *
      
      from  allreviews a
      left join gf g
      on a.review_id = g.review_id
      where g.review_id is null
      ")
mean(non_gfReviews$stars)
# 3.786514

# Calculate gf star uplift
mean(gfReviews$stars) - mean(non_gfReviews$stars)
# percent uplift
(mean(gfReviews$stars) - mean(non_gfReviews$stars))/mean(non_gfReviews$stars)*100
## therefore, people who mention gf in their review tend to rate the review with gf in it than other reviews.

# 1. Go get all restaurants that have gf in their reviews
# 2. Get all restaurants that say they cater for gf
# 3. what is the overlap?
# 4. Is there a difference in star rating?
require(data.table)
# 1. Go get all restaurants that have gf in their reviews
business_gf_review <- sqldf("
      select distinct business_id from gf 
      ")
# 2. Get all restaurants that say they cater for gf
business_with_gf <- data.table(businessf[which(businessf$attributes.Dietary.Restrictions.gluten.free == TRUE ),])

# 3. what is the overlap?
a <- business_gf_review$business_id %in% business_with_gf$business_id

# 3b. look at the businesses in business_gf_review
business_gf_review.details <- businessf[businessf$business_id %in% business_gf_review$business_id, ]
business_gf_review.details$attributes.Dietary.Restrictions.gluten.free
# all the restaurants that have a comment about gluten free in the reviews are not classified as gf in the business detail
mean(gf$stars)
# on avg the sample restaurants with gf in their review had a star rating of 4.081967


# There is something about the businessf df which doesnt work.
# do this funny thing to fix it

# first convert the reviewf df into a dt
require(data.table)
r <- data.table(reviewf)
#rm(reviewf)
reviewf <- r
#rm(r)
r <- subset(reviewf, select=c("user_id", "business_id", "stars", "date"))
head(r)
bus_gf <- subset(business_with_gf, select=c("business_id"))

# 4a. Go get the star ratings for the restaurants that say they are gf
reviews_bus_cat_gf <- sqldf("select a.user_id,
                                    a.business_id,
                                    a.stars,
                                    a.date
                                    
                              from r a
                              inner join bus_gf b
                              on a.business_id = b.business_id
                 
")
mean(reviews_bus_cat_gf$stars)
# Restaurants that say they are gf (there is only 9) 
# have an average star rating of 3.883903



# find people who mention dietary requirements in their reviews
# rows where they mention gluten - there are 61 in the sample, how many in the full set?
gf.2 <- review.samples[ grepl('gluten', reviewf$text, ignore.case=TRUE ),] 











