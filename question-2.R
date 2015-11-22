#
#     Question 2
#     Do the cafes/restaurants that cater for dietary requirements, like gluten free,
#     also have a higher star rating or better customer service? e.g. “Waiter Service”
#
#

# Create a pattern column
businessf$pattern  <- paste0(businessf$attributes.Dietary.Restrictions.dairy.free ,businessf$attributes.Dietary.Restrictions.gluten.free ,businessf$attributes.Dietary.Restrictions.vegan,businessf$attributes.Dietary.Restrictions.kosher ,businessf$attributes.Dietary.Restrictions.halal
, businessf$attributes.Dietary.Restrictions.soy.free, businessf$attributes.Dietary.Restrictions.vegetarian)


table(businessf$pattern)

# find the trues and set the flag
businessf$caters.for.dietry.req <- ifelse(grepl("TRUE", businessf$pattern )==TRUE, 1,0)

# Businesses that cater for some dietary requirement
bus_w_diet_req <- businessf[which(businessf$caters.for.dietry.req == 1),]
sum(businessf$caters.for.dietry.req)
# There are only 131 restaurants that cater for dietry requirements according to their listing out of 61,184
nrow(businessf)

# Restauants that dont cater for DR
businessf[which(businessf$caters.for.dietry.req == 0 && (category == cafe || category==restaurant)),]

# algo to unnest
unnest <- function(x) {
      if(is.null(names(x))) {
            list(unname(unlist(x)))
      }
      else {
            c(list(all=unname(unlist(x))), do.call(c, lapply(x, unnest)))
      }
}

# Find out of these 131 restaurants that cater for dietary requirements, how many reviews do they have?
bus_w_diet_req[, c("business_id", "caters.for.dietry.req")]

dr_bus_reviews  <- reviewf[which(reviewf$business_id %in% bus_w_diet_req$business_id), ]
nrow(dr_bus_reviews)
 # there are 13,005 reviews for these 131 restaurants that cater for DR's


# the average number of stars for a restaurant that caters for dr is 3.986544 from reviews and 3.847328 from business table
mean(dr_bus_reviews$stars)
mean(bus_w_diet_req$stars)

# table(businessf[,c("caters.for.dietry.req", "stars", "attributes.Waiter.Service")])


######################################################
# Ratings calculated from Reviews table:
#
#                 Caters for DR     No Catering
#     avg star    3.99              
#     
######################################################


# Find businesses that do not cater for DRs
bus_non_diet_req <- businessf[which(businessf$caters.for.dietry.req == 0),]
mean(bus_non_diet_req$stars)
# Avg stars for business that does not cater for dietary requirements = 3.672932

# find uplift for catering for dietary requirements
( mean(bus_w_diet_req$stars) - mean(bus_non_diet_req$stars) ) / mean(bus_non_diet_req$stars) * 100

# Find user reviews for businesses that do not cater for DRs
reviews_non_dr_bus <- reviewf[ which(reviewf$business_id %in% bus_non_diet_req$business_id), ]
names(reviews_non_dr_bus)
mean(reviews_non_dr_bus$stars)
# avg stars for non dietary requirement catering business is 3.740618 using the reviews
# uplift of dr catering vs non dr catering business is

( mean(dr_bus_reviews$stars) - mean(reviews_non_dr_bus$stars) ) / mean(reviews_non_dr_bus$stars) * 100






