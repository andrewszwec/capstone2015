##    AUTHOR:     Andrew Szwec
##    DATE:       2015-10-07
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      EXPLORATORY ANALYSIS SCRIPT
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    How to filter for cafes and restaurants
##
setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")
set.seed(1234)

# Get a small set of the busniess array
a <- head(businessf, n=100L)

system.time(
                  categories <- unique( unlist(businessf$categories) )
            )

# There are 781 unique categories to choose from
# Which ones relate to restaurants?

# Save all categories to csv
# write.csv(categories, file="categories.csv",row.names = FALSE)




################################################################################
## Just cafes and restaurants 
################################################################################
# businessf has 61,184 rows, divide into 12 parts for 5,000 each



################################################################################
## Make small chunks of 5000
################################################################################
chunk_size = 5000      
start = 1   
end = nrow(businessf)
num_chunks = floor(nrow(businessf)/chunk_size)
do = 4
low <- 1; high <- chunk_size # init starting values

## Start do chunks
for (h in 1:num_chunks){

      # Get a small set of the busniess array
      
     
      a <- businessf[low:high, ]
            
      ################################################################################
      ## Find out how big matrix needs to be then allocate it
      ################################################################################
      
      n=1   # init m
      for (j in 1:length(a$categories) ){
            
            if ( (length(a$categories[[j]]) > 0) ) {  # Only increment counter if there are categories assigned to the business
                  
                  for (k in 1:length(a$categories[[j]]) ) { 
                        n = n + 1   # increment
                  }
            }
      }
      
      myCats <- data.frame(row=rep(NA, n), bus_id=rep(NA, n), cat=rep("", n),  # as many cols as you need
                       stringsAsFactors=FALSE)          # you don't know levels yet
      
      ################################################################################
      ## Now add data to data frame
      ################################################################################
      
      m=1   # init m
      for (j in 1:length(a$categories) ){
            
            if ( (length(a$categories[[j]]) > 0) ) {
            
                  for (k in 1:length(a$categories[[j]]) ) { 
                        row <- j
                        bus_id <- a$business_id[[j]]
                        cat <- a$categories[[j]][[k]]
                        # Only record the category if it is Restaurant or Cafe
                        #cat <- grep("[Restaurants|Cafes]",a$categories[[j]][[k]] , ignore.case=TRUE, perl=TRUE, value=TRUE)
                        myCats[m, ] <- c(row, bus_id, cat)
                        m = m + 1   # increment
                  }
            
            }
      }
      
      #write.csv(myCats, file=paste0("categories_",h,".csv"), row.names=FALSE)
     
      
      low = low + chunk_size ; high <- high + chunk_size # increment chunks
      
## End do chunks
}


################################################################################
## Do the last chunk I missed
################################################################################


h = num_chunks + 1
start = chunk_size * num_chunks + 1 
end = nrow(businessf)
a <- businessf[start:end, ]

################################################################################
## Find out how big matrix needs to be then allocate it
################################################################################

n=1   # init m
for (j in 1:length(a$categories) ){
      
      if ( (length(a$categories[[j]]) > 0) ) {  # Only increment counter if there are categories assigned to the business
            
            for (k in 1:length(a$categories[[j]]) ) { 
                  n = n + 1   # increment
            }
      }
}

myCats <- data.frame(row=rep(NA, n), bus_id=rep(NA, n), cat=rep("", n),  # as many cols as you need
                     stringsAsFactors=FALSE)          # you don't know levels yet

################################################################################
## Now add data to data frame
################################################################################

m=1   # init m
for (j in 1:length(a$categories) ){
      
      if ( (length(a$categories[[j]]) > 0) ) {
            
            for (k in 1:length(a$categories[[j]]) ) { 
                  row <- j
                  bus_id <- a$business_id[[j]]
                  cat <- a$categories[[j]][[k]]
                  # Only record the category if it is Restaurant or Cafe
                  #cat <- grep("[Restaurants|Cafes]",a$categories[[j]][[k]] , ignore.case=TRUE, perl=TRUE, value=TRUE)
                  myCats[m, ] <- c(row, bus_id, cat)
                  m = m + 1   # increment
            }
            
      }
}

write.csv(myCats, file=paste0("categories_",h,".csv"), row.names=FALSE)






# Time for 1000 records
# user  system elapsed 
# 0.810   0.084   0.896 

# Time for 5000 records
# user  system elapsed 
# 7.882   3.073  10.957 




################################################################################
## Putting it back together
################################################################################

rm(tmp)
tmp <- read.csv("categories_1.csv")
numRowsTmp <- nrow(tmp)

for (i in 2:num_chunks+1){
      tmp <- rbind(tmp, read.csv(paste0("categories_", i, ".csv") ) )
}

################################################################################
## Get businesses that are just cafes or restaurants
################################################################################

cafesRests <-  tmp[which(tmp$cat=="Restaurants" | tmp$cat=="Cafes") , ]
nrow(cafesRests)
# 21087 rows

# Write it out for later
write.csv(cafesRests, file="Business_id-Cafe-Restaurants.csv" , row.names=FALSE)
cafesRests <- read.csv("Business_id-Cafe-Restaurants.csv")




