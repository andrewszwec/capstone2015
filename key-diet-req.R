################################################################################
# load data
################################################################################

load(file="reviewf.Rda")
load(file="bus_non_diet_req.Rda")
load(file="dr_bus_reviews.Rda")
load(file="businessf.Rda")
load(file="allreviews.Rda")
load(file="gfReviews.Rda")
load(file="non_gfReviews.Rda")

words <- read.table("20k.txt", header=FALSE, sep=",")

################################################################################
## Look for the key dietary words
################################################################################
diet_review <- reviewf[ grep("gluten\\sfree|gluten-free|gluten intolerant|dairy intolerant|dairy\\sfree|dairy-free|lactose\\sfree|lactose-free|sugar\\sfree|sugar-free|vegan|vegetarian|coeliac|kosher|halal|soy\\sfree|soy-free|flexitarian|diabetic", reviewf$text, ignore.case=TRUE, value = FALSE ),]
nrow(diet_review)
mean(diet_review$stars)
# 35,112 reviews with some word related to a dietary requirement

################################################################################
## Make Features
################################################################################
# Make new col on end of reviews that indicates whether talks about a dietary requirement or not
reviewf$dietary.req <- ifelse(reviewf$review_id %in% diet_review$review_id, 1, 0)
reviewf$text.len <- nchar( reviewf$text )
system.time( reviewf$num.words <- sapply(gregexpr("\\S+", reviewf$text), length) )
# user  system elapsed 
# 117.079   4.825 122.228 


################################################################################
## Num of Special Characters
################################################################################

a <- head(reviewf, n=1000L)

# for each review in matrix split into letters and count number of special characters
for (i in 1:nrow(a)) {
      b <- strsplit(a$text[i], "")[[1]]        # Split text into letters
      a$numSpecialChars[i] <- length(grep('[^\\w\\s]', b, ignore.case=TRUE, value=TRUE, perl=TRUE))  # count number of special chars
      a$numCapitals[i] <- length(grep('[A-Z]', b, ignore.case=FALSE, value=TRUE, perl=TRUE))      
}



################################################################################
## Bag of words 
################################################################################
## Load the businesses that are just cafes or restaurants
cafesRests <- read.csv("Business_id-Cafe-Restaurants.csv")

# How many reviews total?
nrow(reviewf)
# 1,569,264 rows

# Get the Res+Cafe reviews
justRestAndCafe <- reviewf[reviewf$business_id %in% cafesRests$bus_id, ]

# How many reviews for Cafes and Restaurant?
nrow(justRestAndCafe)
# 932,194 reviews

nrow(justRestAndCafe)/nrow(reviewf)*100
# Cafes and Restaurants make up 59.40% of reviews


#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
# For each review, find unique words, count how many of each word per review
# 
#-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
a <- head(justRestAndCafe, n=100L)
num_rows <- nrow(a)

# For each row in the set of Cafes and Rest (justRestAndCafe) do the following things
# 1. Get record, split up by " " space
# 2. Get the unique words in vector
# 3. Search the text for each of the unique words and count the occurances
#

# Init
split <- " " # string to split on

for (i in 1:3 ){  #num_rows){
      review <- a$text[i]
      review_id <- a$review_id[i]
      words <- strsplit(review, split)[[1]]
      uwords <- unique(words)
      len_uwords <- length(uwords)
      
      # For each unique word count how many occurances in string
      for (j in 1:len_uwords){
            pattern <- uwords[j]
            
            print(c(review_id, pattern ,strcount(review, pattern, split) ) )
  
      }
      
}



# Count how many of each
strcount <- function(x, pattern, split){ 
      unlist(lapply(
            strsplit(x, split),
            function(z) na.omit(length(grep(pattern, z)))
      ))    
}

r <- head(reviewf$text, n=1L)[[1]]
words <- strsplit(r, " ")[[1]]
uwords <- unique(words)
length(uwords)


# for each unique word count how many occurances
for (i in 1:length(uwords)){
      pattern <- uwords[i]
      
      print(c(pattern ,strcount(r, pattern, " ") ) )
     
      
}









# # Extract one text thing to study
# a <- head(reviewf[, c("review_id","text")], n=1L)
# # Split it into pieces
# w <- strsplit(a, "\\s")
# #check if each word is in the dictionary
# lapply(w, function(x){ifelse(words$V1 %in% x ,1,0) } )

# Now on a bigger scale
a <- head(reviewf[, c("review_id","text")], n=3L)
# w is an array of lists that contain all the words in each review
a$split <- strsplit(a$text, "\\s")

a$attr <- lapply(a$split, function(x){ifelse(words$V1 %in% x ,1,0) } )
# init a big df with 16893 cols. each column is a dictionary word
df = data.frame(matrix(vector(), 0, length(a$attr[[1]]),),
                stringsAsFactors=F)

for (i in 1:length(a$attr)){
     print(
         data.frame(a$review_id[[i]], a$attr[[i]]  )  
     )    
}



################################################################################
# Close up the shop
################################################################################
save(reviewf,file="reviewf.Rda")
save(bus_non_diet_req,file="bus_non_diet_req.Rda")
save(dr_bus_reviews ,file="dr_bus_reviews.Rda")
save(businessf ,file="businessf.Rda")
save(allreviews ,file="allreviews.Rda")
save(gfReviews ,file="gfReviews.Rda")
save(non_gfReviews ,file="non_gfReviews.Rda")
