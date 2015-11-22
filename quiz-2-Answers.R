# Quiz Questions

# How many lines of text are there in the reviews file (in orders of magnitude)?

nrow(raw.review)
## 1,569,264

raw.review[90:110,]$text[grepl("eat", raw.review[90:110,]$text)]

raw.review[100,]$text


# Get reviews with 5 stars
fiveStars <- subset(raw.review[raw.review$stars == 5, ], select=c(user_id, stars)) 

numFiveStars <- nrow(fiveStars)

perFiveStars <- numFiveStars/nrow(raw.review)

## Question 3
nrow(raw.business)
61,184

# Question 7
# how many businesses have wifi?
havWiFi <- raw.business$attributes$"Wi-Fi"[which(raw.business$attributes$"Wi-Fi"== "free" | raw.business$attributes$"Wi-Fi"=="paid")]

haveWiFi <- length(havWiFi)

# how many of thes offer free?
haveFree <- length(havWiFi[havWiFi == "free"])

perWithFree  <- haveFree / haveWiFi 

haveFree / nrow(raw.business)

# Method 2
# What percent had free wifi conditional on if they had a response
d <- raw.business[which(raw.business$attributes$"Wi-Fi" != "NA" ),] 
withResponse <- nrow(d)

haveFree / withResponse

# Q9
raw.tip[1000,]

# Q10

# Sum number of funny votes by user
users.votes <- aggregate(userf[,c("compliments.funny")], by=list(userf$name), FUN="sum", na.rm = TRUE)

users.votes[which( users.votes$x > 10000 ), ]
Group.1     x
2075 Anthony 11374
4606   Brian 15362


names(userf)

# Q11

myvotes <- aggregate(userf[,c("compliments.funny", "fans")], by=list(userf$name), FUN="sum", na.rm = TRUE)

# count when a user has more than one fan and more than one compliment vote = funny
myvotes$fanGtOne <- ifelse(myvotes$fans>1, 1, 0)
myvotes$voteGtOne <- ifelse(myvotes$compliments.funny>1, 1, 0)


twoByTwo <- table(myvotes[, c("fanGtOne", "voteGtOne")])

fisher.test(twoByTwo)





