# Names identification

setwd("~/Documents/Coursera/dataScienceSpecialisation/capstone")

girls <- read.fwf("census-dist-female-first.txt", header = FALSE, widths=c(13, 8, 8,8), col.names=c('name', 'v1', 'v2', 'rank'))
boys <- read.fwf("census-dist-male-first.txt", header = FALSE, widths=c(13, 8, 8,8), col.names=c('name', 'v1', 'v2', 'rank'))

#put girls and boys names into a table with a col that says if girl or boy
# then join to the user table to get gender in the user table
# then figure out the distribution of boys and girls?

boys$gender <- "boy"
girls$gender <- "girl"

us.names  <- rbind(data.frame(name = boys$name, gender = boys$gender), data.frame(name = girls$name, gender = girls$gender))

# Prepare data for merging
# Data in girls and boys needs to be trimed
install.packages('gdata')
us.names$name <- trim(us.names$name)

# Turn names in raw data into uppercase for merging
raw.user$name  <- toupper(raw.user$name)

users <- merge(raw.user, us.names, by.x='name', by.y = 'trimName', all.x = TRUE  )

# Take a random sample of girls and boys to see names
# sample <- raw.user[sample(length(raw.user$name), 100), ]




users <- merge(sample, us.names, by.x='name', by.y = 'trimName', all.x = TRUE  )


