##    AUTHOR:     Andrew Szwec
##    DATE:       2015-10-07
##    SUBJECT:    DATA SCIENCE CAPSTONE 
##    TITLE:      Create feature matrix (AUTOMATED)
##    SUBTITLE:   
##    
##    DESCRIPTION: 
##    EC2 MAIN - calls ec2-createFeatures
##     
##
##    TRANSFER FILE TO EC2
##    scp -i /Users/andrewszwec/Documents/AWS/machome.pem /Users/andrewszwec/Documents/Coursera/dataScienceSpecialisation/capstone/ec2-createFeatures.R  ec2-user@ec2-52-65-14-48.ap-southeast-2.compute.amazonaws.com:/home/ec2-user 
##    
##    TO RUN TYPE
##    Rscript ec2-createFeatures.R 
##
##    TO GET FILES FROM S3
##    wget https://s3-ap-southeast-2.amazonaws.com/awsmlsz/justRestAndCafe.csv
##
##    CRAN MIRRORS
##    Select 23 for HTTPS mirrors > then select 4 Canberra


setwd("/home/ec2-user")
set.seed(1234)
#install.packages("tm")
require(tm)

#install.packages("SnowballC")
require(SnowballC)

# Load data file
#justRestAndCafe  <- read.csv("justRestAndCafe.csv")

source("ec2-createFeatures.R", print.eval=TRUE)