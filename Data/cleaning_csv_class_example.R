# Author: Michael Murrietta
# date: 2016-10-13
# note: adapted from 
#     Author: Benjamin Reddy
#     Taken from pages 49-50 of O'Neil and Schutt

#this is intended to be run line-by-line, or a few lines at a time
#to facilitate exploration and data cleaning

library(plyr)
library(gdata)
setwd("C:/SMU/2016 Fall/Doing Data Science/unit 07/Group Project/")

bk <- read.csv("./Data/manhattan.csv",header=TRUE)

## Check the data
head(bk)
summary(bk)
str(bk) #this is probably the most useful of the 3
unique(bk$NEIGHBORHOOD) #this is also useful when deciding how to slice

## clean/format the data with regular expressions
## More on these later. For now, know that the
## pattern "[^[:digit:]]" refers to members of the variable name that
## start with digits. We use the gsub command to replace them with a blank space.
# We create a new variable that is a "clean' version of sale.price.
# And sale.price.n is numeric, not a factor.
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", bk$SALE.PRICE))
count(is.na(bk$SALE.PRICE.N))

names(bk) <- tolower(names(bk)) # make all variable names lower case
## Get rid of leading digits
bk$gross.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$gross.square.feet))
bk$land.sqft <- as.numeric(gsub("[^[:digit:]]","", bk$land.square.feet))
bk$year.built <- as.numeric(as.character(bk$year.built))

## do a bit of exploration to make sure there's not anything
## weird going on with sale prices
attach(bk)
hist(sale.price.n) 
plot(sale.price.n,gross.sqft) #there's a major outlier in sale.price.n
detach(bk)

## keep only the actual sales

bk.sale <- bk[bk$sale.price.n!=0,] #returns all columns of bk but only non-zero sales
plot(bk.sale$gross.sqft,bk.sale$sale.price.n)
plot(log10(bk.sale$gross.sqft),log10(bk.sale$sale.price.n))

## for now, let's look at 1-, 2-, and 3-family homes
#returns data frame but only for rows with "FAMILY" appearing in the
#building.class.category column
bk.homes <- bk.sale[which(grepl("FAMILY",bk.sale$building.class.category)),]
dim(bk.homes)
plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))
summary(bk.homes[which(bk.homes$sale.price.n<100000),])


## remove outliers that seem like they weren't actual sales
bk.homes$outliers <- (log10(bk.homes$sale.price.n) <=5) + 0
bk.homes <- bk.homes[which(bk.homes$outliers==0),]
plot(log10(bk.homes$gross.sqft),log10(bk.homes$sale.price.n))
