# Author: Michael Murrietta
# date: 2016-10-13

library(plyr)
library(gdata)

#set this to your working directory (the one directly above the "Data" folder)
setwd("C:/SMU/2016 Fall/Doing Data Science/unit 07/Group Project/rollingsales_manhattan/")

#read in the data
bk <- read.csv("./Data/manhattan.csv",header=TRUE)

#replace any non-digit sale.price values with blank, return as numeric to 
#the new column sale.price.n (as in example script):
bk$SALE.PRICE.N <- as.numeric(gsub("[^[:digit:]]","", bk$SALE.PRICE))

#straightforward conversions, inspired by looking at the str(bk) output
bk$BLOCK <- as.character(bk$BLOCK)
bk$ZIP.CODE <- as.character(bk$ZIP.CODE)
bk$ADDRESS <- trim(as.character(bk$ADDRESS))
bk$LAND.SQUARE.FEET <- as.numeric(bk$LAND.SQUARE.FEET)
bk$GROSS.SQUARE.FEET <- as.numeric(bk$GROSS.SQUARE.FEET)

#special attention for sale.date column to convert to date
bk$SALE.DATE <- as.Date(bk$SALE.DATE, "%m/%d/%Y") #note the capital Y, it 
summary(bk$SALE.DATE) #check it out, make sure min, max, etc are reasonable
hist(as.numeric(bk$SALE.DATE)) #another way to check it out

#useless columns?
unique(bk$BOROUGH) #all the same, delete?
unique(bk$EASE.MENT) #all the same, delete?
#uncomment the below two lines to drop the columns above
#drops <- c("BOROUGH","EASE.MENT")
#bk[ , !(names(bk) %in% drops)]

#anomalies in the year.built column
hist(bk$YEAR.BUILT) #about 2500 have 0 as year built, assign average year built?

#put the column names in lowercase
names(bk) <- tolower(names(bk))

#used this to see that each date followed a typical date format
#didn't realize I was omitting the second (required) argument of
#as.Date()! Now this isn't needed but I'll leave it as a reference
#for regex usage with grep
#bk$SALE.DATE[which(grepl("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}",bk$SALE.DATE))]

#at this point bk has specifically typed columns and no rows are omitted
#the data is ready to be explored/analyzed