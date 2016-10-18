#Author(s): Michael Murrietta
#Date: 2016-10-13 (started)

#exploring data to visualize relationship between square footage and sale price

#assumes 'clean_csv_for_project.R' has already been run in the current session.
#global environmnet should contain a dataframe called bk

#"clean" set gsqft: no NA in gross.square.feet AND gross.square.feet > 1
#AND no NA in sale.price.n
gsqft <- bk[(is.na(bk$gross.square.feet) == F) & (bk$gross.square.feet > 1) & (is.na(bk$sale.price.n) == F),]
hist(gsqft$sale.price.n) #heavy right skew, we will look instead at the log of this
plot(log(gsqft$gross.square.feet),log(gsqft$sale.price.n))

#there are two unnatural looking line groupings
summary(gsqft[(log(gsqft$gross.square.feet) > 4.8) & (log(gsqft$gross.square.feet) < 5.0),'gross.square.feet'])
summary(gsqft[(log(gsqft$sale.price.n) > 2) & (log(gsqft$sale.price.n) < 3),'sale.price.n'])

#These really don't appear to be natural so I'll omit them from the analysis set
#sale.price.n > 10, gross.square.feet != 129
gsqft <- gsqft[((gsqft$sale.price.n > 10) & (gsqft$gross.square.feet != 129)),]
#reviewing the plot of the logs reveals something looking more natural
plot(log(gsqft$gross.square.feet),log(gsqft$sale.price.n))
abline(v=3.5,col='red',lwd=2)
abline(v=5.5,col='red',lwd=2)

#groups as indicated in the current plot (or above two lines):
#   log(gross.square.feet) < 3.5
#   log(gross.square.feet) >= 3.5 and log(gross.square.feet) < 5.5
#   log(gross.square.feet) >= 5.5
lowsf <- gsqft[(log(gsqft$gross.square.feet) < 3.5),]
midsf <- gsqft[((log(gsqft$gross.square.feet) >= 3.5) & log(gsqft$gross.square.feet) < 5.5),]
hisf <- gsqft[(log(gsqft$gross.square.feet) >= 5.5),]
#plot histogram of each group with median (in read) and mean (in cyan)
par(mfrow=c(2,2), oma = c(0, 0, 2, 0))
hist(log(lowsf$sale.price.n), main = "Low Sqft. Group")
abline(v=median(log(lowsf$sale.price.n)),col='red',lwd=2)
abline(v=mean(log(lowsf$sale.price.n)),col='cyan',lwd=2)
hist(log(midsf$sale.price.n), main = "Mid Sqft. Group")
abline(v=median(log(midsf$sale.price.n)),col='red',lwd=2)
abline(v=mean(log(midsf$sale.price.n)),col='cyan',lwd=2)
hist(log(hisf$sale.price.n), main = "Hi Sqft. Group")
abline(v=median(log(hisf$sale.price.n)),col='red',lwd=2)
abline(v=mean(log(hisf$sale.price.n)),col='cyan',lwd=2)
hist(log(gsqft$sale.price.n), main = "ALL Sqft. Group")
abline(v=median(log(gsqft$sale.price.n)),col='red',lwd=2)
abline(v=mean(log(gsqft$sale.price.n)),col='cyan',lwd=2)
mtext("Distribution of sale prices by group", outer=TRUE,cex=1.5)
par(mfrow=c(1,1)) #reset par

#No real suprise: the "ALL" group has median close enough to the mean
#and seems normal enough so we can use it to make inferences about
#the mean of the non log-transformed data in gsqft
qqnorm(log(gsqft$sale.price.n)) #more evidence of the normal-esque distribution
#so now we can do a one sample t-test and also build the confidence interval
#on the population mean of all sales in manhattan in this data set with
#no NA in gross.square.feet AND gross.square.feet > 1 AND no NA in sale.price.n
#...code to do that here

#ideas: new variable, price per sqft calculated variable,