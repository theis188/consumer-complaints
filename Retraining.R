rm(list = ls())
dat <- read.csv("D:/StatsFun/Consumer_Complaints.csv", header = TRUE) ## read data file

headers <- names(dat) ## View header names

in_progress <- which(sapply(dat[13], function(x) (x=="In progress"))) ## find claims in progress
untimely <- which(sapply(dat[13], function(x) (x=="Untimely response"))) ## find claims in progress

### dim(dat[-c(in_progress,untimely),]) ## clear claims in progress
dat <- dat[-c(in_progress,untimely),]

levels(dat$Issue)[levels(dat$Issue) == "Closed with explanation"] <- "No"

Uniform_prior <- table(dat[15])[2]/(table(dat[15])[1] + table(dat[15])[2])

j <- with(dat, table(Product,Consumer.disputed.))
h <- chisq.test(j)

chisqval <- array(0, dim=c(dim(j)[1],2))

for (i in 1:dim(j)[1])
{
	Total = j[i,1]+j[i,2]
	Expected = Total*Uniform_prior
	Yes = j[i,2]
	chisq_val = (Yes - Expected)^2/Expected
	chisqval[i,1] = 1-pchisq(chisq_val,1)
	chisqval[i,2] = (Yes/Total)/Uniform_prior
}
