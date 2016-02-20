rm(list = ls())
dat <- read.csv("D:/StatsFun/Consumer_Complaints.csv", header = TRUE) ## read data file

headers <- names(dat) ## View header names

### Simplify response codes / eliminate irrelevant cases
in_progress <- which(sapply(dat[13], function(x) (x=="In progress"))) ## find claims in progress
untimely <- which(sapply(dat[13], function(x) (x=="Untimely response"))) ## find claims in progress

### dim(dat[-c(in_progress,untimely),]) ## clear claims in progress
dat <- dat[-c(in_progress,untimely),]

### Code switching ## collapse categories to yes or no
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed with explanation"] <- "No"
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed without relief"] <- "No"
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed"] <- "No"

levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed with monetary relief"] <- "Yes"
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed with non-monetary relief"] <- "Yes"
levels(dat$Company.response.to.consumer)[levels(dat$Company.response.to.consumer) == "Closed with relief"] <- "Yes"

dat$Company.response.to.consumer <- factor(dat$Company.response.to.consumer)

Uniform_prior <- table(dat[13])[2]/(table(dat[13])[1] + table(dat[13])[2])

j <- with(dat, table(Company.response.to.consumer,Company))
jj <- j[1,]+j[2,]
jjj <- jj > 25
newj <- j[,jjj]
newjj <- j[,jjj]

chisqval <- array(0, dim=c(dim(newj)[2],2))

for (i in 1:dim(newj)[2])
{
	Total = newj[1,i]+newj[2,i]
	Expected = Total*Uniform_prior
	Yes = newj[2,i]
	chisq_val = (Yes - Expected)^2/Expected
	chisqval[i,1] = 1-pchisq(chisq_val,1)
	chisqval[i,2] = (Yes/Total)/Uniform_prior
}

rownames(chisqval) <- names(newj[1,])
lift_inds <- sort(chisqval[,2],decreasing = TRUE, index.return = TRUE)$ix
sig_inds <- sort(chisqval[,1],decreasing = TRUE, index.return = TRUE)$ix
lift_list <- chisqval[lift_inds,]
sig_list <- chisqval[sig_inds,]

badnames <- names(lift_list[,1])
j[,badnames]
lift_list <- cbind(lift_list, array(0, dim=c(740,2)))
lift_list[,3:4] <- t(j[,badnames])

#Cutoff_value = 3 ## sets minimum number of reports

#z <- table(dat[8]) > Cutoff_value ##
#x <- which(z %in% c(FALSE)) ##

#for (i in dim(dat)[1]:1)
#{
#	if (z[dat[i,13]]) {
#		dat <- dat[-i]
#	}
#	
#}

 