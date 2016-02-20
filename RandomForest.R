rm(list = ls())
dat <- read.csv("D:/StatsFun/Consumer_Complaints.csv", header = TRUE) ## read data file

headers = names(dat)

in_progress <- which(sapply(dat[13], function(x) (x=="In progress"))) ## find claims in progress
untimely <- which(sapply(dat[13], function(x) (x=="Untimely response"))) ## find claims in progress

### dim(dat[-c(in_progress,untimely),]) ## clear claims in progress
dat <- dat[-c(in_progress,untimely),]

yesrows = which(dat[15] == "Yes")
norows = which(dat[15] == "No")
yesfrac = 0.5
yesnum = ceiling(10000*yesfrac)
nonum = 10000 - yesnum
rows = c(yesrows[1:yesnum], norows[1:nonum])
outcome_ind = 15
toInclude = c(2,3,4,5,9,11)
testdat <- dat[rows,toInclude]
outcome <- dat[rows,outcome_ind]
array <- model.matrix(~., data=testdat[1])

for (j in 2:length(toInclude))
{
	bbb <- model.matrix(~., data=testdat[j])
	array <- cbind(array,bbb)
}

library(randomForest)

forests <- randomForest(x = array, y = outcome)

###########################
toRemove = c(1, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16)
outcome_ind = 15
rows = 1:100
testdat <- dat[-toRemove]
testdat <- testdat[rows,]
outcome <- dat[rows,outcome_ind]
testdat$Company <- factor(testdat$Company)
testdat$Sub.product <- factor(testdat$Sub.product)
testdat$State <- factor(testdat$State)
testdat$Issue <- factor(testdat$Issue)

###toRemove = c(1, 5, 6, 7, "10", )

randomForest()