library(kernlab)
data(spam)
str(spam[,1:5])

set.seed(3435)

trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam <- spam[trainIndicator == 1, ]
testSpam <- spam[trainIndicator == 0, ]

