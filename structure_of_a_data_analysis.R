library(kernlab)
data(spam)
str(spam[,1:5])

set.seed(3435)

trainIndicator <- rbinom(4601, size = 1, prob = 0.5)
table(trainIndicator)

trainSpam <- spam[trainIndicator == 1, ]
testSpam <- spam[trainIndicator == 0, ]

names(trainSpam)

head(trainSpam)

table(trainSpam$type)

setwd("/home/rogelio/Desktop/datasciencecoursera/reproducible_research")

png("./spam_or_not.png")
plot(log10(trainSpam$capitalAve + 1) ~ trainSpam$type)
dev.off()

# plot("./correlations.png")
# plot(log10(trainSpam[, 1:4] + 1))
# dev.off()

png("./hcluster.png")
hcluster <- hclust(dist(t(trainSpam[,1:57])))
plot(hcluster)
dev.off()

hclusterUpdated <- hclust(dist(t(log10(trainSpam[,1:55] + 1))))

png("./hcluster_updated.png")
plot(hclusterUpdated)
dev.off()

trainSpam$numType <- as.numeric(trainSpam$type) - 1
costFunction <- function(x,y) sum(x != (y > 0.5))
cvError <- rep(NA, 55)
library(boot)

# tests which one of the variables in the dataset is the better 
# predictor for an email to be SPAM

for(i in 1:55){
        lmFormula <- reformulate(names(trainSpam)[i], response = "numType")
        glmFit <- glm(lmFormula, family = "binomial", data = trainSpam)
        cvError[i] <- cv.glm(trainSpam, glmFit, costFunction, 2)$delta[2]
}

# see which variable minimizes the error
names(trainSpam)[which.min(cvError)]
# turns out to be charDollar

# use the best model from the group
predictionModel <- glm(numType ~ charDollar, family = "binomial", 
                       data = trainSpam)

# test predictor on test set
predictionTest <- predict(predictionModel, testSpam)
predictedSpam <- rep("nonspam", dim(testSpam)[1])

# clasify as spam those with prob > 0.5
predictedSpam[predictionModel$fitted > 0.5] <- "spam"

# compare outcome of the model with actual classification
a <- table(predictedSpam, testSpam$type)

# margin of error
err <- (a[[2]] + a[[3]])/(a[[1]] + a[[2]] + a[[3]] + a[[4]])


