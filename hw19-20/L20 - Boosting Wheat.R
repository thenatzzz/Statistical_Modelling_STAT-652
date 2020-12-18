# Boosting classification on Wheat Data
##########
# Enter data and do some processing
wheat <-  read.csv("wheat.csv")
head(wheat)
#summary(wheat)

# Variable "type" is the response variable.  "class" is another explanatory.
class(wheat$type)
wheat$type = as.factor(wheat$type)
wheat$class = as.factor(wheat$class)
#summary(wheat)

# Create a numerical version of "class" for methods that need numbers
###Not needed for trees
#wheat$classnum <- as.numeric((wheat$class))
# Remove "id"
wheat = wheat[,-1]
#summary(wheat)

############
# Creating TWO sets: 200 train, 75 test
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm <= 200),]
set2 <- wheat[which(perm>200),]

library(gbm)
####################################################################
## Gradient boosting through gbm() does only multinomial boosting 
##   or 2-class classification problems. 
##  The latter require 0/1 response values
##  Allow different criteria functions through distribution= :
## "multinomial" (currently broken as of Sept 2020), 
##  and 2-class binomial deviance ("bernoulli")
## 
## As before, tuning parameters and defaults include 
# n.trees=100: number of trees (=M)
# interaction.depth=1: implies additive model.  USUALLY need to increase this (HTF suggest 5)
# shrinkage = .001: The learning rate parameter (HTF: 0.1; author says .001 is better)
# bag.fraction=0.5: The subsampling fraction (HTF: 0.5)
# n.minobsinnode=10: Min number of obs in terminal nodes
## Crossvalidation using cv.folds= allows you to estimate the number of trees for your current
##   parameter settings to avoid overfitting.  Slow, though
####################################################################

# Create two binaries
Hbin1 = as.numeric(set1$type=="Healthy")
Hbin2 = as.numeric(set2$type=="Healthy")

head(Hbin1)
tail(Hbin1)

# Run these two functions several times to see change in 
#  suggested n.trees
# Probably need larger training set than default bag.fraction=0.5, 
#   because full set is only n=275
# Using cv for number of trees since it doesn't take too long
wh.boost.2.001 <- gbm(data=set1[,-7], Hbin1~., 
                       distribution="bernoulli", verbose=FALSE, 
                       n.trees=10000, interaction.depth=2, shrinkage=0.001, 
                       bag.fraction=0.8, cv.folds=5)
# 10000 seems optimal following tuning effort
x11(h=7, w=12, pointsize=12)
par(mfrow=c(1,2))
ntrees.2.001 <- gbm.perf(wh.boost.2.001, method="cv" )
ntrees.oob.2.001 <- gbm.perf(wh.boost.2.001, method="OOB" )
ntrees.2.001
ntrees.oob.2.001

# Variable Importance
x11(h=15, w=15, pointsize=10)
summary(wh.boost.2.001, n.trees=ntrees.2.001, las=1)

## Calculate test misclassification rates
### predict() returns class probabilities, not class values.
# Classify according to I(f(x)>0.5)
pred.boost.train.2 <- predict(wh.boost.2.001, newdata=set1, n.trees=ntrees.2.001, type="response")
pred.boost.test.2 <- predict(wh.boost.2.001, newdata=set2, n.trees=ntrees.2.001, type="response")
head(pred.boost.test.2)

(misclass.boost.train.2 <- mean((pred.boost.train.2>.5)!=Hbin1))
(misclass.boost.test.2 <- mean((pred.boost.test.2>.5)!=Hbin2))

### Really needs tuning!  See L16 boosting program for CV example.

