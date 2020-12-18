# Discriminant Analysis on Wheat Data
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
wheat$classnum <- as.numeric((wheat$class))
# Remove "id"
wheat = wheat[,-1]
#summary(wheat)

############

# multinom() requires numerical explanatories, so remove factor class 
# Creating TWO sets: 200 train, 75 test
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm <= 200),-1]
set2 <- wheat[which(perm>200),-1]


###############################################################
## Linear discriminant analysis via MASS::lda()
## Does dimension-reduction version
##  "prior=" allows you to set prior probabilities if they are known.  
#  Default is the sample proportions.
##
## Note that in fit below, the "Linear Discriminants" are directions 
#   of the 2-dim subspace within which the 3 centroids (p-dim means) lie.
#   (kind of like PC's but different)
# The proportion of trace is the amount of variability from
#    conditional Multivariate Normals explained in each direction.
#
###############################################################
library(MASS)

### To interpret class means and discrim coefs better, 
###  rescale data to 0 mean, 1 SD first. Then all 
###  differences in means are comparable for all vars.

set1s <- apply(set1[,-6], 2, scale)
set1s <- data.frame(set1s,type=set1$type)
lda.fit.s <- lda(data=set1s, type~.)
lda.fit.s

# Fit gives identical results as without scaling, but 
#  can't interpret means
lda.fit <- lda(x=set1[,-6], grouping=set1$type)
lda.fit

# Plot results.  Create standard colours for classes. 
class.col <-  ifelse(set1$type=="Healthy",y=53,n=
                 ifelse(set1$type=="Scab",y=68,n=203))
x11(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="histogram")
x11(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="density")

x11(h=7,w=6,pointsize=12)
plot(lda.fit, col=colors()[class.col])

# Calculate in-sample and out-of-sample misclassification error
lda.pred.train <- predict(lda.fit, newdata=set1[,-6])$class
lda.pred.test <- predict(lda.fit, newdata=set2[,-6])$class
(lmisclass.train <- mean(ifelse(lda.pred.train == set1$type, yes=0, no=1)))
(lmisclass.test <- mean(ifelse(lda.pred.test == set2$type, yes=0, no=1)))

# Test set confusion matrix
table(set2$type, lda.pred.test, dnn=c("Obs","Pred"))


##################################################################
## Quadratic fit
##   Fewer options available (no plot function, no canonical variates
##  to plot
##################################################################

qda.fit <- qda(data=set1, type~.)
qda.fit

qda.pred.train <- predict(qda.fit, newdata=set1)$class
qda.pred.test <- predict(qda.fit, newdata=set2)$class
(qmisclass.train <- mean(ifelse(qda.pred.train == set1$type, yes=0, no=1)))
(qmisclass.test <- mean(ifelse(qda.pred.test == set2$type, yes=0, no=1)))

# Test set confusion matrix
table(set2$type, qda.pred.test, dnn=c("Obs","Pred"))

qda.corr = ifelse(qda.pred.test == set2$type, yes="Y", no="N")
lda.corr = ifelse(lda.pred.test == set2$type, yes="Y", no="N")

mcnemar.test(lda.corr, qda.corr)

