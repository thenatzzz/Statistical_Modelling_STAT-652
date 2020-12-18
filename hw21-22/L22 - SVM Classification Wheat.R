# Support Vector Machine classification on Wheat Data
##########
# Enter data and do some processing
wheat <-  read.csv("C:\\Users\\Tom loughin\\sfuvault\\452 Statistical Learning\\R\\wheat.csv")
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
# Creating TWO sets: 200 train, 75 test
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm <= 200),-1]
set2 <- wheat[which(perm>200),-1]


library(e1071)
####################################################################
## Support Vector Machines using e1071::svm
##  Data are scaled internally to mean 0 variance 1 before analysis
##  type = C-classification (same as in notes) is default if y is a factor
##  kernel= "radial" is default, "linear", "polynomial", and "sigmoid" are options
##         Each kernel has its own tuning parameters:
##         radial: gamma=
##         polynomial: gamma= (s), degree= (d), coef0= (c) 
##              
##  cost= is the C-parameter
##  cross= gives # folds for CV estimation of error
##       (I use it below, but better to do your own for tuning)
##
## Note: does 1 vs. 1 approach to K>2 classification
##
## Just using Gaussian Radial Basis below.  Can try others; may work better
####################################################################

svm.1.1 <- svm(data=set1, type ~ ., kernel="radial", 
               gamma=1, cost=1, cross=10)
summary(svm.1.1)
head(svm.1.1$decision.values)
head(svm.1.1$fitted)
svm.1.1$tot.accuracy # Total CV Accuracy
svm.1.1$accuracies # Individual fold accuracies


pred1.1.1 <- predict(svm.1.1, newdata=set1)
table(pred1.1.1, set1$type,  dnn=c("Predicted","Observed"))
(misclass1.1.1 <- mean(ifelse(pred1.1.1 == set1$type, yes=0, no=1)))

pred2.1.1 <- predict(svm.1.1, newdata=set2)
table(pred2.1.1, set2$type,  dnn=c("Predicted","Observed"))
(misclass2.1.1 <- mean(ifelse(pred2.1.1 == set2$type, yes=0, no=1)))

#############################################
# Try tuning with caret::train
# Note that caret uses a different implementation of SVM than e1071
#  Different tuning parameters!
#
# method="svmPoly" has tuninh parameters:
#     degree (Polynomial Degree, d)
#     scale (Scale, s)
#     C (Cost, C)
#
# method="svmRadial"
#     sigma (gamma)
#     C (Cost, C)
#############################################

library(caret)

#Using 10-fold CV so that training sets are not too small
#  ( Starting with 200 in training set)
trcon = trainControl(method="repeatedcv", number=10, repeats=2,
                     returnResamp="all")
parmgrid = expand.grid(C=10^c(0:5), sigma=10^(-c(5:0)))

tuned.nnet <- train(x=set1[,-6], y=set1$type, method="svmRadial", 
                    preProcess=c("center","scale"), trace=FALSE, 
                    tuneGrid=parmgrid, trControl = trcon)

names(tuned.nnet)
tuned.nnet$results[order(-tuned.nnet$results[,3]),]
tuned.nnet$bestTune
head(tuned.nnet$resample)
tail(tuned.nnet$resample)

# Let's rearrange the data so that we can plot the bootstrap resamples in 
#   our usual way, including relative to best
resamples = reshape(data=tuned.nnet$resample[,-2], idvar=c("C", "sigma"), 
                    timevar="Resample", direction="wide")
head(resamples)
(best = apply(X=resamples[,-c(1,2)], MARGIN=2, FUN=max))
C.sigma <- paste(log10(resamples[,1]),"-",log10(resamples[,2]))

x11(h=7, w=12, pointsize=9)
boxplot.matrix(x=t(t(1-resamples[,-c(1:2)])), use.cols=FALSE, names=C.sigma,
               main="Misclassification rates for different Cost-Gamma", las=2)

x11(h=7, w=12, pointsize=9)
boxplot.matrix(x=t(t(1-resamples[,-c(1:2)])/(1-best)), use.cols=FALSE, names=C.sigma,
               main="Relative Misclass rates for different Cost-Gamma", las=2)

x11(h=7, w=12, pointsize=9)
par(mfrow=c(1,2))
boxplot(t(t(1-resamples[,-c(1:2)])/(1-best)) ~ resamples[,1], xlab="C", ylab="Relative Error")
boxplot(t(t(1-resamples[,-c(1:2)])/(1-best)) ~ resamples[,2], xlab="Sigma", ylab="Relative Error")

# Refit the best tuned model

svm.wh.tun <- svm(data=set1, type ~ ., kernel="radial", 
               gamma=10^(-3), cost=10^4)
summary(svm.wh.tun)
head(svm.wh.tun$decision.values)
head(svm.wh.tun$fitted)

pred1.wh.tun <- predict(svm.wh.tun, newdata=set1)
table(pred1.wh.tun, set1$type,  dnn=c("Predicted","Observed"))
(misclass1.wh.tun <- mean(ifelse(pred1.wh.tun == set1$type, yes=0, no=1)))

pred2.wh.tun <- predict(svm.wh.tun, newdata=set2)
table(pred2.wh.tun, set2$type,  dnn=c("Predicted","Observed"))
(misclass2.wh.tun <- mean(ifelse(pred2.wh.tun == set2$type, yes=0, no=1)))


