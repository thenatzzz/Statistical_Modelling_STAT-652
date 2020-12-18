# Naive Bayes on Wheat Data
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
# Creating TWO sets: 200 train, 75 test
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm <= 200),-1]
set2 <- wheat[which(perm>200),-1]


######################################################
## klaR::NaiveBayes is a more developmental function that 
##   does Gaussian Kernel Smothing (usekernel=TRUE)
##  
## ********predict() Gives error messages, but seems to work
######################################################

# For comparison, start without the kernel, which is default
library(klaR)
NBn <- NaiveBayes(x=set1[,-6], grouping=set1[,6], 
                 usekernel=FALSE)

#  Comment this plot out if you don't want to see the 
#    estimated distributions of Xj within classes
x11(h=7,w=10)
par(mfrow=c(2,3))
plot(NBn, lwd=2, main="NB Normal, no PC")

NBn.pred.train <- predict(NBn, newdata=set1[,-6])
table(NBn.pred.train$class, set1[,6], dnn=c("Predicted","Observed"))

NBn.pred.test <- predict(NBn, newdata=set2[,-6])
table(NBn.pred.test$class, set2[,6], dnn=c("Predicted","Observed"))
warnings()
head(round(NBn.pred.test$posterior))

# Error rates
(NBnmisclass.train <- mean(ifelse(NBn.pred.train$class == set1$type, yes=0, no=1)))
(NBnmisclass.test <- mean(ifelse(NBn.pred.test$class == set2$type, yes=0, no=1)))

# Now with kernel estimates
NBk <- NaiveBayes(x=set1[,-6], grouping=set1[,6], 
                 usekernel=TRUE)

#  Comment this plot out if you don't want to see the 
#    estimated distributions of Xj within classes
x11(h=7,w=10)
par(mfrow=c(2,3))
plot(NBk, lwd=2, main="NB Kernel, no PC")

NBk.pred.train <- predict(NBk, newdata=set1[,-6])
table(NBk.pred.train$class, set1[,6], dnn=c("Predicted","Observed"))

NBk.pred.test <- predict(NBk, newdata=set2[,-6])
table(NBk.pred.test$class, set2[,6], dnn=c("Predicted","Observed"))
warnings()
head(round(NBk.pred.test$posterior))

# Error rates
(NBkmisclass.train <- mean(ifelse(NBk.pred.train$class == set1$type, yes=0, no=1)))
(NBkmisclass.test <- mean(ifelse(NBk.pred.test$class == set2$type, yes=0, no=1)))

####################################################################
# Run PCA before Naive Bayes to decorrelate data
#   This is something that has been proposed in the literature
#   See Liwei Fan, Kim Leng Poh, 2007, A Comparative Study of PCA, ICA 
#   and Class-Conditional ICA for Naïve Bayes Classifier.
#   using the same transformation so that they are all aligned.
pc <-  prcomp(x=set1[,-6], scale.=TRUE)

# Create the same transformations in all three data sets 
#   and attach the response variable at the end
#   predict() does this 
xi.1 <- data.frame(pc$x,type = as.factor(set1$type))
xi.2 <- data.frame(predict(pc, newdata=set2), type = as.factor(set2$type))

#  First with normal distributions
NBn.pc <- NaiveBayes(x=xi.1[,-7], grouping=xi.1[,7], usekernel=FALSE)

#  Comment this plot out if you don't want to see the 
#    estimated distributions of Xj within classes
x11(h=7,w=10)
par(mfrow=c(2,3))
plot(NBn.pc, lwd=2, main="NB Normal with PC")

NBnpc.pred.train <- predict(NBn.pc, newdata=xi.1[,-7], type="class")
table(NBnpc.pred.train$class, xi.1[,7], dnn=c("Predicted","Observed"))

NBnpc.pred.test <- predict(NBn.pc, newdata=xi.2[,-7], type="class")
table(NBnpc.pred.test$class, xi.2[,7], dnn=c("Predicted","Observed"))

warnings()

# Error rates
(NBnPCmisclass.train <- mean(ifelse(NBnpc.pred.train$class == xi.1$type, yes=0, no=1)))
(NBnPCmisclass.test <- mean(ifelse(NBnpc.pred.test$class == xi.2$type, yes=0, no=1)))

# Repeat, using kernel density estimates
NBk.pc <- NaiveBayes(x=xi.1[,-7], grouping=xi.1[,7], usekernel=TRUE)

#  Comment this plot out if you don't want to see the 
#    estimated distributions of Xj within classes
x11(h=7,w=10)
par(mfrow=c(2,3))
plot(NBk.pc, lwd=2, main="NB Kernel with PC")

NBkpc.pred.train <- predict(NBk.pc, newdata=xi.1[,-7], type="class")
table(NBkpc.pred.train$class, xi.1[,7], dnn=c("Predicted","Observed"))

NBkpc.pred.test <- predict(NBk.pc, newdata=xi.2[,-7], type="class")
table(NBkpc.pred.test$class, xi.2[,7], dnn=c("Predicted","Observed"))

warnings()

# Error rates
(NBkPCmisclass.train <- mean(ifelse(NBkpc.pred.train$class == xi.1$type, yes=0, no=1)))
(NBkPCmisclass.test <- mean(ifelse(NBkpc.pred.test$class == xi.2$type, yes=0, no=1)))

