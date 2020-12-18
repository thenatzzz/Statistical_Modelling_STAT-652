# Multinomial Logistic Regression on Wheat Data
##########
# Enter data and do some processing
wheat <-  read.csv("wheat.csv")
head(wheat)
summary(wheat)

# Variable "type" is the response variable.  "class" is another explanatory.
class(wheat$type)
wheat$type = as.factor(wheat$type)
wheat$class = as.factor(wheat$class)
summary(wheat)

# Create a numerical version of "class" for methods that need numbers
wheat$classnum <- as.numeric((wheat$class))
# Remove "id"
wheat = wheat[,-1]
summary(wheat)

############

# multinom() requires numerical explanatories, so remove factor class 
# Creating TWO sets: 200 train, 75 test
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm <= 200),-1]
set2 <- wheat[which(perm>200),-1]


################################################################
# Multinomial through nnet?
###############################################################

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

# multinom uses a formula, so need to keep data in data.frame

# Creating training and test X matrices, then scaling them.

set1.rescale <- data.frame(cbind(rescale(set1[,-6], set1[,-6]), type=set1$type))
set2.rescale <- data.frame(cbind(rescale(set2[,-6], set1[,-6]), type=set2$type))

summary(set1.rescale)
summary(set2.rescale)

library(nnet)

##################################################################
# multinom() uses FIRST group as the baseline class
# Default number of iterations is only 100. Often needs to be increased!
#   This often happens with correlated explanatories
#   Make sure it runs to convergence. If it reaches 100, it prints
#   a message saying not converged

##  
mod.fit <- multinom(data=set1.rescale, formula=type ~ ., 
                    trace=TRUE)
summary(mod.fit)

# Adding a function to perform LR Tests.  Legitimate here since I am fitting one model
library(car)
Anova(mod.fit)

# Example Plot results: 
# Plot of two most significant variables, colour coded by class 
class.col = ifelse(set1$type=="Healthy",y=53,n=
                   ifelse(set1$type=="Scab",y=68,n=203))
x11(h=7, w=6)
plot(x=set1$density, y=set1$weight, col=colors()[class.col])
legend(x="topleft", legend=c("Healthy", "Scab", "Sprout"),
       col=colors()[c(53, 68, 203)], pch=1)
class.col=ifelse(set1$type=="Healthy",y=53,n=
         ifelse(set1$type=="Scab",y=68,n=203))

# Misclassification Errors
pred.class.1 <- predict(mod.fit, newdata=set1.rescale, 
                        type="class")
pred.class.2 <- predict(mod.fit, newdata=set2.rescale, 
                        type="class")
(mul.misclass.train <- mean(ifelse(pred.class.1 == set1$type, 
                                   yes=0, no=1)))
(mul.misclass.test <- mean(ifelse(pred.class.2 == set2$type, 
                                  yes=0, no=1)))

# Estimated probabilities for test set
pred.probs.2 <- predict(mod.fit, newdata=set2.rescale, 
                        type="probs")
round(head(pred.probs.2),3)

# Test set confusion matrix
table(set2$type, pred.class.2, dnn=c("Obs","Pred"))
# Number of parameters estimated (just FYI)
mod.fit$edf

##################################################################
#################################################################
# Multinomial Logistic Regression using glmnet()
# Setting lambda penalty parameter to 0 in predict() is full ML fit
#  Could do LASSO here as well
###############################################################
library(glmnet)
logit.fit <- glmnet(x=as.matrix(set1.rescale[,-7]), 
                    y=set1.rescale[,7], family="multinomial")

# Note that parameters are not the same as in multinom()
coef(logit.fit, s=0)

# Predicted probabilities
logit.prob.2 <- predict(logit.fit, s=0, type="response",
                        newx=as.matrix(set2.rescale[,1:6]))
round(head(logit.prob.2[,,1]), 3)

# Calculate in-sample and out-of-sample misclassification error
las0.pred.train <- predict(object=logit.fit, s=0, type="class",
                           newx=as.matrix(set1.rescale[,1:6]))
las0.pred.test <- predict(logit.fit, s=0, type="class",
                          newx=as.matrix(set2.rescale[,1:6]))
(las0misclass.train <- 
    mean(ifelse(las0.pred.train == set1.rescale$type, 
                yes=0, no=1)))
(las0misclass.test <- 
    mean(ifelse(las0.pred.test == set2.rescale$type,
                yes=0, no=1)))


# "Optimal" LASSO Fit
logit.cv <- cv.glmnet(x=as.matrix(set1.rescale[,1:6]), 
                      y=set1.rescale[,7], family="multinomial")
logit.cv
x11()
plot(logit.cv)

## Find nonzero lasso coefficients
c <- coef(logit.fit,s=logit.cv$lambda.min) 
cmat <- cbind(as.matrix(c[[1]]), as.matrix(c[[2]]), 
              as.matrix(c[[3]]))
round(cmat,2)
cmat!=0

lascv.pred.train <- predict(object=logit.cv, type="class", 
                            s=logit.cv$lambda.min, 
                            newx=as.matrix(set1.rescale[,1:6]))
lascv.pred.test <- predict(logit.cv, type="class", 
                           s=logit.cv$lambda.min, 
                           newx=as.matrix(set2.rescale[,1:6]))
(lascvmisclass.train <- 
    mean(ifelse(lascv.pred.train == set1$type, yes=0, no=1)))
(lascvmisclass.test <- 
    mean(ifelse(lascv.pred.test == set2$type, yes=0, no=1)))



