# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
# set.seed(1234,kind="Mersenne-Twister")

############# HW17 ##############################
ve <-  read.csv("vehicle.csv")

######## 1. ######
### 1.a
summary(ve)

#### 1.b
ve$class = factor(ve$class, labels=c('2D','4D','BUS','VAN'))
summary(ve)

#### 1.c
cor_ve = round(cor(ve[,-19]),3)
cor_ve
library(reshape)

cor_ve[cor_ve == 1] <- NA #drop perfect
cor_ve[abs(cor_ve) < 0.7] <- NA # drop less than abs(0.5)
cor_ve <- na.omit(melt(cor_ve)) # melt! 
cor_ve[order(-abs(cor_ve$value)),] # sort
cor_ve
# df_cor_ve = as.data.frame(as.table(cor_ve))
# df_cor_ve

# install.packages("ellipse")

####### 2. ############
set.seed(46685326,kind="Mersenne-Twister")
perm <- sample(x=nrow(ve))
set1 <- ve[which(perm <= 3*nrow(ve)/4),]
set2 <- ve[which(perm >  3*nrow(ve)/4),]
head(set1,6)
head(set2,6)

###### 3. #########
### 3.a
library(FNN)
scale.1 <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- mean(x2[,col])
    b <- sd(x2[,col])
    x1[,col] <- (x1[,col]-a)/b
  }
  x1
}
# Creating training and test X matrices, then scaling them.
x.1.unscaled <- as.matrix(set1[,-19])
x.1 <- scale.1(x.1.unscaled,x.1.unscaled)
x.2.unscaled <- as.matrix(set2[,-19])
x.2 <- scale.1(x.2.unscaled,x.1.unscaled)

summary(x.1)
summary(x.2)

knnfit.1.1 <- knn(train=x.1, test=x.1, cl=set1[,19], k=1)

# Create Confusion Matrix and misclass rate
table(knnfit.1.1, set1[,19],  dnn=c("Predicted","Observed"))
(misclass.knn1.1 <- 
    mean(ifelse(knnfit.1.1 == set1[,19], yes=0, no=1)))

# Fit the 1-NN function using set 1 to train and set2 to test 
#   (compute test error)
knnfit.1.2 <- knn(train=x.1, test=x.2, cl=set1[,19], k=1)
# Create Confusion Matrix and misclass rate
table(knnfit.1.2, set2[,19],  dnn=c("Predicted","Observed"))
(misclass.knn1.2 <- 
    mean(ifelse(knnfit.1.2 == set2[,19], yes=0, no=1)))

##### 3.b
mis.se <- sqrt(misclass.knn1.2*(1-misclass.knn1.2)/nrow(set2)) #SE of misclass rates
mis.se


####### 4. ################

### 4.a
kmax <- 40
k <- matrix(c(1:kmax), nrow=kmax)
runknn <- function(x){
  knncv.fit <- knn.cv(train=x.1, cl=set1[,19], k=x)
  # Fitted values are for deleted data from CV
  mean(ifelse(knncv.fit == set1[,19], yes=0, no=1))
}

mis <- apply(X=k, MARGIN=1, FUN=runknn)
mis.se <- sqrt(mis*(1-mis)/nrow(set2)) #SE of misclass rates

#Now plot results
# Plot like the CV plots, with 1SE bars and a horizontal line 
#   at 1SE above minimum.
x11(h=7,w=7,pointsize=12)
plot(x=k, y=mis, type="b", ylim=c(.25,.50)) 
for(ii in c(1:kmax)){
  lines(x=c(k[ii],k[ii]), y=c(mis[ii]-mis.se[ii], mis[ii]+mis.se[ii]), col=colors()[220])
}
abline(h=min(mis + mis.se), lty="dotted")
which.min(mis)
#### 4.b
mink = which.min(mis)
serule = max(which(mis<mis[mink]+mis.se[mink]))
serule

##### 4.c
# k for Minimum CV error
mink = which.min(mis)
#Trying the value of k with the lowest validation error on test data set.
knnfitmin.2 <- knn(train=x.1, test=x.2, cl=set1[,19], k=mink)

table(knnfitmin.2, set2[,19],  dnn=c("Predicted","Observed"))
(misclass.2.knnmin <- mean(ifelse(knnfitmin.2 == set2[,19], yes=0, no=1)))

# Less variable models have larger k, so find largest k within 
#   1 SE of minimum validation error 
serule = max(which(mis<mis[mink]+mis.se[mink]))
knnfitse.2 <- knn(train=x.1, test=x.2, cl=set1[,19], k=serule)

table(knnfitse.2, set2[,19],  dnn=c("Predicted","Observed"))
(misclass.2.knnse <- mean(ifelse(knnfitse.2 == set2[,19], yes=0, no=1)))

mis.se


##########################################
########## HW 18 ###############
#####################################
rm(list=ls(all=TRUE))

ve <-  read.csv("vehicle.csv")
ve$class = factor(ve$class, labels=c('2D','4D','BUS','VAN'))
# ve$class = as.factor(ve$class)
ve
set.seed(46685326,kind="Mersenne-Twister")
perm <- sample(x=nrow(ve))
set1 <- ve[which(perm <= 3*nrow(ve)/4),]
set2 <- ve[which(perm >  3*nrow(ve)/4),]
# head(set1,6)
# head(set2,6)

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

############## 1.
##### 1.a
set1.rescale <- data.frame(cbind(rescale(set1[,-19], set1[,-19]), class=set1$class))
set2.rescale <- data.frame(cbind(rescale(set2[,-19], set1[,-19]), class=set2$class))

summary(set1.rescale)
summary(set2.rescale)
# str(set1.rescale)
library(nnet)
###### 1.b
mod.fit <- multinom(data=set1.rescale, formula=class ~ ., 
                    trace=TRUE, maxit = 1500)
summary(mod.fit)

# Adding a function to perform LR Tests.  Legitimate here since I am fitting one model
library(car)
Anova(mod.fit)

# Misclassification Errors
pred.class.1 <- predict(mod.fit, newdata=set1.rescale) 
                        # type="class")
pred.class.2 <- predict(mod.fit, newdata=set2.rescale) 
                        # type="class")
(mul.misclass.train <- mean(ifelse(pred.class.1 == set1$class, 
                                   yes=0, no=1)))
(mul.misclass.test <- mean(ifelse(pred.class.2 == set2$class, 
                                  yes=0, no=1)))

# Estimated probabilities for test set
pred.probs.2 <- predict(mod.fit, newdata=set2.rescale, 
                        type="probs")
round(head(pred.probs.2),3)

# Test set confusion matrix
# table(set2$class, pred.class.2, dnn=c("Obs","Pred"))
table(pred.class.2,set2$class, dnn=c("Predicted","Observed"))

# Number of parameters estimated (just FYI)
mod.fit$edf


###### 2.
library(glmnet)
logit.fit <- glmnet(x=as.matrix(set1.rescale[,-19]), 
                    y=set1.rescale[,19], family="multinomial")

# "Optimal" LASSO Fit
logit.cv <- cv.glmnet(x=as.matrix(set1.rescale[,1:18]), 
                      y=set1.rescale[,19], family="multinomial")
logit.cv
# x11()
plot(logit.cv)

## Find nonzero lasso coefficients
# c <- coef(logit.fit,s=logit.cv$lambda.min) 
c <- coef(logit.cv,s=logit.cv$lambda.min) 
# c
# coef(logit.cv, s = logit.cv$lambda.min)

cmat <- cbind(as.matrix(c[[1]]), as.matrix(c[[2]]), 
              as.matrix(c[[3]]),as.matrix(c[[4]]))
# round(cmat,2)
colnames(cmat) = c('2D','4D','BUS','VAN')
round(cmat,2)
cmat!=0

lascv.pred.train <- predict(object=logit.cv, type="class", 
                            s=logit.cv$lambda.min, 
                            newx=as.matrix(set1.rescale[,1:18]))
lascv.pred.test <- predict(logit.cv, type="class", 
                           s=logit.cv$lambda.min, 
                           newx=as.matrix(set2.rescale[,1:18]))
(lascvmisclass.train <- 
    mean(ifelse(lascv.pred.train == set1$class, yes=0, no=1)))
(lascvmisclass.test <- 
    mean(ifelse(lascv.pred.test == set2$class, yes=0, no=1)))

#####################################################
##################### 3.
#### 3.a
###############################################################
library(MASS)

### To interpret class means and discrim coefs better, 
###  rescale data to 0 mean, 1 SD first. Then all 
###  differences in means are comparable for all vars.

set1s <- apply(set1[,-19], 2, scale)
set1s <- data.frame(set1s,class=set1$class)
lda.fit.s <- lda(data=set1s, class~.)
lda.fit.s

# Fit gives identical results as without scaling, but 
#  can't interpret means
lda.fit <- lda(x=set1[,-19], grouping=set1$class)
lda.fit


# Plot results.  Create standard colours for classes. 
class.col <-  ifelse(set1$class=="2D",y=53,n=
                       ifelse(set1$class=="4D",y=68,n=
                                ifelse(set1$class=='BUS',y=203,n=464)))
x11(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="histogram")
x11(h=7,w=6,pointsize=12)
plot(lda.fit, dimen=1, type="density")

x11(h=7,w=6,pointsize=12)
plot(lda.fit, col=colors()[class.col])


# Calculate in-sample and out-of-sample misclassification error
lda.pred.train <- predict(lda.fit, newdata=set1[,-19])$class
lda.pred.test <- predict(lda.fit, newdata=set2[,-19])$class
(lmisclass.train <- mean(ifelse(lda.pred.train == set1$class, yes=0, no=1)))
(lmisclass.test <- mean(ifelse(lda.pred.test == set2$class, yes=0, no=1)))

# Test set confusion matrix
table(set2$class, lda.pred.test, dnn=c("Obs","Pred"))

######## 4.
qda.fit <- qda(data=set1, class~.)
qda.fit

qda.pred.train <- predict(qda.fit, newdata=set1)$class
qda.pred.test <- predict(qda.fit, newdata=set2)$class
(qmisclass.train <- mean(ifelse(qda.pred.train == set1$class, yes=0, no=1)))
(qmisclass.test <- mean(ifelse(qda.pred.test == set2$class, yes=0, no=1)))





