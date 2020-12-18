
# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(2928893) 

######################################
############### HW 6 #################
######################################
help("airquality")

AQ_original = airquality
head(AQ_original,5)

# X= Solar.R, Wind, Temp
# Y = Ozone
AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio

head(AQ)

### 1.
library(MASS) # for ridge
library(glmnet) # for LASSO

ridge1 <- lm.ridge(Ozone ~., lambda = seq(0, 100, .05), data=AQ)
# Show coefficient path
plot(ridge1)
select(ridge1)
(coef.ri.best1 = coef(ridge1)[which.min(ridge1$GCV),])

mod.lm1 = lm(Ozone~., data=AQ)
summary(mod.lm1)

#### 2.
#a. b.
y.1 <- AQ[,1]
x.1 <- as.matrix(AQ[,c(2:6)])

cv.lasso.1 <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
cv.lasso.1
plot(cv.lasso.1) # Plot CV-MSPE
coef(cv.lasso.1) # Print out coefficients at optimal lambda 
coef(cv.lasso.1, s=cv.lasso.1$lambda.min) # Another way to do this.
# Using the "+1SE rule" (see later) produces a sparser solution
coef(cv.lasso.1, s=cv.lasso.1$lambda.1se) # Another way to do this.

#### 3.
# rm(list=ls(all=TRUE))

set.seed(2928893) 
n = nrow(AQ) #store sample size for easy calculations later

# Let's do 10-fold CV
V=10
# Abbreviating sample.int function arguments
folds = floor((sample.int(n)-1)*V/n) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=5)
# colnames(MSPEs.cv) = c("Ridge", "LASSO-min", "LASSO-1SE",'Least-Sqr','Hybrid-Step','PLS')
colnames(MSPEs.cv) = c("Ridge", "LASSO-min", "LASSO-1SE",'Least-Sqr','Hybrid-Step')

for(v in 1:V){
  
  ridge1 <- lm.ridge(Ozone ~., lambda = seq(0, 100, .05), data= AQ[folds != v,] )
  (coef.ri.best1 = coef(ridge1)[which.min(ridge1$GCV),])
  pred.ri1 = as.matrix(cbind(1,AQ[folds==v,2:6])) %*% coef.ri.best1
  MSPEs.cv[v,1] = mean((AQ[folds==v,1]-pred.ri1)^2)

  x.1 <- as.matrix(AQ[folds!=v,c(2:6)])
  y.1 <- AQ[folds!=v,1]
  x.2 <- as.matrix(AQ[folds==v,c(2:6)])
  y.2 <- AQ[folds==v,1]
  
  cv.lasso <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
  pred.las.min <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.min)
  pred.las.1se <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.1se)
  
  MSPEs.cv[v,2] <- mean((y.2 - pred.las.min)^2)
  MSPEs.cv[v,3] <- mean((y.2 - pred.las.1se)^2)
  
   
  fit.lm = lm(Ozone ~ ., data = AQ[folds != v,])
  pred.lm = predict(fit.lm, newdata=AQ[folds==v,])
  MSPEs.cv[v,4] = mean((AQ[folds==v,"Ozone"] - pred.lm)^2)
  
  initial <- lm(data=AQ[folds != v,], 
                formula=Ozone~ 1)
  final <- lm(data=AQ[folds != v,], 
              formula=Ozone~Solar.R+Wind+Temp+TWcp+TWrat)
  n1 = nrow(AQ[folds != v,])
  step <- step(object=initial, scope=list(upper=final), 
               k = log(n1))
  
  pred = predict(step,newdata=AQ[folds==v,])
  MSPEs.cv[v,5] = mean((AQ[folds==v,"Ozone"] - pred)^2)
  
# 
#   mod.pls = plsr(Ozone ~.,data= AQ[folds != v,], validation="CV")
#   mp.cv = mod.pls$validation
#   Opt.Comps= which.min(sqrt(mp.cv$PRESS/nrow(AQ[folds != v,])))
#   # MSPEs.cv[v,1]=Opt.Comps
# 
#   pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=AQ[folds==v,2:6])
#   MSPEs.cv[v,6] = mean((AQ[folds==v,1] - pred.pls)^2)
  
}
MSPEs.cv

par(mar=c(7,5,5,5)+.1)

boxplot(MSPEs.cv, las=2, ylim=c(0,1500),
        main="MSPE \n Cross-Validation")
low.c = apply(MSPEs.cv, 1, min) 
boxplot(MSPEs.cv/low.c, las=2,ylim=c(0.8,3),
        main="Relative MSPE \n Cross-Validation")

MSPEs.cv_mean = MSPEs.cv[,1:3]
colMeans(MSPEs.cv_mean)

### Full MSPE
n = nrow(AQ) #store sample size for easy calculations later

sf1 = 0.75
set.seed(2928893) 

reorder = sample.int(n=n, size=n, replace=FALSE) ### Shuffled numbers from 1 to n
reorder
set = ifelse(test=(reorder < sf1*n), yes=1, 
             no=2)
set
AQ[set==2,]
dim(AQ[set==1,])
dim(AQ[set==2,])


MSPEs.FULL = matrix(NA, nrow=1, ncol=3)
colnames(MSPEs.FULL) = c("Ridge", "LASSO-min", "LASSO-1SE")

# ridge
ridge1 <- lm.ridge(Ozone ~., lambda = seq(0, 100, .05), data=AQ[set==1,])
pred.ri1 = as.matrix(cbind(1,AQ[set==2,2:6])) %*% coef.ri.best1
MSPEs.FULL[1,1] = mean((AQ[set==2,1]-pred.ri1)^2)


# LASSO

y.1 <- AQ[set==1,1]
x.1 <- as.matrix(AQ[set==1,c(2:6)])
y.2 <- AQ[set==2,1]
x.2 <- as.matrix(AQ[set==2,c(2:6)])
cv.lasso.1 <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
coef(cv.lasso.1, s=cv.lasso.1$lambda.min) # Another way to do this.
coef(cv.lasso.1, s=cv.lasso.1$lambda.1se) # Another way to do this.
pred.las1.min <- predict(cv.lasso.1, newx=x.2, s=cv.lasso.1$lambda.min)
pred.las1.1se <- predict(cv.lasso.1, newx=x.2, s=cv.lasso.1$lambda.1se)
MSPEs.FULL[1,2] <- mean((y.2 - pred.las1.min)^2)
MSPEs.FULL[1,3] <- mean((y.2 - pred.las1.1se)^2)
MSPEs.FULL

#######################################
######################################

######################################
############### HW 7 #################
######################################
library(caret)
rm(list=ls(all=TRUE))

ins=read.csv('Insurance.csv',header=TRUE)
ins$zone = as.factor(ins$zone)
ins$make=as.factor(ins$make)
dim(ins)

ins = ins[ins$claims>0,]
dim(ins)

### 1.
ins.dv = data.frame(predict(dummyVars("~.",data=ins),newdata=ins))
head(ins.dv)

dim(ins.dv)

### 2.
#a.
pc <-  prcomp(x=ins.dv[,2:21], scale.=TRUE)
summary(pc)
plot(pc)


evals <- pc$sdev^2
csum = cumsum(evals)
x11(h=7,w=12)
par(mfrow=c(1,2))
plot(y=evals, x=c(1:(ncol(ins.dv)-1)), xlab="PC#", 
     main="Variance explained by PCA", ylab="Variance Explained")
abline(a=0,b=0)
abline(a=1,b=0)

plot(y=c(0,csum/max(csum)), x=c(0:(ncol(ins.dv)-1)), xlab="PC#", ylim=c(0,1),
     main="Cumulative Variance explained by PCA", ylab="Pct Explained")

# Look at eigenvectors to see how variables contribute to PC's
pc$rotation


### Ozone #########
library(pls)

# mod.pls = plsr(lpsa~., data=prostate, ncomp=8, validation="CV")
# summary(mod.pls)
# validationplot(mod.pls)


set.seed(2928893) 
AQ_original = airquality
head(AQ_original,5)

AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio

head(AQ)
# Let's do 10-fold CV
n = nrow(AQ) #store sample size for easy calculations later

V=10
# Abbreviating sample.int function arguments
folds = floor((sample.int(n)-1)*V/n) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=1)
# colnames(MSPEs.cv) = c("Ridge", "LASSO-min", "LASSO-1SE",'Least-Sqr','Hybrid-Step')
colnames(MSPEs.cv) = c("PLS")
# colnames(MSPEs.cv) = c("PLS_component")
for(v in 1:V){
  mod.pls = plsr(Ozone ~.,data= AQ[folds != v,], validation="CV")
  mp.cv = mod.pls$validation
  Opt.Comps= which.min(sqrt(mp.cv$PRESS/nrow(AQ[folds != v,])))
  # MSPEs.cv[v,1]=Opt.Comps
  
  pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,1] = mean((AQ[folds==v,1] - pred.pls)^2)
}
MSPEs.cv
mean(MSPEs.cv)




### Full MSPE
n = nrow(AQ) #store sample size for easy calculations later
sf1 = 0.75
set.seed(2928893) 

reorder = sample.int(n=n, size=n, replace=FALSE) ### Shuffled numbers from 1 to n
reorder
set = ifelse(test=(reorder < sf1*n), yes=1, 
             no=2)
set
AQ[set==2,]
dim(AQ[set==1,])
dim(AQ[set==2,])

MSPEs.FULL = matrix(NA, nrow=1, ncol=1)
colnames(MSPEs.FULL) = c("PLS")

mod.pls = plsr(Ozone ~.,data=AQ[set==1,], validation="CV")
mp.cv = mod.pls$validation
Opt.Comps= which.min(sqrt(mp.cv$PRESS/nrow(AQ[set==1,])))
pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=AQ[set==2,2:6])
MSPEs.FULL[1,1] = mean((AQ[set==2,1] - pred.pls)^2)
MSPEs.FULL

###############################
rm(list=ls(all=TRUE))
V= 10
set.seed(2928893) 

AQ_original = airquality
head(AQ_original,5)

AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio

head(AQ)
# Let's do 10-fold CV
n = nrow(AQ) #store sample size for easy calculations later

folds = floor((sample.int(n)-1)*V/n) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=6)
colnames(MSPEs.cv) = c("Ridge", "LASSO-min", "LASSO-1SE",'Least-Sqr','Hybrid-Step','PLS')

for(v in 1:V){
  
  ridge1 <- lm.ridge(Ozone ~., lambda = seq(0, 100, .05), data= AQ[folds != v,] )
  (coef.ri.best1 = coef(ridge1)[which.min(ridge1$GCV),])
  pred.ri1 = as.matrix(cbind(1,AQ[folds==v,2:6])) %*% coef.ri.best1
  MSPEs.cv[v,1] = mean((AQ[folds==v,1]-pred.ri1)^2)
  
  x.1 <- as.matrix(AQ[folds!=v,c(2:6)])
  y.1 <- AQ[folds!=v,1]
  x.2 <- as.matrix(AQ[folds==v,c(2:6)])
  y.2 <- AQ[folds==v,1]
  
  cv.lasso <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
  pred.las.min <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.min)
  pred.las.1se <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.1se)
  
  MSPEs.cv[v,2] <- mean((y.2 - pred.las.min)^2)
  MSPEs.cv[v,3] <- mean((y.2 - pred.las.1se)^2)
  
  fit.lm = lm(Ozone ~ ., data = AQ[folds != v,])
  pred.lm = predict(fit.lm, newdata=AQ[folds==v,])
  MSPEs.cv[v,4] = mean((AQ[folds==v,"Ozone"] - pred.lm)^2)
  
  initial <- lm(data=AQ[folds != v,], 
                formula=Ozone~ 1)
  final <- lm(data=AQ[folds != v,], 
              formula=Ozone~Solar.R+Wind+Temp+TWcp+TWrat)
  n1 = nrow(AQ[folds != v,])
  step <- step(object=initial, scope=list(upper=final), 
               k = log(n1))
  
  pred = predict(step,newdata=AQ[folds==v,])
  MSPEs.cv[v,5] = mean((AQ[folds==v,"Ozone"] - pred)^2)
  
  mod.pls = plsr(Ozone ~.,data= AQ[folds != v,], validation="CV")
  mp.cv = mod.pls$validation
  Opt.Comps= which.min(sqrt(mp.cv$PRESS/nrow(AQ[folds != v,])))
  # MSPEs.cv[v,1]=Opt.Comps
  
  pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,6] = mean((AQ[folds==v,1] - pred.pls)^2)
  
  
}
MSPEs.cv
MSPEs.cv.mean= apply(X=MSPEs.cv,MARGIN=2,FUN=mean)
MSPEs.cv.mean
par(mar=c(7,5,5,5)+.1)

boxplot(MSPEs.cv, las=2, ylim=c(0,1500),
        main="MSPE \n Cross-Validation")
low.c = apply(MSPEs.cv, 1, min) 
boxplot(MSPEs.cv/low.c, las=2,ylim=c(0.8,3),
        main="Relative MSPE \n Cross-Validation")




