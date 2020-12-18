# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(2928893) 


############################
####### Application #################
help("airquality")

AQ_original = airquality
head(AQ_original,5)

# X= Solar.R, Wind, Temp
# Y = Ozone
AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio
# sort Temp
AQ = AQ[order(AQ$Temp),]
head(AQ)
###################################
######## HW10
#####1.
##a.i

x11(h=7, w=10)

plot(x=AQ$Temp, y=AQ$Ozone, main="Plot Ozone ~ Temp")
# Adding legend to the plot.  Note that "as.numeric(covid$date)"
#   shows that dates are represented numerically by numbers
#   ranging from 18287 to 18527, so putting legend corner at 18300
legend(x="topleft", y=0.92, 
       legend=c("Smoothing Spline 5 df", "Smoothing Spline 7 df", "Smoothing Spline 9 df", 'Smoothing Spline 20 df'), lty="solid",
       col=colors()[c(121,145,84,33)], lwd=2)

# 5 DF spline
sm.spl.5 <- smooth.spline(x=AQ$Temp, y=AQ$Ozone, df=5)
sm.spl.5
lines(sm.spl.5, col=colors()[121], lwd=2)

# 7 DF spline
sm.spl.7 <- smooth.spline(x=AQ$Temp, y=AQ$Ozone, df=7)
sm.spl.7
lines(sm.spl.7, col=colors()[145], lwd=2)

# 9 DF spline
sm.spl.9 <- smooth.spline(x=AQ$Temp, y=AQ$Ozone, df=9)
sm.spl.9
lines(sm.spl.9, col=colors()[84], lwd=2)

# 20 DF spline
sm.spl.20 <- smooth.spline(x=AQ$Temp, y=AQ$Ozone, df=20)
sm.spl.20
lines(sm.spl.20, col=colors()[33], lwd=2)

### b.i, b.ii, b.iii 
x11(h=7, w=10)

plot(x=AQ$Temp, y=AQ$Ozone, main="Comparison of 'Optimum' Smoothing splines")
legend(x="topleft", y=0.92, 
       legend=c("N-Fold CV", "Generalized CV"), lty="solid",
       col=colors()[c(121,91)], lwd=2)

sm.spl.opt <- smooth.spline(x=AQ$Temp, y=AQ$Ozone, cv=TRUE)
sm.spl.opt
lines(sm.spl.opt, col=colors()[121], lwd=2)

sm.spl.opt2 <- smooth.spline(x=AQ$Temp, y=AQ$Ozone, cv=FALSE)
sm.spl.opt2
lines(sm.spl.opt2, col=colors()[91], lwd=2)

#### 2.
x11(h=7, w=10)

plot(x=AQ$Temp, y=AQ$Ozone, main="Plot Ozone ~ Temp")
# Adding legend to the plot.  Note that "as.numeric(covid$date)"
#   shows that dates are represented numerically by numbers
#   ranging from 18287 to 18527, so putting legend corner at 18300
legend(x="topleft", y=0.92, 
       legend=c("LOESS 5 df", "LOESS 7 df", "LOESS 9 df", 'LOESS 20 df'), lty="solid",
       col=colors()[c(121,145,84,33)], lwd=2)


lo.5 <- loess(data=AQ, formula=Ozone~as.numeric(Temp), enp.target=5)
summary(lo.5)
lines(x=AQ$Temp, y=predict(lo.5), col=colors()[121], lwd=2)

lo.7 <- loess(data=AQ, formula=Ozone~Temp, enp.target=7)
summary(lo.7)
lines(x=AQ$Temp, y=predict(lo.7), col=colors()[145], lwd=2)

lo.9 <- loess(data=AQ, formula=Ozone~Temp, enp.target=9)
summary(lo.9)
lines(x=AQ$Temp, y=predict(lo.9), col=colors()[84], lwd=2)

lo.20 <- loess(data=AQ, formula=Ozone~Temp, enp.target=20)
summary(lo.20)
lines(x=AQ$Temp, y=predict(lo.20), col=colors()[33], lwd=2)
#######################################
##########################################


###### HW 11
rm(list=ls(all=TRUE))

AQ_original = airquality

# X= Solar.R, Wind, Temp
# Y = Ozone
AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio
# sort Temp
# AQ = AQ[order(AQ$Temp),]
head(AQ)

######1.
####1.a

library(mgcv)

#  Generalized additive model on all variables
gam.all <- gam(data=AQ,
               formula=Ozone ~s(Temp)+s(Wind) + s(Solar.R) + s(TWcp) + s(TWrat) , 
               family=gaussian(link=identity)) 
summary(gam.all)


x11(h=7,w=15,pointsize=12)
par(mfrow=c(2,3))
plot(gam.all,main='GAM marginal splines')
# gam.1 <- gam(data=AQ,
#              formula=Ozone ~s(Temp), 
#              family=gaussian(link=identity)) 
# gam.2 <- gam(data=AQ,
#              formula=Ozone ~s(Wind), 
#              family=gaussian(link=identity)) 
# gam.3 <- gam(data=AQ,
#              formula=Ozone ~s(Solar.R), 
#              family=gaussian(link=identity)) 
# gam.4 <- gam(data=AQ,
#              formula=Ozone ~s(TWcp), 
#              family=gaussian(link=identity)) 
# gam.5 <- gam(data=AQ,
#              formula=Ozone ~s(TWrat), 
#              family=gaussian(link=identity)) 
# 
# plot(gam.1, main="GAM marginal splines")
# plot(gam.2, main="GAM marginal splines")
# plot(gam.3, main="GAM marginal splines")
# plot(gam.4, main="GAM marginal splines")
# plot(gam.5, main="GAM marginal splines")


#### 2.
rm(list=ls(all=TRUE))
library(pls) # pls
library(MASS) # for ridge
library(glmnet) # for LASSO
library(mgcv) # gam

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

MSPEs.cv = matrix(NA, nrow=V, ncol=7)
colnames(MSPEs.cv) = c("Ridge", "LASSO-min", "LASSO-1SE",'Least-Sqr','Hybrid-Step','PLS','GAM-all')

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
  pred.lm = predict(fit.lm, newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,4] = mean((AQ[folds==v,"Ozone"] - pred.lm)^2)
  
  initial <- lm(data=AQ[folds != v,], 
                formula=Ozone~ 1)
  final <- lm(data=AQ[folds != v,], 
              formula=Ozone~Solar.R+Wind+Temp+TWcp+TWrat)
  n1 = nrow(AQ[folds != v,])
  step <- step(object=initial, scope=list(upper=final), 
               k = log(n1))
  
  pred = predict(step,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,5] = mean((AQ[folds==v,"Ozone"] - pred)^2)
  
  mod.pls = plsr(Ozone ~.,data= AQ[folds != v,], validation="CV")
  mp.cv = mod.pls$validation
  Opt.Comps= which.min(sqrt(mp.cv$PRESS/nrow(AQ[folds != v,])))
  # MSPEs.cv[v,1]=Opt.Comps
  pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,6] = mean((AQ[folds==v,1] - pred.pls)^2)
  
  gam.all <- gam(data=AQ[folds != v,],
                 formula=Ozone ~s(Temp)+s(Wind) + s(Solar.R) + s(TWcp) + s(TWrat) , 
                 family=gaussian(link=identity)) 
  pred.gam <- predict(gam.all ,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,7] = mean((AQ[folds==v,1] - pred.gam)^2)
  
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
colnames(MSPEs.FULL) = c("GAM-all")

gam.all <- gam(data=AQ[set==1,],
               formula=Ozone ~s(Temp)+s(Wind) + s(Solar.R) + s(TWcp) + s(TWrat) , 
               family=gaussian(link=identity)) 
pred.gam <- predict(gam.all ,newdata=AQ[set==2,2:6])
MSPEs.FULL[1,1] = mean((AQ[set==2,1] - pred.gam)^2)
MSPEs.FULL
###################################

################### HW12#########################
#### 1.
rm(list=ls(all=TRUE))
set.seed(2928893) 

AQ_original = airquality

# X= Solar.R, Wind, Temp
# Y = Ozone
AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio
# sort Temp
# AQ = AQ[order(AQ$Temp),]
head(AQ)

### a.1 , a.2
ppr1 <- ppr(data=AQ, Ozone~ ., 
            nterms=1,max.terms=5, sm.method="gcvspline") 
summary(ppr1)

# Plots of results
x11(h=7,w=6,pointsize=12)
plot(ppr1, main="Projection Pursuit Regression 1 term", col="orange", ylim=c(-2,4), xlab="Z-ppr")
# par(mfrow = c(1,1))
# plot(ppr1)
### b.1 , b.2
ppr2 <- ppr(data=AQ, Ozone~ ., 
            nterms=2,max.terms=5, sm.method="gcvspline") 
summary(ppr2)
# plot(ppr2, main="Projection Pursuit Regression 2 terms", col="orange", ylim=c(-2,4), xlab="Z-ppr")
par(mfrow = c(1,2))
# x11(h=7,w=6,pointsize=12)
plot(ppr2, col="orange", ylim=c(-2,4), xlab="Z-ppr")
par(mfrow = c(1,1)) # Reset to the usual 1x1 plotting structure

################ based on TA code ####################
###hw12-2. #####
source("Helper Functions.R")
# data=AQ
max.terms = 5 # Maximum number of terms for PPR

### Everything starts off normally for CV.

### Create folds
K = 10 # Number of folds
set.seed(2928893) 
folds = get.folds(nrow(AQ), K) # Our helper function
folds
### Create container for CV MSPEs
all.models = c("PPR-1", "PPR-2","PPR-3", "PPR-4","PPR-5")
n.models = length(all.models)

CV.MSPEs = array(0, dim = c(K,n.models))
colnames(CV.MSPEs) = all.models

# CV.MSPEs = array(0, dim = c(n.models, K))
# rownames(CV.MSPEs) = all.models

for(i in 1:K){
  ### Print a status update
  print(paste0(i, " of ", K))
  
  ### Split data
  data.train = AQ[folds != i,]
  data.valid = AQ[folds == i,2:6]
  # data.valid = AQ[folds == i,]
  # Y.valid = data.valid$Ozone
  Y.valid = AQ[folds == i,1]
  
  ###########
  ### PPR ###
  ###########
  for (p in 1:max.terms){
    ppr1 <- ppr(data=data.train, Ozone~ .,
                nterms=p,max.terms=5, sm.method="gcvspline")
    pred.ppr1 = predict(ppr1, data.valid)
    MSPE.ppr1 = get.MSPE(Y.valid, pred.ppr1) # Our helper function
    CV.MSPEs[i,all.models[p]] = MSPE.ppr1

    print(CV.MSPEs)
  }

}
# CV.MSPEs = t(CV.MSPEs)
CV.MSPEs
CV.MSPEs.mean= apply(X=CV.MSPEs,MARGIN=2,FUN=mean)
CV.MSPEs.mean
### Make a boxplot of MSPEs for the different number of terms
boxplot(CV.MSPEs, las=2, ylim=c(0,1500),
        main="MSPE \n Cross-Validation")
low.c = apply(CV.MSPEs, 1, min) 
boxplot(CV.MSPEs/low.c, las=2,ylim=c(0.8,2.5),
        main="Relative MSPE \n Cross-Validation")


##############################################
################ based on hw6-7 #########
### hw12-2 ####
rm(list=ls(all=TRUE))

V= 10
set.seed(2928893) 

AQ_original = airquality
head(AQ_original,5)

AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio

# Let's do 10-fold CV
n = nrow(AQ) #store sample size for easy calculations later
set.seed(2928893) 

folds = floor((sample.int(n)-1)*V/n) + 1 
folds
MSPEs.cv = matrix(NA, nrow=V, ncol=5)
colnames(MSPEs.cv) = c("PPR-1", "PPR-2","PPR-3", "PPR-4","PPR-5")
max.terms=5
for(v in 1:V){
  
  for (p in 1:max.terms){
    ppr1 <- ppr(data=AQ[folds != v,], Ozone~ .,
                nterms=p,max.terms=5, sm.method="gcvspline")
    pred.ppr1 = predict(ppr1,newdata=AQ[folds==v,2:6])
    MSPE.ppr1 =  mean((AQ[folds==v,1] - pred.ppr1)^2)
    MSPEs.cv[v,p] = MSPE.ppr1
    
    print(MSPEs.cv)
  }
  
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

############################################
##############################################
################ HW12 Q3 #########

rm(list=ls(all=TRUE))
library(pls) # pls
library(MASS) # for ridge
library(glmnet) # for LASSO
library(mgcv) # gam
source("Helper Functions.R")

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

MSPEs.cv = matrix(NA, nrow=V, ncol=8)
colnames(MSPEs.cv) = c('Least-Sqr',"Ridge", "LASSO-min",
                       "LASSO-1SE",'Hybrid-Step','PLS',
                       'GAM-all','PPR')
MSPEs.PPR.term = matrix(NA, nrow=V, ncol=1)
colnames(MSPEs.PPR.term) = c('PPR-terms')

for(v in 1:V){
  ### Print a status update
  print(paste0(v, " of ", V))
  
  ### Leaste sqrt ###
  fit.lm = lm(Ozone ~ ., data = AQ[folds != v,])
  pred.lm = predict(fit.lm, newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,1] = mean((AQ[folds==v,"Ozone"] - pred.lm)^2)
  
  ### Ridge ###
  ridge1 <- lm.ridge(Ozone ~., lambda = seq(0, 100, .05), data= AQ[folds != v,] )
  (coef.ri.best1 = coef(ridge1)[which.min(ridge1$GCV),])
  pred.ri1 = as.matrix(cbind(1,AQ[folds==v,2:6])) %*% coef.ri.best1
  MSPEs.cv[v,2] = mean((AQ[folds==v,1]-pred.ri1)^2)

  ### Lasso-min,1se ###
  x.1 <- as.matrix(AQ[folds!=v,c(2:6)])
  y.1 <- AQ[folds!=v,1]
  x.2 <- as.matrix(AQ[folds==v,c(2:6)])
  y.2 <- AQ[folds==v,1]
  cv.lasso <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
  pred.las.min <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.min)
  pred.las.1se <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.1se)
  MSPEs.cv[v,3] <- mean((y.2 - pred.las.min)^2)
  MSPEs.cv[v,4] <- mean((y.2 - pred.las.1se)^2)

  ### Step ###
  initial <- lm(data=AQ[folds != v,],
                formula=Ozone~ 1)
  final <- lm(data=AQ[folds != v,],
              formula=Ozone~Solar.R+Wind+Temp+TWcp+TWrat)
  n1 = nrow(AQ[folds != v,])
  step <- step(object=initial, scope=list(upper=final),
               k = log(n1))
  pred = predict(step,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,5] = mean((AQ[folds==v,"Ozone"] - pred)^2)

  ### PLS ###
  mod.pls = plsr(Ozone ~.,data= AQ[folds != v,], validation="CV")
  mp.cv = mod.pls$validation
  Opt.Comps= which.min(sqrt(mp.cv$PRESS/nrow(AQ[folds != v,])))
  # MSPEs.cv[v,1]=Opt.Comps
  pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,6] = mean((AQ[folds==v,1] - pred.pls)^2)

  ### GAM ####
  gam.all <- gam(data=AQ[folds != v,],
                 formula=Ozone ~s(Temp)+s(Wind) + s(Solar.R) + s(TWcp) + s(TWrat) ,
                 family=gaussian(link=identity))
  pred.gam <- predict(gam.all ,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,7] = mean((AQ[folds==v,1] - pred.gam)^2)

  ### PPR ###
  max.terms = 5
  ### To fit PPR, we need to do another round of CV. This time, do 5-fold
  K.ppr = 5
  n.train = nrow(AQ[folds != v,])
  folds.ppr = get.folds(n.train, K.ppr)
  # print(folds.ppr)
  MSPEs.ppr = array(0, dim = c(max.terms, K.ppr))
  colnames(MSPEs.ppr) = c("ppr1", "ppr2", "ppr3",
                         'ppr4','ppr5')
  for(j in 1:K.ppr){
    train.ppr = AQ[folds != v,][folds.ppr != j,]
    valid.ppr = AQ[folds != v,][folds.ppr == j,2:6]
    Y.valid.ppr = AQ[folds != v,][folds.ppr == j,1]
    # train.ppr = data.train[folds.ppr != j,]
    # valid.ppr = data.train[folds.ppr == j,] 
    # Y.valid.ppr = valid.ppr$alcohol

        for(l in 1:max.terms){
      fit.ppr = ppr(Ozone ~ ., data = train.ppr, 
                    max.terms = max.terms, nterms = l, sm.method = "gcvspline")
      pred.ppr = predict(fit.ppr, valid.ppr)
      MSPE.ppr = get.MSPE(Y.valid.ppr, pred.ppr) # Our helper function
      # MSPEs.ppr[l, j] = MSPE.ppr
      MSPEs.ppr[j, l] = MSPE.ppr
      # print(MSPEs.ppr)
      
    }
  }
  # print(MSPEs.ppr)
  # print('---')
  ave.MSPE.ppr = apply(MSPEs.ppr, 2, mean)
  print(ave.MSPE.ppr)
  best.terms = which.min(ave.MSPE.ppr)
  print(best.terms)
  MSPEs.PPR.term[v,1] = best.terms
  
  ### Fit PPR on the whole CV training set using the optimal number of terms
  fit.ppr.best = ppr(Ozone ~ ., data = AQ[folds != v,],
                     max.terms = max.terms, nterms = best.terms, sm.method = "gcvspline")
  pred.ppr.best = predict(fit.ppr.best, AQ[folds == v,2:6])
  MSPE.ppr.best = get.MSPE(AQ[folds == v,1], pred.ppr.best) # Our helper function
  
  MSPEs.cv[v, 8] = MSPE.ppr.best
  
}

MSPEs.cv
MSPEs.cv.mean= apply(X=MSPEs.cv,MARGIN=2,FUN=mean)
MSPEs.cv.mean
par(mar=c(7,5,5,5)+.1)

boxplot(MSPEs.cv, las=2, ylim=c(0,1500),
        main="MSPE \n Cross-Validation")
low.c = apply(MSPEs.cv, 1, min) 
boxplot(MSPEs.cv/low.c, las=2,ylim=c(0.8,8),
        main="Relative MSPE \n Cross-Validation")

MSPEs.PPR.term

