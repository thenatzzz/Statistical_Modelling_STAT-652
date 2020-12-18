#  Crossvalidation to get prediction error.  The crossval() function in package bootstrap seems most direct
#  Data Splitting, CV, and Bootstrap measures of prediction error.  
#  Using Prostate data, will fit three linear regression models from before
#  Then will use single split, multiple splits, CV, and bootstrap to estimate error
#  Also will show how to use boxplots to compare models

prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")

#  Three models we will compare
#   There is no specific reason for choosing these three.  Could compare others as well.

mod.vol = lm(lpsa ~ lcavol, data=prostate)
mod.pgg = lm(lpsa ~ pgg45, data=prostate)
mod2 =  lm(lpsa ~ lcavol + pgg45, data=prostate)

n = nrow(prostate) #store sample size for easy calculations later

##########################################################################
# Part 1a: Random single split

# Set seed to get repeatable results.  Rerun without resetting seed to see variability
set.seed(120401002) 

# Set sampling fraction.  Here I choose 70/15/15 percents
sf1 = 0.7
sf2 = 0.15
#  Logic:
#    1. randomly permute (reorder) numbers 1,2,...,n
#    2. take positions of first 70% as training set "set=1"
#    3. take positions of next 15% as validation set "set=2"
#    4. take positions of last 15% as test set "set=3

reorder = sample.int(n=n, size=n, replace=FALSE)
reorder

set = ifelse(test=(reorder < sf1*n), yes=1, 
             no=ifelse(test=(sf1*n < reorder & reorder < (sf1+sf2)*n), yes=2, no=3))
set


mod.vol.s1 = lm(lpsa ~ lcavol, data=prostate[set==1,])
mod.pgg.s1 = lm(lpsa ~ pgg45, data=prostate[set==1,])
mod2.s1 =  lm(lpsa ~ lcavol + pgg45, data=prostate[set==1,])

pred.v.s1 = predict(mod.vol.s1, newdata=prostate[set==2,])
pred.p.s1 = predict(mod.pgg.s1, newdata=prostate[set==2,])
pred.2.s1 = predict(mod2.s1, newdata=prostate[set==2,])

(MSPE.v.s1 = mean((prostate[set==2,"lpsa"] - pred.v.s1)^2))
(MSPE.p.s1 = mean((prostate[set==2,"lpsa"] - pred.p.s1)^2))
(MSPE.2.s1 = mean((prostate[set==2,"lpsa"] - pred.2.s1)^2))

#  Best model is 2-variable model, so compute test error there using all other data
mod2.s2 =  lm(lpsa ~ lcavol + pgg45, data=prostate[set<3,])
pred.2.s2 = predict(mod2.s2, newdata=prostate[set==3,])
(MSPE.2.s2 = mean((prostate[set==3,"lpsa"] - pred.2.s2)^2))

##########################################################################
# Part 1b: Random Multiple splits
#   Logic:
#     Just put the code above into a "for()" loop
#     Save results in a matrix
#     Need to create empty matrix to start
#
#   Note that we hold out the test set from above in all of this

# These are simple models on a small data set.  Can afford LOTS of reps.
R = 100

prostate2 = prostate[set<3,]
n2 = nrow(prostate2)

# Set up storage matrices: 1 row per rep, 1 column per model
MSPEs = matrix(NA, nrow=R, ncol=3)
colnames(MSPEs) = c("lcavol-s", "pgg45-s", "both-s")

for(r in 1:R){
  reorder = sample.int(n=n2, size=n2, replace=FALSE)
  set2 = ifelse(test=(reorder < sf1*n2), yes=1, no=2)
  mod.vol.s1 = lm(lpsa ~ lcavol, data=prostate2[set2==1,])
  mod.pgg.s1 = lm(lpsa ~ pgg45, data=prostate2[set2==1,])
  mod2.s1 =  lm(lpsa ~ lcavol + pgg45, data=prostate2[set2==1,])
  
  pred.v.s1 = predict(mod.vol.s1, newdata=prostate2[set2==2,])
  pred.p.s1 = predict(mod.pgg.s1, newdata=prostate2[set2==2,])
  pred.2.s1 = predict(mod2.s1, newdata=prostate2[set2==2,])
  
  MSPEs[r,1] = mean((prostate2[set2==2,"lpsa"] - pred.v.s1)^2)
  MSPEs[r,2] = mean((prostate2[set2==2,"lpsa"] - pred.p.s1)^2)
  MSPEs[r,3] = mean((prostate2[set2==2,"lpsa"] - pred.2.s1)^2)
}

head(MSPEs)

(MSPE.mean = apply(X=MSPEs, MARGIN=2, FUN=mean))
(MSPE.sd = apply(X=MSPEs, MARGIN=2, FUN=sd))
MSPE.CIl = MSPE.mean - qt(p=.975, df=R-1)*MSPE.sd/sqrt(R)
MSPE.CIu = MSPE.mean + qt(p=.975, df=R-1)*MSPE.sd/sqrt(R)
round(cbind(MSPE.CIl, MSPE.CIu),2)

# We could then compute test error on chosen model

##########################################################################
# Part 2a: Cross-Validation model selection (training and validation)

#  Same logic as multiple splits, just more organized
#    V = number of folds
#    N=nrow(DATA)
#    index = int(sample(N)*V/N)+1  # Creates numbers 1,...,V, each in 1/v of sample
#    Then write loop over the values of v=1 to V, selecting [index==v,] or [index!=v,]
#
# Let's do 5-fold CV
V=5
# Abbreviating sample.int function arguments
folds = floor((sample.int(n2)-1)*V/n2) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=3)
colnames(MSPEs.cv) = c("lcavol-c", "pgg45-c", "both-c")

for(v in 1:V){
  mod.vol.cv = lm(lpsa ~ lcavol, data=prostate2[folds != v,])
  mod.pgg.cv= lm(lpsa ~ pgg45, data=prostate2[folds != v,])
  mod2.cv =  lm(lpsa ~ lcavol + pgg45, data=prostate2[folds != v,])
  
  pred.v.cv = predict(mod.vol.cv, newdata=prostate2[folds==v,])
  pred.p.cv = predict(mod.pgg.cv, newdata=prostate2[folds==v,])
  pred.2.cv = predict(mod2.cv, newdata=prostate2[folds==v,])
  
  MSPEs.cv[v,1] = mean((prostate2[folds==v,"lpsa"] - pred.v.cv)^2)
  MSPEs.cv[v,2] = mean((prostate2[folds==v,"lpsa"] - pred.p.cv)^2)
  MSPEs.cv[v,3] = mean((prostate2[folds==v,"lpsa"] - pred.2.cv)^2)
}

MSPEs.cv

(MSPEcv = apply(X=MSPEs.cv, MARGIN=2, FUN=mean))
(MSPEcv.sd = apply(X=MSPEs.cv, MARGIN=2, FUN=sd))
MSPEcv.CIl = MSPEcv - qt(p=.975, df=V-1)*MSPEcv.sd/sqrt(V)
MSPEcv.CIu = MSPEcv + qt(p=.975, df=V-1)*MSPEcv.sd/sqrt(V)
round(cbind(MSPEcv.CIl, MSPEcv.CIu),2)

##########################################################################
# Part 2b: Repeated Cross-validation
#  Just put the CV algorithm in a loop over R

V=5
R=15 #creates a total of 15*n2 predicted values, comparable to multile splits above

MSPEs.cv15 = matrix(NA, nrow=V*R, ncol=3)
colnames(MSPEs.cv15) = c("lcavol-c", "pgg45-c", "both-c")

for (r in 1:R){ 

  folds = floor((sample.int(n2)-1)*V/n2) + 1 
  
  for(v in 1:V){
    mod.vol.cv = lm(lpsa ~ lcavol, data=prostate2[folds != v,])
    mod.pgg.cv= lm(lpsa ~ pgg45, data=prostate2[folds != v,])
    mod2.cv =  lm(lpsa ~ lcavol + pgg45, data=prostate2[folds != v,])
    
    pred.v.cv = predict(mod.vol.cv, newdata=prostate2[folds==v,])
    pred.p.cv = predict(mod.pgg.cv, newdata=prostate2[folds==v,])
    pred.2.cv = predict(mod2.cv, newdata=prostate2[folds==v,])
    
    MSPEs.cv15[(r-1)*V+v,1] = mean((prostate2[folds==v,"lpsa"] - pred.v.cv)^2)
    MSPEs.cv15[(r-1)*V+v,2] = mean((prostate2[folds==v,"lpsa"] - pred.p.cv)^2)
    MSPEs.cv15[(r-1)*V+v,3] = mean((prostate2[folds==v,"lpsa"] - pred.2.cv)^2)
  }
}
MSPEs.cv15

(MSPEcv15 = apply(X=MSPEs.cv15, MARGIN=2, FUN=mean))
(MSPEcv15.sd = apply(X=MSPEs.cv15, MARGIN=2, FUN=sd))
MSPEcv15.CIl = MSPEcv15 - qt(p=.975, df=R*V-1)*MSPEcv15.sd/sqrt(R*V)
MSPEcv15.CIu = MSPEcv15 + qt(p=.975, df=R*V-1)*MSPEcv15.sd/sqrt(R*V)
round(cbind(MSPEcv15.CIl, MSPEcv15.CIu),2)


##########################################################################
# Part 3a: Bootstrap
#   The key change here is that we just add "replace=TRUE" to our sampling function
#   Then the  "unique()" function comes in handy

set1 = sample.int(n2, replace=TRUE)
# set1 contains indexes of all observations selected

#How many unique original data were selected
length(unique(set1))
# what fraction of sample is out-of-bag validation set?
1-length(unique(set1))/n2

mod.vol.b = lm(lpsa ~ lcavol, data=prostate2[set1,])
mod.pgg.b = lm(lpsa ~ pgg45, data=prostate2[set1,])
mod2.b =  lm(lpsa ~ lcavol + pgg45, data=prostate2[set1,])

pred.v.b = predict(mod.vol.b, newdata=prostate2[-unique(set1),])
pred.p.b = predict(mod.pgg.b, newdata=prostate2[-unique(set1),])
pred.2.b = predict(mod2.b, newdata=prostate2[-unique(set1),])

(MSPE.v.b = mean((prostate2[-unique(set1),"lpsa"] - pred.v.b)^2))
(MSPE.p.b = mean((prostate2[-unique(set1),"lpsa"] - pred.p.b)^2))
(MSPE.2.b = mean((prostate2[-unique(set1),"lpsa"] - pred.2.b)^2))


##########################################################################
# Part 3b: Repeated Bootstrap
#  Replicate above R times.
#
# Using 40 reps, which creates similar number of predicted values as above methods
# 
R=40

MSPEs.b40 = matrix(NA, nrow=R, ncol=3)
colnames(MSPEs.b40) = c("lcavol-b", "pgg45-b", "both-b")

for(r in 1:R){
  set1 = sample.int(n2, replace=TRUE)

  mod.vol.b = lm(lpsa ~ lcavol, data=prostate2[set1,])
  mod.pgg.b = lm(lpsa ~ pgg45, data=prostate2[set1,])
  mod2.b =  lm(lpsa ~ lcavol + pgg45, data=prostate2[set1,])
  
  pred.v.b = predict(mod.vol.b, newdata=prostate2[-unique(set1),])
  pred.p.b = predict(mod.pgg.b, newdata=prostate2[-unique(set1),])
  pred.2.b = predict(mod2.b, newdata=prostate2[-unique(set1),])
  
  MSPEs.b40[r,1] = mean((prostate2[-unique(set1),"lpsa"] - pred.v.b)^2)
  MSPEs.b40[r,2] = mean((prostate2[-unique(set1),"lpsa"] - pred.p.b)^2)
  MSPEs.b40[r,3] = mean((prostate2[-unique(set1),"lpsa"] - pred.2.b)^2)
}

head(MSPEs.b40)

(MSPEb40 = apply(X=MSPEs.b40, MARGIN=2, FUN=mean))
(MSPEb40.sd = apply(X=MSPEs.b40, MARGIN=2, FUN=sd))
MSPEb40.CIl = MSPEb40 - qt(p=.975, df=R*V-1)*MSPEb40.sd/sqrt(R*V)
MSPEb40.CIu = MSPEb40 + qt(p=.975, df=R*V-1)*MSPEb40.sd/sqrt(R*V)
round(cbind(MSPEb40.CIl, MSPEb40.CIu),2)

##########################################################################
# Part 4: Boxplots and relative MSPE
#  Replicate above R times.
#

par(mfrow=c(1,3))
boxplot(MSPEs, las=2, ylim=c(0,2.5),
        main="MSPE \n Random data splits")
boxplot(MSPEs.cv15, las=2, ylim=c(0,2.5),
        main="MSPE \n Cross-Validation")
boxplot(MSPEs.b40, las=2, ylim=c(0,2.5),
        main="MSPE \n Bootstrap")

#Relative MSPE: divided by minimim error for each rep

par(mfrow=c(1,3))
low.s = apply(MSPEs, 1, min) 
boxplot(MSPEs/low.s, las=2,
        main="Relative MSPE \n Random data splits")
low.c = apply(MSPEs.cv15, 1, min) 
boxplot(MSPEs.cv15/low.c, las=2,
        main="Relative MSPE \n Cross-Validation")
low.b = apply(MSPEs.b40, 1, min) 
boxplot(MSPEs.b40/low.b, las=2,
        main="Relative MSPE \n Bootstrap")

# Limit range to see comparison of two good models
par(mfrow=c(1,3))
boxplot(MSPEs/low.s, las=2, ylim=c(1,1.5),
        main="Focused Relative MSPE \n Random data splits")
boxplot(MSPEs.cv15/low.c, las=2, ylim=c(1,1.5),
        main="Focused Relative MSPE \n Cross-Validation")
boxplot(MSPEs.b40/low.b, las=2, ylim=c(1,1.5),
        main="Focused Relative MSPE \n Bootstrap")

