#  Crossvalidation to get prediction error.  The crossval() function in package bootstrap seems most direct
#  Data Splitting, CV, and Bootstrap measures of prediction error.  
#  Using Prostate data, will fit three linear regression models from before
#  Then will use single split, multiple splits, CV, and bootstrap to estimate error
#  Also will show how to use boxplots to compare models

help("airquality")
# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot


AQ_original = airquality
head(AQ_original,5)
dim(AQ_original) # 153 x 6
#######1.deal with missing data: delete null ###
AQ = na.omit(airquality[,1:4])
dim(AQ) # 111 x 4
head(AQ)
########################################

#### 2. seed = 4099183, train/validation: 75/25
##### which obs are in the validation set

n = nrow(AQ) #store sample size for easy calculations later


###########################################

####### 3. fit each model to training and 
####### report MSPEs from the validation

sf1 = 0.75
set.seed(4099183) 

reorder = sample.int(n=n, size=n, replace=FALSE) ### Shuffled numbers from 1 to n
# reorder = sample.int(n=n)
reorder
set = ifelse(test=(reorder < sf1*n), yes=1, 
             no=2)
set
AQ[set==2,]
dim(AQ[set==1,])
dim(AQ[set==2,])

fit.Solar.R = lm(Ozone ~ Solar.R, data = AQ[set==1,])
fit.Wind = lm(Ozone ~ Wind, data = AQ[set==1,])
fit.Temp = lm(Ozone ~Temp, data = AQ[set==1,])
fit.all = lm(Ozone ~ ., data = AQ[set==1,])
fit.int = lm(formula= Ozone~Temp+Wind+Solar.R+I(Temp^2)+I(Wind^2)+I(Solar.R^2)
             +Temp*Wind+Temp*Solar.R+Wind*Solar.R, data = AQ[set==1,])

pred.Solar.R = predict(fit.Solar.R, newdata=AQ[set==2,])
pred.Wind = predict(fit.Wind, newdata=AQ[set==2,])
pred.Temp = predict(fit.Temp, newdata=AQ[set==2,])
pred.all = predict(fit.all, newdata=AQ[set==2,])
pred.int = predict(fit.int,newdata=AQ[set==2,])

(MSPE.Solar.R = mean((AQ[set==2,"Ozone"] - pred.Solar.R)^2))
(MSPE.Wind = mean((AQ[set==2,"Ozone"] - pred.Wind)^2))
(MSPE.Temp = mean((AQ[set==2,"Ozone"] - pred.Temp)^2))
(MSPE.all = mean((AQ[set==2,"Ozone"] - pred.all)^2))
(MSPE.int = mean((AQ[set==2,"Ozone"] - pred.int)^2))
MSPE.complex = MSPE.int

MSPE.Solar.R
MSPE.Wind
MSPE.Temp
MSPE.all
MSPE.complex

##########################################################################

##### 4. 10-fold CV to estimate MSPEs for 5 models
# Let's do 10-fold CV
V=10
# Abbreviating sample.int function arguments
folds = floor((sample.int(n)-1)*V/n) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=5)
colnames(MSPEs.cv) = c("Solar.R", "Wind", "Temp","all","complex")

for(v in 1:V){
  fit.Solar.R = lm(Ozone ~ Solar.R, data = AQ[folds != v,])
  fit.Wind = lm(Ozone ~ Wind, data = AQ[folds != v,])
  fit.Temp = lm(Ozone ~Temp, data = AQ[folds != v,])
  fit.all = lm(Ozone ~ ., data = AQ[folds != v,])
  fit.int = lm(formula= Ozone~Temp+Wind+Solar.R+I(Temp^2)+I(Wind^2)+I(Solar.R^2)
               +Temp*Wind+Temp*Solar.R+Wind*Solar.R, data = AQ[folds != v,])
  
  pred.Solar.R = predict(fit.Solar.R, newdata=AQ[folds==v,])
  pred.Wind = predict(fit.Wind, newdata=AQ[folds==v,])
  pred.Temp = predict(fit.Temp, newdata=AQ[folds==v,])
  pred.all = predict(fit.all, newdata=AQ[folds==v,])
  pred.int = predict(fit.int,newdata=AQ[folds==v,])
  
  MSPEs.cv[v,1] = mean((AQ[folds==v,"Ozone"] - pred.Solar.R)^2)
  MSPEs.cv[v,2] = mean((AQ[folds==v,"Ozone"] - pred.Wind)^2)
  MSPEs.cv[v,3] = mean((AQ[folds==v,"Ozone"] - pred.Temp)^2)
  MSPEs.cv[v,4] = mean((AQ[folds==v,"Ozone"] - pred.all)^2)
  MSPEs.cv[v,5] = mean((AQ[folds==v,"Ozone"] - pred.int)^2)
  
}
MSPEs.cv

# R = 100

(MSPEcv = apply(X=MSPEs.cv, MARGIN=2, FUN=mean))
(MSPEcv.sd = apply(X=MSPEs.cv, MARGIN=2, FUN=sd))
# MSPEcv.CIl = MSPEcv - qt(p=.975, df=R-1)*MSPEcv.sd/sqrt(V)
# MSPEcv.CIu = MSPEcv + qt(p=.975, df=R-1)*MSPEcv.sd/sqrt(V)
MSPEcv.CIl = MSPEcv - qt(p=.975, df=V-1)*MSPEcv.sd/sqrt(V)
MSPEcv.CIu = MSPEcv + qt(p=.975, df=V-1)*MSPEcv.sd/sqrt(V)
round(cbind(MSPEcv.CIl, MSPEcv.CIu),2)
table = round(cbind(MSPEcv.CIl, MSPEcv.CIu,MSPEcv),3)
colnames(table)[3] = "mean"
table

#############################################

##### 5.CV 20 times, MSPEs, RSMPEs

V=10
R=20 #creates a total of 20*n2 predicted values, comparable to multile splits above

MSPEs.cv20 = matrix(NA, nrow=V*R, ncol=5)
colnames(MSPEs.cv20) = c("Solar.R", "Wind", "Temp","all","complex")

for (r in 1:R){ 
  
  folds = floor((sample.int(n)-1)*V/n) + 1 
  
  for(v in 1:V){
  
    fit.Solar.R = lm(Ozone ~ Solar.R, data = AQ[folds != v,])
    fit.Wind = lm(Ozone ~ Wind, data = AQ[folds != v,])
    fit.Temp = lm(Ozone ~Temp, data = AQ[folds != v,])
    fit.all = lm(Ozone ~ ., data = AQ[folds != v,])
    fit.int = lm(formula= Ozone~Temp+Wind+Solar.R+I(Temp^2)+I(Wind^2)+I(Solar.R^2)
                 +Temp*Wind+Temp*Solar.R+Wind*Solar.R, data = AQ[folds != v,])
    
    pred.Solar.R = predict(fit.Solar.R, newdata=AQ[folds==v,])
    pred.Wind = predict(fit.Wind, newdata=AQ[folds==v,])
    pred.Temp = predict(fit.Temp, newdata=AQ[folds==v,])
    pred.all = predict(fit.all, newdata=AQ[folds==v,])
    pred.int = predict(fit.int,newdata=AQ[folds==v,])
    
    MSPEs.cv20[(r-1)*V+v,1] = mean((AQ[folds==v,"Ozone"] - pred.Solar.R)^2)
    MSPEs.cv20[(r-1)*V+v,2] = mean((AQ[folds==v,"Ozone"] - pred.Wind)^2)
    MSPEs.cv20[(r-1)*V+v,3] = mean((AQ[folds==v,"Ozone"] - pred.Temp)^2)
    MSPEs.cv20[(r-1)*V+v,4] = mean((AQ[folds==v,"Ozone"] - pred.all)^2)
    MSPEs.cv20[(r-1)*V+v,5] = mean((AQ[folds==v,"Ozone"] - pred.int)^2)
    
  }
}
MSPEs.cv20

(MSPEcv20 = apply(X=MSPEs.cv20, MARGIN=2, FUN=mean))
(MSPEcv20.sd = apply(X=MSPEs.cv20, MARGIN=2, FUN=sd))
MSPEcv20.CIl = MSPEcv20 - qt(p=.975, df=R*V-1)*MSPEcv20.sd/sqrt(R*V)
MSPEcv20.CIu = MSPEcv20 + qt(p=.975, df=R*V-1)*MSPEcv20.sd/sqrt(R*V)
round(cbind(MSPEcv20.CIl, MSPEcv20.CIu),2)

##############################################

# Boxplot

boxplot(MSPEs.cv20, las=2, ylim=c(0,2800),
        main="MSPE \n Cross-Validation")

low.c = apply(MSPEs.cv20, 1, min) 
boxplot(MSPEs.cv20/low.c, las=2,ylim=c(0,15),
        main="Relative MSPE \n Cross-Validation")

boxplot(MSPEs.cv20/low.c, las=2, ylim=c(1,1.6),
        main="Focused Relative MSPE \n Cross-Validation")


