
# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot

### HW 4 ###############
## Concept ###
#X = numerical
#Z = categorical
Z = rep(x=c("A", "B"), each=10)
X = round(rnorm(20),2)
mean.y = rep(c(3,1), each=10)
epsilon = rnorm(20)
Y = round(mean.y + epsilon, 2)
dat = data.frame(Y,Z,X)
dat

cbind(dat[1:10,],dat[11:20,])

class(dat$Z)
levels(dat$Z)

mod.cat = lm(Y ~ Z, data=dat)
summary(mod.cat)
length(mod.cat$coefficients)

mod.cat2 = lm(Y ~ Z + X, data=dat)
summary(mod.cat2)
length(mod.cat2$coefficients)

library(caret)
# turn categorical to one-hot encoding
dv1 = dummyVars("~.", data=dat)
dv2 = predict(dv1, newdata=dat)
dat.dv = data.frame(dv2)

mod.cat.dv = lm(Y ~ ., data=dat.dv)
summary(mod.cat.dv)


mod.cp = lm(Y ~ Z + X + Z:X, data=dat.dv)
summary(mod.cp)



###########################
#### Application

help("airquality")

AQ_original = airquality
head(AQ_original,5)

# X= Solar.R, Wind, Temp
# Y = Ozone
AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
head(AQ)



################## 1. ##############################
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio

# minimum, maximum, mean
min(AQ$TWcp)
max(AQ$TWcp)
mean(AQ$TWcp)

min(AQ$TWrat)
max(AQ$TWrat)
mean(AQ$TWrat)
# X against Y 
# attach(AQ)
###2.a report 3 slopes and t-values in a table
lm_TWcp = lm(Ozone ~ Temp + Wind + TWcp, data = AQ)
summary(lm_TWcp)
# t.test(Ozone ~ Temp + Wind + TWcp, data = AQ)

lm_TWrat = lm(Ozone ~Temp + Wind + TWrat, data = AQ)
summary(lm_TWrat)
# 
# lm_TempOzone = lm(Ozone ~ Temp, data = AQ)
# summary(lm_TempOzone)

### c.
min(AQ$Wind)
max(AQ$Wind)

######################################################
##########3.#############
# clear var
# rm(list=ls(all=TRUE))

dim(AQ) # 111 x 4
head(AQ)

n = nrow(AQ) #store sample size for easy calculations later

sf1 = 0.75
set.seed(4099183) 

reorder = sample.int(n=n, size=n, replace=FALSE) ### Shuffled numbers from 1 to n
reorder
set = ifelse(test=(reorder < sf1*n), yes=1, 
             no=2)
set
AQ[set==2,]
dim(AQ[set==1,])
dim(AQ[set==2,])
# 
# fit.Solar.R = lm(Ozone ~ Solar.R, data = AQ[set==1,])
# fit.Wind = lm(Ozone ~ Wind, data = AQ[set==1,])
# fit.Temp = lm(Ozone ~Temp, data = AQ[set==1,])
# fit.all = lm(Ozone ~ ., data = AQ[set==1,])
# fit.int = lm(formula= Ozone~Temp+Wind+Solar.R+I(Temp^2)+I(Wind^2)+I(Solar.R^2)
#              +Temp*Wind+Temp*Solar.R+Wind*Solar.R, data = AQ[set==1,])
fit.lm_TWcp = lm(Ozone ~ Temp + Wind + TWcp, data = AQ[set==1,])
fit.lm_TWrat = lm(Ozone ~ Temp + Wind + TWrat, data = AQ[set==1,])

# 
# pred.Solar.R = predict(fit.Solar.R, newdata=AQ[set==2,])
# pred.Wind = predict(fit.Wind, newdata=AQ[set==2,])
# pred.Temp = predict(fit.Temp, newdata=AQ[set==2,])
# pred.all = predict(fit.all, newdata=AQ[set==2,])
# pred.int = predict(fit.int,newdata=AQ[set==2,])
pred.lm_TWcp = predict(fit.lm_TWcp, newdata=AQ[set==2,])
pred.lm_TWrat = predict(fit.lm_TWrat,newdata=AQ[set==2,])

# (MSPE.Solar.R = mean((AQ[set==2,"Ozone"] - pred.Solar.R)^2))
# (MSPE.Wind = mean((AQ[set==2,"Ozone"] - pred.Wind)^2))
# (MSPE.Temp = mean((AQ[set==2,"Ozone"] - pred.Temp)^2))
# (MSPE.all = mean((AQ[set==2,"Ozone"] - pred.all)^2))
# (MSPE.int = mean((AQ[set==2,"Ozone"] - pred.int)^2))
(MSPE.lm_TWcp = mean((AQ[set==2,"Ozone"] - pred.lm_TWcp)^2))
(MSPE.lm_TWrat = mean((AQ[set==2,"Ozone"] - pred.lm_TWrat)^2))

# 
# MSPE.complex = MSPE.int
# MSPE.Solar.R
# MSPE.Wind
# MSPE.Temp
# MSPE.all
# MSPE.complex
MSPE.lm_TWcp
MSPE.lm_TWrat


########4.###################

V=10
R=20 #creates a total of 20*n2 predicted values, comparable to multile splits above

MSPEs.cv20 = matrix(NA, nrow=V*R, ncol=7)
colnames(MSPEs.cv20) = c("Solar.R", "Wind", "Temp","all","complex",'lm_TWcp','lm_TWrat')

for (r in 1:R){ 
  
  folds = floor((sample.int(n)-1)*V/n) + 1 
  
  for(v in 1:V){
    
    fit.Solar.R = lm(Ozone ~ Solar.R, data = AQ[folds != v,])
    fit.Wind = lm(Ozone ~ Wind, data = AQ[folds != v,])
    fit.Temp = lm(Ozone ~Temp, data = AQ[folds != v,])
    fit.all = lm(Ozone ~ ., data = AQ[folds != v,])
    fit.int = lm(formula= Ozone~Temp+Wind+Solar.R+I(Temp^2)+I(Wind^2)+I(Solar.R^2)
                 +Temp*Wind+Temp*Solar.R+Wind*Solar.R, data = AQ[folds != v,])
    fit.lm_TWcp = lm(Ozone ~ Temp + Wind + TWcp, data = AQ[folds != v,])
    fit.lm_TWrat = lm(Ozone ~ Temp + Wind + TWrat, data = AQ[folds != v,])
    
    pred.Solar.R = predict(fit.Solar.R, newdata=AQ[folds==v,])
    pred.Wind = predict(fit.Wind, newdata=AQ[folds==v,])
    pred.Temp = predict(fit.Temp, newdata=AQ[folds==v,])
    pred.all = predict(fit.all, newdata=AQ[folds==v,])
    pred.int = predict(fit.int,newdata=AQ[folds==v,])
    pred.lm_TWcp = predict(fit.lm_TWcp, newdata=AQ[folds==v,])
    pred.lm_TWrat = predict(fit.lm_TWrat,newdata=AQ[folds==v,])
    
    MSPEs.cv20[(r-1)*V+v,1] = mean((AQ[folds==v,"Ozone"] - pred.Solar.R)^2)
    MSPEs.cv20[(r-1)*V+v,2] = mean((AQ[folds==v,"Ozone"] - pred.Wind)^2)
    MSPEs.cv20[(r-1)*V+v,3] = mean((AQ[folds==v,"Ozone"] - pred.Temp)^2)
    MSPEs.cv20[(r-1)*V+v,4] = mean((AQ[folds==v,"Ozone"] - pred.all)^2)
    MSPEs.cv20[(r-1)*V+v,5] = mean((AQ[folds==v,"Ozone"] - pred.int)^2)
    MSPEs.cv20[(r-1)*V+v,6] = mean((AQ[folds==v,"Ozone"] - pred.lm_TWcp)^2)
    MSPEs.cv20[(r-1)*V+v,7] = mean((AQ[folds==v,"Ozone"] - pred.lm_TWrat)^2)
    
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


#######################################
###### Categorical variable exercise
# clear var
rm(list=ls(all=TRUE))

ins=read.csv('Insurance.csv',header=TRUE)
ins$zone = as.factor(ins$zone)
ins$make=as.factor(ins$make)
dim(ins)

ins = ins[ins$claims>0,]
dim(ins)

lm_per = lm(per ~ ., data = ins)
summary(lm_per)
length(lm_per$coefficients)


###############################





################# hw5 ###############
rm(list=ls(all=TRUE))

AQ_original = airquality
head(AQ_original,5)

# X= Solar.R, Wind, Temp
# Y = Ozone
# AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])

AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio

head(AQ)
dim(AQ)

#### 1.###########
library(leaps)
allsub1 <- regsubsets(x=AQ[,2:6], 
                      y=AQ[,1], nbest=1)

allsub1
summ.1 <- summary(allsub1)
summ.1
names(summ.1)
summ.1$bic

bic_table <- data.frame(summ.1$bic)
bic_table


x11(h=15, w=10, pointsize=12)
par(mfrow=c(1,1))
plot(allsub1, main="All AQ data")

##########2.########
## stepwise
n1 = nrow(AQ)

initial.1 <- lm(data=AQ, 
                formula=Ozone~ 1)
final.1 <- lm(data=AQ, 
              formula=Ozone~Solar.R+Wind+Temp+TWcp+TWrat)

step1 <- step(object=initial.1, scope=list(upper=final.1), 
              k = log(n1))
summary(step1)
step1

########3.############
# Let's do 10-fold CV
set.seed(2928893)
n = nrow(AQ)

V=10
# Abbreviating sample.int function arguments
folds = floor((sample.int(n)-1)*V/n) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=1)
# colnames(MSPEs.cv) = c("Solar.R", "Wind", "Temp","TWcp","TWrat")
MSPEs.cv
for(v in 1:V){
  # fit.Solar.R = lm(Ozone ~ Solar.R, data = AQ[folds != v,])
  # fit.Wind = lm(Ozone ~ Wind, data = AQ[folds != v,])
  # fit.Temp = lm(Ozone ~Temp, data = AQ[folds != v,])
  # fit.all = lm(Ozone ~ ., data = AQ[folds != v,])
  # fit.int = lm(formula= Ozone~Temp+Wind+Solar.R+I(Temp^2)+I(Wind^2)+I(Solar.R^2)
  #              +Temp*Wind+Temp*Solar.R+Wind*Solar.R, data = AQ[folds != v,])
  # 
  # pred.Solar.R = predict(fit.Solar.R, newdata=AQ[folds==v,])
  # pred.Wind = predict(fit.Wind, newdata=AQ[folds==v,])
  # pred.Temp = predict(fit.Temp, newdata=AQ[folds==v,])
  # pred.all = predict(fit.all, newdata=AQ[folds==v,])
  # pred.int = predict(fit.int,newdata=AQ[folds==v,])
  # 
  # MSPEs.cv[v,1] = mean((AQ[folds==v,"Ozone"] - pred.Solar.R)^2)
  # MSPEs.cv[v,2] = mean((AQ[folds==v,"Ozone"] - pred.Wind)^2)
  # MSPEs.cv[v,3] = mean((AQ[folds==v,"Ozone"] - pred.Temp)^2)
  # MSPEs.cv[v,4] = mean((AQ[folds==v,"Ozone"] - pred.all)^2)
  # MSPEs.cv[v,5] = mean((AQ[folds==v,"Ozone"] - pred.int)^2)
  # 
  initial <- lm(data=AQ[folds != v,], 
                  formula=Ozone~ 1)
  final <- lm(data=AQ[folds != v,], 
                formula=Ozone~Solar.R+Wind+Temp+TWcp+TWrat)
  n1 = nrow(AQ[folds != v,])
  step <- step(object=initial, scope=list(upper=final), 
                k = log(n1))
  
  pred = predict(step,newdata=AQ[folds==v,])
  MSPEs.cv[v,1] = mean((AQ[folds==v,"Ozone"] - pred)^2)
}

colnames(MSPEs.cv) = c('MSPE each fold')
MSPEs.cv
mean(MSPEs.cv)
