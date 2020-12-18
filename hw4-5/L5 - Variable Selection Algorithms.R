#########################################################################
# Variable Selection Algorithms on Prostate Data
#
# Splitting data in half to show that model selection is quite variable!
#
# 1. All subsets regression
# 2. Stepwise
#########################################################################

prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")
head(prostate)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
prostate$set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

#########################################################################
# 1. All subsets 
#  Several R packages and functions do all-subsets. 
#  I will show leaps::regsubsets()
#  Need to separately give data for response and explanatories
#
# regsubsets() will return information oi the bese models of each size
#   but does not compare them to choose one best
#
# Have to do that separately.  I make a loop to do this
#   a. fit best model of each size
#   b. Compute sMSE, BIC, and MSPE from other half of data
#   c. Make plots of these.  
#  Could also select smallest BIC or MSPE and refit model for that size

library(leaps)
#  Note: default is to limit to 8-variable models.  
#  Add nvmax argument to increase when needed.
allsub1 <- regsubsets(x=prostate[prostate$set==1,1:8], 
                      y=prostate[prostate$set==1,9], nbest=1)
allsub2 <- regsubsets(x=prostate[prostate$set==2,1:8], 
                      y=prostate[prostate$set==2,9], nbest=1)


# Store summary() so we can see BICs 
summ.1 <- summary(allsub1)
summ.2 <- summary(allsub2)

summ.1
summ.2

names(summ.1)
summ.1$bic
summ.2$bic

# Plot of results in a special form
x11(h=7, w=10, pointsize=12)
par(mfrow=c(1,2))
plot(allsub1, main="All Subsets on half of Prostate data")
plot(allsub2, main="All Subsets on other half of Prostate data")

# Fitting the models in succession from smallest to largest.  
# Fit empty model and save results. 
# Then fit best 1-var, 2-var, etc., in loop
# Each time computing sample-MSE (sMSE), BIC, and mean squared pred. error (MSPE). 

# Sample sizes in two splits
n1 = nrow(prostate[prostate$set==1,])
n2 = nrow(prostate[prostate$set==2,])

results1 <- matrix(data=NA, nrow=9, ncol=4)
mod0 <- lm(lpsa ~ 1, data=prostate[prostate$set==1,])

#   (I usually use this for model evaluation when I am not using it for model selection)
pred11 <- predict(mod0, newdata=prostate[prostate$set==1,])
sMSE <- mean((pred11-prostate[prostate$set==1,]$lpsa)^2)

BIC <- extractAIC(mod0, k=log(n1))
pred2 <- predict(mod0, newdata=prostate[prostate$set==2,])
MSPE <- mean((pred2-prostate[prostate$set==2,]$lpsa)^2)
results1[1,] <- c(0, sMSE, BIC[2], MSPE)

colnames(results1) = c("p", "sMSE", "BIC", "MSPE")

# Move response to 1st column to be included every time below.
#  ("Disguising" it in the intercept slot)
prostate2 <- prostate[,c(9,1:8)]

for(v in 1:8){
  mod1 <- lm(lpsa ~ ., data=prostate2[prostate$set==1, summ.1$which[v,]])
  BIC <- extractAIC(mod1, k=log(n1))
  pred1 <- predict(mod1)
  sMSE <- mean((pred1-prostate2[prostate$set==1,]$lpsa)^2)
  pred2 <- predict(mod1, newdata=prostate2[prostate$set==2,])
  MSPE <- mean((pred2-prostate2[prostate$set==2,]$lpsa)^2)
  results1[v+1,] <- c(v, sMSE, BIC[2], MSPE)
}

round(results1, digits=2)
# Best size according to BIC
results1[which.min(results1[,3]),1]
# Best size according to MSPE
results1[which.min(results1[,4]),1]

# All 3 plots together
x11(width=10,height=5,pointsize=18)
par(mfrow=c(1,3))
plot(x=results1[,1], y=results1[,2], xlab="Vars in model", ylab="sample-MSE",
     main="SampleMSE vs Vars: 1st", type="b")
plot(x=results1[,1], y=results1[,3], xlab="Vars in model", ylab="BIC",
     main="BIC vs Vars: 1st", type="b")
plot(x=results1[,1], y=results1[,4], xlab="Vars in model", ylab="MSPE",
     main="MSPE vs Vars: 1st", type="b")

# Could repeat for second half of data

#########################################################################
# 2. 
# Stepwise selection using "step()".  
#   You'll see what it's doing when you look at results
#
# Procedure:
# Fit minimum and maximum model first ("initial" and "final" below)
# In step(), object=SMALLEST MODEL, scope=list(upper=LARGEST MODEL),
#   argument to k gives it the penalty coefficient for the IC to use.  
#   Here we use BIC via log(n).

# First half of data

initial.1 <- lm(data=prostate[prostate$set==1,], 
                formula=lpsa~ 1)
final.1 <- lm(data=prostate[prostate$set==1,], 
              formula=lpsa~lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45)

step1 <- step(object=initial.1, scope=list(upper=final.1), 
              k = log(n1))

# Second half of data

initial.2 <- lm(data=prostate[prostate$set==2,], 
                formula=lpsa~ 1)
final.2 <- lm(data=prostate[prostate$set==2,], 
              formula=lpsa~lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45)
step2 <- step(object=initial.2, scope=list(upper=final.2),  
              k = log(n2))

# summary() does lm() regression on the best model.  
#   Can save object for later use if you want. 
summary(step1)
summary(step2)

