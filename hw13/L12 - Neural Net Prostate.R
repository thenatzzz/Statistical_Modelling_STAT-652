#########################################################################
# Neural net on Prostate Data 
# 
# Will fit 1-, 2-, and 3- node models to the two variables.  
#   Then moving to complete analysis of full data.  
# Long program, but lots going on.  
# 
# Warning: this is complicated.  
#  I will simplify it as much as possible, but it required some care to use properly. 
rm(list=ls(all=TRUE))


prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")
#head(prostate)

# Here I will make a small test set so I can show how to compute test/validation error.
# Selecting 20 obs for test set

set.seed(2371903) 
reorder = sample.int(n=nrow(prostate))
reorder

set = ifelse(test=(reorder > 20), yes=1, no=2)
set


library(nnet)

############
## Computing will work better if explanatories are rescaled to lie in [0,1]
# Function below scales values in a single object according to its own min and max.
#   Can apply it to training data, but validation set needs to be scaled
#   BY THE TRAINING SET MIN AND MAX!
############

# Use min and max from each column of x2 to rescale 
#   corresponding columns of x1.
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

# Split data into training and test data and store as matrices.
y.1 <- as.matrix(prostate[set==1,9])
x.1.unscaled <- as.matrix(prostate[set==1,-9]) # Original data set 1
x.1 <- rescale(x.1.unscaled, x.1.unscaled) #scaled data set 1
summary(x.1.unscaled)
summary(x.1)

#Test
y.2 <- as.matrix(prostate[set==2,9])
x.2.unscaled <- as.matrix(prostate[set==2,-9]) # Original data set 2
x.2 = rescale(x.2.unscaled, x.1.unscaled)
summary(x.2)

# Training neural net
###########################################################################
# First using nnet::nnet.  This function can do regression or classification.
#  For regression, defaults 
#     loss function is RSS, 
#     sigmoidal activation functon, 
#     linear output function (MUST SPECIFY "linout=TRUE"), 
#     ONE hidden layer, 
#     generate initial weights randomly, 
#     NO SHRINKAGE ("weight decay") 
# Recommend decay between .01 and .0001 (Venables and Ripley 2002, Sec 8.10)
#   Probably need more as M ("size") gets larger 
###########################################################################

### Fit to set 1, test on Set 2
# Use only lcavol and pgg45
### size = #hidden nodes, 
### maxit = maximum number of iterations before termination
### RERUN THE CODE BELOW SEVERAL TIMES
nn.1 <- nnet(y=y.1, x=x.1[,c(1,8)], linout=TRUE, size=1, maxit=1000)
p.nn.1 <-predict(nn.1, newdata=x.2)
#(MSPR.nn1 <- mean((y.2 - p.nn.1)^2))
(MSE.nn1 <- nn.1$value/nrow(x.1))

plot(x=y.2, y=p.nn.1, main="Predicted vs. Actual")
abline(a=0,b=1)
# Shows estimated weights, in case you care
summary(nn.1)

# Plot the surface
library(rgl)  

x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

pred2 <- predict(nn.1 ,newdata=rescale(xy1,x.1.unscaled[,c(1,8)]))
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")

################################################
### Repeat with 3 hidden nodes, add some shrinkage
nn.3.1 <- nnet(y=y.1, x=x.1[,c(1,8)], linout=TRUE, size=3, 
             decay=0.1,maxit=500)
p.nn.3.1 <-predict(nn.3.1, newdata=x.2)
(MSPR.nn3.1 <- mean((y.2 - p.nn.3.1)^2))
(MSE.nn3.1 <- nn.3.1$value/nrow(x.1))

plot(x=y.2, y=p.nn.3.1, main="Predicted vs. Actual")
abline(a=0,b=1)
# Shows estimated weights, in case you care
summary(nn.3.1)

# Plot the surface
library(rgl)  

x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

pred2 <- predict(nn.3.1 ,newdata=rescale(xy1,x.1.unscaled[,c(1,8)]))
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")



###########################################################################
# We now fit to the full data.
# We need to tune the NN to figure out what size and decay to use
#
# "caret" is a library of functions for Classification And REgression Training. 
#   The train() function trains various predictors using bootstrap 
#   to fit model to data and predict remaining data. (Can change to CV)
#   See http://topepo.github.io/caret/training.html for details
# It uses the average performance across the multiple resampling iterations
#   as the primary performance criterion.  
#
# The warning about missing values is an error that is produced when 
#   a predictor returns constant values. Why??? 
# THIS IS A PROBLEM WITH CARET! It does not let you rerun the nnet.
###########################################################################

library(caret)

# This function will do min-max scaling internally with preProcess="range"
# Need to use y as a numeric vector and not a matrix.
# Can change numbers used in tune.Grid as needed.

# Default is 25 bootstrap reps 
# Can be changed by adding
# trcon = trainControl(method=..., number=..., repeats=...)
#  Can do method= "boot", "cv", "repeatedcv", "LOOCV", and several others
#  number= is number of boot reps or cv folds
#  repeats= number of CV replicates
#  returnResamp="all" retains the error measures from each split.

#Specify 5-fold CV run twice
trcon = trainControl(method="repeatedcv", number=5, repeats=2,
                     returnResamp="all")
parmgrid = expand.grid(size=c(1,2,4,6),decay= c(0,.1, .5, 1))

tuned.nnet <- train(x=prostate[,-9], y=prostate[,9], method="nnet", 
                    trace=FALSE, linout=TRUE, 
                    trControl=trcon, preProcess="range", 
                    tuneGrid = parmgrid)
tuned.nnet
names(tuned.nnet)
tuned.nnet$bestTune

#If I want to make plots, I need to rearrange the resamples
(resamp.caret = tuned.nnet$resample[,-c(2,3)])

library(reshape)
RMSPE.caret = reshape(resamp.caret, direction="wide", v.names="RMSE",
                      idvar=c("size","decay"), timevar="Resample")


# Plot results. 
siz.dec <- paste("NN",RMSPE.caret[,1],"/",RMSPE.caret[,2])
x11(pointsize=10)
boxplot(x=as.matrix(RMSPE.caret[,-c(1,2)]), use.cols=FALSE, names=siz.dec,
        las=2, main="caret Root-MSPE boxplot for various NNs")

# Plot RELATIVE results. 
lowt = apply(RMSPE.caret[,-c(1,2)], 2, min)

x11(pointsize=10)
boxplot(x=t(as.matrix(RMSPE.caret[,-c(1,2)]))/lowt, las=2, 
        names=siz.dec)

#Focused 
x11(pointsize=10)
boxplot(x=t(as.matrix(RMSPE.caret[,-c(1,2)]))/lowt, las=2, 
        names=siz.dec, ylim=c(1,2))

R=2
V=5
relMSPE = t(RMSPE.caret[,-c(1,2)])/lowt
relMSPE
(RRMSPE = apply(X=relMSPE, MARGIN=2, FUN=mean))
(RRMSPE.sd = apply(X=relMSPE, MARGIN=2, FUN=sd))
RRMSPE.CIl = RRMSPE - qt(p=.975, df=R*V-1)*RRMSPE.sd/sqrt(R*V)
RRMSPE.CIu = RRMSPE + qt(p=.975, df=R*V-1)*RRMSPE.sd/sqrt(R*V)
(all.rrcv = cbind(RMSPE.caret[,1:2],round(cbind(RRMSPE,RRMSPE.CIl, RRMSPE.CIu),2)))

all.rrcv[order(RRMSPE),]



#######################################################################
##
#######################################################################

# If you like having more control over what is being produced,  
#   you can make your own tuning code?
# Will use V-fold CV with R reps, where user specifies each
# For each resample, will examine every combination of tuning parameters
# Set of tuning parameters is in the "siz" and "dec" objects, number of bootstrap reps is "reps"

#For simplicity, rename data as "train.x" and "train.y"
train.x = prostate[,-9]
train.y = prostate[,9]

#  Let's do R=2 reps of 5-fold CV.
set.seed(39021039)
V=5
R=2 
n2 = nrow(train.x)
# Create the folds and save in a matrix
folds = matrix(NA, nrow=n2, ncol=R)
for(r in 1:R){
  folds[,r]=floor((sample.int(n2)-1)*V/n2) + 1
}

# Grid for tuning parameters and number of restarts of nnet
siz <- c(1,2,4,6)
dec <- c(0,.1, .5, 1)
nrounds=10

# Prepare matrix for storing results: 
#   row = 1 combination of tuning parameters
#   column = 1 split
#   Add grid values to first two columns

MSPEs.cv = matrix(NA, nrow=length(siz)*length(dec), ncol=V*R+2)
MSPEs.cv[,1:2] = as.matrix(expand.grid(siz,dec))
MSPEs.cv
# Start loop over all reps and folds.  
for (r in 1:R){ 
  for(v in 1:V){
    
    y.1 <- as.matrix(train.y[folds[,r]!=v])
    x.1.unscaled <- as.matrix(train.x[folds[,r]!=v,]) 
    x.1 <- rescale(x.1.unscaled, x.1.unscaled) 

    #Test
    y.2 <- as.matrix(train.y[folds[,r]==v])
    x.2.unscaled <- as.matrix(train.x[folds[,r]==v,]) # Original data set 2
    x.2 = rescale(x.2.unscaled, x.1.unscaled)

# Start counter to add each model's MSPE to row of matrix
    qq=1
# Start Analysis Loop for all combos of size and decay on chosen data set
    for(d in dec){
      for(s in siz){

        ## Restart nnet nrounds times to get best fit for each set of parameters 
        MSE.final <- 9e99
        #  check <- MSE.final
        for(i in 1:nrounds){
          nn <- nnet(y=y.1, x=x.1, linout=TRUE, size=s, decay=d, maxit=500, trace=FALSE)
          MSE <- nn$value/nrow(x.1)
          if(MSE < MSE.final){ 
            MSE.final <- MSE
            nn.final <- nn
          }
        }
        pred.nn = predict(nn.final, newdata=x.2)
        MSPEs.cv[qq,(r-1)*V+v+2] = mean((y.2 - pred.nn)^2)
        qq = qq+1
      }
    }
  }
}
MSPEs.cv

(MSPEcv = apply(X=MSPEs.cv[,-c(1,2)], MARGIN=1, FUN=mean))
(MSPEcv.sd = apply(X=MSPEs.cv[,-c(1,2)], MARGIN=1, FUN=sd))
MSPEcv.CIl = MSPEcv - qt(p=.975, df=R*V-1)*MSPEcv.sd/sqrt(R*V)
MSPEcv.CIu = MSPEcv + qt(p=.975, df=R*V-1)*MSPEcv.sd/sqrt(R*V)
(all.cv = cbind(MSPEs.cv[,1:2],round(cbind(MSPEcv,MSPEcv.CIl, MSPEcv.CIu),2)))
all.cv[order(MSPEcv),]


# Plot results. 
siz.dec <- paste("NN",MSPEs.cv[,1],"/",MSPEs.cv[,2])
x11(pointsize=10)
boxplot(x=sqrt(MSPEs.cv[,-c(1,2)]), use.cols=FALSE, names=siz.dec,
               las=2, main="Root-MSPE boxplot for various NNs")

# Plot RELATIVE results. 
lowt = apply(MSPEs.cv[,-c(1,2)], 2, min)

x11(pointsize=10)
boxplot(x=sqrt(t(MSPEs.cv[,-c(1,2)])/lowt), las=2 ,names=siz.dec)

#Focused 
x11(pointsize=10)
boxplot(x=sqrt(t(MSPEs.cv[,-c(1,2)])/lowt), las=2 ,names=siz.dec, ylim=c(1,2))

relMSPE = sqrt(t(MSPEs.cv[,-c(1,2)])/lowt)
(RRMSPE = apply(X=relMSPE, MARGIN=2, FUN=mean))
(RRMSPE.sd = apply(X=relMSPE, MARGIN=2, FUN=sd))
RRMSPE.CIl = RRMSPE - qt(p=.975, df=R*V-1)*RRMSPE.sd/sqrt(R*V)
RRMSPE.CIu = RRMSPE + qt(p=.975, df=R*V-1)*RRMSPE.sd/sqrt(R*V)
(all.rrcv = cbind(MSPEs.cv[,1:2],round(sqrt(cbind(RRMSPE,RRMSPE.CIl, RRMSPE.CIu)),2)))
all.rrcv[order(RRMSPE),]



