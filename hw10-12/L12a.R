###########################################################################
### In this tutorial, we will use PPR to predict alcohol using all the  ###
### other predictors in the wine dataset. We will also see how to       ###
### include a tuned model inside a CV model comparison (that is, how to ###
### do CV inside CV).                                                   ###
###########################################################################
rm(list=ls(all=TRUE))

### This script contains randomness, so we need to set the seed
set.seed(50297026)

### Read-in the data
source("Read Wine Data - All Vars.R")

### It will be convenient to have access to our helper functions from
### previous lectures. I have also added a function which creates CV folds.
source("Helper Functions.R")

### We fit PPR models using the ppr() function. This function uses data frame-
### model formula syntax. We also need to specify the number of terms to
### include in our model, and what type of smoother to use. Number of terms is
### set using the nterms input, and type of smoother is set using sm.method.
### Options for smoothers include "spline" (smoothing spline where you have
### to also specify the number of degrees of freedom using df) and "gcvspline"
### (smoothing spline with degrees of freedom chosen by GCV).
### Let's use just density, pH, sulphates and type so we can compare PPR to
### GAM.
fit.ppr = ppr(alcohol ~ ., data = data,
  max.terms = 8, nterms = 6, sm.method = "gcvspline")

### Get information about the fitted PPR model
summary(fit.ppr)

### Plot the smoothers in a 2x3 grid
par(mar=c(1,1,1,1))
par(mfrow = c(2,3))
plot(fit.ppr)
par(mfrow = c(1,1)) # Reset to the usual 1x1 plotting structure


###########################################################################
### When using PPR in practice, we need to tune the number of terms.    ###
### If we are using CV to choose the best model, we will have to do CV  ###
### within CV. Let's look at how this works. For reference, we will     ###
### compare tuned PPR to lm.                                            ###
###########################################################################

max.terms = 8 # Maximum number of terms for PPR

### Everything starts off normally for CV.

### Create folds
K = 10 # Number of folds
folds = get.folds(nrow(data), K) # Our helper function

### Create container for CV MSPEs
all.models = c("LM", "PPR")
n.models = length(all.models)
CV.MSPEs = array(0, dim = c(n.models, K))
rownames(CV.MSPEs) = all.models

for(i in 1:K){
  ### Print a status update
  print(paste0(i, " of ", K))
  
  ### Split data
  data.train = data[folds != i,]
  data.valid = data[folds == i,]
  Y.valid = data.valid$alcohol
  
  ##########
  ### LM ###
  ##########
  
  fit.lm = lm(alcohol ~ ., data = data.train)
  pred.lm = predict(fit.lm, data.valid)
  MSPE.lm = get.MSPE(Y.valid, pred.lm) # Our helper function
  CV.MSPEs["LM", i] = MSPE.lm
  
  ###########
  ### PPR ###
  ###########
  
  ### To fit PPR, we need to do another round of CV. This time, do 5-fold
  K.ppr = 5
  n.train = nrow(data.train)
  folds.ppr = get.folds(n.train, K.ppr)
  
  ### Container to store MSPEs for each number of terms on each sub-fold
  MSPEs.ppr = array(0, dim = c(max.terms, K.ppr))
  
  for(j in 1:K.ppr){
    ### Split the training data.
    ### Be careful! We are constructing an internal validation set by 
    ### splitting the training set from outer CV.
    train.ppr = data.train[folds.ppr != j,]
    valid.ppr = data.train[folds.ppr == j,] 
    Y.valid.ppr = valid.ppr$alcohol
    
    ### We need to fit several different PPR models, one for each number
    ### of terms. This means another for loop (make sure you use a different
    ### index variable for each loop).
    for(l in 1:max.terms){
      ### Fit model
      fit.ppr = ppr(alcohol ~ ., data = train.ppr, 
        max.terms = max.terms, nterms = l, sm.method = "gcvspline")
      
      ### Get predictions and MSPE
      pred.ppr = predict(fit.ppr, valid.ppr)
      MSPE.ppr = get.MSPE(Y.valid.ppr, pred.ppr) # Our helper function

      ### Store MSPE. Make sure the indices match for MSPEs.ppr
      MSPEs.ppr[l, j] = MSPE.ppr
    }
  }
  print(MSPEs.ppr)
  ### Get average MSPE for each number of terms
  ave.MSPE.ppr = apply(MSPEs.ppr, 1, mean)
  # print(dim(ave.MSPE.ppr))
  print(ave.MSPE.ppr)
  ### Get optimal number of terms
  best.terms = which.min(ave.MSPE.ppr)
  print(best.terms)
  
  ### Fit PPR on the whole CV training set using the optimal number of terms
  fit.ppr.best = ppr(alcohol ~ ., data = data.train,
    max.terms = max.terms, nterms = best.terms, sm.method = "gcvspline")

  ### Get predictions, MSPE and store results
  pred.ppr.best = predict(fit.ppr.best, data.valid)
  MSPE.ppr.best = get.MSPE(Y.valid, pred.ppr.best) # Our helper function

  CV.MSPEs["PPR", i] = MSPE.ppr.best
  print(CV.MSPEs)
}
t(CV.MSPEs)
### Make a boxplot of MSPEs for the different number of terms
boxplot(t(CV.MSPEs))

### Make a boxplot of RMSPEs for the different number of terms
CV.RMSPEs = apply(CV.MSPEs, 2, function(W) W/min(W))
boxplot(t(CV.RMSPEs))


