###########################################################################
### In this tutorial, we will use boosting on the wine quality data to  ###
### predict alcohol using a subset of the other predictors.             ###
###########################################################################

library(gbm)

set.seed(17299534)

source("Read Wine Data.R")
source("Helper Functions.R")


### We will start by fitting one boosting model so we can see how it works. 
### Boosting is done in R using the gbm() function in the gbm package. This function 
### uses formula/data syntax. Other options include n.trees, interaction.depth,
### shrinkage and bag.fraction for, respectively, the number of trees to fit, the 
### maximum tree depth, the learning rate, and the subsampling proportion. You 
### also need to set distribution to "gaussian" when doing regression. If you want
### to do even more tuning, you can use n.minobsinnode to control the terminal
### node size.
### Let's try 60 trees, interaction depth of 1, shrinkage of 0.1, and bag
### fraction of 0.8
fit.gbm.1 = gbm(alcohol ~ ., data = data, distribution = "gaussian", n.trees = 100,
  interaction.depth = 1, shrinkage = 0.1, bag.fraction = 0.8)

### We can get various performance measures using the gbm.perf() function. Setting
### oobag.curve to T gives a plot of improvement in out-of-bag (OOB) error of each 
### tree. The gbm.perf() function returns the number of trees at which (smoothed)
### OOB improvement goes from positive (good) to negative (bad). We can just get out
### the chosen number of trees without any plots by setting plot.it to F.
### Note: This process for choosing the number of trees tends to give too few. Tom's
###       approach is to double whatever gbm.perf() says is best.
gbm.perf(fit.gbm.1, oobag.curve = T)
n.trees.gbm = gbm.perf(fit.gbm.1, plot.it=F)
n.trees.Tom = 2 * n.trees.gbm

### By default, gbm() fits 100 trees. Tom's rule says we need 112, so we need to
### fit more. Fortunately, the gbm.more() function lets us add trees to an 
### existing gbm fit. By default, gbm.more() adds 100 new trees, but you can change
### this by setting the number explicitly.
fit.gbm.2 = gbm.more(fit.gbm.1, 20)

### For completeness, let's get the SMSE. The predict function requires that we
### specify the number of trees to use.
pred.gbm = predict(fit.gbm.2, data, n.trees = n.trees.gbm)
pred.Tom = predict(fit.gbm.2, data, n.trees = n.trees.Tom)
(SMSE.gbm = get.MSPE(data$alcohol, pred.gbm))
(SMSE.Tom = get.MSPE(data$alcohol, pred.Tom))


###############################################################################
### Neither of these models do very well, even on SMSE. Let's see if we can ###
### improve by tuning.                                                      ###
###############################################################################

set.seed(10326789)

### Set parameter values
### We will stick to resampling rate of 0.8, maximum of 10000 trees, and Tom's rule
### for choosing how many trees to keep.
max.trees = 10000
all.shrink = c(0.001, 0.01, 0.1)
all.depth = c(1, 2, 3)
all.pars = expand.grid(shrink = all.shrink, depth = all.depth)
n.pars = nrow(all.pars)

### Number of folds
K = 5

### Get folds
n = nrow(data)
folds = get.folds(n, K)

### Create container for CV MSPEs
CV.MSPEs = array(0, dim = c(K, n.pars))


for(i in 1:K){
  ### Print progress update
  print(paste0(i, " of ", K))
  
  ### Split data
  data.train = data[folds != i,]
  data.valid = data[folds == i,]
  Y.valid = data.valid$alcohol
  
  
  
  ### Fit boosting models for each parameter combination
  for(j in 1:n.pars){
    ### Get current parameter values
    this.shrink = all.pars[j,"shrink"]
    this.depth = all.pars[j,"depth"]
    
    ### Fit model using current parameter values.
    fit.gbm = gbm(alcohol ~ ., data = data.train, distribution = "gaussian", 
      n.trees = max.trees, interaction.depth = this.depth, shrinkage = this.shrink, 
      bag.fraction = 0.8)
    
    ### Choose how many trees to keep using Tom's rule. This will print many
    ### warnings about not just using the number of trees recommended by
    ### gbm.perf(). We have already addressed this problem though, so we can
    ### just ignore the warnings.
    n.trees = gbm.perf(fit.gbm, plot.it = F) * 2
    
    ### Check to make sure that Tom's rule doesn't tell us to use more than 1000
    ### trees. If it does, add extra trees as necessary
    if(n.trees > max.trees){
      extra.trees = n.trees - max.trees
      fit.gbm = gbm.more(fit.gbm, extra.trees)
    }
    
    ### Get predictions and MSPE, then store MSPE
    pred.gbm = predict(fit.gbm, data.valid, n.trees)
    MSPE.gbm = get.MSPE(Y.valid, pred.gbm)
    
    CV.MSPEs[i, j] = MSPE.gbm # Be careful with indices for CV.MSPEs
  }
}


### We can now make an MSPE boxplot. First, add column names to indicate
### which parameter combination was used. Format is shrinkage-depth
names.pars = paste0(all.pars$shrink,"-",
  all.pars$depth)
colnames(CV.MSPEs) = names.pars

### Make boxplot
boxplot(CV.MSPEs, las = 2, main = "MSPE Boxplot")


### Get relative MSPEs and make boxplot
CV.RMSPEs = apply(CV.MSPEs, 1, function(W) W/min(W))
CV.RMSPEs = t(CV.RMSPEs)
boxplot(CV.RMSPEs, las = 2, main = "RMSPE Boxplot")

###############################################################################
### This dataset appears to prefer higher interaction depth. It's not clear ###
### about shrinkage though. Let's run another round of CV with larger       ###
### interaction depths but the same shrinkage.                              ###
###############################################################################

set.seed(68211069)

### Set parameter values
### We will stick to resampling rate of 0.8, maximum of 10000 trees, and Tom's rule
### for choosing how many trees to keep.
max.trees = 10000
all.shrink = c(0.001, 0.01, 0.1)
all.depth = c(3, 4, 5)
all.pars = expand.grid(shrink = all.shrink, depth = all.depth)
n.pars = nrow(all.pars)

### Number of folds
K = 5

### Create folds
folds = get.folds(n, K)

### Create container for CV MSPEs
CV.MSPEs2 = array(0, dim = c(K, n.pars))

for(i in 1:K){
  ### Print progress update
  print(paste0(i, " of ", K))
  
  ### Split data
  data.train = data[folds != i,]
  data.valid = data[folds == i,]
  Y.valid = data.valid$alcohol
  
  
  
  ### Fit boosting models for each parameter combination
  for(j in 1:n.pars){
    ### Get current parameter values
    this.shrink = all.pars[j,"shrink"]
    this.depth = all.pars[j,"depth"]
    
    ### Fit model using current parameter values.
    fit.gbm = gbm(alcohol ~ ., data = data.train, distribution = "gaussian", 
      n.trees = max.trees, interaction.depth = this.depth, shrinkage = this.shrink, 
      bag.fraction = 0.8)
    
    ### Choose how many trees to keep using Tom's rule. This will print many
    ### warnings about not just using the number of trees recommended by
    ### gbm.perf(). We have already addressed this problem though, so we can
    ### just ignore the warnings.
    n.trees = gbm.perf(fit.gbm, plot.it = F) * 2
    
    ### Check to make sure that Tom's rule doesn't tell us to use more than 1000
    ### trees. If it does, add extra trees as necessary
    if(n.trees > max.trees){
      extra.trees = n.trees - max.trees
      fit.gbm = gbm.more(fit.gbm, extra.trees)
    }
    
    ### Get predictions and MSPE, then store MSPE
    pred.gbm = predict(fit.gbm, data.valid, n.trees)
    MSPE.gbm = get.MSPE(Y.valid, pred.gbm)
    
    CV.MSPEs2[i, j] = MSPE.gbm # Be careful with indices for CV.MSPEs
  }
}


### We can now make an MSPE boxplot. First, add column names to indicate
### which parameter combination was used. Format is shrinkage-depth
names.pars = paste0(all.pars$shrink,"-",
  all.pars$depth)
colnames(CV.MSPEs2) = names.pars

### Make boxplot
boxplot(CV.MSPEs2, las = 2, main = "MSPE Boxplot")


### Get relative MSPEs and make boxplot
CV.RMSPEs2 = apply(CV.MSPEs2, 1, function(W) W/min(W))
CV.RMSPEs2 = t(CV.RMSPEs2)
boxplot(CV.RMSPEs2, las = 2, main = "RMSPE Boxplot")



### Based on the RMSPE boxplot, the model with shrinkage = 0.1 and depth = 3 looks
### best. Let's fit this model and compare it to the default one we fit above.

fit.gbm.best = gbm(alcohol ~ ., data = data, distribution = "gaussian", 
  n.trees = 10000, interaction.depth = 3, shrinkage = 0.1, bag.fraction = 0.8)

n.trees.best = gbm.perf(fit.gbm.best, plot.it = F) * 2 # Number of trees


### Lets compare the default boosting model to our tuned version. To do this,
### we will need a test set. Fortunately, I have uploaded a 1000 observation
### subsample of the wine data to Canvas for just this purpose. Let's read-in
### the test set and get MSPEs
data.test.raw = read.csv("Datasets/Wine Quality - Test.csv")
data.test = data.test.raw[, c(4, 8, 9, 10, 11)]
colnames(data.test)[1] = "sugar"

pred.gbm = predict(fit.gbm, data.test, n.trees.gbm)
(MSPE.gbm = get.MSPE(data.test$alcohol, pred.gbm))

pred.Tom = predict(fit.gbm, data.test, n.trees.Tom)
(MSPE.Tom = get.MSPE(data.test$alcohol, pred.Tom))

pred.best = predict(fit.gbm.best, data.test, n.trees.best)
(MSPE.best = get.MSPE(data.test$alcohol, pred.best))
