### We will perform an unsupervised analysis of the wine quality data
### using PCA, and a supervised analysis using PLS. We will also 
### start using all the predictors in the wine quality dataset (there
### is a new script to read the dataset)

### This analysis will need a package
library(pls)    # For partial least squares

### There were some functions we defined and used last week which
### made life easier. I've put them in a separate R script so we
### can give ourselves access to them without running all of last
### week's code.
source("Helper Functions.R")

### We are going to do CV, so we need to run set.seed()
set.seed(31295905)

### Read-in the data, and keep all the predictors
source("Read Wine Data - All Vars.R")


#########################################################################
### Let's start with PCA. We do PCA using the prcomp() function. This ###
### function takes a matrix containing our explanatory variables,     ###
### with no intercept and any categorical predictors converted to     ###
### indicators. We can construct such a matrix using the              ###
### model.matrix() function.                                          ###
### Note: We can do PCA on the whole dataset without worrying about   ###
###       overfitting, because the delta errors only affect our       ###
###       response, not the predictors.                               ###
#########################################################################

### Construct matrix of predictors. We don't want an intercept term,
### but the model.matrix() function gets confused if we ask it to create
### indicators for our categorical predictors without also including
### an intercept. We can correct this by including an intercept in 
### model.matrix(), then deleting the intercept immediately after.
data.matrix.raw = model.matrix(alcohol ~ ., data = data)
data.matrix = data.matrix.raw[,-1]

### Perform PCA. The syntax is pretty straightforward. Setting 
### scale. = T scales each predictor so they have the same SD. This
### tends to produce a better PCA
fit.PCA = prcomp(data.matrix, scale. = T)
print(fit.PCA)

### There is a default plotting method for prcomp() objects. However,
### it's kind of hard to read, so let's make a better version.

### First, get the variance of each component (i.e. SD squared)
vars = fit.PCA$sdev^2

### Plot the variance of each component (called a scree plot). 
### We can also add a horizontal line at 1, the average amount of 
### variability, using the abline() function. Setting h equal to a 
### number adds a horizontal line at that number to the most recent 
### plot.
plot(1:length(vars), vars, main = "Variability Explained", 
  xlab = "Principal Component", ylab = "Variance Explained")
abline(h = 1)

### We can also plot the cumulative proportion of variability explained
### if we include the first 1, 2, etc. components. We can use the
### cumsum() function to compute the cumulative sum of a vector (i.e.
### position 4 is the sum of the first 4 components of the original
### vector).
c.vars = cumsum(vars)   ### Cumulative variance explained
rel.c.vars = c.vars / max(c.vars)   ### Cumulative proportion of 
                                    ### variance explained
plot(1:length(rel.c.vars), rel.c.vars,
  main = "Proportion of Variance Explained by First W PCs",
  xlab = "W", ylab = "Proportion of Variance Explained")


### Based on the scree plot, I think that it is appropriate to
### include 4 or 5 PCs. Let's look at the weights of the first 5 PCs.
### We can get all the weights from prcomp() using $rotation, then
### we an just extract the columns we are interested in.
all.PCs = fit.PCA$rotation
print(all.PCs[,1:5])


#########################################################################
### Next, we will do a partial least squares analysis. This method    ###
### uses the plsr() function from the pls package. Let's also use CV  ###
### to calculate the MSPE of our model. Remember that we have to fit  ###
### the model and choose the appropriate number of terms separately   ###
### for each fold. More generally, whatever your tuning procedure     ###
### looks like, it must be repeated for each fold when doing CV.      ###
#########################################################################

### Let's define a function for constructing CV folds
get.folds = function(n, K) {
  ### Get the appropriate number of fold labels
  n.fold = ceiling(n / K) # Number of observations per fold (rounded up)
  fold.ids.raw = rep(1:K, times = n.fold) # Generate extra labels
  fold.ids = fold.ids.raw[1:n] # Keep only the correct number of labels
  
  ### Shuffle the fold labels
  folds.rand = fold.ids[sample.int(n)]
  
  return(folds.rand)
}


### Number of folds
K = 10

### Construct folds
n = nrow(data) # Sample size
folds = get.folds(n, K)

### Create a container for MSPEs. Let's include ordinary least-squares
### regression for reference
all.models = c("LS", "PLS")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models

### Begin cross-validation
for(i in 1:K){
  ### Split data
  data.train = data[folds != i,]
  data.valid = data[folds == i,]
  n.train = nrow(data.train)
  
  ### Get response vector
  Y.valid = data.valid$alcohol
  
  ###################################################################
  ### First, let's quickly do LS so we have a reference point for ###
  ### how well the other models do.                               ###
  ###################################################################
  
  fit.ls = lm(alcohol ~ ., data = data.train)
  pred.ls = predict(fit.ls, newdata = data.valid)
  MSPE.ls = get.MSPE(Y.valid, pred.ls)
  all.MSPEs[i, "LS"] = MSPE.ls
  
  ### Now, let's do PLS using the plsr() function. The syntax is 
  ### very similar to lm(). If we set validation = "CV", the plsr()
  ### function will do its own internal CV, and give MSPEs for each
  ### number of components. We can then use this to choose how many
  ### componenets to keep when doing prediction on the validation
  ### fold. We can use an optional input called segments to specify 
  ### how many folds we want plsr() to use for its internal CV 
  ### (default is 10).
  fit.pls = plsr(alcohol ~ ., data = data.train, validation = "CV",
    segments = 5)

  ### Investigate the fitted PLS model. Comment out the next two 
  ### lines when running a CV loop
  
  ### The summary function gives us lots of information about how
  ### errors change as we increase the number of components
  # summary(fit.pls)
  
  ### The validationplot() function shows how MSPE from the internal 
  ### CV of plsr() changes with the number of included components.
  # validationplot(fit.pls)
  
  ### Get the best model from PLS. To do this, we need to find the model
  ### that minimizes MSPE for the plsr() function's internal CV. It 
  ### takes a few steps, but all the information we need is contained
  ### in the output of plsr().
  CV.pls = fit.pls$validation # All the CV information
  PRESS.pls = CV.pls$PRESS    # Sum of squared CV residuals
  CV.MSPE.pls = PRESS.pls / nrow(data.train)  # MSPE for internal CV
  ind.best.pls = which.min(CV.MSPE.pls) # Optimal number of components
  
  
  ### Get predictions and calculate MSPE on the validation fold
  ### Set ncomps equal to the optimal number of components
  pred.pls = predict(fit.pls, data.valid, ncomp = ind.best.pls)
  MSPE.pls = get.MSPE(Y.valid, pred.pls)
  all.MSPEs[i, "PLS"] = MSPE.pls
}

### Make boxplots
boxplot(all.MSPEs, main = "MSPE for 10-fold CV")
