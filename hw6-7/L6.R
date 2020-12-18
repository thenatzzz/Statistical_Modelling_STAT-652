### We will analyze the wine quality dataset using Ridge regression and
### the LASSO. You have seen cross-validation a few times now, so we're
### going to jump straight into using CV to estimate the models' MSPEs.
### Since we're working on variable selection, let's include all the
### predictors in the wine quality dataset, instead of just the ones
### we have been focusing on up until now (there is a new script to 
### read the dataset)

### This analysis will need some packages
### Note: no dplyr this time because it and MASS both have a function
###       called select()
library(MASS)   # For ridge regression
library(glmnet) # For LASSO

### There were some functions we defined and used last week which
### made life easier. I've put them in a separate R script so we
### can give ourselves access to them without running all of last
### week's code.
source("Helper Functions.R")

### We are going to do CV, so we need to run set.seed()
set.seed(93425633)

### Read-in the data, and keep all predictors
source("Read Wine Data - All Vars.R")

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
all.models = c("LS", "Ridge", "LASSO-Min", "LASSO-1se")
all.MSPEs = array(0, dim = c(K, length(all.models)))
colnames(all.MSPEs) = all.models
  
### Begin cross-validation
for(i in 1:K){
  ### Split data
  data.train = data[folds != i,]
  data.valid = data[folds == i,]
  n.train = nrow(data.train)
  
  ### Get response vectors
  Y.train = data.train$alcohol
  Y.valid = data.valid$alcohol
  
  ###################################################################
  ### First, let's quickly do LS so we have a reference point for ###
  ### how well the other models do.                               ###
  ###################################################################
  
  fit.ls = lm(alcohol ~ ., data = data.train)
  pred.ls = predict(fit.ls, newdata = data.valid)
  MSPE.ls = get.MSPE(Y.valid, pred.ls)
  all.MSPEs[i, "LS"] = MSPE.ls
  
  
  #######################################################################
  ### Next, let's do ridge regression. This model is fit using the    ###
  ### lm.ridge() function in the MASS package. We will need to make a ###
  ### list of candidate lambda values for the function to choose      ###
  ### from. Prediction also has some extra steps, but we'll discuss   ###
  ### that when we get there.                                         ###
  #######################################################################
  
  ### Make a list of lambda values. The lm.ridge() function will
  ### then choose the best value from this list. Use the seq()
  ### function to create an equally-spaced list.
  lambda.vals = seq(from = 0, to = 100, by = 0.05)
  
  ### Use the lm.ridge() function to fit a ridge regression model. The
  ### syntax is almost identical to the lm() function, we just need
  ### to set lambda equal to our list of candidate values.
  fit.ridge = lm.ridge(alcohol ~ ., lambda = lambda.vals, 
    data = data.train)
  
  ### To get predictions, we need to evaluate the fitted regression
  ### equation directly (sadly, no predict() function to do this for us).
  ### You could do this using a for loop if you prefer, but there is
  ### a shortcut which uses matrix-vector multiplication. The syntax
  ### for this multiplication method is much shorter.
  
  ### Get best lambda value and its index
  ### Note: Best is chosen according to smallest GCV value. We can 
  ###       get GCV from a ridge regression object using $GCV
  ind.min.GCV = which.min(fit.ridge$GCV)
  lambda.min = lambda.vals[ind.min.GCV]
  
  ### Get coefficients corresponding to best lambda value
  ### We can get the coefficients for every value of lambda using
  ### the coef() function on a ridge regression object
  all.coefs.ridge = coef(fit.ridge)
  coef.min = all.coefs.ridge[ind.min.GCV,]
  
  ### We will multiply the dataset by this coefficients vector, but 
  ### we need to add a column to our dataset for the intercept and 
  ### create indicators for our categorical predictors. A simple
  ### way to do this is using the model.matrix() function from last
  ### week.
  matrix.valid.ridge = model.matrix(alcohol ~ ., data = data.valid)
  
  ### Now we can multiply the data by our coefficient vector. The
  ### syntax in R for matrix-vector multiplication is %*%. Note that,
  ### for this type of multiplication, order matters. That is,
  ### A %*% B != B %*% A. Make sure you do data %*% coefficients.
  ### For more information, see me in a Q&A session or, better still,
  ### take a course on linear algebra (it's really neat stuff)
  pred.ridge = matrix.valid.ridge %*% coef.min
  
  ### Now we just need to calculate the MSPE and store it
  MSPE.ridge = get.MSPE(Y.valid, pred.ridge)
  all.MSPEs[i, "Ridge"] = MSPE.ridge
  
  
  #######################################################################
  ### Now we can do the LASSO. This model is fit using the glmnet()   ###
  ### or cv.glmnet() functions in the glmnet package. LASSO also has  ###
  ### a tuning parameter, lambda, which we have to choose.            ###
  ### Fortunately, the cv.glmnet() function does CV internally, and   ###
  ### lets us automatically find the 'best' value of lambda.          ###
  #######################################################################
  
  ### The cv.glmnet() function has different syntax from what we're 
  ### used to. Here, we have to provide a matrix with all of our
  ### predictors, and a vector of our response. LASSO handles
  ### the intercept differently, so we want to make sure our data
  ### matrix does not include an intercept (then let cv.glmnet() add
  ### an intercept later). Unfortunately, the model.matrix() function
  ### gets confused if we ask it to construct indicators for our 
  ### categorical predictors without also including an intercept.
  ### A simple way to fix this is to create the data matrix with an
  ### intercept, then delete the intercept.
  matrix.train.raw = model.matrix(alcohol ~ ., data = data.train)
  matrix.train = matrix.train.raw[,-1]
  
  ### The cv.glmnet() function creates a list of lambda values, then
  ### does CV internally to choose the 'best' one. 'Best' can refer to
  ### either the value of lambda which gives the smallest CV-MSPE 
  ### (called the min rule), or the value of lambda which gives the
  ### simplest model that gives CV-MSPE close to the minimum (called
  ### the 1se rule). The cv.glmnet() function gets both of these
  ### lambda values.
  all.LASSOs = cv.glmnet(x = matrix.train, y = Y.train)
  
  ### Get both 'best' lambda values using $lambda.min and $lambda.1se
  lambda.min = all.LASSOs$lambda.min
  lambda.1se = all.LASSOs$lambda.1se
  
  ### cv.glmnet() has a predict() function (yay!). This predict function
  ### also does other things, like get the coefficients, or tell us
  ### which predictors get non-zero coefficients. We are also able
  ### to specify the value of lambda for which we want our output
  ### (remember that, with ridge, we got a matrix of coefficients and
  ### had to choose the row matching our lambda). Strangely, the name
  ### of the input where we specify our value of lambda is s.
  
  ### Get the coefficients for our two 'best' LASSO models
  coef.LASSO.min = predict(all.LASSOs, s = lambda.min, type = "coef")
  coef.LASSO.1se = predict(all.LASSOs, s = lambda.1se, type = "coef")
  
  ### Get which predictors are included in our models (i.e. which 
  ### predictors have non-zero coefficients)
  included.LASSO.min = predict(all.LASSOs, s = lambda.min, 
    type = "nonzero")
  included.LASSO.1se = predict(all.LASSOs, s = lambda.1se, 
    type = "nonzero")
  
  ### Get predictions from both models on the validation fold. First,
  ### we need to create a predictor matrix from the validation set.
  ### Remember to include the intercept in model.matrix(), then delete
  ### it in the next step. 
  matrix.valid.LASSO.raw = model.matrix(alcohol ~ ., data = data.valid)
  matrix.valid.LASSO = matrix.valid.LASSO.raw[,-1]
  pred.LASSO.min = predict(all.LASSOs, newx = matrix.valid.LASSO,
    s = lambda.min, type = "response")
  pred.LASSO.1se = predict(all.LASSOs, newx = matrix.valid.LASSO,
    s = lambda.1se, type = "response")
  
  ### Calculate MSPEs and store them
  MSPE.LASSO.min = get.MSPE(Y.valid, pred.LASSO.min)
  all.MSPEs[i, "LASSO-Min"] = MSPE.LASSO.min
  
  MSPE.LASSO.1se = get.MSPE(Y.valid, pred.LASSO.1se)
  all.MSPEs[i, "LASSO-1se"] = MSPE.LASSO.1se
}

### Make a boxplot of MSPEs. I would like to include the number of folds
### in the title. This can be done by using the paste0() function,
### which concatenates strings (i.e. attaches them end-to-end), and
### can be provided numeric variables.
boxplot(all.MSPEs, main = paste0("CV MSPEs over ", K, " folds"))

### Calculate RMSPEs
all.RMSPEs = apply(all.MSPEs, 1, function(W){
  best = min(W)
  return(W / best)
})
all.RMSPEs = t(all.RMSPEs)

### Make a boxplot of RMSPEs
boxplot(all.RMSPEs, main = paste0("CV RMSPEs over ", K, " folds"))

### One model is much worse than the others. Let's zoom in on the
### good models.
boxplot(all.RMSPEs, ylim = c(1, 1.03),
  main = paste0("CV RMSPEs over ", K, 
    " folds (enlarged to show texture)"),
  )
