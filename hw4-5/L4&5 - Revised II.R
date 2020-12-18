### We will analyze the wine dataset again, this time including some
### categorical predictors: wine type and quality.
### Note: quality is a numeric ordinal variable, but we will re-code
###       it as categorical with levels low, medium and high. The 
###       re-coding is done in a separate R file

### We will use multiple packages in this script. I like to activate
### packages and set the seed at the beginning.

library(dplyr) # I like using filter and select sometimes
library(leaps) # This package contains the regsubsets() function,
               # which does all subsets selection

set.seed(6588719)

#####################################################################
### If you find yourself writing code to do the same thing        ###
### repeatedly, it is often helpful to write a function that does ###
### that thing. Let's make some such functions here.              ###
#####################################################################

### We will regularly need to shuffle a vector. This function
### does that for us.
shuffle = function(X){
  new.order = sample.int(length(X))
  new.X = X[new.order]
  return(new.X)
}

### We will also often need to calculate MSE using an observed
### and a prediction vector. This will be another useful function.
get.MSPE = function(Y, Y.hat){
  return(mean((Y - Y.hat)^2))
}

### The way that the lm() function calculates predicted values
### is somewhat limited. Specifically, the predict function only 
### works if our new data are in a data frame which contains 
### columns who's names match the variables in our model. Sometimes,
### we will want to format the new data as a matrix. This function
### lets us still get predicted values.
### Note: This function uses matrix-vector multiplication via the
### %*% operation. If you have taken a course in linear algebra, you
### will have seen how useful this tool can be. If not, don't
### worry, you don't need to understand the details of this function
predict.matrix = function(fit.lm, X.mat){
  coeffs = fit.lm$coefficients
  Y.hat = X.mat %*% coeffs
  return(Y.hat)
}

##############################
### Now on to our analysis ###
##############################

### Use a new script to read-in and clean the dataset. The new version
### keeps quality and type. It also recodes quality into low, 
### medium and high
source("Read Wine Data - Categorical.R")
head(data)

### Let's split the data into training and validation sets (75%/25%)
n = nrow(data)
n.train = floor(n * 0.75)
n.valid = n - n.train
groups = c(rep(1, times = n.train), rep(2, times = n.valid))
groups.shuffle = shuffle(groups)
data.train = data[groups.shuffle == 1,]
data.valid = data[groups.shuffle == 2,]

### Note: We split the data into training and validation sets before
### doing any kind of analysis. This is because we want to avoid
### letting the validation set influence any modelling decisions

### We should check what these new variables look like. We can get counts
### using the table() function
table(data.train$type)
table(data.train$quality)


########################################################################
### For our analysis, let's do variable selection using all subsets  ###
### and forward stepwise selection with second-order interactions.   ### 
### Each model's MSPE will be calculated on the validation set.      ###
########################################################################

### Create a container to store MSPEs
all.models = c("subsets.AIC", "subsets.BIC",
  "stepwise.AIC", "stepwise.BIC")
all.MSPEs = rep(0, times = length(all.models))
names(all.MSPEs) = all.models

#############################################
### All subsets selection via AIC and BIC ###
#############################################

### Note: we use the regsubsets() function from the leaps package
### to do all subsets selection. It chooses the best model of each size,
### but doesn't compare across sizes. We will have to re-fit the
### best model of each size before we can get the AICs (or BICs).
### The best model overall is the one with smallest AIC (or BIC),
### which is the one we use to predict the validation set.
###
### The syntax for regsubsets() is a little different from lm. 
### To use regsubsets(), we will need to construct all of our
### explanatory variables manually and store them in a matrix
### with one row per observation and one column per variable.
### We then pass this model matrix, along with our response variable,
### to regsubsets. This allows us to extract just the variables
### we need when we re-fit the models of various sizes

### The model.matrix lets us specify the regression formula we want
### to use, and outputs the corresponding model matrix
data.matrix = model.matrix(alcohol ~ .^2, data = data.train)

### We also need the response variable (i.e. alcohol)
Y.train = data.train$alcohol

### Now we can run all.subsets. There are a couple of extra inputs
### here. nvmax is the largest number of variables we are willing
### to include. 30 seems like plenty. intercept specifies whether we 
### want regsubsets() to add an intercept term. Since model.matrix()
### already adds an intercept, we don't want regsubsets() to add
### another one.
all.subsets = regsubsets(x = data.matrix, y = Y.train, nvmax = 30,
  intercept = F)

### The output of regsubsets isn't really useful. We need to run the
### summary() function on it to get useful information
info.subsets = summary(all.subsets)

### The output of summary contains an array with columns corresponding
### to predictors and rows corresponding to model sizes. This array 
### tells us which variables are included at each size. 
all.subsets.models = info.subsets$which

### We can get the AIC and BIC of each of these models by re-fitting
### the models and running extractAIC(). The extractAIC() function
### has an input called k, which is the coefficient on the penalty term.
### We can get AIC by setting k=2 (the default), and BIC by setting
### k equal to the logarithm of the sample size.
### Note: We are going to fit 15 models here. That's way too many
### to code manually, so we will use a for loop.
n.models = nrow(all.subsets.models) # Number of candidate models
all.AICs = rep(0, times = n.models) # Container to store AICs
all.BICs = all.AICs # Copy all.AICs to get a container for BICs

for(i in 1:n.models){
  ### We can actually supply a model matrix and response vector 
  ### to lm, without using a data frame. Remember that our model matrix
  ### already has an intercept, so we need to make sure lm doesn't
  ### include another one. We do this by including -1 in the right side
  ### of the model formula.
  this.data.matrix = data.matrix[,all.subsets.models[i,]]
  fit = lm(Y.train ~ this.data.matrix - 1)
  
  ### Get the AIC using extractAIC(). This function takes a regression
  ### model as input, as well as (optionally) an input called k, which
  ### specifies the penalty on the number of variables in our model.
  ### The AIC value is in the second component of the output object.
  this.AIC = extractAIC(fit)[2]
  all.AICs[i] = this.AIC
  
  ### Get the BIC using extractAIC(). This time, we need to set k equal
  ### to the log of the number of observations used to fit our model
  this.BIC = extractAIC(fit, k = log(n.train))[2]
  all.BICs[i] = this.BIC
}

### Get the optimal model for AIC and BIC
AIC.ind = which.min(all.AICs)
AIC.model = all.subsets.models[AIC.ind,]
BIC.ind = which.min(all.BICs)
BIC.model = all.subsets.models[BIC.ind,]

### Question: The model chosen by BIC is smaller. This is actually
### true in general (more precisely, BIC never chooses a larger
### model). Why do you think this is true?

### Next, we need to fit these models and get predictions
### on the validation set. To do so, we will need to construct
### model matrices for the training and validation sets which
### correspond to the chosen models for AIC and BIC
data.matrix.valid = model.matrix(alcohol ~ .^2, data = data.valid)

AIC.train = data.matrix[,AIC.model]
BIC.train = data.matrix[,BIC.model]
AIC.valid = data.matrix.valid[,AIC.model]
BIC.valid = data.matrix.valid[,BIC.model]

### Now we can fit the optimal models on the training set, and get
### validation set MSPEs (remember the -1)
AIC.fit = lm(data.train$alcohol ~ AIC.train - 1)
BIC.fit = lm(data.train$alcohol ~ BIC.train - 1)

AIC.pred = predict.matrix(AIC.fit, AIC.valid)
BIC.pred = predict.matrix(BIC.fit, BIC.valid)

Y.valid = data.valid$alcohol # Response vector in validation set
AIC.err = get.MSPE(Y.valid, AIC.pred)
BIC.err = get.MSPE(Y.valid, BIC.pred)

### Finally, we can store these MSPEs in all.MSPEs
all.MSPEs["subsets.AIC"] = AIC.err
all.MSPEs["subsets.BIC"] = BIC.err


##########################################
### Stepwise selection via AIC and BIC ###
##########################################

### To do stepwise selection, we use the step() function (no package
### required). This function takes the starting model and a list 
### containing the ending model, then fits a sequence of nested
### models from start to end. Optionally, we can also provide an
### input called k, which is the penalty for the number of variables
### in a model. For AIC, set k = 2 (this is the default). For BIC, set
### k = log(n), where n is the number of observations used to fit the 
### model. Note: For BIC, n is usually the size of the training set,
### not the size of the entire sample.
### By default, the step() function prints information about every model
### it considers. This is great for understanding your data, but
### if you want to put step() inside a loop (e.g. for CV), you will
### end up with a lot of text that you're not going to read. There is 
### an optional input to step() called trace. Setting trace = 0 
### will stop step() from printing anything. 

fit.start = lm(alcohol ~ 1, data = data.train)
fit.end = lm(alcohol ~ .^2, data = data.train)

step.AIC = step(fit.start, list(upper = fit.end), k = 2)
step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train),
  trace = 0)

pred.step.AIC = predict(step.AIC, data.valid)
pred.step.BIC = predict(step.BIC, data.valid)

err.step.AIC = get.MSPE(Y.valid, pred.step.AIC)
err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)

all.MSPEs["stepwise.AIC"] = err.step.AIC
all.MSPEs["stepwise.BIC"] = err.step.BIC


#####################################################################
### We have now fit all subsets and stepwise selection models,    ###
### and calculated their MSPEs on a single data split. It would   ###
### be nice to have a measure of the uncertainty in these MSPE.   ###
### A great way to get this is with cross validation. Let's use 5 ###
### folds instead of 10, because the dataset is kind of small     ###
#####################################################################


### First we need to set the number of folds
K = 5

### Construct folds
### Don't attach fold labels to dataset because we would just have
### to remove this later
n = nrow(data)
n.fold = n/K # Approximate number of observations per fold
n.fold = ceiling(n.fold)
ordered.ids = rep(1:10, each = n.fold)
ordered.ids = ordered.ids[1:n]
fold.ids = shuffle(ordered.ids)


### Create a container to store CV MSPEs
### One column per model, and one row per fold
CV.models = c("subsets.AIC", "subsets.BIC",
  "stepwise.AIC", "stepwise.BIC")
errs.CV = array(0, dim = c(K,length(CV.models)))
colnames(errs.CV) = CV.models

### Perform cross-validation to estimate MSPE for each model
### It takes a few seconds to do all-subsets at each iteration, so
### the entire loop will take some time. I like to print out a
### status update at the start of each iteration. The paste0()
### function attaches all of its inputs into a single string.
for(i in 1:K){
  print(paste0(i, " of ", K))
  
  ### Construct training and validation sets by either removing
  ### or extracting the current fold. 
  ### Also, get the response vectors
  data.train = data[fold.ids != i,]
  data.valid = data[fold.ids == i,]
  Y.train = data.train$alcohol
  Y.valid = data.train$alcohol
  
  

  #################################################################
  ### I just copy-and-pasted the code from above and made some  ###
  ### small changes. I removed any comments describing how the  ###
  ### code works and just commented any changes I made          ###
  #################################################################
  
  #############################################
  ### All subsets selection via AIC and BIC ###
  #############################################
  
  data.matrix = model.matrix(alcohol ~ .^2, data = data.train)
  
  ### Y.train was calculated at the beginning of the loop,
  ### so no need to re-extract it here.
  all.subsets = regsubsets(x = data.matrix, y = Y.train, nvmax = 30,
    intercept = F)
  
  info.subsets = summary(all.subsets)
  
  all.subsets.models = info.subsets$which

  n.models = nrow(all.subsets.models) 
  all.AICs = rep(0, times = n.models)
  all.BICs = all.AICs
  
  ### We have used i as the looping variable in our CV for loop,
  ### so we need a different variable here. Let's use j
  for(j in 1:n.models){
    this.data.matrix = data.matrix[,all.subsets.models[j,]]
    fit = lm(Y.train ~ this.data.matrix - 1)
    
    this.AIC = extractAIC(fit)[2]
    all.AICs[j] = this.AIC

    this.BIC = extractAIC(fit, k = log(n.train))[2]
    all.BICs[j] = this.BIC
  }
  
  AIC.ind = which.min(all.AICs)
  AIC.model = all.subsets.models[AIC.ind,]
  BIC.ind = which.min(all.BICs)
  BIC.model = all.subsets.models[BIC.ind,]

  data.matrix.valid = model.matrix(alcohol ~ .^2, data = data.valid)
  
  AIC.train = data.matrix[,AIC.model]
  BIC.train = data.matrix[,BIC.model]
  AIC.valid = data.matrix.valid[,AIC.model]
  BIC.valid = data.matrix.valid[,BIC.model]
  
  AIC.fit = lm(data.train$alcohol ~ AIC.train - 1)
  BIC.fit = lm(data.train$alcohol ~ BIC.train - 1)
  
  AIC.pred = predict.matrix(AIC.fit, AIC.valid)
  BIC.pred = predict.matrix(BIC.fit, BIC.valid)
  
  Y.valid = data.valid$alcohol # Response vector in validation set
  AIC.err = get.MSPE(Y.valid, AIC.pred)
  BIC.err = get.MSPE(Y.valid, BIC.pred)
  
  ### Store errors in errs.CV, which has two dimensions, so 
  ### we need two indices
  errs.CV[i,"subsets.AIC"] = AIC.err
  errs.CV[i,"subsets.BIC"] = BIC.err
  
  
  ##########################################
  ### Stepwise selection via AIC and BIC ###
  ##########################################
  
  fit.start = lm(alcohol ~ 1, data = data.train)
  fit.end = lm(alcohol ~ .^2, data = data.train)
  
  ### These functions will run several times each. We don't need
  ### to print out all the details, so set trace = 0.
  step.AIC = step(fit.start, list(upper = fit.end), k=2,
    trace = 0)
  step.BIC = step(fit.start, list(upper = fit.end), k = log(n.train),
    trace = 0)
  
  pred.step.AIC = predict(step.AIC, data.valid)
  pred.step.BIC = predict(step.BIC, data.valid)
  
  err.step.AIC = get.MSPE(Y.valid, pred.step.AIC)
  err.step.BIC = get.MSPE(Y.valid, pred.step.BIC)
  
  ### Store errors in errs.CV, which has two dimensions, so 
  ### we need two indices
  errs.CV[i, "stepwise.AIC"] = err.step.AIC
  errs.CV[i, "stepwise.BIC"] = err.step.BIC
}


### Now that we have multiple estimates of the models' MSPEs, let's
### make a boxplot
boxplot(errs.CV, main = "CV Error Estimates")


### Finally, we can get the relative MSPEs and make the corresponding
### boxplot. See Lecture 3 for details of the apply() function.
rel.errs.CV = apply(errs.CV, 1, function(W){
  best = min(W)
  return(W / best)
})
rel.errs.CV = t(rel.errs.CV) # Re-orient output

boxplot(rel.errs.CV, main = "Relative CV Error Estimates")
