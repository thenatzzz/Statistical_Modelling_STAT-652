#################################################################################
### In this tutorial, we will use random forests on the wine quality data to  ###
### predict alcohol using a subset of the other predictors. We will also see  ###
### a new way to do tuning using out-of-bag error                             ###
#################################################################################

library(randomForest)

set.seed(36597804)

source("Read Wine Data.R")
source("Helper Functions.R")


### We will start by fitting a single random forest so we can see how it works. 
### Random forests are fit in R using the randomForest() function in the randomForest
### package. This function uses formula/data syntax. Other options include ntree,
### nodesize, and mtry for, respectively, the number of trees to fit, the terminal 
### node size, and the number of predictor candidates to include for each tree. You 
### can also set importance to TRUE to get variable importance, and keep.forest to 
### TRUE if you want to store the resulting forest (you will pretty much always want 
### to do this, and the default is TRUE, so we will just leave out keep.forest).
### Let's just leave all the settings at their default values and see how we do
### (we do need to set importance to TRUE)
fit.rf.1 = randomForest(alcohol ~ ., data = data, importance = T)

### Plotting this RF object lets us see if we used enough trees. If not, you can
### change this by re-running randomForest() and setting ntree to a higher
### number (default is 500)
plot(fit.rf.1)

### We can get variable importance measures using the importance() function, and
### we can plot them using VarImpPlot()
importance(fit.rf.1)
varImpPlot(fit.rf.1)

### We can get out-of-bag (OOB) error directly from the predict() function. 
### Specifically, if we don't include a new dataset, R gives the OOB predictions
### on the training set.
OOB.pred.1 = predict(fit.rf.1)
(OOB.MSPE.1 = get.MSPE(data$alcohol, OOB.pred.1))

### For reference, we can get the SMSE by using our training set with predict()
sample.pred.1 = predict(fit.rf.1, data)
(SMSE.1 = get.MSPE(data$alcohol, sample.pred.1))



###############################################################################
### Now, let's look at how to tune random forests using OOB error. We will  ###
### consider mtry = 1,2,3,4 and nodesize = 2, 5, 8.                         ###
###############################################################################

### Set parameter values
all.mtry = 1:4
all.nodesize = c(2, 5, 8)
all.pars = expand.grid(mtry = all.mtry, nodesize = all.nodesize)
n.pars = nrow(all.pars)

### Number of times to replicate process. OOB errors are based on bootstrapping,
### so they are random and we should repeat multiple runs
M = 5

### Create container for OOB MSPEs
OOB.MSPEs = array(0, dim = c(M, n.pars))

for(i in 1:n.pars){
  ### Print progress update
  print(paste0(i, " of ", n.pars))
  
  ### Get current parameter values
  this.mtry = all.pars[i,"mtry"]
  this.nodesize = all.pars[i,"nodesize"]
  
  ### Fit random forest models for each parameter combination
  ### A second for loop will make our life easier here
  for(j in 1:M){
    ### Fit model using current parameter values. We don't need variable
    ### importance measures here and getting them takes time, so set
    ### importance to F
    fit.rf = randomForest(alcohol ~ ., data = data, importance = F,
      mtry = this.mtry, nodesize = this.nodesize)
    
    ### Get OOB predictions and MSPE, then store MSPE
    OOB.pred = predict(fit.rf)
    OOB.MSPE = get.MSPE(data$alcohol, OOB.pred)
    
    OOB.MSPEs[j, i] = OOB.MSPE # Be careful with indices for OOB.MSPEs
  }
}


### We can now make an MSPE boxplot. First, add column names to indicate
### which parameter combination was used. Format is mtry-nodesize
names.pars = paste0(all.pars$mtry,"-",
  all.pars$nodesize)
colnames(OOB.MSPEs) = names.pars

### Make boxplot
boxplot(OOB.MSPEs, las = 2, main = "MSPE Boxplot")


### Get relative MSPEs and make boxplot
OOB.RMSPEs = apply(OOB.MSPEs, 1, function(W) W/min(W))
OOB.RMSPEs = t(OOB.RMSPEs)
boxplot(OOB.RMSPEs, las = 2, main = "RMSPE Boxplot")

### Zoom in on the competitive models
boxplot(OOB.RMSPEs, las = 2, main = "RMSPE Boxplot", ylim = c(1, 1.02))



### Based on the RMSPE boxplot, the model with mtry=4 and nodesize=2 looks best
### to me. Let's fit this model and see how it compares to the default one from
### above.
fit.rf.2 = randomForest(alcohol ~ ., data = data, importance = T,
  mtry = 4, nodesize = 2)

### Did we use enough trees?
plot(fit.rf.2)

### How important are the predictors?
varImpPlot(fit.rf.2)

### What is the OOB error?
OOB.pred.2 = predict(fit.rf.2)
(OOB.MSPE.2 = get.MSPE(data$alcohol, OOB.pred.2))

### How about the SMSE
sample.pred.2 = predict(fit.rf.2, data)
(SMSE.2 = get.MSPE(data$alcohol, sample.pred.2))
