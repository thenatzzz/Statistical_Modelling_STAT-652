### In this tutorial, we will use tree based classifiers on the wine quality
### dataset to predict quality. Specifically, we will look at classification
### trees, random forests and boosting. The function for boosting can only
### do binary classification.

source("Helper Functions.R")

### Read-in and process the data
source("Read Wine Data - Class.R")

### Activate packages
library(rpart)        # For fitting classification trees
library(rpart.plot)   # For plotting classification trees
library(randomForest) # For random forests
library(gbm)          # For boosting

### Set random seed, using Mersenne-Twister for compatibility.
### Note: We use the same seed here so that we get the same data split
### as the previous lecture and we can compare models' performance.
set.seed(46536737, kind = "Mersenne-Twister")

############################
### Classification Trees ###
############################

### Split the data into training and validation sets
p.train = 0.75
n = nrow(data)
n.train = floor(p.train*n)

ind.random = sample(1:n)
data.train = data[ind.random <= n.train,]
data.valid = data[ind.random > n.train,]
Y.valid = data.valid[,1]


### Fitting classification trees is mostly the same as fitting regression
### trees. We use the rpart() function from the rpart package. This function
### uses model formula/data frame syntax. We can set the CP using the cp 
### input; usually this is set to 0. When doing classification, we have to
### set method="class".
fit.tree.full = rpart(quality ~ ., data = data.train, method = "class", 
  cp = 0)

### We can plot our tree using the prp() function from the rpart.plot package.
### As with regression trees, the prp() function has many specialized inputs, 
### but we will just set type=1 and extra=1. This is a plotting function,
### so you can also set things like main, xlab, ylab, etc.
prp(fit.tree.full, type = 1, extra = 1, main = "Full Tree")

### We can tune our tree using information from the CP table.
info.tree = fit.tree.full$cptable

### The actual process of tuning is the same. We can use Tom's code to do this

# Find location of minimum error
minrow <- which.min(info.tree[,4])
# Take geometric mean of cp values at min error and one step up 
cplow.min <- info.tree[minrow,1]
cpup.min <- ifelse(minrow==1, yes=1, no=info.tree[minrow-1,1])
cp.min <- sqrt(cplow.min*cpup.min)

# Find smallest row where error is below +1SE
se.row <- min(which(info.tree[,4] < info.tree[minrow,4]+info.tree[minrow,5]))
# Take geometric mean of cp values at min error and one step up 
cplow.1se <- info.tree[se.row,1]
cpup.1se <- ifelse(se.row==1, yes=1, no=info.tree[se.row-1,1])
cp.1se <- sqrt(cplow.1se*cpup.1se)

# Creating a pruned tree using the CV-min rule.
fit.tree.min <- prune(fit.tree.full, cp=cp.min)
# Creating a pruned tree using the CV-1se rule.
fit.tree.1se <- prune(fit.tree.full, cp=cp.1se)

### Now that we have our pruned trees, let's plot them.
prp(fit.tree.min, type = 1, extra = 1, main = "CV-min Tree")
prp(fit.tree.1se, type = 1, extra = 1, main = "CV-1se Tree")


### Finally, let's get predictions and evaluate performance for each tree.
### We have to specify what format we want predictions to be presented in.
### Setting type="class" gives us class labels, setting type="vector" gives
### us numeric class labels, and type="prob" gives us predicted class 
### probabilities.
pred.tree.full = predict(fit.tree.full, data.valid, type = "class")
table(Y.valid, pred.tree.full, dnn = c("Obs", "Pred"))
(mis.tree.full = mean(Y.valid != pred.tree.full))

pred.tree.min = predict(fit.tree.min, data.valid, type = "class")
table(Y.valid, pred.tree.min, dnn = c("Obs", "Pred"))
(mis.tree.min = mean(Y.valid != pred.tree.min))

pred.tree.1se = predict(fit.tree.1se, data.valid, type = "class")
table(Y.valid, pred.tree.1se, dnn = c("Obs", "Pred"))
(mis.tree.1se = mean(Y.valid != pred.tree.1se))


#####################
### Random Forest ###
#####################

### The function that fits random forests requires that our response
### variable be a factor. We need to make a copy of our dataset and
### use the factor() function on quality.
data.rf = data
data.rf$quality = factor(data.rf$quality)

### Split the data into training and validation sets
data.train.rf = data.rf[ind.random <= n.train,]
data.valid.rf = data.rf[ind.random > n.train,]
Y.train.rf = data.train.rf$quality
Y.valid.rf = data.valid.rf$quality



### Random forests for classification are mostly the same as when we use 
### them for regression. See Lecture 15 for more information.
### Remember that we have to use tuning if we want to get the
###  most out of this model. Conveniently, we can use out-of-bag
### error to tune, so we don't need to write a full CV loop.
### Tuning parameters are mtry and nodesize.

### Set tuning parameters
all.mtrys = 1:6
all.nodesizes = c(1, 5, 10, 15, 20)
all.pars.rf = expand.grid(mtry = all.mtrys, nodesize = all.nodesizes)
n.pars = nrow(all.pars.rf)

M = 5 # Number of times to repeat RF fitting. I.e. Number of OOB errors

### Container to store OOB errors. This will be easier to read if we name
### the columns.
# all.OOB.rf = array(0, dim = c(M, n.pars))
names.pars = apply(all.pars.rf, 1, paste0, collapse = "-")
colnames(all.OOB.rf) = names.pars


for(i in 1:n.pars){
  ### Progress update
  print(paste0(i, " of ", n.pars))
  
  ### Get tuning parameters for this iteration
  this.mtry = all.pars.rf[i, "mtry"]
  this.nodesize = all.pars.rf[i, "nodesize"]
  
  for(j in 1:M){
    ### Fit RF, then get and store OOB errors
    this.fit.rf = randomForest(quality ~ ., data = data.train.rf,
      mtry = this.mtry, nodesize = this.nodesize)
    
    pred.this.rf = predict(this.fit.rf)
    this.err.rf = mean(Y.train.rf != pred.this.rf)
    
    all.OOB.rf[j, i] = this.err.rf
  }
}

### Make a regular and relative boxplot
boxplot(all.OOB.rf, las=2, main = "OOB Boxplot")
rel.OOB.rf = apply(all.OOB.rf, 1, function(W) W/min(W))
boxplot(t(rel.OOB.rf), las=2,  # las sets the axis label orientation
  main = "Relative OOB Boxplot") 

### Best model looks like mtry = 1 and nodesize = 15.
fit.rf = randomForest(quality ~ ., data = data.train.rf,
  mtry = 1, nodesize = 15)

### Get predictions and evaluate performance
pred.rf = predict(fit.rf, data.valid.rf)

table(Y.valid, pred.rf, dnn = c("Obs", "Pred"))
(mis.rf = mean(Y.valid != pred.rf))


################
### Boosting ###
################

### Boosting in R is done using the gbm() function from the gbm package.
### Unfortunately, this function is currently broken and unable to do
### multinomial classification. It is however, still able to do binary
### classification. We will combine medium and high quality into a single
### category, then try to classify low vs medium/high.
data.boost = data
data.boost$quality = ifelse(data.boost$quality == "low", "low", "med-high")
head(data.boost)
table(data.boost$quality)

### We also need to convert our response to have values 0 or 1
### (0 for low, 1 for medium/high)
qual.boost.fact = factor(data.boost$quality, levels = c("low", "med-high"))
qual.boost.num = as.numeric(qual.boost.fact) - 1
data.boost$quality = qual.boost.num

### Now we can split the data into training and validation sets.
data.train.boost = data.boost[ind.random <= n.train,]
data.valid.boost = data.boost[ind.random > n.train,]
Y.valid.boost = data.valid.boost$quality

### Boosting requires careful tuning. To do this properly, we need to use
### CV. We go through this process thoroughly in Lecture 16. Here I will omit
### some details. To do classification with gbm(), we need to set
### distribution="bernoulli".

### Set parameter values
### We will stick to resampling rate of 0.8, maximum of 10000 trees, and Tom's rule
### for choosing how many trees to keep.
max.trees = 10000
all.shrink = c(0.001, 0.01, 0.1)
all.depth = c(1, 2, 3)
all.pars.boost = expand.grid(shrink = all.shrink, depth = all.depth)
n.pars = nrow(all.pars.boost)

### Number of folds
K = 5

### Get folds
n = nrow(data.train.boost)
folds = get.folds(n, K)

### Create container for CV MSPEs
# CV.MSPEs = array(0, dim = c(K, n.pars))
names.pars = apply(all.pars.boost, 1, paste0, collapse = "-")
colnames(CV.MSPEs) = names.pars


for(i in 1:K){
  ### Print progress update
  print(paste0(i, " of ", K))
  
  ### Split data
  data.train.inner = data.train.boost[folds != i,]
  data.valid.inner = data.train.boost[folds == i,]
  Y.valid.inner = data.valid.inner$quality
  
  ### Fit boosting models for each parameter combination
  for(j in 1:n.pars){
    ### Get current parameter values
    this.shrink = all.pars.boost[j,"shrink"]
    this.depth = all.pars.boost[j,"depth"]
    
    ### Fit model using current parameter values.
    fit.gbm = gbm(quality ~ ., data = data.train.inner, 
      distribution = "bernoulli", n.trees = max.trees,
       interaction.depth = this.depth, shrinkage = this.shrink, 
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
    
    ### Get predictions and misclassification rate. We can get predicted
    ### probability of class 1 by setting type="response". We then need to 
    ### round our class probabilities to get class labels.
    pred.gbm.prob = predict(fit.gbm, data.valid.inner, n.trees, 
      type = "response")
    pred.gbm = round(pred.gbm.prob, 0)
    mis.gbm = mean(Y.valid.inner != pred.gbm)
    

    CV.MSPEs[i, j] = mis.gbm
  }
}

### We can now make absolute and relative error rate boxplots.
boxplot(CV.MSPEs, las = 2, main = "MSPE Boxplot")
CV.RMSPEs = apply(CV.MSPEs, 1, function(W) W/min(W))
boxplot(t(CV.RMSPEs), las = 2, main = "RMSPE Boxplot")
boxplot(t(CV.RMSPEs), las = 2, main = "RMSPE Boxplot", ylim = c(1, 1.2))

### The best model looks like the one with interaction.depth=2 and 
### shrinkage=0.01. Fit this model, then evaluate its performance.
fit.gbm = gbm(quality ~ ., data = data.train.boost, 
  distribution = "bernoulli", 
  n.trees = max.trees, interaction.depth = 2, shrinkage = 0.01, 
  bag.fraction = 0.8)
n.trees = gbm.perf(fit.gbm, plot.it = F) * 2
if(n.trees > max.trees){
  extra.trees = n.trees - max.trees
  fit.gbm = gbm.more(fit.gbm, extra.trees)
}
pred.gbm.prob = predict(fit.gbm, data.valid.boost, n.trees, 
  type = "response")
pred.gbm = round(pred.gbm.prob, 0)

table(Y.valid.boost, pred.gbm, dnn = c("Obs", "Pred"))
(mis.rf = mean(Y.valid.boost != pred.gbm))

### For reference, let's also fit an untuned RF to the binary response data
data.bin.rf = data.boost
data.bin.rf$quality = factor(data.bin.rf$quality)

data.train.bin.rf = data.bin.rf[ind.random <= n.train,]
data.valid.bin.rf = data.bin.rf[ind.random > n.train,]
Y.valid.bin.rf = data.valid.bin.rf$quality

fit.rf.bin = randomForest(quality ~ ., data = data.train.bin.rf)
pred.rf.bin = predict(fit.rf.bin, data.valid.bin.rf)
(mis.rf.bin = mean(Y.valid.bin.rf != pred.rf.bin))



### Validation-set misclassification rates
### KNN, K=3:               0.508
### KNN, CV-min:            0.460
### KNN, CV-1se:            0.427
### Logistic Regression:    0.387
### Logistic LASSO, CV-min: 0.387
### Logistic LASSO, CV-1se: 0.476
### LDA:                    0.379
### QDA:                    0.444
### GAM:                    0.363
### Naive Bayes:            0.524
### Naive Bayes with PCA:   0.492
### Full Tree:              0.508
### Tree, CV-min:           0.419
### Tree, CV-1se:           0.435
### Random Forest:          0.444