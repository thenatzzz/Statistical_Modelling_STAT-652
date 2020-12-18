### In this tutorial, we will use neural networks on the wine quality
### dataset to predict quality. 
### The code is very similar to when we used NNets for regression in 
### Lecture 12b. See the material from that lecture for more details.

source("Helper Functions.R")

### Read-in and process the data
source("Read Wine Data - Class.R")

### Activate packages
library(nnet)        # For neural networks

### Set random seed, using Mersenne-Twister for compatibility.
### Note: We use the same seed here so that we get the same data split
### as the previous lecture and we can compare models' performance.
set.seed(46536737, kind = "Mersenne-Twister")

#######################
### Neural Networks ###
#######################


### Split the data into training and validation sets
p.train = 0.75
n = nrow(data)
n.train = floor(p.train*n)

ind.random = sample(1:n)
data.train = data[ind.random <= n.train,]
X.train.raw = data.train[,-1]
Y.train = data.train[,1]
data.valid = data[ind.random > n.train,]
X.valid.raw = data.valid[,-1]
Y.valid = data.valid[,1]

### Rescale columns of X to fall between 0 and 1 using Tom's function
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}
X.train = rescale(X.train.raw, X.train.raw)
X.valid = rescale(X.valid.raw, X.train.raw)

### Make sure that we rescaled correctly
summary(X.train)
summary(X.valid)

### Convert Y to a factor and the corresponding indicator. The nnet() function
### needs a separate indicator for each class. We can get these indicators
### using the class.ind() function after converting our response to a factor
Y.train.fact = factor(Y.train, levels = c("low", "med", "high"))
Y.train.num = class.ind(Y.train.fact)
Y.valid.fact = factor(Y.valid, levels = c("low", "med", "high"))
Y.valid.num = class.ind(Y.valid.fact)

### Check that our conversion worked
head(Y.train.fact)
head(Y.train.num)
head(Y.valid.fact)
head(Y.valid.num)


### Now we can fit a neural network model. We do this using the nnet()
### function from the nnet package. We still use size and decay to set
### the number of hidden nodes and the shrinkage respectively. maxit still
### controls the maximum number of iterations (this just needs to be large
### enough that the function says it has converged). Instead of setting 
### linout, we now need to set softmax to TRUE to get classification labels.
### To demonstrate, let's fit a neural net with arbitrarily chosen tuning
### parameters (please don't do this in practice; it is a terrible idea).
fit.nnet.0 = nnet(X.train, Y.train.num, size = 1, decay = 0, maxit = 20000, 
  softmax = T)

### Remember that the nnet function can get stuck in local minima, so
### we need to re-run the function a few times with the same parameter
### values and choose the model with the lowest sMSE. Let's use Tom's method
### to do this. 
MSE.best = Inf    ### Initialize sMSE to largest possible value (infinity)
M = 20            ### Number of times to refit.

for(i in 1:M){
  ### For convenience, we stop nnet() from printing information about
  ### the fitting process by setting trace = F.
  this.nnet = nnet(X.train, Y.train.num, size = 1, decay = 0, maxit = 2000, 
    softmax = T, trace = F)
  this.MSE = this.nnet$value
  if(this.MSE < MSE.best){
    NNet.best.0 = this.nnet
    MSE.best = this.MSE
  }
}

### Now we can evaluate the validation-set performance of our naive neural
### network. We can get the predicted class labels using the predict()
### function and setting type to "class"
pred.nnet.0 = predict(NNet.best.0, X.valid, type = "class")

table(Y.valid, pred.nnet.0, dnn = c("Obs", "Pred"))

(mis.nnet.0 = mean(Y.valid != pred.nnet.0))


### This misclassification rate isn't terrible, but we know that we can 
### do better if we tune NNets. Let's do repeated CV tuning. We will do 3
### runs of 10-fold CV

set.seed(30386417, kind = "Mersenne-Twister")

### Candidate parameter values
all.sizes = c(1, 3, 6, 10)
all.decays = c(0, 0.001, 0.01, 0.1, 1)
all.pars = expand.grid(size = all.sizes, decay = all.decays)
n.pars = nrow(all.pars)
par.names = apply(all.pars, 1, paste, collapse = "-")

H = 5   # Number of times to repeat CV
K = 5   # Number of folds for CV
M = 20  # Number of times to re-run nnet() at each iteration

### Container for CV misclassification rates. Need to include room for
### H*K CV errors
CV.misclass = array(0, dim = c(H*K, n.pars))
colnames(CV.misclass) = par.names

for(h in 1:H) {
  ### Get all CV folds
  folds = get.folds(n.train, K)
  
  for (i in 1:K) {
    print(paste0(h, "-", i, " of ", H, "-", K))
    
    ### Split training set according to fold i
    data.train.inner = data.train[folds != i, ]
    data.valid.inner = data.train[folds == i, ]
    
    ### Separate response from predictors
    Y.train.inner = data.train.inner[, 1]
    X.train.inner.raw = data.train.inner[, -1]
    Y.valid.inner = data.valid.inner[, 1]
    X.valid.inner.raw = data.valid.inner[, -1]
    
    ### Transform predictors and response for nnet()
    X.train.inner = rescale(X.train.inner.raw, X.train.inner.raw)
    X.valid.inner = rescale(X.valid.inner.raw, X.train.inner.raw)
    Y.train.inner.num = class.ind(factor(Y.train.inner))
    Y.valid.inner.num = class.ind(factor(Y.valid.inner))
    
    for (j in 1:n.pars) {
      ### Get parameter values
      this.size = all.pars[j, "size"]
      this.decay = all.pars[j, "decay"]
      
      ### Get ready to re-fit NNet with current parameter values
      MSE.best = Inf
      
      ### Re-run nnet() M times and keep the one with best sMSE
      for (l in 1:M) {
        this.nnet = nnet(
          X.train.inner,
          Y.train.inner.num,
          size = this.size,
          decay = this.decay,
          maxit = 10000,
          softmax = T,
          trace = F
        )
        this.MSE = this.nnet$value
        if (this.MSE < MSE.best) {
          nnet.best = this.nnet
          MSE.best = this.MSE
        }
      }
      
      ### Get CV misclassification rate for chosen nnet()
      pred.nnet.best = predict(nnet.best, X.valid.inner, type = "class")
      this.mis.CV = mean(Y.valid.inner != pred.nnet.best)
      
      ### Store this CV error. Be sure to put it in the correct row
      ind.row = (h - 1) * K + i
      CV.misclass[ind.row, j] = this.mis.CV
    }
  }
}

### Make absolute and relative boxplots
boxplot(CV.misclass, las = 2)
rel.CV.misclass = apply(CV.misclass, 1, function(W) W/min(W))
boxplot(t(rel.CV.misclass), las=2)



### None of these values is obviously best. Let's do another round of
### tuning; focusing in on the best-looking region

set.seed(32147863, kind = "Mersenne-Twister")

### Candidate parameter values
all.sizes2 = c(1, 3, 5, 7, 9, 11)
all.decays2 = c(0.01, 0.05, 0.1, 0.5, 1)
all.pars2 = expand.grid(size = all.sizes2, decay = all.decays2)
n.pars2 = nrow(all.pars2)
par.names2 = apply(all.pars2, 1, paste, collapse = "-")

### Container for CV misclassification rates. Need to include room for
### H*K CV errors
CV.misclass2 = array(0, dim = c(H*K, n.pars2))
colnames(CV.misclass2) = par.names2

for(h in 1:H) {
  ### Get all CV folds
  folds = get.folds(n.train, K)
  
  for (i in 1:K) {
    print(paste0(h, "-", i, " of ", H, "-", K))
    
    ### Split training set according to fold i
    data.train.inner = data.train[folds != i, ]
    data.valid.inner = data.train[folds == i, ]
    
    ### Separate response from predictors
    Y.train.inner = data.train.inner[, 1]
    X.train.inner.raw = data.train.inner[, -1]
    Y.valid.inner = data.valid.inner[, 1]
    X.valid.inner.raw = data.valid.inner[, -1]
    
    ### Transform predictors and response for nnet()
    X.train.inner = rescale(X.train.inner.raw, X.train.inner.raw)
    X.valid.inner = rescale(X.valid.inner.raw, X.train.inner.raw)
    Y.train.inner.num = class.ind(factor(Y.train.inner))
    Y.valid.inner.num = class.ind(factor(Y.valid.inner))
    
    for (j in 1:n.pars2) {
      ### Get parameter values
      this.size = all.pars2[j, "size"]
      this.decay = all.pars2[j, "decay"]
      
      ### Get ready to re-fit NNet with current parameter values
      MSE.best = Inf
      
      ### Re-run nnet() M times and keep the one with best sMSE
      for (l in 1:M) {
        this.nnet = nnet(
          X.train.inner,
          Y.train.inner.num,
          size = this.size,
          decay = this.decay,
          maxit = 10000,
          softmax = T,
          trace = F
        )
        this.MSE = this.nnet$value
        if (this.MSE < MSE.best) {
          nnet.best = this.nnet
          MSE.best = this.MSE
        }
      }
      
      ### Get CV misclassification rate for chosen nnet()
      pred.nnet.best = predict(nnet.best, X.valid.inner, type = "class")
      this.mis.CV = mean(Y.valid.inner != pred.nnet.best)
      
      ### Store this CV error. Be sure to put it in the correct row
      ind.row = (h - 1) * K + i
      CV.misclass2[ind.row, j] = this.mis.CV
    }
  }
}

### Make absolute and relative boxplots
boxplot(CV.misclass2, las = 2)
rel.CV.misclass2 = apply(CV.misclass2, 1, function(W) W/min(W))
boxplot(t(rel.CV.misclass2), las=2)
boxplot(t(rel.CV.misclass2), las=2, ylim = c(1, 1.5))

### When using CV to do model comparison, we can't look at a boxplot for
### every CV fold. Let's use the parameter combination with smallest
### mean misclassification rate.
ave.mis.nnet = apply(CV.misclass2, 2, mean)
best.ind.nnet = which.min(ave.mis.nnet)
(best.pars.nnet = all.pars2[best.ind.nnet,])

### I think that my favorite parameter combination is 3 hidden nodes with
### 0.1 shrinkage. Let's fit this NNet model to the entire training set. 
### Remember to re-run nnet() (since we're just fitting one model, we can
### afford more re-runs)
MSE.best = Inf
for(i in 1:(10*M)){
  fit.nnet = nnet(X.train, Y.train.num, size = 3, decay = 0.05, 
    maxit = 1000, softmax = T, trace = F)
  this.MSE = fit.nnet$value
  if(this.MSE < MSE.best){
    nnet.best = fit.nnet
    MSE.best = this.MSE
  }
}

### Evaluate performance on the validation set
pred.nnet = predict(nnet.best, X.valid, type = "class")
table(Y.valid, pred.nnet, dnn = c("Obs", "Pred"))
(mis.nnet = mean(Y.valid != pred.nnet))


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
### Naive NNet              0.452
### Tuned NNet              0.419