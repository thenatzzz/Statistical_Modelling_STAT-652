### In this tutorial, we will use non-linear classifiers on the wine quality
### dataset to predict quality. Specifically, we will look at GAM and Naive
### Bayes.

### Read-in and process the data
source("Read Wine Data - Class.R")

### Activate packages
library(mgcv)   # For GAM
library(klaR)   # For naive Bayes

### Set random seed, using Mersenne-Twister for compatibility.
set.seed(46536737, kind = "Mersenne-Twister")

###########
### GAM ###
###########

### To fit GAM models, we need to represent the different classes numerically,
### with class labels starting at 0. We can do this using the factor()
### function. This function converts a list of character strings to a factor
### object, which we can then convert to numbered labels. When using the 
### factor function, it is convenient to set the order of the levels, so that
### you know what each number will represent.
### We convert from a factor object to its numeric labels using as.numeric(). 
### These numeric labels start at 1. To get labels starting at 0, we can just 
### subtract 1.
quality.fact.raw = factor(data$quality, levels = c("low", "med", "high"))
head(quality.fact.raw)
quality.fact.1 = as.numeric(quality.fact.raw)
head(quality.fact.1)
quality.fact.0 = quality.fact.1 - 1

### Let's create a copy of our dataset with this numeric representation of
### quality. 
data.num = data
data.num$quality = quality.fact.0
head(data.num)

### Split the data into training and validation sets
p.train = 0.75
n = nrow(data.num)
n.train = floor(p.train*n)

ind.random = sample(1:n)
data.train.num = data.num[ind.random <= n.train,]
data.valid.num = data.num[ind.random > n.train,]
Y.valid.num = data.valid.num[,1]


### Now we can fit a GAM model to the training set to predict quality. We
### do this using the gam() function from the mgcv package.
### The syntax for fitting GAM to do multinomial classification is weird.
### See the tutorial video for a more detailed discussion.
### Note: we use the multinom() function to tell gam() that we are doing
### multinomial classification. There is also a function in the nnet
### package called multinom(). If you have both packages loaded, you will
### need to specify which version of the function you want using :: (e.g.
### mgcv::multinom() for inside gam()).
fit.gam = gam(data = data.train.num, family = multinom(K=2),
  list(quality ~ s(sugar) + s(density) + s(pH) + s(sulphates) + 
      s(chlor) + s(alcohol),
               ~ s(sugar) + s(density) + s(pH) + s(sulphates) + 
      s(chlor) + s(alcohol))
)

### We can get predictions using the predict() function with type="response",
### but this gives us predicted probabilities. To get predicted class labels,
### we need to manually find which level has the highest probability in each
### row using apply() and which.max()
pred.gam.prob = predict(fit.gam, data.valid.num, type = "response")
head(pred.gam.prob)

pred.gam.1 = apply(pred.gam.prob, 1, which.max)
head(pred.gam.1)

pred.gam = pred.gam.1 - 1


### Finally, let's get the confusion matrix and misclassification rate for GAM
table(Y.valid.num, pred.gam, dnn = c("Obs", "Pred"))

(mis.gam = mean(pred.gam != Y.valid.num))


###################
### Naive Bayes ###
###################

### To do naive Bayes in R, we need the response variable to be a factor 
### object (unlike GAM). Let's re-split the data using the same indices we 
### computed above and convert our response to a factor. I like to also 
### explicitly assign an order to facors using the levels input.
data.train = data[ind.random <= n.train,]
data.valid = data[ind.random > n.train,]
X.train = data.train[,-1]
X.valid = data.valid[,-1]
Y.train = factor(data.train[,1], levels = c("low", "med", "high"))
Y.valid = factor(data.valid[,1], levels = c("low", "med", "high"))

### We fit naive Bayes models using the NaiveBayes() function from the klaR
### package. This function uses predictor matrix/response vector syntax. You
### can also specify if you want to use kernel density estimation by setting
### usekernel=TRUE. Setting this to false uses the normal distribution for
### each predictor.
fit.NB = NaiveBayes(X.train, Y.train, usekernel = T)

### We can plot the kernel density estimates. This gives us a separate plot
### for each predictor, so 6 total plots. Best to use par() to get all
### these plots simultaneously. 
par(mfrow = c(2,3)) # Set plotting to 2x3
plot(fit.NB)
1 # Dummy line because the plot function for NaiveBayes models takes the
  # next line of code as an extra input. It's weird. Just keep it here.
par(mfrow = c(1,1)) # Reset plotting to 1x1 


### Next, let's get predictions, their corresponding confusion matrix and
### the misclassification rate. Predictions from NaiveBayes models give
### predicted class labels and probabilities, so we have to extract the
### class labels using $class
pred.NB.raw = predict(fit.NB, X.valid)
pred.NB = pred.NB.raw$class

table(Y.valid, pred.NB, dnn = c("Obs", "Pred"))
(mis.NB = mean(Y.valid != pred.NB))



### It can be helpful with naive Bayes to first do a principal components
### analysis (see Lecture 7). We will do PCA on the training set, then
### apply the same transformation to the validation set using the predict()
### function. Remember to scale the predictors by setting scale. to true.
fit.PCA = prcomp(X.train, scale. = T)

X.train.PC = fit.PCA$x  # Extract the PCs
X.valid.PC = predict(fit.PCA, data.valid)


### Now we can use the NaiveBayes() function in exactly the same way as
### above.
fit.NB.PC = NaiveBayes(X.train.PC, Y.train, usekernel = T)

### Plot the densities
par(mfrow = c(2,3)) # Set plotting to 2x3
plot(fit.NB.PC)
1 # Dummy line because the plot function for NaiveBayes models takes the
# next line of code as an extra input. It's weird. Just keep it here.
par(mfrow = c(1,1)) # Reset plotting to 1x1 

### Get predictions and measure their performance
pred.NB.PC.raw = predict(fit.NB.PC, X.valid.PC)
pred.NB.PC = pred.NB.PC.raw$class

table(Y.valid, pred.NB.PC, dnn = c("Obs", "Pred"))
(mis.NB.PC = mean(Y.valid != pred.NB.PC))


### Validation-set misclassification rates
### KNN, K=3:               0.508
### KNN, CV-min:            0.460
### KNN, CV-1se:            0.427
### Logistic Regression:    0.387
### Logistic LASSO, CV.min: 0.387
### Logistic LASSO, CV.1se: 0.476
### LDA:                    0.379
### QDA:                    0.444
### GAM:                    0.363
### Naive Bayes:            0.524
### Naive Bayes with PCA:   0.492