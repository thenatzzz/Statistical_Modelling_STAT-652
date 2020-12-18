### In this tutorial, we will use linear classifiers on the wine quality
### dataset to predict quality. Specifically, we will look at logistic
### regression and discriminant analysis.

### Read-in and process the data
source("Read Wine Data - Class.R")

### Activate packages
library(nnet)   # For logistic regression
library(car)    # For ANOVA after logistic regression with nnet
library(glmnet) # For logistic regression and LASSO
library(MASS)   # For discriminant analysis

### Set random seed, using Mersenne-Twister for compatibility.
set.seed(46536737, kind = "Mersenne-Twister")


### Split the data into training and validation sets
p.train = 0.75
n = nrow(data)
n.train = floor(p.train*n)

ind.random = sample(1:n)
data.train = data[ind.random <= n.train,]
data.valid = data[ind.random > n.train,]
Y.valid = data.valid[,1]

### There are multiple packages in R which fit logistic regression (LR). We 
### will look at nnet and glmnet (both are a bit different). First, LR using
### the multinom() function from the nnet package.

### Remember that the nnet package requires that we rescale our predictors
### to all fall between 0 and 1. We can use Tom's function for this.

### Rescale the columns of x1 so that the columns of x2 fall between 0 and 1
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

### Create copies of our datasets and rescale
data.train.scale = data.train
data.valid.scale = data.valid
data.train.scale[,-1] = rescale(data.train.scale[,-1], data.train[,-1])
data.valid.scale[,-1] = rescale(data.valid.scale[,-1], data.train[,-1])

### Fit a logistic regression model using the multinom() function from the
### nnet package. This function uses formula/data frame syntax. We can 
### optionally specify the maximum number of iterations multinom is
### allowed to use to fit the model using the maxit input. The defult is 100,
### and this is usually adequate.
fit.log.nnet = multinom(quality ~ ., data = data.train.scale, maxit = 200)

### We can get information about our fitted model using the summary()
### function.
summary(fit.log.nnet)

### We can do significance tests for the coefficients in our model using
### the Anova() function from the car package.
### Note: The Anova() function reports Type II hypothesis tests. These are
### tests for a variable's effect in a model already containing the
### other predictors. Type I tests are sequential (i.e. first vs nothing,
### first and second vs only first, etc.).
Anova(fit.log.nnet)

### Let's plot the two most significant predictors, using color to 
### distinguish quality. To get the color for each level of quality,
### we will use the ifelse() function. See this tutorial's video for
### details of the syntax.
col.qual = ifelse(data.train.scale[,1] == "low", "Red", 
           ifelse(data.train.scale[,1] == "med", "Blue", "Green"))

plot(data.train$sulphates, data.train$alcohol, col = col.qual,
  xlab = "Sulphates", ylab = "Alcohol", 
  main = "Wine Quality by Alcohol and Sulphates")
legend(x = "topright", legend = c("Low", "Medium", "High"),
  col = c("Red", "Blue", "Green"), pch = 1)


### Next, let's investigate the LR's performance on the test set
pred.log.nnet = predict(fit.log.nnet, data.valid.scale)

table(Y.valid, pred.log.nnet,        ### Confusion matrix
  dnn = c("Observed", "Predicted"))

(misclass.log.nnet = mean(pred.log.nnet != Y.valid)) ### Misclass rate



### Let's repeat our logistic regression analysis using the glmnet package.
### While we're at it, we can also fit a LASSO logistic regression model.
### We will use the glmnet() and cv.glmnet() functions respectively.

### The glmnet() function uses predictor matrix/response vector syntax,
### so we need to extract these from our training and validation sets.
### We also have to convert the predictors to a matrix using the
### as.matrix() function.
X.train.scale = as.matrix(data.train.scale[,-1])
Y.train = data.train.scale[,1]
X.valid.scale = as.matrix(data.valid.scale[,-1])
Y.valid = data.valid.scale[,1]

### The glmnet() function in the glmnet() package can be used to fit
### logistic regression models. The syntax is the same as for regression, 
### but now we have to set family="multinomial". This approach actually
### fits a LASSO version, but we can get ordinary logistic regression by 
### setting s=0 when doing prediction (they use s instead of lambda when
### doing prediction...sometimes R is weird).
### Note: The code will allow you to specify lambda when fitting your model.
### DON'T DO THIS. Counterintuitively, it is actually faster and more
### stable to fit models for a whole sequence of lambda values than to
### fit a single LASSO model.
fit.log.glmnet = glmnet(X.train.scale, Y.train, family = "multinomial")

### Get predictions and investigate performance. The predict() function
### for glmnet() can give several different types of output. To get
### predicted values, set type="class". Remember to set s=0 for logistic
### regression.
pred.log.glmnet = predict(fit.log.glmnet, X.valid.scale, type = "class",
  s = 0)

table(Y.valid, pred.log.glmnet, dnn = c("Observed", "Predicted"))

(misclass.log.glmnet = mean(Y.valid != pred.log.glmnet))
  

### While we're looking at the glmnet package, let's do LASSO. We need to
### choose lambda using CV. Fortunately, the cv.glmnet() function does this
### for us. The syntax for cv.glmnet() is the same as for glmnet().
fit.CV.lasso = cv.glmnet(X.train.scale, Y.train, family = "multinomial")

### Plotting the cv.glmnet() output shows us how CV misclassification
### rate changes with lambda (actually, it plots the multinomial deviance,
### which is a transformation of the misclassification rate).
plot(fit.CV.lasso)

### The CV-min and CV-1se lambda values are stored in the output from 
### cv.glmnet()
lambda.min = fit.CV.lasso$lambda.min
lambda.1se = fit.CV.lasso$lambda.1se

### Let's check which predictors are included in each "best" model. We
### can get the coefficients using the coef() function, setting s to
### the appropriate lambda value.
coef(fit.CV.lasso, s = lambda.min)
coef(fit.CV.lasso, s = lambda.1se)


### Now we can get predictions for both "best" models
pred.lasso.min = predict(fit.CV.lasso, X.valid.scale, s = lambda.min,
  type = "class")
pred.lasso.1se = predict(fit.CV.lasso, X.valid.scale, s = lambda.1se,
  type = "class")

table(Y.valid, pred.lasso.min, dnn = c("Obs", "Pred"))
table(Y.valid, pred.lasso.1se, dnn = c("Obs", "Pred"))

(miss.lasso.min = mean(Y.valid != pred.lasso.min))
(miss.lasso.1se = mean(Y.valid != pred.lasso.1se))



### We have now covered linear regression based classifiers. Let's
### move on to discriminant analysis. We fit LDA and QDA using the 
### lda() and qda() functions in the MASS package.

### For discriminant analysis, it's best to scale predictors
### to have mean 0 and SD 1 (this makes the results easier to 
### interpret). We can do this using using the following function.

### Rescale x1 using the means and SDs of x2
scale.1 <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- mean(x2[,col])
    b <- sd(x2[,col])
    x1[,col] <- (x1[,col]-a)/b
  }
  x1
}


X.train.DA = scale.1(data.train[,-1], data.train[,-1])
X.valid.DA = scale.1(data.valid[,-1], data.train[,-1])

### Fit an LDA model using the lda() funtion from the MASS package. This
### function uses predictor/response syntax.
fit.lda = lda(X.train.DA, Y.train)


### We can plot the data using the linear discriminants. It's best to
### include colors. Let's just recycle the colors from above.
### There is no simple way to change the axis labels. Sometimes we just need
### to live with the defaults.
plot(fit.lda, col = col.qual)

### We can also get histograms of the data along the main linear 
### discriminant by setting type="histogram" and setting dimen=1.
plot(fit.lda, type = "histogram", dimen = 1)

### We can also add smoothed density curves to these histograms by setting
### type="both".
plot(fit.lda, type = "both", dimen = 1)

### We get predictions by extracting the class object from the predict()
### function's output.
pred.lda = predict(fit.lda, X.valid.DA)$class

table(Y.valid, pred.lda, dnn = c("Obs", "Pred"))

(miss.lda = mean(Y.valid != pred.lda))


### Finally, QDA works much the same way as LDA.

fit.qda = qda(X.train.DA, Y.train)

pred.qda = predict(fit.qda, X.valid.DA)$class

table(Y.valid, pred.qda, dnn = c("Obs", "Pred"))
(miss.qda = mean(Y.valid != pred.qda))
