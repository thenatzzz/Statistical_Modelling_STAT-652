### In this tutorial, we are starting classification. We will still use
### the wine data, but now we will be predicting quality (specifically,
### we group quality values into high, medium and low). For now, we will
### look at nearest-neighbours.

library(FNN)

### Read-in and process the data
source("Read Wine Data - Class.R")
head(data)
summary(data)
table(data$quality)

### Some of our code will be random, so we have to set the seed. 
### Use Mersenne-Twister for compatibility.
set.seed(46536737, kind = "Mersenne-Twister")


### Split the data into training and validation sets
p.train = 0.75
n = nrow(data)
n.train = floor(p.train*n)

ind.random = sample(1:n)
data.train = data[ind.random <= n.train,]
data.valid = data[ind.random > n.train,]

### Split up the predictor variables from the class labels.
X.train.raw = data.train[,-1]
X.valid.raw = data.valid[,-1]
Y.train = data.train[,1]
Y.valid = data.valid[,1]


### KNN is based on distances. If variables are measured on different
### scales, we can change which points are neighbours by measuring
### in different units.
### Tom has written a function we can use to rescale the columns of
### a data frame to have mean 0 and SD 1. We can also use it to rescale
### a data frame based on the means and SDs of another (this is useful
### for scaling the validation set to match the training set).

### Rescale x1 using the means and SDs of x2
scale.1 <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- mean(x2[,col])
    b <- sd(x2[,col])
    x1[,col] <- (x1[,col]-a)/b
  }
  x1
}

### Rescale our training and validation sets
X.train = scale.1(X.train.raw, X.train.raw)
X.valid = scale.1(X.valid.raw, X.train.raw) # Watch the order


### Now we can fit a KNN model using the knn function in the FNN 
### package. The syntax of the knn function is a bit different from 
### what we're used to. The first two inputs are the training and 
### validation predictor matrices. The third input is the class labels
### for the training set. We can also set k to the number of 
### neighbours we want. The function then outputs predicted class 
### labels for the validation set. Let's use 3 neighbours.
pred.knn = knn(X.train, X.valid, Y.train, k=3)

### Let's make a confusion matrix. We get this using the table()
### function and providing both the predicted and true class labels
### for the validation set. We can also set the axis labels using
### the dnn input.
table(pred.knn, Y.valid, dnn = c("Predicted", "Observed"))

### Next, let's get the misclassification rate
(misclass.knn = mean(pred.knn != Y.valid))


### That's a pretty bad error rate. Can we do any better by tuning the
### number of neighbours? Fortunately, the knn.cv() function 
### automatically computes CV misclassification rates on a training set.
### This function does leave-one-out CV, and returns the predicted class
### at each point based on the rest of the dataset.

K.max = 200 # Maximum number of neighbours

### Container to store CV misclassification rates
mis.CV = rep(0, times = K.max)

for(i in 1:K.max){
  ### Progress update
  print(paste0(i, " of ", K.max))
  
  ### Fit leave-one-out CV
  this.knn = knn.cv(X.train, Y.train, k=i)
  
  ### Get and store CV misclassification rate
  this.mis.CV = mean(this.knn != Y.train)
  mis.CV[i] = this.mis.CV
}

### Get SEs
SE.mis.CV = sapply(mis.CV, function(r){
  sqrt(r*(1-r)/nrow(X.train))
})


### Plot CV misclassification rates
plot(1:K.max, mis.CV, xlab = "K", ylab = "Misclassification Rate",
  ylim = c(0.4, 0.64))

### Add +/- 1 SE error bars. The easiest way to do this is with the
### lines() function. For each value of K, create a line joining
### the bottom to the top of our error bar
for(i in 1:K.max){
  lower = mis.CV[i] - SE.mis.CV[i]
  upper = mis.CV[i] + SE.mis.CV[i]
  
  lines(x = c(i, i), y = c(lower, upper))
}

### Get CV min value for K
k.min = which.min(mis.CV)

### Add a horizontal line at min + 1SE
thresh = mis.CV[k.min] + SE.mis.CV[k.min]
abline(h = thresh, col = "red")

### Get CV 1SE value for K
k.1se = max(which(mis.CV <= thresh))


### Finally, let's see how our tuned KNN models do
knn.min = knn(X.train, X.valid, Y.train, k.min)
knn.1se = knn(X.train, X.valid, Y.train, k.1se)

table(knn.min, Y.valid, dnn = c("Predicted", "Observed"))
table(knn.1se, Y.valid, dnn = c("Predicted", "Observed"))

(mis.min = mean(Y.valid != knn.min))
(mis.1se = mean(Y.valid != knn.1se))
