# K-Nearest neighbour on the Wheat data
##########
# Enter data and do some processing
wheat <-  read.csv("wheat.csv")
head(wheat)
summary(wheat)

# Variable "type" is the response variable.  "class" is another explanatory.
class(wheat$type)
wheat$type = as.factor(wheat$type)
wheat$class = as.factor(wheat$class)
summary(wheat)

# Create a numerical version of "class" for methods that need numbers
wheat$classnum <- as.numeric((wheat$class))
# Remove "id"
wheat = wheat[,-1]
summary(wheat)

############

# KNN requires numerical explanatories, so remove factor class 
# Creating TWO sets: 200 train, 75 test
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm <= 200),-1]
set2 <- wheat[which(perm>200),-1]

############
## Need to scale all variables to have same SD
#   so that "distance" is not dominated by different scalings

# Function to Scaling x1 using mean and SD from set2
############
scale.1 <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- mean(x2[,col])
    b <- sd(x2[,col])
    x1[,col] <- (x1[,col]-a)/b
  }
  x1
}

# Creating training and test X matrices, then scaling them.
x.1.unscaled <- as.matrix(set1[,-6])
x.1 <- scale.1(x.1.unscaled,x.1.unscaled)
x.2.unscaled <- as.matrix(set2[,-6])
x.2 <- scale.1(x.2.unscaled,x.1.unscaled)

summary(x.1)
summary(x.2)


library(FNN)
###########################################################################
# FNN
# The knn() function uses the X-values from the train= set as a supply of neighbours
#   from which to choose for each row in test= .  The true classes for the training 
#   set are given in cl= .  Obviously, k=  gives the number of neighbours.
# The output is a factor list containing the predicted values (no "predict()" needed).
###########################################################################
# Fit the 1-NN function using set 1 to train AND test 
#   (compute training error)
knnfit.1.1 <- knn(train=x.1, test=x.1, cl=set1[,6], k=1)

# Create Confusion Matrix and misclass rate
table(knnfit.1.1, set1[,6],  dnn=c("Predicted","Observed"))
(misclass.knn1.1 <- 
         mean(ifelse(knnfit.1.1 == set1[,6], yes=0, no=1)))

# Fit the 1-NN function using set 1 to train and set2 to test 
#   (compute test error)
knnfit.1.2 <- knn(train=x.1, test=x.2, cl=set1[,6], k=1)
# Create Confusion Matrix and misclass rate
table(knnfit.1.2, set2[,6],  dnn=c("Predicted","Observed"))
(misclass.knn1.2 <- 
         mean(ifelse(knnfit.1.2 == set2[,6], yes=0, no=1)))

#  Now we tune using cv.knn(), which does Leave-one-out (n-fold) CV
# I created the steps below to fit a sequence of k  values to tune the knn.
#  Enter the maximum k as kmax.  Run knn on training set and predict test set.
#  Then compute test misclassification proportion as the output,
#  Need to change the data sets in the two lines in the "runknn" function.
kmax <- 40
k <- matrix(c(1:kmax), nrow=kmax)
runknn <- function(x){
  knncv.fit <- knn.cv(train=x.1, cl=set1[,6], k=x)
  # Fitted values are for deleted data from CV
  mean(ifelse(knncv.fit == set1[,6], yes=0, no=1))
}

mis <- apply(X=k, MARGIN=1, FUN=runknn)
mis.se <- sqrt(mis*(1-mis)/nrow(set2)) #SE of misclass rates

#Now plot results
# Plot like the CV plots, with 1SE bars and a horizontal line 
#   at 1SE above minimum.
x11(h=7,w=7,pointsize=12)
plot(x=k, y=mis, type="b", ylim=c(.25,.50)) 
for(ii in c(1:kmax)){
  lines(x=c(k[ii],k[ii]), y=c(mis[ii]-mis.se[ii], mis[ii]+mis.se[ii]), col=colors()[220])
}
abline(h=min(mis + mis.se), lty="dotted")

# k for Minimum CV error
mink = which.min(mis)
#Trying the value of k with the lowest validation error on test data set.
knnfitmin.2 <- knn(train=x.1, test=x.2, cl=set1[,6], k=mink)

table(knnfitmin.2, set2[,6],  dnn=c("Predicted","Observed"))
(misclass.2.knnmin <- mean(ifelse(knnfitmin.2 == set2[,6], yes=0, no=1)))

# Less variable models have larger k, so find largest k within 
#   1 SE of minimum validation error 
serule = max(which(mis<mis[mink]+mis.se[mink]))
knnfitse.2 <- knn(train=x.1, test=x.2, cl=set1[,6], k=serule)

table(knnfitse.2, set2[,6],  dnn=c("Predicted","Observed"))
(misclass.2.knnse <- mean(ifelse(knnfitse.2 == set2[,6], yes=0, no=1)))

