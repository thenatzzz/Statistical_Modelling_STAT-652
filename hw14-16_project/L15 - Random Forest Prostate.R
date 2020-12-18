#########################################################################
# Random Forest on Prostate Data 
# 

prostate <-  read.table("C:\\Users\\Tom loughin\\sfuvault\\452 Statistical Learning\\R\\Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")

library(randomForest)
####################################################################
## Random forests.
## Default is to do classification trees if response is a factor and 
##  regression trees if numeric.
## Default is sqrt(p) regressors for classification, p/3 for regression
##   Can be overridden with mtry=  .  
## Specifying importance=TRUE to get variable importance measures
## Default is ntree=500 trees; usually enough.  Can do diagnostics on this.
## 
## Can tune "nodesize=", which is the smallest size of node that will 
##    still be split.  All terminal nodes will be smaller than this.
####################################################################

pro.rf <- randomForest(data=prostate, lpsa~lcavol+pgg45, 
                       importance=TRUE, ntree=1000, mtry=1, 
                       keep.forest=TRUE)
pro.rf             # Barely useful here
summary(pro.rf)    # Not useful here.

# Default plot method shows OOB error vs. number of trees.

x11(h=7,w=6,pointsize=12)
plot(pro.rf)

x11(h=7,w=6,pointsize=12)
plot(pro.rf, xlim=c(50,1000), ylim=c(0.6,0.8))

# Histogram of tree sizes
x11(h=7,w=6,pointsize=12)
hist(treesize(pro.rf))

# Note: If you specify no newdata= in predict.randomforest, 
#   it gives the OOB predictions for the sample, same as the
#   $predicted element of the randomforest object.
(pro.oob <- mean((predict(pro.rf) - prostate$lpsa)^2))
(pro.oob.p <- mean((pro.rf$predicted - prostate$lpsa)^2))
# Specifying the original data set uses the usual prediction, which results in training error.
(pro.mse <- mean((predict(pro.rf, newdata=prostate) - prostate$lpsa)^2))

# Variable Importance
importance(pro.rf) # Print out importance measures
x11()
varImpPlot(pro.rf) # Plot of importance measures; more interesting with more variables

library(rgl)  

x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

pred <- predict(pro.rf ,newdata=xy1)
surface = matrix(pred, nrow=length(x1))

open3d()
persp3d(x = x1, y = x2, 
        z = surface, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")


###############################################################
## Now let's look at entire data set, default values

pro.rf8 <- randomForest(data=prostate, lpsa~., 
                       importance=TRUE, ntree=1000, 
                       keep.forest=TRUE)

# Default plot method shows OOB error vs. number of trees.

x11(h=7,w=6,pointsize=12)
plot(pro.rf8, main="RF OOB Error vs. Number of trees")

# Note: If you specify no newdata= in predict.randomforest, 
#   it gives the OOB predictions for the sample, same as the
#   $predicted element of the randomforest object.
(pro.oob8 <- mean((predict(pro.rf8) - prostate$lpsa)^2))

# Variable Importance
importance(pro.rf8) # Print out importance measures
x11()
varImpPlot(pro.rf8, main="RF Variable Importance Plots") # Plot of importance measures; more interesting with more variables

###### tuning mtry using OOB error
# Usually, don't need to try every m, but can do it cheap here
# Also could put this whole ting inside another loop and replicate

reps=20 # Doing lots of reps here because it's cheap
varz = 1:8
nodez = c(3,5,7,10,15,20)

NS = length(nodez)
M = length(varz)
rf.oob = matrix(NA, nrow=M*NS, ncol=reps)

for(r in 1:reps){
  counter=1
  for(m in varz){
    for(ns in nodez){
      pro.rfm <- randomForest(data=prostate, lpsa~., ntree=500, 
                              mtry=m, nodesize=ns)
      rf.oob[counter,r] = mean((predict(pro.rfm) - prostate$lpsa)^2)
      counter=counter+1
    }
  }
}

parms = expand.grid(nodez,varz)
row.names(rf.oob) = paste(parms[,2], parms[,1], sep="|")

mean.oob = apply(rf.oob, 1, mean)
min.oob = apply(rf.oob, 2, min)

x11(h=7,w=10,pointsize=8)
boxplot(rf.oob, use.cols=FALSE, las=2)

x11(h=7,w=10,pointsize=8)
boxplot(t(rf.oob)/min.oob, use.cols=TRUE, las=2, 
        main="RF Tuning Variables and Node Sizes")

