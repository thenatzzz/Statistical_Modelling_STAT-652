# Gradient Boosted Trees using gbm 
# Prostate data

prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")

library(gbm)
####################################################################
## Gradient boosting through gbm() allows different models to 
#    be used for boosting.  To fit regression trees with RSS
#    for splits, specify, "distribution=gaussian"  
## Tuning parameters and defaults include 
# n.trees=100: number of trees (=B). Usually need to increase this
# interaction.depth = 1: Relates to d. Default implies additive model.  
#    USUALLY need to increase this. 
# shrinkage = .001: The learning rate parameter "lambda" 
#
### Optional tuning parameters
# bag.fraction=0.5: The subsampling fraction
# n.minobsinnode=10: number of obs in terminal nodes
#         Tuning this can improve performance, especially making it smaller 
#         in smaller data sets.
####################################################################

# Only 2 variables, so trying small tree size, lots of trees
# Run these two functions several times to see change in CV suggestion
# Doing 10,000 trees because prior runs with fewer suggested need for more
prb.1_.001 <- gbm(data=prostate, lpsa~lcavol+pgg45, distribution="gaussian", 
                 n.trees=10000, interaction.depth=1, shrinkage=0.001, 
                 bag.fraction=0.8)

x11(h=7, w=14)
par(mfrow=c(1,2))
gbm.perf(prb.1_.001, method="OOB", oobag.curve=TRUE )  
gbm.perf(prb.1_.001, method="OOB", plot.it=FALSE) #No Plot

### NOTE: Can add trees using gbm.more(), in case you have a long-running 
#    function and need more trees.
# Need to include "keep.data=TRUE" if you plan to do this

# Variable Importance
x11(h=7, w=6)
summary(prb.1_.001)

# 3D plot of surface 

library(rgl)  
x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

# predict.gbm() needs to know how many trees to use 
### Convenient! (you can fit too many but base predictions on a smaller number)
# I am using a hack that doubles the number of treed that OOB gives me.
#  I made this up, but it comes closer to what CV would estimate is optimal.

opt.trees = gbm.perf(prb.1_.001, method="OOB", plot.it=FALSE) *2

surface = matrix(predict(prb.1_.001, newdata=xy1, n.trees=opt.trees, 
                                    type="response"),nrow=length(x1))

open3d()
persp3d(x = x1, y = x2, 
        z = surface, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")

####################################################################
#Repeat, increasing the tree depth to 6, 
#        Due to limited sample size, increasing bag.fraction 
#        to allow larger trees
####################################################################

prb.6_.001 <- gbm(data=prostate, lpsa~lcavol+pgg45, distribution="gaussian", 
                  n.trees=10000, interaction.depth=6, shrinkage=0.001, 
                  bag.fraction=0.8)

x11(h=7, w=14)
par(mfrow=c(1,2))
gbm.perf(prb.6_.001, method="OOB", oobag.curve=TRUE )  
gbm.perf(prb.6_.001, method="OOB", plot.it=FALSE) #No Plot

# Variable Importance
x11(h=7, w=6)
summary(prb.6_.001)

# 3D plot of surface 

library(rgl)  
x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

# predict.gbm() needs to know how many trees to use 
### Convenient! (you can fit too many but base predictions on a smaller number)
# I am using a hack that doubles the number of treed that OOB gives me.
#  I made this up, but it comes closer to what CV would estimate is optimal.

opt.trees = gbm.perf(prb.6_.001, method="OOB", plot.it=FALSE) *2

surface = matrix(predict(prb.6_.001, newdata=xy1, n.trees=opt.trees, 
                         type="response"),nrow=length(x1))

open3d()
persp3d(x = x1, y = x2, 
        z = surface, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")


####################################################################
#Repeat, increasing learning rqate (larger shrinkage lambda)
####################################################################

prb.6_.1 <- gbm(data=prostate, lpsa~lcavol+pgg45, distribution="gaussian", 
                  n.trees=10000, interaction.depth=6, shrinkage=0.1, 
                  bag.fraction=0.8)

x11(h=7, w=14)
par(mfrow=c(1,2))
gbm.perf(prb.6_.1, method="OOB", oobag.curve=TRUE )  
gbm.perf(prb.6_.1, method="OOB", plot.it=FALSE) #No Plot

# Variable Importance
x11(h=7, w=6)
summary(prb.6_.001)

# 3D plot of surface 

library(rgl)  
x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

# predict.gbm() needs to know how many trees to use 
### Convenient! (you can fit too many but base predictions on a smaller number)
# I am using a hack that doubles the number of treed that OOB gives me.
#  I made this up, but it comes closer to what CV would estimate is optimal.

opt.trees = gbm.perf(prb.6_.1, method="OOB", plot.it=FALSE) *2
opt.trees
surface = matrix(predict(prb.6_.1, newdata=xy1, n.trees=opt.trees, 
                         type="response"),nrow=length(x1))

open3d()
persp3d(x = x1, y = x2, 
        z = surface, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")



###############################################################
## Now let's look at entire data set, adding tuning

#  Let's do R=2 reps of 5-fold CV.
set.seed(182674455)
V=5
R=2 
n2 = nrow(prostate)
# Create the folds and save in a matrix
folds = matrix(NA, nrow=n2, ncol=R)
for(r in 1:R){
        folds[,r]=floor((sample.int(n2)-1)*V/n2) + 1
}

shr = c(.001,.005,.025,.125)
dep = c(2,4,6)
### Second grid 
#dep = c(1,2,3,4)
#shr = c(0.0025, 0.005, 0.0075, 0.01)
trees = 10000

NS = length(shr)
ND = length(dep)
gb.cv = matrix(NA, nrow=ND*NS, ncol=V*R)
opt.tree = matrix(NA, nrow=ND*NS, ncol=V*R)

qq = 1
for(r in 1:R){
  for(v in 1:V){
    pro.train = prostate[folds[,r]!=v,]
    pro.test = prostate[folds[,r]==v,]
    counter=1
    for(d in dep){
      for(s in shr){
        pro.gbm <- gbm(data=pro.train, lpsa~., distribution="gaussian", 
                       n.trees=trees, interaction.depth=d, shrinkage=s, 
                       bag.fraction=0.8)
        treenum = min(trees, 2*gbm.perf(pro.gbm, method="OOB", plot.it=FALSE))
        opt.tree[counter,qq] = treenum
        preds = predict(pro.gbm, newdata=pro.test, n.trees=treenum)
        gb.cv[counter,qq] = mean((preds - pro.test$lpsa)^2)
        counter=counter+1
      }
    }
    qq = qq+1
  }  
}

parms = expand.grid(shr,dep)
row.names(gb.cv) = paste(parms[,2], parms[,1], sep="|")
row.names(opt.tree) = paste(parms[,2], parms[,1], sep="|")

opt.tree
gb.cv

(mean.tree = apply(opt.tree, 1, mean))
(mean.cv = sqrt(apply(gb.cv, 1, mean)))
min.cv = apply(gb.cv, 2, min)

# x11(h=7,w=10,pointsize=8)
boxplot(sqrt(gb.cv), use.cols=FALSE, las=2)
# boxplot(gb.cv, use.cols=FALSE, las=2)

# x11(h=7,w=10,pointsize=8)
boxplot(sqrt(t(gb.cv)/min.cv), use.cols=TRUE, las=2, 
        main="GBM Fine-Tuning Variables and Node Sizes")

##############################################################
# Refit Final Model

pro.opt <- gbm(data=prostate, lpsa~., distribution="gaussian", 
                n.trees=500, interaction.depth=2, shrinkage=0.0075, 
                bag.fraction=0.8)

# Variable Importance
# x11(h=7, w=6)
summary(pro.opt)

