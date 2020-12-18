#########################################################################
# Regression Trees on Prostate Data 
# 
set.seed(2385660,kind="Mersenne-Twister") 

help("airquality")

AQ_original = airquality
head(AQ_original,5)

# X= Solar.R, Wind, Temp
# Y = Ozone
AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio
# sort Temp
AQ = AQ[order(AQ$Temp),]
head(AQ)

# prostate = AQ
prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")

library(rpart)
####################################################################
## Default tree on just two variables to make plot
##  method="anova" specifies using residual SS for optimizing
####################################################################

# pr.tree <- rpart(lpsa ~ lcavol + pgg45, method="anova", data=prostate)
pr.tree <- rpart(Ozone ~Wind+Temp , method="anova", data=AQ, cp=0)
pr.tree
pr.tree$cptable
# I like this order better!
pr.tree$cptable[,c(2:5,1)]
summary(pr.tree)

#############################
#rpart.plot package makes nicer trees than rpart().
#   See the package vignette for examples and options.
#############################

library(rpart.plot)
x11(h=7, w=6, pointsize=11)
prp(pr.tree, type=1, extra=1, main="Original full tree")


# Plot of the cross-validation for the complexity parameter.
x11(h=7, w=8)
plotcp(pr.tree)

# Creating a pruned tree using minimum CV 
# See below for automated CP selection, both for minimum and 1SE.
pr.prune <- prune(pr.tree, cp=0.016)

win.graph(h=7, w=6, pointsize=11)
prp(pr.prune, type=1, extra=1, main="Final pruned tree")



# 3D plot of surface from full unpruned tree

#  Creating matrix of estimated means by direct calculation.  
#     Matrix form required by contour()  
library(rgl)  

x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

pred2 <- predict(pr.tree ,newdata=xy1)
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")

####################################################################
## Full analysis using my preferred settings
##   Includes code for automatic pruning using 
##    both minimum cv and 1SE rule
####################################################################
set.seed(2385660,kind="Mersenne-Twister") 

# pr.tree2 <- rpart(lpsa ~ ., method="anova", data=prostate, cp=0)
pr.tree2 <- rpart(Ozone ~ Temp + Wind, method="anova", data=AQ, cp=0)

pr.tree2
pr.tree2$cptable[,c(2:5,1)]
cpt <- pr.tree2$cptable

# x11(h=7, w=8)
plotcp(pr.tree2)

# The code below shows how to select the tuning parameter using
#   either the +1SE or the true min CV error

# Find location of minimum error
minrow <- which.min(cpt[,4])
# Take geometric mean of cp values at min error and one step up 
cplow.min <- cpt[minrow,1]
cpup.min <- ifelse(minrow==1, yes=1, no=cpt[minrow-1,1])
cp.min <- sqrt(cplow.min*cpup.min)

# Find smallest row where error is below +1SE
se.row <- min(which(cpt[,4] < cpt[minrow,4]+cpt[minrow,5]))
# Take geometric mean of cp values at min error and one step up 
cplow.1se <- cpt[se.row,1]
cpup.1se <- ifelse(se.row==1, yes=1, no=cpt[se.row-1,1])
cp.1se <- sqrt(cplow.1se*cpup.1se)

# Do pruning each way
pr2.prune.min <- prune(pr.tree2, cp=cp.min)
pr2.prune.1se <- prune(pr.tree2, cp=cp.1se)

library(rpart.plot)

x11(h=10, w=12, pointsize=11)
par(mfrow=c(1,2))
prp(pr2.prune.min, type=1, extra=1, main="Tree pruned to Min CV Error")

prp(pr2.prune.1se, type=1, extra=1, main="Tree pruned to +1SE CV Error")
