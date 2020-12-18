# Classification Tree on Wheat Data
##########
# Enter data and do some processing
wheat <-  read.csv("wheat.csv")
head(wheat)
#summary(wheat)

# Variable "type" is the response variable.  "class" is another explanatory.
class(wheat$type)
wheat$type = as.factor(wheat$type)
wheat$class = as.factor(wheat$class)
#summary(wheat)

# Create a numerical version of "class" for methods that need numbers
###Not needed for trees
#wheat$classnum <- as.numeric((wheat$class))
# Remove "id"
wheat = wheat[,-1]
#summary(wheat)

############
# Creating TWO sets: 200 train, 75 test
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm <= 200),]
set2 <- wheat[which(perm>200),]


library(rpart)
####################################################################
## Default tree
## Specifying method="class" for classification
##   Split criterion is Gini
##   Deviance is available through parms=list(split="information")
####################################################################

wh.tree <- rpart(data=set1, type ~ ., method="class", cp=0)
printcp(wh.tree)
round(wh.tree$cptable[,c(2:5,1)],4)

# summary(wh.tree) #Lots of output

# See pdf of this---Note that it IS making splits that improve 
#   probabilities but do not change classes
library(rpart.plot)
x11(h=10, w=10)
prp(wh.tree, type=1, extra=1, main="Original full tree")

# Plot of the cross-validation for the complexity parameter.
##  NOTE: Can be very variable, depending on CV partitioning
x11(h=7, w=10, pointsize=10)
plotcp(wh.tree)


# Find location of minimum error
cpt = wh.tree$cptable
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

# Creating a pruned tree using a selected value of the CP by CV.
wh.prune.cv.1se <- prune(wh.tree, cp=cp.1se)
# Creating a pruned tree using a selected value of the CP by CV.
wh.prune.cv.min <- prune(wh.tree, cp=cp.min)

# Plot the pruned trees
x11(h=12, w=18)
par(mfrow=c(1,2))
prp(wh.prune.cv.1se, type=1, extra=1, main="Pruned CV-1SE tree")
prp(wh.prune.cv.min, type=1, extra=1, main="Pruned CV-min tree")


# Predict results of classification. "Vector" means store class as a number
pred.train.cv.1se <- predict(wh.prune.cv.1se, newdata=set1, type="class")
pred.train.cv.min <- predict(wh.prune.cv.min, newdata=set1, type="class")
pred.train.full <- predict(wh.tree, newdata=set1, type="class")

# Predict results of classification. "Vector" means store class as a number
pred.test.cv.1se <- predict(wh.prune.cv.1se, newdata=set2, type="class")
pred.test.cv.min <- predict(wh.prune.cv.min, newdata=set2, type="class")
pred.test.full <- predict(wh.tree, newdata=set2, type="class")

(misclass.train.cv.1se <- mean(ifelse(pred.train.cv.1se == set1$type, yes=0, no=1)))
(misclass.train.cv.min <- mean(ifelse(pred.train.cv.min == set1$type, yes=0, no=1)))
(misclass.train.full <- mean(ifelse(pred.train.full == set1$type, yes=0, no=1)))

(misclass.test.cv.1se <- mean(ifelse(pred.test.cv.1se == set2$type, yes=0, no=1)))
(misclass.test.cv.min <- mean(ifelse(pred.test.cv.min == set2$type, yes=0, no=1)))
(misclass.test.full <- mean(ifelse(pred.test.full == set2$type, yes=0, no=1)))

# Confusion Matrices
table(set2$type, pred.test.full,  dnn=c("Observed","Predicted"))

