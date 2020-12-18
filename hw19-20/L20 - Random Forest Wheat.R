# Random Forest classification on Wheat Data
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


library(randomForest)
####################################################################
## Random forests.
## Default is to do classification trees if response is a factor and 
##  regression trees if numeric.
## Default is sqrt(p) regressors for classification, p/3 for regression
##   Can be overridden with mtry=  .  
## Specifying importance=TRUE to get variable importance measures
## Default is ntree=500 trees; usually enough.  Can do diagnostics on this.
## Default nodesize = 1
####################################################################

# Starting with default, no real reason
wh.rf <- randomForest(data=set1, type~., 
                         importance=TRUE, keep.forest=TRUE)
wh.rf             # more useful here

### Is 500 enough trees???  Probably so.  
x11(h=7,w=6,pointsize=12)
plot(wh.rf)


round(importance(wh.rf),3) # Print out importance measures
x11(h=7,w=15)
varImpPlot(wh.rf) # Plot of importance measures; more interesting with more variables


# Predict results of classification. 
pred.rf.train <- predict(wh.rf, newdata=set1, type="response")
pred.rf.test <- predict(wh.rf, newdata=set2, type="response")
#"vote" gives proportions of trees voting for each class
pred.rf.vtrain <- predict(wh.rf, newdata=set1, type="vote")
pred.rf.vtest <- predict(wh.rf, newdata=set2, type="vote")
head(cbind(pred.rf.test,pred.rf.vtest))

(misclass.train.rf <- mean(ifelse(pred.rf.train == set1$type, yes=0, no=1)))
(misclass.test.rf <- mean(ifelse(pred.rf.test == set2$type, yes=0, no=1)))

###################################################
# Tuning variables and node sizes

set.seed(879417)
reps=5
varz = 1:6
nodez = c(1,3,5,7,10)

NS = length(nodez)
M = length(varz)
rf.oob = matrix(NA, nrow=M*NS, ncol=reps)

for(r in 1:reps){
  counter=1
  for(m in varz){
    for(ns in nodez){
      wh.rfm <- randomForest(data=set1, type~., 
                              mtry=m, nodesize=ns)
      rf.oob[counter,r] = mean(predict(wh.rfm, type="response") != set1$type)
      counter=counter+1
    }
  }
}

parms = expand.grid(nodez,varz)
row.names(rf.oob) = paste(parms[,2], parms[,1], sep="|")

mean.oob = apply(rf.oob, 1, mean)
mean.oob[order(mean.oob)]

min.oob = apply(rf.oob, 2, min)

x11(h=7,w=10,pointsize=8)
boxplot(rf.oob, use.cols=FALSE, las=2)

x11(h=7,w=10,pointsize=8)
boxplot(t(rf.oob)/min.oob, use.cols=TRUE, las=2, 
        main="RF Tuning Variables and Node Sizes")

# Suggested parameters are mtry=4, nodesize=1

wh.rf.tun <- randomForest(data=set1, type~., mtry=4, nodesize=1,
                      importance=TRUE, keep.forest=TRUE)

### Is 500 enough trees???  Probably so.  
x11(h=7,w=6,pointsize=12)
plot(wh.rf.tun)

# Predict results of classification. 
pred.rf.train.tun <- predict(wh.rf.tun, newdata=set1, type="response")
pred.rf.test.tun <- predict(wh.rf.tun, newdata=set2, type="response")
#"vote" gives proportions of trees voting for each class
pred.rf.vtrain.tun <- predict(wh.rf.tun, newdata=set1, type="vote")
pred.rf.vtest.tun <- predict(wh.rf.tun, newdata=set2, type="vote")
head(cbind(pred.rf.test.tun,pred.rf.vtest.tun))

(misclass.train.rf.tun <- mean(ifelse(pred.rf.train.tun == set1$type, yes=0, no=1)))
(misclass.test.rf.tun <- mean(ifelse(pred.rf.test.tun == set2$type, yes=0, no=1)))

