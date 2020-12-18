# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(46685326,kind="Mersenne-Twister")

#####################################

df <-  read.csv("P2Data2020.csv")
df$Y = factor(df$Y)
head(df,5)

# shuffle data
rows <- sample(nrow(df))
df <- df[rows, ]

# delete some column
df[,c("X3","X5","X13",'X15')] <- list(NULL)
# df[,c("X13")] <- list(NULL)
# df[,c("X3","X5","X13")] <- list(NULL)


head(df,5)
#######################################
perm <- sample(x=nrow(df))
# set1 <- df[which(perm <= 3*nrow(df)/4),]
# set2 <- df[which(perm >  3*nrow(df)/4),]

set1 <- df[which(perm <= 9*nrow(df)/10),]
set2 <- df[which(perm >  9*nrow(df)/10),]
head(set1,6)
head(set2,6)


######################################


# RF

library(randomForest)
wh.rf <- randomForest(data=set1, Y~., 
                      importance=TRUE, keep.forest=TRUE)
wh.rf             # more useful here

round(importance(wh.rf),3) # Print out importance measures
# x11(h=7,w=15)
# varImpPlot(wh.rf) # Plot of importance measures; more interesting with more variables
# 

# Predict results of classification. 
pred.rf.train <- predict(wh.rf, newdata=set1, type="response")
pred.rf.test <- predict(wh.rf, newdata=set2, type="response")
#"vote" gives proportions of trees voting for each class
pred.rf.vtrain <- predict(wh.rf, newdata=set1, type="vote")
pred.rf.vtest <- predict(wh.rf, newdata=set2, type="vote")
head(cbind(pred.rf.test,pred.rf.vtest))

(misclass.train.rf <- mean(ifelse(pred.rf.train == set1$Y, yes=0, no=1)))
(misclass.test.rf <- mean(ifelse(pred.rf.test == set2$Y, yes=0, no=1)))


####### 3.###########
reps=5
varz = c(2,4,6,10,18)
nodez = c(1,3,5,7,10)

NS = length(nodez)
M = length(varz)
rf.oob = matrix(NA, nrow=M*NS, ncol=reps)
rf.oob
for(r in 1:reps){
  counter=1
  for(m in varz){
    for(ns in nodez){
      wh.rfm <- randomForest(data=set1, Y~., 
                             mtry=m, nodesize=ns, ntree=1000)
      rf.oob[counter,r] = mean(predict(wh.rfm, type="response") != set1$Y)
      counter=counter+1
    }
  }
}

parms = expand.grid(nodez,varz)
row.names(rf.oob) = paste(parms[,2], parms[,1], sep="|")

mean.oob = apply(rf.oob, 1, mean)
mean.oob[order(mean.oob)]

min.oob = apply(rf.oob, 2, min)

# x11(h=7,w=10,pointsize=8)
boxplot(rf.oob, use.cols=FALSE, las=2)

# x11(h=7,w=10,pointsize=8)
boxplot(t(rf.oob)/min.oob, use.cols=TRUE, las=2, 
        main="RF Tuning Variables and Node Sizes")

# Suggested parameters are mtry=4, nodesize=7
set.seed(46685326,kind="Mersenne-Twister")
wh.rf.tun <- randomForest(data=set1, Y~., mtry=4, nodesize=7,ntree=1000,
                          importance=TRUE, keep.forest=TRUE)


# Predict results of classification. 
pred.rf.train.tun <- predict(wh.rf.tun, newdata=set1, type="response")
pred.rf.test.tun <- predict(wh.rf.tun, newdata=set2, type="response")
#"vote" gives proportions of trees voting for each class
pred.rf.vtrain.tun <- predict(wh.rf.tun, newdata=set1, type="vote")
pred.rf.vtest.tun <- predict(wh.rf.tun, newdata=set2, type="vote")
head(cbind(pred.rf.test.tun,pred.rf.vtest.tun))

(misclass.train.rf.tun <- mean(ifelse(pred.rf.train.tun == set1$Y, yes=0, no=1)))
(misclass.test.rf.tun <- mean(ifelse(pred.rf.test.tun == set2$Y, yes=0, no=1)))

######## tuning #######
# Create model with default paramters
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(46685326,kind="Mersenne-Twister")
metric <- "Accuracy"
mtry <- sqrt(ncol(df))
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(Y~., data=df, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

### caret
# Random Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(46685326,kind="Mersenne-Twister")
mtry <- sqrt(ncol(df))
rf_random <- train(Y~., data=df, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)


# ### custom
# customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
# customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
# customRF$grid <- function(x, y, len = NULL, search = "grid") {}
# customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
#   randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
# }
# customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata)
# customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
#   predict(modelFit, newdata, type = "prob")
# customRF$sort <- function(x) x[order(x[,1]),]
# customRF$levels <- function(x) x$classes
# 
# # train model
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
# set.seed(46685326,kind="Mersenne-Twister")
# custom <- train(Y~., data=df, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
# summary(custom)
# plot(custom)
# 

#########

rf.fit <- train(Y ~ ., 
                data = set1, 
                method = "rf",     # Use the "random forest" algorithm
                importance = TRUE, # importance=TRUE allows to inspect variable importance
                trControl = trainControl(method = "repeatedcv", # Use cross-validation
                                         number = 10, # Use 10 folds for cross-validation
                                         repeats = 5)
)


rf.fit$finalModel
plot(rf.fit)

plot(rf.fit$finalModel)

plot(varImp(rf.fit), top = 10)

##
rf.predict<-predict(rf.fit , set2)
confusionMatrix(rf.predict, set2$Y)


######################################
source("Helper Functions.R")

n = nrow(set1)
M = 10 # Number of times to re-fit each model

### Define parameter values and use expand.grid() to get all combinations
all.mtry = c(2,3,4,6,10,18) #mtry
all.nodesize = c(1,3,5,7,10)  # nodesize


all.pars = expand.grid(mtries = all.mtry,
                       nodesizes = all.nodesize)
n.pars = nrow(all.pars) # Number of parameter combinations
K = 5 # Number of folds
### Create folds
folds = get.folds(n, K)
### Create container for test Merror
CV.Merror = array(0, dim = c(K, n.pars))
for(i in 1:K){
  ### Print progress update
  print(paste0(i, " of ", K))
  
  ### Split data and rescale predictors
  data.train = set1[folds != i,]
  XY.train = data.train
  
  data.valid = set1[folds == i,]
  XY.valid =data.valid
  
  ### Fit RF models for each parameter combination. A second 
  ### for loop will make our life easier here
  for(j in 1:n.pars){
    ### Get current parameter values
    this.mtry = all.pars[j,1]
    this.nodesize = all.pars[j,2]
    
    ### We need to run RF multiple times to avoid bad local minima. Create
    ### containers to store the models and their errors.
    all.rfs = list(1:M)
    all.Merror = rep(0, times = M)
    
    ### We need to fit each model multiple times. This calls for another
    ### for loop.
    for(l in 1:M){
      ### Fit model
      wh.rfm <- randomForest(data=XY.train, Y~., 
                             mtry=this.mtry, nodesize=this.nodesize, ntree=1000)
      
      # get misclassification rate
      Merror.rf = mean(predict(wh.rfm, type="response") != XY.train$Y)
      
      all.rfs[[l]] = wh.rfm
      all.Merror[l] = Merror.rf
    }
    
    ### Get best fit using current parameter values
    ind.best = which.min(all.Merror)
    fit.rf.best = all.rfs[[ind.best]]
    
    ### Get predictions and Test Merror, then store Test Merror
    Merror.val.rf = mean(predict(fit.rf.best, type="response",newdata=XY.valid) != XY.valid$Y)
    CV.Merror[i, j] = Merror.val.rf # Be careful with indices for CV.Merror
    
  }
}
CV.Merror

dim(CV.Merror)
(mean.MerrorCV = apply(X=CV.Merror, MARGIN=2, FUN=mean))

V=K

(mean.MerrorCV.sd = apply(X=CV.Merror, MARGIN=2, FUN=sd))
Merror.cv.CIl = mean.MerrorCV - qt(p=.975, df=V-1)*mean.MerrorCV.sd/V
Merror.cv.CIu = mean.MerrorCV + qt(p=.975, df=V-1)*mean.MerrorCV.sd/V

round(cbind(Merror.cv.CIl, Merror.cv.CIu),2)
table = round(cbind(Merror.cv.CIl, Merror.cv.CIu,mean.MerrorCV),3)
colnames(table)[3] = "mean"
names.pars = paste0(all.pars$mtries,",",
                    all.pars$nodesizes)
names.pars
rownames(table)= names.pars
table

colnames(CV.Merror) = names.pars

### Make boxplot
boxplot(CV.Merror, las = 2, main = "Test Accuracy Boxplot")
boxplot(CV.Merror, las = 2, main = "Focused Test Accuracy Boxplot",ylim=c(0.78,0.85))

### Get relative test Merror and make boxplot
CV.RMerror = apply(CV.Merror, 1, function(W) W/min(W))
CV.RMerror = t(CV.RMerror)
boxplot(CV.RMerror, las = 2, main = "Relative Test Merror Boxplot",ylim=c(0.99,1.02))


########### final model #########
# Suggested parameters are mtry=3/2, nodesize=7/5
set.seed(46685326,kind="Mersenne-Twister")
wh.rf.tun <- randomForest(data=set1, Y~., mtry=3, nodesize=7,ntree=1000,
                          importance=TRUE, keep.forest=TRUE)


# Predict results of classification. 
pred.rf.train.tun <- predict(wh.rf.tun, newdata=set1, type="response")
pred.rf.test.tun <- predict(wh.rf.tun, newdata=set2, type="response")
#"vote" gives proportions of trees voting for each class
pred.rf.vtrain.tun <- predict(wh.rf.tun, newdata=set1, type="vote")
pred.rf.vtest.tun <- predict(wh.rf.tun, newdata=set2, type="vote")
head(cbind(pred.rf.test.tun,pred.rf.vtest.tun))

(misclass.train.rf.tun <- mean(ifelse(pred.rf.train.tun == set1$Y, yes=0, no=1)))
(misclass.test.rf.tun <- mean(ifelse(pred.rf.test.tun == set2$Y, yes=0, no=1)))

table(set2[,'Y'],pred.rf.test.tun, dnn=c("Observed","Predicted"))

