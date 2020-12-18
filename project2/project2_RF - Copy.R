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
set1 <- df[which(perm <= 3*nrow(df)/4),]
set2 <- df[which(perm >  3*nrow(df)/4),]
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
wh.rf.tun <- randomForest(data=set1, Y~., mtry=4, nodesize=10,ntree=1000,
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
n.train = floor(n*0.75)
n.valid = n - n.train

inds = c(rep(1, times = n.train), rep(2, times = n.valid))
inds.rand = inds[sample.int(n)]

M = 10 # Number of times to re-fit each model

### Define parameter values and use expand.grid() to get all combinations
# all.n.hidden = c(1,3, 5,7, 9)
# all.shrink = c(0.001, 0.1, 0.5, 1,2)

all.n.hidden = c(2,4,6,10,18) #mtry
all.shrink = c(1,3,5,7,10)  # nodesize


all.pars = expand.grid(n.hidden = all.n.hidden,
                       shrink = all.shrink)
n.pars = nrow(all.pars) # Number of parameter combinations
K = 5 # Number of folds
### Create folds
folds = get.folds(n, K)
### Create container for MSPEs
CV.MSPEs = array(0, dim = c(K, n.pars))
for(i in 1:K){
  ### Print progress update
  print(paste0(i, " of ", K))
  
  ### Split data and rescale predictors
  data.train = set1[folds != i,]
  # X.train.raw = data.train[,-1]
  X.train = data.train[,-1]
  # X.train = rescale(X.train.raw, X.train.raw)
  Y.train = data.train[,1]
  
  XY.train = data.train
  
  data.valid = set1[folds == i,]
  # X.valid.raw = data.valid[,-1]
  X.valid = data.valid[,-1]
  # X.valid = rescale(X.valid.raw, X.train.raw)
  Y.valid = data.valid[,1]
  
  XY.valid =data.valid
  
  ### Fit neural net models for each parameter combination. A second 
  ### for loop will make our life easier here
  for(j in 1:n.pars){
    ### Get current parameter values
    this.n.hidden = all.pars[j,1]
    this.shrink = all.pars[j,2]
    
    ### We need to run nnet multiple times to avoid bad local minima. Create
    ### containers to store the models and their errors.
    all.nnets = list(1:M)
    all.SSEs = rep(0, times = M)
    
    ### We need to fit each model multiple times. This calls for another
    ### for loop.
    for(l in 1:M){
      ### Fit model
      # fit.nnet = nnet(X.train, Y.train, linout = TRUE, size = this.n.hidden,
                      # decay = this.shrink, maxit = 500, trace = FALSE)
      wh.rfm <- randomForest(data=XY.train, Y~., 
                             mtry=this.n.hidden, nodesize=this.shrink, ntree=1000)
      
      # get misclassification rate
      # rf.oob[counter,r] = mean(predict(wh.rfm, type="response") != set1$Y)
      SSE.nnet = mean(predict(wh.rfm, type="response") != XY.train$Y)
      
      all.nnets[[l]] = wh.rfm
      all.SSEs[l] = SSE.nnet
      
      ### Get model SSE
      # SSE.nnet = fit.nnet$value
      
      ### Store model and its SSE
      # all.nnets[[l]] = fit.nnet
      # all.SSEs[l] = SSE.nnet
    }
    
    ### Get best fit using current parameter values
    ind.best = which.max(all.SSEs)
    fit.nnet.best = all.nnets[[ind.best]]
    
    ### Get predictions and MSPE, then store MSPE
    # pred.nnet = predict(fit.nnet.best, X.valid)
    # MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
    MSPE.nnet = mean(predict(fit.nnet.best, type="response") != XY.valid$Y)
    CV.MSPEs[i, j] = MSPE.nnet # Be careful with indices for CV.MSPEs
    
    
    # CV.MSPEs[i, j] = MSPE.nnet # Be careful with indices for CV.MSPEs
  }
}
CV.MSPEs

dim(CV.MSPEs)
(mean.MSPEcv = apply(X=CV.MSPEs, MARGIN=2, FUN=mean))

V=K

(mean.MSPEcv.sd = apply(X=root.CV.MSPEs, MARGIN=2, FUN=sd))
MSPEcv.CIl = mean.MSPEcv - qt(p=.975, df=V-1)*mean.MSPEcv.sd/V
MSPEcv.CIu = mean.MSPEcv + qt(p=.975, df=V-1)*mean.MSPEcv.sd/V

round(cbind(MSPEcv.CIl, MSPEcv.CIu),2)
table = round(cbind(MSPEcv.CIl, MSPEcv.CIu,mean.MSPEcv),3)
colnames(table)[3] = "mean"
names.pars = paste0(all.pars$n.hidden,",",
                    all.pars$shrink)
names.pars
rownames(table)= names.pars
table

# all.pars$shrink)
colnames(CV.MSPEs) = names.pars

### Make boxplot
boxplot(CV.MSPEs, las = 2, main = "MSPE Boxplot")
boxplot(CV.MSPEs, las = 2, main = "Focused MSPE Boxplot",ylim=c(0.78,0.85))

### Get relative MSPEs and make boxplot
CV.RMSPEs = apply(CV.MSPEs, 1, function(W) W/min(W))
CV.RMSPEs = t(CV.RMSPEs)
boxplot(CV.RMSPEs, las = 2, main = "Relative MSPE Boxplot",ylim=c(0.99,1.02))


########### final model #########
# Suggested parameters are mtry=4, nodesize=7
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

