# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(2928893,kind="Mersenne-Twister")
# set.seed(123,kind="Mersenne-Twister") 

########### Project 1 ########
data <-  read.table("Data2020.csv", 
                    header=TRUE, sep=",", na.strings=" ")
dim(data)
head(data)

# shuffle data
rows <- sample(nrow(data))
data <- data[rows, ]
head(data)

#######################
source("Helper Functions.R")
library(pls) # pls
library(MASS) # for ridge
library(glmnet) # for LASSO
library(mgcv) # gam
library(rpart)
source("Helper Functions.R")
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(h2o)          # a java-based platform

h2o.no_progress()
h2o.init(max_mem_size = "5g")
#######################

V= 10

# Let's do 10-fold CV
n = nrow(data) #store sample size for easy calculations later

folds = floor((sample.int(n)-1)*V/n) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=15)
colnames(MSPEs.cv) = c('Least-Sqr',"Ridge", "LASSO-min","LASSO-1SE",
                       'Hybrid-Step','PLS','GBM','PPR',
                       'Tree','Tree-min','Tree-1se','GBM-2',
                       'H2O-GBM','H2O-GBM2','RF')
MSPEs.PPR.term = matrix(NA, nrow=V, ncol=1)
colnames(MSPEs.PPR.term) = c('PPR-terms')

for(v in 1:V){
  print(paste0(v, " of ", V))
  
  col_train = 2:16 # col_train except 1
  # col_train = append(append(c(2:7),c(9)),c(12:16))  # exclude 8,10,11
  
  data_train = data[folds != v,append(c(1),c(col_train))]
  data_valid = data[folds == v,col_train]
  # data_valid = data[folds==v,2:16]
  
  ### Leaste sqrt ###
  fit.lm = lm(Y ~ ., data = data_train)
  pred.lm = predict(fit.lm, newdata=data_valid)
  MSPEs.cv[v,1] = mean((data[folds==v,"Y"] - pred.lm)^2)
  
  ### Ridge ###
  ridge1 <- lm.ridge(Y ~., lambda = seq(0, 100, .05), data= data_train )
  (coef.ri.best1 = coef(ridge1)[which.min(ridge1$GCV),])
  pred.ri1 = as.matrix(cbind(1,data_valid)) %*% coef.ri.best1
  MSPEs.cv[v,2] = mean((data[folds==v,1]-pred.ri1)^2)
  
  ### Lasso-min,1se ###
  x.1 <- as.matrix(data[folds!=v,c(col_train)])
  y.1 <- data[folds!=v,1]
  x.2 <- as.matrix(data[folds==v,c(col_train)])
  y.2 <- data[folds==v,1]
  cv.lasso <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
  pred.las.min <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.min)
  pred.las.1se <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.1se)
  MSPEs.cv[v,3] <- mean((y.2 - pred.las.min)^2)
  MSPEs.cv[v,4] <- mean((y.2 - pred.las.1se)^2)
  
  ### Step ###
  initial <- lm(data=data_train,
                formula=Y~ 1)
  final <- lm(data=data_train,
              formula=Y~.)
  n1 = nrow(data_train)
  step <- step(object=initial, scope=list(upper=final),
               k = log(n1))
  pred = predict(step,newdata=data_valid)
  MSPEs.cv[v,5] = mean((data[folds==v,"Y"] - pred)^2)
  
  ### PLS ###
  mod.pls = plsr(Y ~.,data= data_train, validation="CV")
  mp.cv = mod.pls$validation
  Opt.Comps= which.min(sqrt(mp.cv$PRESS/nrow(data_train)))
  pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=data_valid)
  MSPEs.cv[v,6] = mean((data[folds==v,1] - pred.pls)^2)
  
  #### GBM ###########
  gbm.fit.final <- gbm(
    formula = Y ~ .,
    distribution = "gaussian",
    data = data_train,
    n.trees = 284,
    interaction.depth = 3,
    shrinkage = 0.1,
    n.minobsinnode = 5,
    bag.fraction = 0.8,
    train.fraction = 1,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  pred.gbm <- predict(gbm.fit.final ,newdata=data_valid)
  MSPEs.cv[v,7] = mean((data[folds==v,1] - pred.gbm)^2)
  
  
  ### PPR ###
  max.terms = 5
  ### To fit PPR, we need to do another round of CV. This time, do 5-fold
  K.ppr = 5
  n.train = nrow(data_train)
  folds.ppr = get.folds(n.train, K.ppr)
  MSPEs.ppr = array(0, dim = c(max.terms, K.ppr))
  colnames(MSPEs.ppr) = c("ppr1", "ppr2", "ppr3",
                          'ppr4','ppr5')
  for(j in 1:K.ppr){
    train.ppr = data_train[folds.ppr != j,]
    # valid.ppr = data_train[folds.ppr == j,2:16]
    # valid.ppr = data_train[folds.ppr == j,col_train]
    valid.ppr = data_train[folds.ppr == j,] 
    Y.valid.ppr = data_train[folds.ppr == j,1]
    
    for(l in 1:max.terms){
      fit.ppr = ppr(Y ~ ., data = train.ppr, 
                    max.terms = max.terms, nterms = l, sm.method = "gcvspline")
      pred.ppr = predict(fit.ppr, valid.ppr)
      MSPE.ppr = get.MSPE(Y.valid.ppr, pred.ppr) # Our helper function
      MSPEs.ppr[j, l] = MSPE.ppr
    }
  }
  
  ave.MSPE.ppr = apply(MSPEs.ppr, 2, mean)
  best.terms = which.min(ave.MSPE.ppr)
  MSPEs.PPR.term[v,1] = best.terms
  
  ### Fit PPR on the whole CV training set using the optimal number of terms
  fit.ppr.best = ppr(Y ~ ., data = data_train,
                     max.terms = max.terms, nterms = best.terms, sm.method = "gcvspline")
  pred.ppr.best = predict(fit.ppr.best, data_valid)
  MSPE.ppr.best = get.MSPE(data[folds == v,1], pred.ppr.best) # Our helper function
  
  MSPEs.cv[v, 8] = MSPE.ppr.best
  
  ### Regression tree ####
  pr.tree2 <- rpart(Y ~. , method="anova", data=data_train, cp=0)
  pr.tree2$cptable[,c(2:5,1)]
  cpt <- pr.tree2$cptable
  
  # Find location of minimum error
  minrow <- which.min(cpt[,4])
  minrow
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
  
  pred.tree <- predict(pr.tree2 ,newdata=data_valid)
  MSPEs.cv[v,9] = mean((data[folds==v,1] - pred.tree)^2)
  pred.prune.min <- predict(pr2.prune.min ,newdata=data_valid)
  MSPEs.cv[v,10] = mean((data[folds==v,1] - pred.prune.min)^2)
  pred.prune.1se <- predict(pr2.prune.1se ,newdata=data_valid)
  MSPEs.cv[v,11] = mean((data[folds==v,1] - pred.prune.1se)^2)
  
  #### GBM2 ###########
  gbm2.fit.final <- gbm(
    formula = Y ~ .,
    distribution = "gaussian",
    data = data_train,
    n.trees = 213,
    interaction.depth = 3,
    shrinkage = 0.1,
    n.minobsinnode = 15,
    bag.fraction = 0.8,
    train.fraction = 1,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  pred.gbm2 <- predict(gbm2.fit.final ,newdata=data_valid)
  MSPEs.cv[v,12] = mean((data[folds==v,1] - pred.gbm2)^2)
  
  # ##### H2O gbm ########
  # create feature names
  y <- "Y"
  x <- setdiff(names(data_train), y)
  
  # turn training set into h2o object
  train.h2o <- as.h2o(data_train)
  test.h2o <- as.h2o(data_valid)
  
  h2o.final <- h2o.gbm(
    x = x,
    y = y,
    training_frame = train.h2o,
    nfolds = 5,
    ntrees = 10000,
    learn_rate = 0.05,
    learn_rate_annealing = 0.99,
    max_depth = 5,
    min_rows = 5,
    sample_rate = 0.75,
    col_sample_rate = 0.9,
    stopping_rounds = 10,
    stopping_tolerance = 0,
    seed = 123
  )
  h2o.pred=h2o.predict(h2o.final, newdata = test.h2o)
  h2o.pred=as.vector(h2o.pred$predict)
  MSPEs.cv[v,13] = mean((data[folds==v,1] - h2o.pred)^2)
  
  # ##### H2O gbm2 ###################### 
  h2o.final2 <- h2o.gbm(
    x = x,
    y = y,
    training_frame = train.h2o,
    nfolds = 5,
    ntrees = 10000,
    learn_rate = 0.05,
    learn_rate_annealing = 1,
    max_depth = 3,
    min_rows = 5,
    sample_rate = 1,
    col_sample_rate = 1,
    stopping_rounds = 10,
    stopping_tolerance = 0,
    seed = 123
  )
  h2o.pred2=h2o.predict(h2o.final2, newdata = test.h2o)
  h2o.pred2=as.vector(h2o.pred2$predict)
  MSPEs.cv[v,14] = mean((data[folds==v,1] - h2o.pred2)^2)
  
  ## RF ###
  RF <- randomForest(
    y         = data_train$Y, 
    x            = data_train[,2:16], 
    num.trees       = 300,
    mtry            = 5,
    min.node.size   = 9,
    sample.fraction = .55,
    importance=TRUE
  )
  pred.RF <- predict(RF ,newdata=data[folds==v,1:16])
  MSPEs.cv[v,15] = mean((data[folds==v,1] - pred.RF)^2)
}
MSPEs.cv
MSPEs.cv.mean= apply(X=MSPEs.cv,MARGIN=2,FUN=mean)
MSPEs.cv.mean


par(mar=c(7,5,5,5)+.1)
boxplot(MSPEs.cv, las=2, ylim=c(1,3),
        main="MSPE \n Cross-Validation")
low.c = apply(MSPEs.cv, 1, min)
boxplot(MSPEs.cv/low.c, las=2,ylim=c(0.8,2),
        main="Relative MSPE \n Cross-Validation")
# 
# > MSPEs.cv.mean
# Least-Sqr       Ridge   LASSO-min   LASSO-1SE Hybrid-Step         PLS         GBM 
# 1.741335    1.736873    1.744621    1.851266    1.770584    1.744218    1.489358 
# PPR        Tree    Tree-min    Tree-1se       GBM-2     H2O-GBM    H2O-GBM2 
# 1.804570    2.134116    1.744076    1.828955    1.464010    1.504648    1.498780 