# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(1234,kind="Mersenne-Twister")

################## read data ###################
data <-  read.table("Data2020.csv", 
                    header=TRUE, sep=",", na.strings=" ")
dim(data)
head(data)

# shuffle data
rows <- sample(nrow(data))
data <- data[rows, ]
head(data)

########### Train model #######################

source("Helper Functions.R")
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(h2o)          # an extremely fast java-based platform
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models

data_split <- initial_split(data, prop = 0.8)
data_train <- training(data_split)
data_test  <- testing(data_split)

#### 1. GBM##################

##train GBM model # BEST !
# gbm.fit.final <- gbm(
#   formula = Y ~ .,
#   distribution = "gaussian",
#   data = data_train,
#   n.trees = 284,
#   interaction.depth = 3,
#   shrinkage = 0.1,
#   n.minobsinnode = 5,
#   bag.fraction = 0.8,
#   train.fraction = 1,
#   n.cores = NULL, # will use all cores by default
#   verbose = FALSE
# )

gbm.fit.final <- gbm(
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
RF <- randomForest(
  y         = data_train$Y, 
  x            = data_train[,2:16], 
  num.trees       = 300,
  mtry            = 5,
  min.node.size   = 9,
  sample.fraction = .55,
  importance=TRUE
)

gbm.fit.final_2 <- gbm(
  formula = Y ~ .,
  distribution = "gaussian",
  data = data_train,
  n.trees = 500,
  interaction.depth = 6,
  shrinkage = 0.025,
  # n.minobsinnode = 15,
  bag.fraction = 0.8,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
####### TEST (prediction) #########
### prediction ### 
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, data_test)
# pred <- predict(gbm.fit.final, data_test)

pred2 <- predict(RF, n.trees = RF$n.trees, data_test)
pred3 <- predict(gbm.fit.final_2, n.trees = gbm.fit.final_2$n.trees, data_test)


# 
# caret::RMSE(pred, data_test$Y)
MSPE = get.MSPE(data_test$Y, pred) # Our helper function
MSPE
MSPE = get.MSPE(data_test$Y, pred2) # Our helper function
MSPE
MSPE = get.MSPE(data_test$Y, pred3) # Our helper function
MSPE



########## CREATE new prediction (result) ################
# read data for training
data <-  read.table("Data2020.csv", 
                    header=TRUE, sep=",", na.strings=" ")
# shuffle data
rows <- sample(nrow(data))
data <- data[rows, ]

# build model
gbm.fit.final <- gbm(
  formula = Y ~ .,
  distribution = "gaussian",
  data = data,
  n.trees = 213,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 15,
  bag.fraction = 0.8,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)
# RF <- randomForest(
#   y         = data$Y, 
#   x            = data[,2:16], 
#   num.trees       = 300,
#   mtry            = 5,
#   min.node.size   = 9,
#   sample.fraction = .55,
#   importance=TRUE
# )

# read data for prediction
data_test <-  read.table("Data2020testX.csv", 
                         header=TRUE, sep=",", na.strings=" ")
predictions=predict(gbm.fit.final, data_test, n.trees = gbm.fit.final$n.trees)
# predictions=predict(gbm.fit.final_2, data_test, n.trees = gbm.fit.final_2$n.trees)

# predictions=predict(RF, data_test)
predictions

write.table(predictions, 'prediction.csv', sep = ",", row.names = F, col.names =F)















######### XGboost ############
#### H2O #####
# h2o.no_progress()
# h2o.init(max_mem_size = "5g")
# 
# 
# data_split <- initial_split(data, prop = 0.8)
# data_train <- training(data_split)
# data_test  <- testing(data_split)
# # create feature names
# y <- "Y"
# x <- setdiff(names(data_train), y)
# 
# # turn training set into h2o object
# train.h2o <- as.h2o(data_train)
# test.h2o <- as.h2o(data_test)
# 
# 
# h2o.final <- h2o.gbm(
#   x = x,
#   y = y,
#   training_frame = train.h2o,
#   nfolds = 5,
#   ntrees = 10000,
#   learn_rate = 0.05,
#   learn_rate_annealing = 0.99,
#   max_depth = 5,
#   min_rows = 5,
#   sample_rate = 0.75,
#   col_sample_rate = 0.9,
#   stopping_rounds = 10,
#   stopping_tolerance = 0,
#   seed = 123
# )
# 
# h2o.final@parameters$ntrees
# h2o.rmse(h2o.final, xval = TRUE)
# 
# h2o.performance(model = h2o.final, newdata = test.h2o)
# h2o.pred=h2o.predict(h2o.final, newdata = test.h2o)
# h2o.pred=as.vector(h2o.pred$predict)
# 
# MSPE.h2o = get.MSPE(data_test$Y, h2o.pred) # Our helper function
# 
# 









#############################
rm(list=ls(all=TRUE))

library(gbm)

# prostate <-  read.table("Prostate.csv", 
                        # header=TRUE, sep=",", na.strings=" ")
prostate <-  read.table("Data2020.csv", 
                    header=TRUE, sep=",", na.strings=" ")
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
        pro.gbm <- gbm(data=pro.train, Y~., distribution="gaussian", 
                       n.trees=trees, interaction.depth=d, shrinkage=s, 
                       bag.fraction=0.8)
        treenum = min(trees, 2*gbm.perf(pro.gbm, method="OOB", plot.it=FALSE))
        opt.tree[counter,qq] = treenum
        preds = predict(pro.gbm, newdata=pro.test, n.trees=treenum)
        gb.cv[counter,qq] = mean((preds - pro.test$Y)^2)
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
(sqrt.mean.cv = sqrt(apply(gb.cv, 1, mean)))
(mean.cv = apply(gb.cv, 1, mean))

min.cv = apply(gb.cv, 2, min)

boxplot(sqrt(gb.cv), use.cols=FALSE, las=2)
boxplot(gb.cv, use.cols=FALSE, las=2)

# boxplot(sqrt(t(gb.cv)/min.cv), use.cols=TRUE, las=2, 
        # main="GBM Fine-Tuning Variables and Node Sizes")
boxplot(t(gb.cv)/min.cv, use.cols=TRUE, las=2, 
        main="GBM Fine-Tuning Variables and Node Sizes")

##############################################################
# Refit Final Model

pro.opt <- gbm(data=prostate, Y~., distribution="gaussian", 
               n.trees=500, interaction.depth=6, shrinkage=0.025, 
               bag.fraction=0.8)

# Variable Importance
# x11(h=7, w=6)
summary(pro.opt)


# 6 interaction depth, 0.025 shrinkage
