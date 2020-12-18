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
rm(list=ls(all=TRUE))

set.seed(46685326,kind="Mersenne-Twister")

library(h2o)
library(dplyr)

h2o.init(nthreads=-1)
df_original <-  read.csv("P2Data2020.csv")
# delete some column
# df_original[,c("X3","X5","X13",'X15')] <- list(NULL)
# df<-data.matrix(df)
# names(df_original)[names(df_original) == "X16"] <- "X3"
# names(df_original)[names(df_original) == "X14"] <- "X5"

head(df_original,2)
df <- as.h2o(df_original)

# df <- h2o.importFile(path = "P2Data2020.csv")
response <- "Y"
df[[response]] <- as.factor(df[[response]])           
predictors <- setdiff(names(df), c(response, "name")) 

splits <- h2o.splitFrame(
  data = df, 
  ratios = c(0.8,0.1),   ## only need to specify 2 fractions, the 3rd is implied
  destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
)
train <- splits[[1]]
valid <- splits[[2]]
test  <- splits[[3]]

gbm <- h2o.gbm(x = predictors,
                    y = response,
                    nfolds = 5,
                    seed = 1111,
                    keep_cross_validation_predictions = TRUE,
                    training_frame = train)
# perf <- h2o.performance(gbm)
pred.train <- h2o.predict(gbm, newdata = train)
pred.valid <- h2o.predict(gbm, newdata = valid)
pred.test <- h2o.predict(gbm, newdata = test)

(misclass.train <- mean(ifelse(pred.train$predict == train$Y, yes=0, no=1)))
(misclass.valid <- mean(ifelse(pred.valid$predict == valid$Y, yes=0, no=1)))
(misclass.test <- mean(ifelse(pred.test$predict == test$Y, yes=0, no=1)))

gbm <- h2o.gbm(
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  ## more trees is better if the learning rate is small enough 
  ## here, use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  ## smaller learning rate is better (this is a good value for most datasets, but see below for annealing)
  learn_rate=0.01,                                                         
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, 
  ## sample 80% of rows per tree
  sample_rate = 0.8,                                                       
  ## sample 80% of columns per split
  col_sample_rate = 0.8,                                                   
  ## fix a random number generator seed for reproducibility
  seed = 1234,                                                             
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10                                                 
)
pred.train <- h2o.predict(gbm, newdata = train)
pred.valid <- h2o.predict(gbm, newdata = valid)
pred.test <- h2o.predict(gbm, newdata = test)

(misclass.train <- mean(ifelse(pred.train$predict == train$Y, yes=0, no=1)))
(misclass.valid <- mean(ifelse(pred.valid$predict == valid$Y, yes=0, no=1)))
(misclass.test <- mean(ifelse(pred.test$predict == test$Y, yes=0, no=1)))


###
# hyper_params = list( max_depth = seq(1,29,2) )
# # hyper_params = list( max_depth = c(4,6,8,12,16,20) ) ##faster for larger datasets
# grid <- h2o.grid(
#   ## hyper parameters
#   hyper_params = hyper_params,
#   
#   ## full Cartesian hyper-parameter search
#   search_criteria = list(strategy = "Cartesian"),
#   
#   ## which algorithm to run
#   algorithm="gbm",
#   
#   ## identifier for the grid, to later retrieve it
#   grid_id="depth_grid",
#   
#   ## standard model parameters
#   x = predictors, 
#   y = response, 
#   training_frame = train, 
#   validation_frame = valid,
#   
#   ## more trees is better if the learning rate is small enough 
#   ## here, use "more than enough" trees - we have early stopping
#   ntrees = 10000,                                                            
#   
#   ## smaller learning rate is better
#   ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
#   learn_rate = 0.05,                                                         
#   
#   ## learning rate annealing: learning_rate shrinks by 1% after every tree 
#   ## (use 1.00 to disable, but then lower the learning_rate)
#   learn_rate_annealing = 0.99,                                               
#   
#   ## sample 80% of rows per tree
#   sample_rate = 0.8,                                                       
#   ## sample 80% of columns per split
#   col_sample_rate = 0.8, 
#   
#   ## fix a random number generator seed for reproducibility
#   seed = 1234,                                                             
#   
#   ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
#   stopping_rounds = 5,
#   stopping_tolerance = 1e-4,
#   stopping_metric='misclassification',
#   ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
#   score_tree_interval = 10                                                
# )
# ## by default, display the grid search results sorted by increasing logloss (since this is a classification task)
# # grid                                                                       
# ## sort the grid models by decreasing AUC
# sortedGrid <- h2o.getGrid("depth_grid", sort_by="err", decreasing = FALSE)    
# sortedGrid

## find the range of max_depth for the top 5 models
# topDepths = sortedGrid@summary_table$max_depth[1:5]                       
# minDepth = min(as.numeric(topDepths))
# maxDepth = max(as.numeric(topDepths))
minDepth = 6
maxDepth =12


hyper_params = list( 
  ## restrict the search to the range of max_depth established above
  max_depth = seq(minDepth,maxDepth,1),                                      
  ## search a large space of row sampling rates per tree
  sample_rate = seq(0.2,1,0.01),                                             
  ## search a large space of column sampling rates per split
  col_sample_rate = seq(0.2,1,0.01),                                         
  ## search a large space of column sampling rates per tree
  col_sample_rate_per_tree = seq(0.2,1,0.01),                                
  ## search a large space of how column sampling per split should change as a function of the depth of the split
  col_sample_rate_change_per_level = seq(0.9,1.1,0.01),                      
  ## search a large space of the number of min rows in a terminal node
  min_rows = 2^seq(0,log2(nrow(train))-1,1),                                 
  ## search a large space of the number of bins for split-finding for continuous and integer columns
  nbins = 2^seq(4,10,1),                                                     
  ## search a large space of the number of bins for split-finding for categorical columns
  nbins_cats = 2^seq(4,12,1),                                                
  ## search a few minimum required relative error improvement thresholds for a split to happen
  min_split_improvement = c(0,1e-8,1e-6,1e-4),                               
  ## try all histogram types (QuantilesGlobal and RoundRobin are good for numeric columns with outliers)
  histogram_type = c("UniformAdaptive","QuantilesGlobal","RoundRobin")       
)
search_criteria = list(
  ## Random grid search
  strategy = "RandomDiscrete",      
  
  ## limit the runtime to 60 minutes
  max_runtime_secs = 3600,         
  
  ## build no more than 100 models
  max_models = 100,                  
  
  ## random number generator seed to make sampling of parameter combinations reproducible
  seed = 1234,                        
  
  ## early stopping once the leaderboard of the top 5 models is converged to 0.1% relative difference
  stopping_rounds = 5,                
  stopping_metric = "misclassification",
  stopping_tolerance = 1e-3
)
grid <- h2o.grid(
  ## hyper parameters
  hyper_params = hyper_params,
  
  ## hyper-parameter search configuration (see above)
  search_criteria = search_criteria,
  
  ## which algorithm to run
  algorithm = "gbm",
  
  ## identifier for the grid, to later retrieve it
  grid_id = "final_grid", 
  
  ## standard model parameters
  x = predictors, 
  y = response, 
  training_frame = train, 
  validation_frame = valid,
  
  ## more trees is better if the learning rate is small enough
  ## use "more than enough" trees - we have early stopping
  ntrees = 10000,                                                            
  
  ## smaller learning rate is better
  ## since we have learning_rate_annealing, we can afford to start with a bigger learning rate
  learn_rate = 0.05,                                                         
  
  ## learning rate annealing: learning_rate shrinks by 1% after every tree 
  ## (use 1.00 to disable, but then lower the learning_rate)
  learn_rate_annealing = 0.99,                                               
  
  ## early stopping based on timeout (no model should take more than 1 hour - modify as needed)
  max_runtime_secs = 3600,                                                 
  
  ## early stopping once the validation AUC doesn't improve by at least 0.01% for 5 consecutive scoring events
  stopping_rounds = 5, stopping_tolerance = 1e-4, stopping_metric = "misclassification", 
  
  ## score every 10 trees to make early stopping reproducible (it depends on the scoring interval)
  score_tree_interval = 10,                                                
  
  ## base random number generator seed for each model (automatically gets incremented internally for each model)
  seed = 1234                                                             
)
## Sort the grid models by AUC
sortedGrid <- h2o.getGrid("final_grid", sort_by = "err", decreasing = FALSE)    
sortedGrid

gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
pred.train <- h2o.predict(gbm, newdata = train)
pred.valid <- h2o.predict(gbm, newdata = valid)
pred.test <- h2o.predict(gbm, newdata = test)


(misclass.train <- mean(ifelse(pred.train$predict == train$Y, yes=0, no=1)))
(misclass.valid <- mean(ifelse(pred.valid$predict == valid$Y, yes=0, no=1)))
(misclass.test <- mean(ifelse(pred.test$predict == test$Y, yes=0, no=1)))

gbm@parameters

model <- do.call(h2o.gbm,
                 ## update parameters in place
                 {
                   p <- gbm@parameters
                   p$model_id = NULL          ## do not overwrite the original grid model
                   p$training_frame = df      ## use the full dataset
                   p$validation_frame = NULL  ## no validation frame
                   p$nfolds = 5               ## cross-validation
                   p
                 }
)
model@model$cross_validation_metrics_summary

for (i in 1:5) {
  gbm <- h2o.getModel(sortedGrid@model_ids[[i]])
  cvgbm <- do.call(h2o.gbm,
                   ## update parameters in place
                   {
                     p <- gbm@parameters
                     p$model_id = NULL          ## do not overwrite the original grid model
                     p$training_frame = df      ## use the full dataset
                     p$validation_frame = NULL  ## no validation frame
                     p$nfolds = 5               ## cross-validation
                     p
                   }
  )
  print(gbm@model_id)
  print(cvgbm@model$cross_validation_metrics_summary[2,]) ## Pick out the "AUC" row
}

#
gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
preds <- h2o.predict(gbm, test)
head(preds)
(misclass.test <- mean(ifelse(preds$predict == test$Y, yes=0, no=1)))
#

# splits <- h2o.splitFrame(
#   data = df, 
#   ratios = c(0.6,0.1),   ## only need to specify 2 fractions, the 3rd is implied
#   destination_frames = c("train.hex", "valid.hex", "test.hex"), seed = 1234
# )
# train <- splits[[1]]
# valid <- splits[[2]]
# test  <- splits[[3]]











##################################
###############
# RF

library(randomForest)
wh.rf <- randomForest(data=set1, Y~., 
                      importance=TRUE, keep.forest=TRUE)
wh.rf             # more useful here

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
                             mtry=m, nodesize=ns)
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
wh.rf.tun <- randomForest(data=set1, Y~., mtry=4, nodesize=7,
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



################################
library(tidyverse)
library(caret)
library(xgboost)

## XGBoost
# Fit the model on the training set
# set.seed(123)
set.seed(46685326,kind="Mersenne-Twister")

model <- train(
  Y ~., data = set1, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
model$bestTune
model
# Make predictions on the test data
predicted.classes <- model %>% predict(set2)
head(predicted.classes)

# Compute model prediction accuracy rate
mean(predicted.classes == set2$Y)

varImp(model)
# X3,X5,X15,X13
# X15, X4

####
set.seed(46685326,kind="Mersenne-Twister")

xgb <- xgboost(data = data.matrix(set1[,-1]), 
               label = set1[,'Y'], 
               eta = 0.113,
               max_depth =3, 
               nround=300, 
               subsample = 0.9888,
               colsample_bytree = 0.614,
               eval_metric = "merror",
               num_class = 6,
               min_child_weight=3,
               nthread = 4
)

library(dplyr)
# convert Y categories to numerical
must_convert<-sapply(set2,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
Y<-sapply(set2[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
set2_new<-cbind(set2[,!must_convert],Y)        # complete data.frame with all variables put together
head(set2_new,5)

y_pred <- predict(xgb, data.matrix(set2[,-1]))
mean(y_pred == set2_new$Y)

##################
# Take start time to measure time of random search algorithm
start.time <- Sys.time()

# Create empty lists
lowest_error_list = list()
parameters_list = list()

# Create 10,000 rows with random hyperparameters
set.seed(46685326,kind="Mersenne-Twister")
for (iter in 1:10000){
  param <- list(booster = "xgbtree",
                max_depth = sample(3:10, 1),
                eta = runif(1, .01, .3),
                subsample = runif(1, .7, 1),
                colsample_bytree = runif(1, .6, 1),
                min_child_weight = sample(0:10, 1)
  )
  parameters <- as.data.frame(param)
  parameters_list[[iter]] <- parameters
}

# Create object that contains all randomly created hyperparameters
parameters_df = do.call(rbind, parameters_list)
head(parameters_df,10)
# Use randomly created parameters to create 10,000 XGBoost-models

# Prepare matrix for XGBoost algorithm
training_matrix <-model.matrix(Y ~.-1, data = set1)
validation_matrix <-model.matrix(Y ~.-1, data = set2)
dtrain <- xgb.DMatrix(data = training_matrix, label = set1$Y) 
dvalid <- xgb.DMatrix(data = validation_matrix, label = set2$Y)

for (row in 1:nrow(parameters_df)){
  set.seed(46685326,kind="Mersenne-Twister")
  mdcv <- xgb.train(data = dtrain, 
                    booster = "gbtree",
                    max_depth = parameters_df$max_depth[row],
                    eta = parameters_df$eta[row],
                    subsample = parameters_df$subsample[row],
                    colsample_bytree = parameters_df$colsample_bytree[row],
                    min_child_weight = parameters_df$min_child_weight[row],
                    nrounds= 300,
                    eval_metric = "merror",
                    early_stopping_rounds= 30,
                    num_class = 6,
                    print_every_n = 500,
                    watchlist = list(train= dtrain, 
                                      val= dvalid )
  )
  # mdcv
  lowest_error <- as.data.frame(1 - min(mdcv$evaluation_log$val_merror))
  lowest_error_list[[row]] <- lowest_error
}

# Create object that contains all accuracy's
lowest_error_df = do.call(rbind, lowest_error_list)

# Bind columns of accuracy values and random hyperparameter values
randomsearch = cbind(lowest_error_df, parameters_df)
# randomsearch
# Quickly display highest accuracy
max(randomsearch$`1 - min(mdcv$evaluation_log$val_merror)`)

# Stop time and calculate difference
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# randomsearch
randomsearch=randomsearch[order(randomsearch$`1 - min(mdcv$evaluation_log$val_merror)`, decreasing = TRUE),]
head(randomsearch,10)
# write_csv(randomsearch, "randomsearch.csv")

# 
# ###########

set.seed(46685326,kind="Mersenne-Twister")
# Prepare matrix for XGBoost algorithm
training_matrix <-model.matrix(Y ~.-1, data = set1)
validation_matrix <-model.matrix(Y ~.-1, data = set2)
dtrain <- xgb.DMatrix(data = training_matrix, label = set1$Y) 
dvalid <- xgb.DMatrix(data = validation_matrix, label = set2$Y)

library(dplyr)
# convert Y categories to numerical
must_convert<-sapply(set2,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
Y<-sapply(set2[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
set2_new<-cbind(set2[,!must_convert],Y)        # complete data.frame with all variables put together
head(set2_new,5)

# randomsearch= rd

randomsearch2 <-  read.table("randomsearch10000_new.csv", 
                         header=TRUE, sep=",", na.strings=" ")
randomsearch= randomsearch2

set.seed(46685326,kind="Mersenne-Twister")

params <- list(booster = "gbtree",
              max_depth = randomsearch[1,]$max_depth,
              eta = randomsearch[1,]$eta,
              subsample = randomsearch[1,]$subsample,
              colsample_bytree = randomsearch[1,]$colsample_bytree,
              min_child_weight = randomsearch[1,]$min_child_weight)
params
xgb_tuned <- xgb.train(params = params,
                       data = dtrain,
                       nrounds =1000,
                       print_every_n = 10,
                       eval_metric = "merror",
                       num_class = 6,
                       early_stopping_rounds = 30,
                       watchlist = list(train= dtrain, val= dvalid))

y_pred <- predict(xgb_tuned, dvalid)
mean(y_pred == set2_new$Y)

table(set2_new[,'Y'],y_pred, dnn=c("Observed","Predicted"))

#

data_test <-  read.table("P2Data2020testX.csv", 
                         header=TRUE, sep=",", na.strings=" ")
data_test[,c("X3","X5","X13",'X15')] <- list(NULL)

test_matrix <-model.matrix( ~.-1, data = data_test)
dtest <- xgb.DMatrix(data = test_matrix)

predictions=predict(xgb_tuned, dtest)
library(car)
result=recode(predictions, "1='A'; 2='B';3='C';4='D';5='E';")
result= as.factor(result)
result

# write.table(result, 'prediction.csv', sep = ",", row.names = F, col.names =F)


predictions=predict(xgb_tuned, dvalid)
set2$Y
colnames(dtrain)
colnames(dtest)
#
























###############################################
################ NN ###############
# 
# cor = round(cor(df[,-1]),3)
# cor
set.seed(46685326,kind="Mersenne-Twister")

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}
set1.rescale <- data.frame(cbind(rescale(set1[,-1], set1[,-1]), Y=set1$Y))
set2.rescale <- data.frame(cbind(rescale(set2[,-1], set1[,-1]), Y=set2$Y))

summary(set1.rescale)
summary(set2.rescale)
# str(set1.rescale)
library(nnet)
###### 1.b
mod.fit <- multinom(data=set1.rescale, formula=Y ~ ., 
                    trace=TRUE, maxit = 1500)
summary(mod.fit)

# Adding a function to perform LR Tests.  Legitimate here since I am fitting one model
library(car)
Anova(mod.fit)

# Misclassification Errors
pred.class.1 <- predict(mod.fit, newdata=set1.rescale) 
# type="class")
pred.class.2 <- predict(mod.fit, newdata=set2.rescale) 
# type="class")
(mul.misclass.train <- mean(ifelse(pred.class.1 == set1$Y, 
                                   yes=0, no=1)))
(mul.misclass.test <- mean(ifelse(pred.class.2 == set2$Y, 
                                  yes=0, no=1)))

# Estimated probabilities for test set
pred.probs.2 <- predict(mod.fit, newdata=set2.rescale, 
                        type="probs")
round(head(pred.probs.2),3)

# Test set confusion matrix
# table(set2$class, pred.class.2, dnn=c("Obs","Pred"))
table(pred.class.2,set2$Y, dnn=c("Predicted","Observed"))

#####################################
################ glmnet #########
library(glmnet)
# "Optimal" LASSO Fit
logit.cv <- cv.glmnet(x=as.matrix(within(set1.rescale,rm('Y'))), 
                      y=set1.rescale[,'Y'], family="multinomial")
logit.cv
# plot(logit.cv)

## Find nonzero lasso coefficients
c <- coef(logit.cv,s=logit.cv$lambda.min) 

lascv.pred.train <- predict(object=logit.cv, type="class", 
                            s=logit.cv$lambda.min, 
                            newx=as.matrix(within(set1.rescale,rm('Y')))  )
lascv.pred.test <- predict(logit.cv, type="class", 
                           s=logit.cv$lambda.min, 
                           newx=as.matrix(within(set2.rescale,rm('Y')))   )
(lascvmisclass.train <- 
    mean(ifelse(lascv.pred.train == set1$Y, yes=0, no=1)))
(lascvmisclass.test <- 
    mean(ifelse(lascv.pred.test == set2$Y, yes=0, no=1)))

###############################
######## LDA , QDA
library(MASS)

set1s <- apply(within(set1,rm('Y')), 2, scale)
set1s <- data.frame(set1s,Y=set1$Y)
lda.fit.s <- lda(data=set1s, Y~.)
lda.fit.s

# Fit gives identical results as without scaling, but 
#  can't interpret means
lda.fit <- lda(x=within(set1,rm('Y')), grouping=set1$Y)
lda.fit


# Calculate in-sample and out-of-sample misclassification error
lda.pred.train <- predict(lda.fit, newdata=within(set1,rm('Y')))$class
lda.pred.test <- predict(lda.fit, newdata=within(set2,rm('Y')))$class
(lmisclass.train <- mean(ifelse(lda.pred.train == set1$Y, yes=0, no=1)))
(lmisclass.test <- mean(ifelse(lda.pred.test == set2$Y, yes=0, no=1)))

# Test set confusion matrix
table(set2$Y, lda.pred.test, dnn=c("Obs","Pred"))

######## 4.
qda.fit <- qda(data=set1, Y ~ .)
qda.fit

qda.pred.train <- predict(qda.fit, newdata=set1)$class
qda.pred.test <- predict(qda.fit, newdata=set2)$class
(qmisclass.train <- mean(ifelse(qda.pred.train == set1$Y, yes=0, no=1)))
(qmisclass.test <- mean(ifelse(qda.pred.test == set2$Y, yes=0, no=1)))




