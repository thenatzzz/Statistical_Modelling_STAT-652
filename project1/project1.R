# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(2928893,kind="Mersenne-Twister") 

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

V= 10


# Let's do 10-fold CV
n = nrow(data) #store sample size for easy calculations later

folds = floor((sample.int(n)-1)*V/n) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=12)
colnames(MSPEs.cv) = c('Least-Sqr',"Ridge", "LASSO-min",
                       "LASSO-1SE",'Hybrid-Step','PLS',
                       'GBM','PPR','Tree','Tree-min','Tree-1se','GBM-2')
MSPEs.PPR.term = matrix(NA, nrow=V, ncol=1)
colnames(MSPEs.PPR.term) = c('PPR-terms')

for(v in 1:V){
  ### Print a status update
  print(paste0(v, " of ", V))
  
  col_train = 2:16 # col_train except 1
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
  # x.1 <- as.matrix(data[folds!=v,c(2:16)])
  x.1 <- as.matrix(data[folds!=v,c(col_train)])
  y.1 <- data[folds!=v,1]
  # x.2 <- as.matrix(data[folds==v,c(2:16)])
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
  # MSPEs.cv[v,1]=Opt.Comps
  pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=data_valid)
  MSPEs.cv[v,6] = mean((data[folds==v,1] - pred.pls)^2)
  
  ### GAM ####
  # gam.all <- gam(data=data_train,
  #                formula=Y ~s(Temp)+s(Wind) + s(Solar.R) + s(TWcp) + s(TWrat) ,
  #                family=gaussian(link=identity))
  # form <- as.formula(paste0("Y~",paste0("s(X",1:15,")",collapse="+")))
  # gam.all <- gam(data=data_train,
  #                formula=form ,
  #                family=gaussian(link=identity))
  # pred.gam <- predict(gam.all ,newdata=data_valid)
  # MSPEs.cv[v,7] = mean((data[folds==v,1] - pred.gam)^2)
  
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
  # print(folds.ppr)
  MSPEs.ppr = array(0, dim = c(max.terms, K.ppr))
  colnames(MSPEs.ppr) = c("ppr1", "ppr2", "ppr3",
                          'ppr4','ppr5')
  for(j in 1:K.ppr){
    train.ppr = data_train[folds.ppr != j,]
    # valid.ppr = data_train[folds.ppr == j,2:16]
    valid.ppr = data_train[folds.ppr == j,col_train]
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
  print(ave.MSPE.ppr)
  best.terms = which.min(ave.MSPE.ppr)
  print(best.terms)
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
  
  # The code below shows how to select the tuning parameter using
  #   either the +1SE or the true min CV error
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
    n.trees = 28,
    interaction.depth = 5,
    shrinkage = 0.3,
    n.minobsinnode = 15,
    bag.fraction = 0.8,
    train.fraction = 1,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  pred.gbm2 <- predict(gbm2.fit.final ,newdata=data_valid)
  MSPEs.cv[v,12] = mean((data[folds==v,1] - pred.gbm2)^2)
  
 
  
  
}
MSPEs.cv
MSPEs.cv.mean= apply(X=MSPEs.cv,MARGIN=2,FUN=mean)
MSPEs.cv.mean
# Least-Sqr       Ridge   LASSO-min   LASSO-1SE Hybrid-Step         PLS     GAM-all 
# 1.759171    1.752084    1.761781    1.857792    1.794165    1.751907          NA 
# PPR        Tree    Tree-min    Tree-1se 
# 1.742359    2.143002    1.710911    1.787533 

rootMSPEs.cv =sqrt(MSPEs.cv) 
RMSPEs.cv.mean= apply(X=rootMSPEs.cv,MARGIN=2,FUN=mean)
RMSPEs.cv.mean
# Least-Sqr       Ridge   LASSO-min   LASSO-1SE Hybrid-Step         PLS     GAM-all 
# 1.321475    1.318837    1.322540    1.358510    1.335388    1.318986          NA 
# PPR        Tree    Tree-min    Tree-1se 
# 1.315253    1.456455    1.301272    1.332236 
par(mar=c(7,5,5,5)+.1)

boxplot(MSPEs.cv, las=2, ylim=c(0,3),
        main="MSPE \n Cross-Validation")
# low.c = apply(MSPEs.cv, 1, min) 
# boxplot(MSPEs.cv/low.c, las=2,ylim=c(0.8,3),
#         main="Relative MSPE \n Cross-Validation")
# boxplot(MSPEs.cv/low.c, las=2,ylim=c(0.8,3),
#         main="Focused Relative MSPE \n Cross-Validation")

########################
#########################
### 75%/25% split into training and validation sets
# rm(list=ls(all=TRUE))

n = nrow(data)
n.train = floor(n*0.75)
n.valid = n - n.train

inds = c(rep(1, times = n.train), rep(2, times = n.valid))
inds.rand = inds[sample.int(n)]

data.train = data[inds.rand == 1,]
# X.train.raw = data.train[,2:6]
X.train.raw = data.train[,3:4]
Y.train = data.train[,1]

data.valid = data[inds.rand == 2,]
# X.valid.raw = data.valid[,2:6]
X.valid.raw = data.valid[,3:4]
Y.valid = data.valid[,1]


### Leaste sqrt ###
fit.lm = lm(Y ~ ., data = data.train)
pred.lm = predict(fit.lm, newdata=data.valid)
MSPEs.cv = mean((data.valid[,"Y"] - pred.lm)^2)
MSPEs.cv
summary(fit.lm)

















#####################
################ NN ###################

library(nnet)
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}
full.X.raw = data[,2:16]
full.Y = data[,1]
full.X = rescale(full.X.raw, full.X.raw)
# summary(full.X.raw)
summary(full.X)
n.hidden = 6
shrink = 0.001
# fit.nnet = nnet(y = Y.train, x = X.train, linout = TRUE, size = n.hidden,
#                 decay = shrink, maxit = 500)
fit.nnet = nnet(y = full.Y, x = full.X, linout = TRUE, size = n.hidden,
                decay = shrink, maxit = 500)
n.nnets = 20 # Number of times to re-fit

### Container for SSEs
all.SSEs = rep(0, times = 20)
all.sMSEs = rep(0, times = 20)
all.nnets = list(1:20)
for(i in 1:n.nnets){
  ### Fit model. We can set the input "trace" to FALSE to suppress the
  ### printed output from the nnet() function.
  # this.nnet = nnet(y = Y.train, x = X.train, linout = TRUE, size = n.hidden,
  # decay = shrink, maxit = 500, trace = FALSE)
  this.nnet = nnet(y = full.Y, x = full.X, linout = TRUE, size = n.hidden,
                   decay = shrink, maxit = 500, trace = FALSE)
  
  ### Get the model's SSE, sMSE
  this.SSE = this.nnet$value
  # this.sMSE = this.SSE/nrow(X.train)
  this.sMSE = this.SSE/nrow(full.X)
  
  
  ### Store results. We have to use double square brackets when storing or
  ### retrieving from a list.
  all.SSEs[i] = this.SSE
  all.nnets[[i]] = this.nnet
  all.sMSEs[i] = this.sMSE
}
all.sMSEs
# all.SSEs
min(all.sMSEs)
### Get the best model. We have to use double square brackets when storing or
### retrieving from a list.
ind.best = which.min(all.SSEs)
fit.nnet.best = all.nnets[[ind.best]]

#################
library(nnet)

rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}
### 75%/25% split into training and validation sets
n = nrow(data)
n.train = floor(n*0.75)
n.valid = n - n.train

inds = c(rep(1, times = n.train), rep(2, times = n.valid))
inds.rand = inds[sample.int(n)]

col_feature= 2:16
data.train = data[inds.rand == 1,]
X.train.raw = data.train[,col_feature]
Y.train = data.train[,1]

data.valid = data[inds.rand == 2,]
X.valid.raw = data.valid[,col_feature]
Y.valid = data.valid[,1]

X.train = rescale(X.train.raw, X.train.raw)
X.valid = rescale(X.valid.raw, X.train.raw) # Be careful with the order
# summary(X.train.raw)
# summary(X.train)
# summary(X.valid.raw)
# summary(X.valid)


M = 10 # Number of times to re-fit each model

### Define parameter values and use expand.grid() to get all combinations
all.n.hidden = c(1,3, 5,7, 9)
all.shrink = c(0.001, 0.1, 0.5, 1,2)

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
  data.train = data[folds != i,]
  X.train.raw = data.train[,-1]
  X.train = rescale(X.train.raw, X.train.raw)
  Y.train = data.train[,1]
  
  data.valid = data[folds == i,]
  X.valid.raw = data.valid[,-1]
  X.valid = rescale(X.valid.raw, X.train.raw)
  Y.valid = data.valid[,1]
  
  
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
      fit.nnet = nnet(X.train, Y.train, linout = TRUE, size = this.n.hidden,
                      decay = this.shrink, maxit = 500, trace = FALSE)
      
      ### Get model SSE
      SSE.nnet = fit.nnet$value
      
      ### Store model and its SSE
      all.nnets[[l]] = fit.nnet
      all.SSEs[l] = SSE.nnet
    }
    
    ### Get best fit using current parameter values
    ind.best = which.min(all.SSEs)
    fit.nnet.best = all.nnets[[ind.best]]
    
    ### Get predictions and MSPE, then store MSPE
    pred.nnet = predict(fit.nnet.best, X.valid)
    MSPE.nnet = get.MSPE(Y.valid, pred.nnet)
    
    CV.MSPEs[i, j] = MSPE.nnet # Be careful with indices for CV.MSPEs
  }
}
CV.MSPEs

CV.MSPEs
dim(CV.MSPEs)
(mean.MSPEcv = apply(X=CV.MSPEs, MARGIN=2, FUN=mean))
# mean.MSPEcv
# [1] 1.773157 2.598860 3.844510 2.830349 4.416297 1.726178 1.781640 1.992669 2.289797 2.299029 1.732118
# [12] 1.719073 1.780288 1.847084 1.851874 1.729562 1.708779 1.699488 1.701482 1.698946 1.727093 1.701836
# [23] 1.710038 1.714944 1.718617
V=K
root.CV.MSPEs =sqrt(CV.MSPEs) 
root.CV.MSPEs
(root.MSPEcv = apply(X=root.CV.MSPEs, MARGIN=2, FUN=mean))
# [1] 1.330726 1.580553 1.910067 1.681563 2.092939 1.313039 1.332861 1.410659 1.511588 1.514929 1.315346
# [12] 1.310438 1.333822 1.358616 1.359965 1.314331 1.306509 1.302975 1.303753 1.302734 1.313303 1.303832
# [23] 1.306962 1.308839 1.310248
(root.MSPEcv.sd = apply(X=root.CV.MSPEs, MARGIN=2, FUN=sd))
root.MSPEcv.CIl = root.MSPEcv - qt(p=.975, df=V-1)*root.MSPEcv.sd/sqrt(V)
root.MSPEcv.CIu = root.MSPEcv + qt(p=.975, df=V-1)*root.MSPEcv.sd/sqrt(V)

round(cbind(root.MSPEcv.CIl, root.MSPEcv.CIu),2)
table = round(cbind(root.MSPEcv.CIl, root.MSPEcv.CIu,root.MSPEcv),3)
colnames(table)[3] = "mean"
names.pars = paste0(all.pars$n.hidden,",",
                    all.pars$shrink)
names.pars
rownames(table)= names.pars
table

# all.pars$shrink)
colnames(root.CV.MSPEs) = names.pars

### Make boxplot
boxplot(root.CV.MSPEs, las = 2, main = "root-MSPE Boxplot")
boxplot(root.CV.MSPEs, las = 2, main = "Focused root-MSPE Boxplot",ylim=c(1.2,1.5))

### Get relative MSPEs and make boxplot
root.CV.RMSPEs = apply(root.CV.MSPEs, 1, function(W) W/min(W))
root.CV.RMSPEs = t(root.CV.RMSPEs)
boxplot(root.CV.RMSPEs, las = 2, main = "Relative root-MSPE Boxplot",ylim=c(0.8,2.5))




########################################
############################## RF ####################
# install.packages("rsample")
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform

library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%
set.seed(123)
# data_split <- initial_split(data, prop = .75)
# data_split <- initial_split(data, prop = .99)
# data.train <- training(data_split)
# data.test  <- testing(data_split)

data.train = data

# default RF model
m1 <- randomForest(
  formula = Y ~ .,
  data    = data.train
)
m1
# rf.cv <- rfcv(data.train[,2:16],data.train[,1], cv.fold=10)
# rf.cv$error.cv
plot(m1)

# number of trees with lowest MSE
which.min(m1$mse)
# RMSE of this optimal random forest
sqrt(m1$mse[which.min(m1$mse)])

# create training and validation data 
set.seed(123)
valid_split <- initial_split(data.train, .8)

# training data
ames_train_v2 <- analysis(valid_split)

# validation data
ames_valid <- assessment(valid_split)
x_test <- ames_valid[setdiff(names(ames_valid), "Y")]
y_test <- ames_valid$Y

rf_oob_comp <- randomForest(
  formula = Y ~ .,
  data    = ames_train_v2,
  xtest   = x_test,
  ytest   = y_test
)

# extract OOB & validation errors
oob <- sqrt(rf_oob_comp$mse)
# validation <- sqrt(rf_oob_comp$test$mse)
validation <- rf_oob_comp$test$mse

# compare error rates
tibble::tibble(
  `Out of Bag Error` = oob,
  `Test error` = validation,
  ntrees = 1:rf_oob_comp$ntree
) %>%
  gather(Metric, MSE, -ntrees) %>%
  ggplot(aes(ntrees, MSE, color = Metric)) +
  geom_line() +
  scale_y_continuous() +
  xlab("Number of trees")
## OOB MSPE around 1.5

############# initial tuning #####
features <- setdiff(names(data.train), "Y")

# set.seed(123)

m2 <- tuneRF(
  x          = data.train[features],
  y          = data.train$Y,
  ntreeTry   = 500,
  mtryStart  = 5,
  stepFactor = 1.5,
  improve    = 0.01,
  trace      = FALSE      # to not show real-time progress 
)
# plot(m2)
############# grid search ###########
# hyperparameter grid search
hyper_grid <- expand.grid(
  mtry       = seq(1, 15, by = 1),
  node_size  = seq(3, 9, by = 2),
  sampe_size = c(.55, .632, .70, .80),
  OOB_RMSE   = 0
)

# total number of combinations
nrow(hyper_grid)
for(i in 1:nrow(hyper_grid)) {
  
  # train model
  model <- ranger(
    formula         = Y ~ ., 
    data            = data.train, 
    num.trees       = 500,
    mtry            = hyper_grid$mtry[i],
    min.node.size   = hyper_grid$node_size[i],
    sample.fraction = hyper_grid$sampe_size[i],
    seed            = 123
  )
  
  # add OOB error to grid
  hyper_grid$OOB_RMSE[i] <- sqrt(model$prediction.error)
}

hyper_grid %>% 
  dplyr::arrange(OOB_RMSE) %>%
  head(10)
# mtry node_size sampe_size OOB_RMSE        # very few training data !!
# 1     9         7      0.800 1.277726
# 2     5         3      0.700 1.278000
# 3    13         3      0.700 1.278025
# 4    11         7      0.700 1.279321
# 5    10         9      0.800 1.279643
# 6     9         9      0.800 1.280201
# 7    11         5      0.632 1.280887
# 8     6         7      0.800 1.281554
# 9    10         7      0.632 1.281716
# 10   11         3      0.632 1.281761

# mtry node_size sampe_size OOB_RMSE          # more training data 
# 1    15         9        0.8 1.246674
# 2    11         7        0.8 1.246909
# 3    14         9        0.8 1.247299
# 4    11         3        0.7 1.247314
# 5    14         7        0.8 1.247795
# 6    15         7        0.8 1.248281
# 7    13         9        0.8 1.248504
# 8    12         7        0.8 1.248593
# 9    15         5        0.8 1.248693
# 10   14         5        0.8 1.248794

# ########### try dummy encoding #######
# # one-hot encode our categorical variables
# one_hot <- dummyVars(~ ., data.train, fullRank = FALSE)
# ames_train_hot <- predict(one_hot, data.train) %>% as.data.frame()
# 
# # make ranger compatible names
# names(ames_train_hot) <- make.names(names(ames_train_hot), allow_ = FALSE)
# dim(ames_train_hot)
# # hyperparameter grid search --> same as above but with increased mtry values
# hyper_grid_2 <- expand.grid(
#   mtry       = seq(1, 15, by = 2),
#   node_size  = seq(3, 9, by = 2),
#   sampe_size = c(.55, .632, .70, .80),
#   OOB_RMSE  = 0
# )
# 
# # perform grid search
# for(i in 1:nrow(hyper_grid_2)) {
#   
#   # train model
#   model <- ranger(
#     formula         = Y ~ ., 
#     data            = ames_train_hot, 
#     num.trees       = 500,
#     mtry            = hyper_grid_2$mtry[i],
#     min.node.size   = hyper_grid_2$node_size[i],
#     sample.fraction = hyper_grid_2$sampe_size[i],
#     seed            = 123
#   )
#   
#   # add OOB error to grid
#   hyper_grid_2$OOB_RMSE[i] <- sqrt(model$prediction.error)
# }
# 
# hyper_grid_2 %>% 
#   dplyr::arrange(OOB_RMSE) %>%
#   head(10)
######################################
###### best 1.275 at num.tree= 500,mtry=7, node_size=5, sample_size=0.8
###### best 1.246 at num.tree= 500,mtry=15, node_size=9, sample_size=0.8

OOB_RMSE <- vector(mode = "numeric", length = 100)

for(i in seq_along(OOB_RMSE)) {
  
  optimal_ranger <- ranger(
    formula         = Y ~ ., 
    data            = data.train, 
    num.trees       = 500,
    mtry            = 15,
    min.node.size   = 9,
    sample.fraction = .8,
    importance      = 'impurity'
  )
  
  OOB_RMSE[i] <- sqrt(optimal_ranger$prediction.error)
}
mean(OOB_RMSE)
# [1] 1.291381 # few training data
# [1] 1.253

hist(OOB_RMSE, breaks = 20)

# optimal_ranger$variable.importance %>%
#   tidy() %>%
#   dplyr::arrange(desc(x)) %>%
#   dplyr::top_n(15) %>%
#   ggplot(aes(reorder(names, x), x)) +
#   geom_col() +
#   coord_flip() +
#   ggtitle("Top 15 important variables")
### variable importance
plot(optimal_ranger$variable.importance)

################################## H2O tuning ##################################
### h20 grid #####
# start up h2o (I turn off progress bars when creating reports/tutorials)
h2o.no_progress()
h2o.init(max_mem_size = "5g")

# create feature names
y <- "Y"
x <- setdiff(names(data.train), y)

# turn training set into h2o object
train.h2o <- as.h2o(data.train)

# hyperparameter grid
hyper_grid.h2o <- list(
  ntrees      = seq(200, 500, by = 100),
  mtries      = seq(1, 15, by = 1),
  sample_rate = c(.55, .632, .70, .80)
)

# build grid search
grid <- h2o.grid(
  algorithm = "randomForest",
  grid_id = "rf_grid",
  x = x,
  y = y,
  training_frame = train.h2o,
  hyper_params = hyper_grid.h2o,
  search_criteria = list(strategy = "Cartesian")
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "rf_grid",
  sort_by = "rmse",
  decreasing = FALSE
)
print(grid_perf)

# 
# Hyper-Parameter Search Summary: ordered by increasing rmse        # few training data
#   mtries ntrees sample_rate         model_ids               rmse
# 1      4    300       0.632  rf_grid_model_79 1.2548523441851454
# 2      2    300       0.632  rf_grid_model_77 1.2613272162854612
# 3      2    500       0.632 rf_grid_model_107 1.2671251821255527
# 4      4    500        0.55  rf_grid_model_49 1.2680285778497806
# 5      5    400         0.7 rf_grid_model_155 1.2680478442955645
# 
# Hyper-Parameter Search Summary: ordered by increasing rmse
#    mtries ntrees sample_rate         model_ids               rmse
# 1      5    300        0.55  rf_grid_model_20 1.2395559161628948
# 2     10    300        0.55  rf_grid_model_25 1.2411833708955584
# 3      4    300         0.7 rf_grid_model_139 1.2423715791964807
# 4     15    200        0.55  rf_grid_model_15 1.2424080397793995
# 5      4    500       0.632 rf_grid_model_109 1.2432111818193248
RF <- randomForest(
  y         = data.train$Y, 
  x            = data.train[,2:16], 
  num.trees       = 300,
  mtry            = 5,
  min.node.size   = 9,
  sample.fraction = .55,
  importance=TRUE
)
RF




###################################
################# GBM ###########
# install.packages("vtreat")

library(rsample)      # data splitting 
library(gbm)          # basic implementation
library(xgboost)      # a faster implementation of gbm
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # a java-based platform
library(pdp)          # model visualization
library(ggplot2)      # model visualization
library(lime)         # model visualization
library(vtreat)
# ames_split <- initial_split(data, prop = .9)
ames_split <- initial_split(data, prop = 0.99)

ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

####### initial fit ##########
# train GBM model
gbm.fit <- gbm(
  formula = Y~ .,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
# print results
print(gbm.fit)
# get MSE and compute RMSE
min(gbm.fit$cv.error)
sqrt(min(gbm.fit$cv.error))
## [1] 1.266571
## [1] 1.255174 split at 0.9

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")

######### initial tuning #####
# train GBM model
gbm.fit2 <- gbm(
  formula = Y ~ .,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  
# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit2$cv.error)
# get MSE and compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])
## [1] 1.282526
# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit2, method = "cv")
## seem overfitting

######## grid search ########
# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = Y ~ .,
    distribution = "gaussian",
    data = ames_train,
    n.trees = 6000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)
#   shrinkage   interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
# 1       0.10                 3             15          1.0            57 1.236997
# 2       0.01                 5             10          1.0           398 1.239535
# 3       0.30                 3             10          1.0            17 1.239929
# 4       0.10                 5             10          1.0            42 1.240323
# 5       0.01                 5             15          1.0           269 1.242259
# 6       0.01                 3             15          1.0           558 1.242413
# 7       0.10                 5             15          1.0            85 1.242651
# 8       0.30                 3             10          0.8            31 1.243259
# 9       0.30                 3             15          1.0            18 1.243331
# 10      0.01                 3             10          1.0           661 1.249933
# 
#    shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
# 1       0.30                 5             10         1.00            55 1.227005
# 2       0.30                 5              5         1.00            15 1.256649
# 3       0.10                 5              5         1.00            68 1.268666
# 4       0.10                 3              5         0.65            85 1.269674
# 5       0.30                 3             10         1.00            21 1.274431
# 6       0.10                 3             10         1.00           104 1.276428
# 7       0.10                 5              5         0.65            93 1.278393
# 8       0.10                 5             10         1.00            38 1.278743
# 9       0.01                 5              5         0.80           865 1.280557
# 10      0.30                 3              5         0.65            60 1.280665
# ###### focus grid search #######
# train GBM model
hyper_grid <- expand.grid(
  shrinkage = c( .1, .3),
  interaction.depth = c(3, 5),
  n.minobsinnode = c(10, 5),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model
  gbm.tune <- gbm(
    formula = Y ~ .,
    distribution = "gaussian",
    data = ames_train,
    n.trees = 6000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)
#        shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
# 1       0.10                 7             15          1.0            34 1.236092
# 2       0.10                 3             15          1.0            57 1.236997
# 3       0.30                 3              7          0.8            16 1.237950
# 4       0.01                 5             10          1.0           398 1.239535
# 5       0.30                 3             10          1.0            17 1.239929
# 6       0.10                 5             10          1.0            42 1.240323
# 7       0.01                 7             15          1.0           286 1.241532
# 8       0.01                 5             15          1.0           269 1.242259
# 9       0.01                 3             15          1.0           558 1.242413
# 10      0.01                 7             10          1.0           335 1.242530
# 
#     shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
# 1        0.3                 5             10         1.00            55 1.227005
# 2        0.3                 5              5         1.00            15 1.256649
# 3        0.1                 5              5         1.00            68 1.268666
# 4        0.1                 3              5         0.65            85 1.269674
# 5        0.3                 3             10         1.00            21 1.274431
# 6        0.1                 3             10         1.00           104 1.276428
# 7        0.1                 5              5         0.65            93 1.278393
# 8        0.1                 5             10         1.00            38 1.278743
# 9        0.3                 3              5         0.65            60 1.280665
# 10       0.1                 5              5         0.80            74 1.281195
# 
#     shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
# 1       0.10                 3              5         0.80           284 1.194069
# 2       0.30                 5             15         0.80            28 1.195188
# 3       0.10                 3             15         0.80           213 1.197654
# 4       0.01                 3              5         0.65          2556 1.198462
# 5       0.30                 3             15         1.00            38 1.198813
# 6       0.10                 3             10         0.80           195 1.201779
# 7       0.01                 5             10         0.80          1131 1.202718
# 8       0.10                 5              5         0.80           153 1.205742
# 9       0.01                 5              5         0.65          1854 1.206415
# 10      0.01                 3             10         0.80          2350 1.206996

####### final GBM #######
# for reproducibility
set.seed(123)

# train GBM model # BEST !
gbm.fit.final <- gbm(
  formula = Y ~ .,
  distribution = "gaussian",
  data = ames_train,
  n.trees = 284,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 5,
  bag.fraction = 0.8, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

par(mar = c(5, 8, 1, 1))
summary(
  gbm.fit.final, 
  cBars = 15,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
)
# best var: X15,X12 , worst: X10,X9,X7
# partial dependency plot
gbm.fit.final %>%
  partial(pred.var = "X12", n.trees = gbm.fit.final$n.trees, grid.resolution = 100) %>%
  autoplot(rug = TRUE, train = ames_train) +
  scale_y_continuous()

# ICE plot
ice1 <- gbm.fit.final %>%
  partial(
    pred.var = "X12", 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1) +
  ggtitle("Non-centered") +
  scale_y_continuous(labels = scales::dollar)

ice2 <- gbm.fit.final %>%
  partial(
    pred.var = "X12", 
    n.trees = gbm.fit.final$n.trees, 
    grid.resolution = 100,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
  ggtitle("Centered") +
  scale_y_continuous(labels = scales::dollar)

gridExtra::grid.arrange(ice1, ice2, nrow = 1)

### prediction ### 
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, ames_test)
# results: root_MSPE at 1.2544  (with test data)
# 1.06514

caret::RMSE(pred, ames_test$Y)


####################################################################
######### XGboost ############
#### H2O #####
h2o.no_progress()
h2o.init(max_mem_size = "5g")

# create feature names
y <- "Y"
x <- setdiff(names(ames_train), y)

# turn training set into h2o object
train.h2o <- as.h2o(ames_train)

# training basic GBM model with defaults
h2o.fit1 <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 5
)
# assess model results
h2o.fit1
# H2ORegressionMetrics: gbm
# ** Reported on cross-validation data. **
#   ** 5-fold cross-validation on training data (Metrics computed for combined holdout predictions) **
#   MSE:  1.5648
# RMSE:  1.25092
# MAE:  0.9659445
# RMSLE:  0.09876902
# Mean Residual Deviance :  1.5648
# 
# Cross-Validation Metrics Summary: 
#                          mean          sd    cv_1_valid cv_2_valid cv_3_valid cv_4_valid cv_5_valid
# mae                    0.97028494  0.10687791 0.85685873  1.1424869 0.91519934  0.9765418 0.96033776
# mean_residual_deviance  1.5875148  0.31145623  1.1953517  2.0259576  1.4894905   1.488791  1.7379832
# mse                     1.5875148  0.31145623  1.1953517  2.0259576  1.4894905   1.488791  1.7379832
# r2                     0.18743326 0.111664735 0.35890532 0.13204241 0.07455758 0.23409635 0.13756464
# residual_deviance       1.5875148  0.31145623  1.1953517  2.0259576  1.4894905   1.488791  1.7379832
# rmse                    1.2551231 0.123393014  1.0933214  1.4233614  1.2204468  1.2201602  1.3183259
# rmsle                   0.0991246  0.00933093 0.08705594 0.10746158 0.09398262 0.09776073 0.10936213

# training basic GBM model with defaults
h2o.fit2 <- h2o.gbm(
  x = x,
  y = y,
  training_frame = train.h2o,
  nfolds = 5,
  ntrees = 5000,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123
)
# model stopped after xx trees
h2o.fit2@parameters$ntrees
## [1] 51

# cross validated RMSE
h2o.rmse(h2o.fit2, xval = TRUE)
# 1.22897

##################################
######### h2o full grid search ######
# create training & validation sets
split <- h2o.splitFrame(train.h2o, ratios = 0.75)
train <- split[[1]]
valid <- split[[2]]

# create hyperparameter grid
hyper_grid <- list(
  max_depth = c(1, 3, 5),
  min_rows = c(1, 5, 10),
  learn_rate = c(0.01, 0.05, 0.1),
  learn_rate_annealing = c(.99, 1),
  sample_rate = c(.5, .75, 1),
  col_sample_rate = c(.8, .9, 1)
)

# perform grid search 
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm_grid1",
  x = x, 
  y = y, 
  training_frame = train,
  validation_frame = valid,
  hyper_params = hyper_grid,
  ntrees = 5000,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "gbm_grid1", 
  sort_by = "mse", 
  decreasing = FALSE
)
grid_perf
# Hyper-Parameter Search Summary: ordered by increasing mse
#       col_sample_rate learn_rate learn_rate_annealing max_depth min_rows sample_rate           model_ids                mse
# 1             1.0        0.1                 0.99         5      1.0        0.75 gbm_grid1_model_207 1.5490647298558797
# 2             1.0        0.1                 0.99         5      1.0         1.0 gbm_grid1_model_369 1.5745125558631083
# 3             0.9       0.05                  1.0         5      1.0         0.5  gbm_grid1_model_50 1.5747593385288152
# 4             1.0        0.1                  1.0         3      5.0        0.75 gbm_grid1_model_252 1.5774484688877943
# 5             0.9        0.1                 0.99         3      5.0        0.75 gbm_grid1_model_242   1.57934237793691

# Hyper-Parameter Search Summary: ordered by increasing mse
#     col_sample_rate learn_rate learn_rate_annealing max_depth min_rows sample_rate           model_ids                mse
# 1             0.9       0.05                 0.99         5      5.0        0.75 gbm_grid1_model_743  1.233783957885197
# 2             0.9       0.01                  1.0         1      5.0        0.75 gbm_grid1_model_713 1.2389943622946653
# 3             0.8       0.01                  1.0         1      5.0        0.75 gbm_grid1_model_712 1.2392253224437302
# 4             0.8       0.01                  1.0         1      5.0         0.5 gbm_grid1_model_550 1.2392847427850997
# 5             0.9       0.01                  1.0         1      5.0         0.5 gbm_grid1_model_551  1.239297887037975

# 0.9/0.1 train/valid
# Hyper-Parameter Search Summary: ordered by increasing mse
#     col_sample_rate learn_rate learn_rate_annealing max_depth min_rows sample_rate           model_ids                mse
# 1             1.0       0.05                  1.0         3      5.0         1.0 gbm_grid1_model_897 1.4111432711699303
# 2             1.0       0.01                  1.0         1      5.0         0.5 gbm_grid1_model_552 1.4133565265906916
# 3             0.9       0.01                  1.0         1      5.0        0.75 gbm_grid1_model_713 1.4138665661507455
# 4             0.8       0.01                  1.0         1      5.0         1.0 gbm_grid1_model_874 1.4143734652869657
# 5             0.9       0.01                  1.0         1      5.0         0.5 gbm_grid1_model_551  1.414488943871397
###

# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let's get performance metrics on the best model
h2o.performance(model = best_model, valid = TRUE)
# H2ORegressionMetrics: gbm
# ** Reported on validation data. **
#   
#   MSE:  1.549065
# RMSE:  1.244614
# MAE:  0.9514999
# RMSLE:  0.1009621
# Mean Residual Deviance :  1.549065
# H2ORegressionMetrics: gbm
# ** Reported on validation data. **
#   
#   MSE:  1.233784
# RMSE:  1.110758
# MAE:  0.8424431
# RMSLE:  0.08935877
# Mean Residual Deviance :  1.233784

# H2ORegressionMetrics: gbm
# ** Reported on validation data. **
#   
#   MSE:  1.411143
# RMSE:  1.187916
# MAE:  0.940059
# RMSLE:  0.09391098
# Mean Residual Deviance :  1.411143


##################################
######### h2o random discrete search ######
# random grid search criteria
search_criteria <- list(
  strategy = "RandomDiscrete",
  stopping_metric = "mse",
  stopping_tolerance = 0.005,
  stopping_rounds = 10,
  max_runtime_secs = 60*60
)

# perform grid search 
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "gbm_grid2",
  x = x, 
  y = y, 
  training_frame = train,
  validation_frame = valid,
  hyper_params = hyper_grid,
  search_criteria = search_criteria, # add search criteria
  ntrees = 5000,
  stopping_rounds = 10,
  stopping_tolerance = 0,
  seed = 123
)

# collect the results and sort by our model performance metric of choice
grid_perf <- h2o.getGrid(
  grid_id = "gbm_grid2", 
  sort_by = "mse", 
  decreasing = FALSE
)
grid_perf
# Hyper-Parameter Search Summary: ordered by increasing mse
#        col_sample_rate learn_rate learn_rate_annealing max_depth min_rows sample_rate           model_ids                mse
# 1             1.0        0.1                 0.99         5      1.0        0.75 gbm_grid2_model_220 1.5490647298558797
# 2             1.0        0.1                 0.99         5      1.0         1.0 gbm_grid2_model_125 1.5745125558631083
# 3             0.9       0.05                  1.0         5      1.0         0.5 gbm_grid2_model_464 1.5747593385288152
# 4             1.0        0.1                  1.0         3      5.0        0.75  gbm_grid2_model_36 1.5774484688877943
# 5             0.9        0.1                 0.99         3      5.0        0.75 gbm_grid2_model_463   1.57934237793691

# Hyper-Parameter Search Summary: ordered by increasing mse
#    col_sample_rate learn_rate learn_rate_annealing max_depth min_rows sample_rate           model_ids                mse
# 1             0.9       0.05                 0.99         5      5.0        0.75 gbm_grid2_model_904  1.233783957885197
# 2             0.9       0.01                  1.0         1      5.0        0.75 gbm_grid2_model_892 1.2389943622946653
# 3             0.8       0.01                  1.0         1      5.0        0.75 gbm_grid2_model_794 1.2392253224437302
# 4             0.8       0.01                  1.0         1      5.0         0.5 gbm_grid2_model_806 1.2392847427850997
# 5             0.9       0.01                  1.0         1      5.0         0.5 gbm_grid2_model_920  1.239297887037975


# Grab the model_id for the top model, chosen by validation error
best_model_id <- grid_perf@model_ids[[1]]
best_model <- h2o.getModel(best_model_id)

# Now let's get performance metrics on the best model
h2o.performance(model = best_model, valid = TRUE)
# H2ORegressionMetrics: gbm
# ** Reported on validation data. **
#   MSE:  1.549065
# RMSE:  1.244614
# MAE:  0.9514999
# RMSLE:  0.1009621
# Mean Residual Deviance :  1.549065

# H2ORegressionMetrics: gbm
# ** Reported on validation data. **
#   MSE:  1.233784
# RMSE:  1.110758
# MAE:  0.8424431
# RMSLE:  0.08935877
# Mean Residual Deviance :  1.233784

#### final model for xgb ########
#     col_sample_rate learn_rate learn_rate_annealing max_depth min_rows sample_rate           model_ids                mse
# Grid search
#0.75/0.25
# 1             1.0        0.1                 0.99         5      1.0        0.75 gbm_grid1_model_207 1.5490647298558797
# 0.99/0.01
# 1             0.9       0.05                 0.99         5      5.0        0.75 gbm_grid1_model_743  1.233783957885197
# 0.9/0.1
# 1             1.0       0.05                  1.0         3      5.0         1.0 gbm_grid1_model_897 1.4111432711699303

# Discrete search
#0.75/0.25
# 1             1.0        0.1                 0.99         5      1.0        0.75 gbm_grid2_model_220 1.5490647298558797
#0.99/0.01
# 1             0.9       0.05                 0.99         5      5.0        0.75 gbm_grid2_model_904  1.233783957885197


# train final model
# h2o.final <- h2o.gbm(
#   x = x,
#   y = y,
#   training_frame = train.h2o,
#   nfolds = 5,
#   ntrees = 10000,
#   learn_rate = 0.05,
#   learn_rate_annealing = 1,
#   max_depth = 3,
#   min_rows = 5,
#   sample_rate = 1,
#   col_sample_rate = 1,
#   stopping_rounds = 10,
#   stopping_tolerance = 0,
#   seed = 123
# )
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

# model stopped after xx trees
h2o.final@parameters$ntrees
## [1] 42
## [1] 138


# cross validated RMSE
h2o.rmse(h2o.final, xval = TRUE)
## [1] 1.269  #0.75/0.25
## [1] 1.251 # 0.99/0.01
## [1] 1.21 # 0.9/0.1
h2o.varimp_plot(h2o.final, num_of_features = 16)
# best x12,x15 worst x9,x10,x7

# plot PDP
pfun <- function(object, newdata) {
  as.data.frame(predict(object, newdata = as.h2o(newdata)))[[1L]]
}
pdp <- h2o.final %>%
  partial(
    pred.var = "X12", 
    pred.fun = pfun,
    grid.resolution = 20, 
    train = ames_train
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("PDP")

ice <- h2o.final %>%
  partial(
    pred.var = "X12", 
    pred.fun = pfun,
    grid.resolution = 20, 
    train = ames_train,
    ice = TRUE
  ) %>%
  autoplot(rug = TRUE, train = ames_train, alpha = .1, center = TRUE) +
  scale_y_continuous(labels = scales::dollar) +
  ggtitle("ICE")

gridExtra::grid.arrange(pdp, ice, nrow = 1)

h2o.partialPlot(h2o.final, data = train.h2o, cols = "X12")

###### predicting
# convert test set to h2o object
test.h2o <- as.h2o(ames_test)

# evaluate performance on new data
h2o.performance(model = h2o.final, newdata = test.h2o)
# H2ORegressionMetrics: gbm
# MSE:  1.540998
# RMSE:  1.24137
# MAE:  0.9661222
# RMSLE:  0.09857327
# Mean Residual Deviance :  1.540998
# predict with h2o.predict
h2o.predict(h2o.final, newdata = test.h2o)

# predict values with predict
predict(h2o.final, test.h2o)
# test.h2o
# xc=predict(h2o.final, train.h2o)
# xc=predict(gbm.fit.final, ames_train)
# tail(x,20)
# tail(ames_train,20)

data_test <-  read.table("Data2020testX.csv", 
                    header=TRUE, sep=",", na.strings=" ")

data_test
data_test.h2o <- as.h2o(data_test)

predictions=predict(h2o.final, data_test.h2o)
fit.lm = lm(Y ~ ., data = data)
fit.nnet = nnet(data[,2:16], data[,1], linout = TRUE, size = 6,
                decay = 0.01, maxit = 500, trace = FALSE)
predictions=predict(gbm.fit.final, data_test)
# predictions=predict(fit.lm, data_test)
# predictions=predict(RF, data_test)

predictions=as.vector(predictions$predict)
write.table(predictions, 'prediction.csv', sep = ",", row.names = NA, col.names =NA)
