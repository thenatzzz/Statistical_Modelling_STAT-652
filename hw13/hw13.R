# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(2928893) 

############# HW13 ##################
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
# 1 Ozone , 2-6 X vars
#### 1.####################
source("Helper Functions.R")

# set.seed(2371903) 
# reorder = sample.int(n=nrow(AQ))
# reorder
# set = ifelse(test=(reorder > 20), yes=1, no=2)
# set

library(nnet)
rescale <- function(x1,x2){
  for(col in 1:ncol(x1)){
    a <- min(x2[,col])
    b <- max(x2[,col])
    x1[,col] <- (x1[,col]-a)/(b-a)
  }
  x1
}

# # Split data into training and test data and store as matrices.
# y.1 <- as.matrix(AQ[set==1,1])
# x.1.unscaled <- as.matrix(AQ[set==1,2:6]) # Original data set 1
# x.1 <- rescale(x.1.unscaled, x.1.unscaled) #scaled data set 1
# summary(x.1.unscaled)
# summary(x.1)
# 
# #Test
# y.2 <- as.matrix(AQ[set==2,1])
# x.2.unscaled <- as.matrix(AQ[set==2,2:6]) # Original data set 2
# x.2 = rescale(x.2.unscaled, x.1.unscaled)
# summary(x.2)

### 75%/25% split into training and validation sets
n = nrow(AQ)
n.train = floor(n*0.75)
n.valid = n - n.train

inds = c(rep(1, times = n.train), rep(2, times = n.valid))
inds.rand = inds[sample.int(n)]

data = AQ
data.train = data[inds.rand == 1,]
# X.train.raw = data.train[,2:6]
X.train.raw = data.train[,3:4]
Y.train = data.train[,1]

data.valid = data[inds.rand == 2,]
# X.valid.raw = data.valid[,2:6]
X.valid.raw = data.valid[,3:4]
Y.valid = data.valid[,1]

X.train = rescale(X.train.raw, X.train.raw)
X.valid = rescale(X.valid.raw, X.train.raw) # Be careful with the order
summary(X.train.raw)
summary(X.train)
summary(X.valid.raw)
summary(X.valid)

# full data
# full.X.raw = AQ[,2:6]
# full.Y = AQ[,1]
full.X.raw = AQ[,3:4]
full.Y = AQ[,1]
full.X = rescale(full.X.raw, full.X.raw)
# summary(full.X.raw)
summary(full.X)

#### 2.#################### Temp and Wind
# Plot the surface
library(rgl)  


n.hidden = 6
shrink = 1
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


# ### Get predictions and MSPE
# pred.nnet = predict(fit.nnet.best, X.valid)
# MSPE.nnet = get.MSPE(Y.valid, pred.nnet)

#######3. 3D plot ##################

x1 <- seq(from=0, to=25, by=.05) # Wind min,max = 2.3,20.7
x2 = seq(from=0, to=100, by=.5) # Temp min,max = 57,97
xy1 <- data.frame(expand.grid(Wind=x1, Temp=x2))

# X.train = rescale(X.train.raw, X.train.raw)
# X.valid = rescale(X.valid.raw, X.train.raw)
# pred2 <- predict(nn1 ,newdata=rescale(xy1,x.1.unscaled[,c(3,4)]))
n.hidden = 6
shrink = 0.001
# nn1 = nnet(y = Y.train, x = X.train, linout = TRUE, size = n.hidden,
#                 decay = shrink, maxit = 500)
# pred2 <- predict(nn1 ,newdata=rescale(xy1,X.train.raw))
nn1 = nnet(y = full.Y, x = full.X, linout = TRUE, size = n.hidden,
           decay = shrink, maxit = 500)
pred2 <- predict(nn1 ,newdata=rescale(xy1,full.X.raw))
surface2 = matrix(pred2, nrow=length(x1))

open3d()
persp3d(x = x1, y = x2,
        z = surface2, col = "orange", xlab="Wind", ylab="Temp",
        zlab="Predicted Ozone")
points3d(AQ$Ozone ~ AQ$Wind + AQ$Temp, col="blue")



##########3. ################

M = 10 # Number of times to re-fit each model

### Define parameter values and use expand.grid() to get all combinations
# all.n.hidden = c(1, 5, 9)
all.n.hidden = c(1,3, 5,7, 9)

# all.shrink = c(0, 1, 5, 10)
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
  # X.train.raw = data.train[,-5]
  X.train.raw = data.train[,-1]
  X.train = rescale(X.train.raw, X.train.raw)
  # Y.train = data.train[,5]
  Y.train = data.train[,1]
  
  
  data.valid = data[folds == i,]
  # X.valid.raw = data.valid[,-5]
  X.valid.raw = data.valid[,-1]
  
  X.valid = rescale(X.valid.raw, X.train.raw)
  # Y.valid = data.valid[,5]
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

V=K
root.CV.MSPEs =sqrt(CV.MSPEs) 
root.CV.MSPEs
(root.MSPEcv = apply(X=root.CV.MSPEs, MARGIN=2, FUN=mean))
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




### We can now make an MSPE boxplot. It would be nice to have more 
### informative names though. We can construct names from all.pars
### using the paste0() function.
# names.pars = paste0(all.pars$n.hidden,",",
                    # all.pars$shrink)
colnames(root.CV.MSPEs) = names.pars

### Make boxplot
boxplot(root.CV.MSPEs, las = 2, main = "root-MSPE Boxplot")


### Get relative MSPEs and make boxplot
root.CV.RMSPEs = apply(root.CV.MSPEs, 1, function(W) W/min(W))
root.CV.RMSPEs = t(root.CV.RMSPEs)
boxplot(root.CV.RMSPEs, las = 2, main = "Relative root-MSPE Boxplot",ylim=c(0.8,8))



###########################################
################## HW regression Tree ############
# clear var
library(rpart)

rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
# set.seed(2928893) 
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



# get only Wind and Temp
### 75%/25% split into training and validation sets
# n = nrow(AQ)
# n.train = floor(n*0.75)
# n.valid = n - n.train
# 
# inds = c(rep(1, times = n.train), rep(2, times = n.valid))
# inds.rand = inds[sample.int(n)]
# 
# data = AQ
# data.train = data[inds.rand == 1,]
# X.train.raw = data.train[,3:4]
# Y.train = data.train[,1]
# 
# data.valid = data[inds.rand == 2,]
# X.valid.raw = data.valid[,3:4]
# Y.valid = data.valid[,1]

### Q1 and Q2
set.seed(2385660,kind="Mersenne-Twister") 

pr.tree2 <- rpart(Ozone ~Wind+Temp , method="anova", data=AQ, cp=0)
pr.tree2
pr.tree2$cptable[,c(2:5,1)]
cpt <- pr.tree2$cptable

cpt
# ind.min = which.min(cpt[,"xerror"])
# CP.min.raw = cpt[ind.min, "CP"]
# CP.min.raw

# x11(h=7, w=8)
plotcp(pr.tree2)

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

library(rpart.plot)

x11(h=10, w=12, pointsize=11)
par(mfrow=c(1,2))
prp(pr2.prune.min, type=1, extra=1, main="Tree pruned to Min CV Error")

prp(pr2.prune.1se, type=1, extra=1, main="Tree pruned to +1SE CV Error")


###### Q3 ########

rm(list=ls(all=TRUE))
library(pls) # pls
library(MASS) # for ridge
library(glmnet) # for LASSO
library(mgcv) # gam
source("Helper Functions.R")

V= 10
set.seed(2928893)
# set.seed(2385660,kind="Mersenne-Twister") 

AQ_original = airquality
head(AQ_original,5)

AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio

head(AQ)
# Let's do 10-fold CV
n = nrow(AQ) #store sample size for easy calculations later

folds = floor((sample.int(n)-1)*V/n) + 1 

MSPEs.cv = matrix(NA, nrow=V, ncol=11)
colnames(MSPEs.cv) = c('Least-Sqr',"Ridge", "LASSO-min",
                       "LASSO-1SE",'Hybrid-Step','PLS',
                       'GAM-all','PPR','Tree','Tree-min','Tree-1se')
MSPEs.PPR.term = matrix(NA, nrow=V, ncol=1)
colnames(MSPEs.PPR.term) = c('PPR-terms')

for(v in 1:V){
  ### Print a status update
  print(paste0(v, " of ", V))
  
  ### Leaste sqrt ###
  fit.lm = lm(Ozone ~ ., data = AQ[folds != v,])
  pred.lm = predict(fit.lm, newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,1] = mean((AQ[folds==v,"Ozone"] - pred.lm)^2)
  
  ### Ridge ###
  ridge1 <- lm.ridge(Ozone ~., lambda = seq(0, 100, .05), data= AQ[folds != v,] )
  (coef.ri.best1 = coef(ridge1)[which.min(ridge1$GCV),])
  pred.ri1 = as.matrix(cbind(1,AQ[folds==v,2:6])) %*% coef.ri.best1
  MSPEs.cv[v,2] = mean((AQ[folds==v,1]-pred.ri1)^2)
  
  ### Lasso-min,1se ###
  x.1 <- as.matrix(AQ[folds!=v,c(2:6)])
  y.1 <- AQ[folds!=v,1]
  x.2 <- as.matrix(AQ[folds==v,c(2:6)])
  y.2 <- AQ[folds==v,1]
  cv.lasso <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
  pred.las.min <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.min)
  pred.las.1se <- predict(cv.lasso, newx=x.2, s=cv.lasso$lambda.1se)
  MSPEs.cv[v,3] <- mean((y.2 - pred.las.min)^2)
  MSPEs.cv[v,4] <- mean((y.2 - pred.las.1se)^2)
  
  ### Step ###
  initial <- lm(data=AQ[folds != v,],
                formula=Ozone~ 1)
  final <- lm(data=AQ[folds != v,],
              formula=Ozone~Solar.R+Wind+Temp+TWcp+TWrat)
  n1 = nrow(AQ[folds != v,])
  step <- step(object=initial, scope=list(upper=final),
               k = log(n1))
  pred = predict(step,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,5] = mean((AQ[folds==v,"Ozone"] - pred)^2)
  
  ### PLS ###
  mod.pls = plsr(Ozone ~.,data= AQ[folds != v,], validation="CV")
  mp.cv = mod.pls$validation
  Opt.Comps= which.min(sqrt(mp.cv$PRESS/nrow(AQ[folds != v,])))
  # MSPEs.cv[v,1]=Opt.Comps
  pred.pls = predict(mod.pls,ncomp=Opt.Comps, newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,6] = mean((AQ[folds==v,1] - pred.pls)^2)
  
  ### GAM ####
  gam.all <- gam(data=AQ[folds != v,],
                 formula=Ozone ~s(Temp)+s(Wind) + s(Solar.R) + s(TWcp) + s(TWrat) ,
                 family=gaussian(link=identity))
  pred.gam <- predict(gam.all ,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,7] = mean((AQ[folds==v,1] - pred.gam)^2)
  
  ### PPR ###
  max.terms = 5
  ### To fit PPR, we need to do another round of CV. This time, do 5-fold
  K.ppr = 5
  n.train = nrow(AQ[folds != v,])
  folds.ppr = get.folds(n.train, K.ppr)
  # print(folds.ppr)
  MSPEs.ppr = array(0, dim = c(max.terms, K.ppr))
  colnames(MSPEs.ppr) = c("ppr1", "ppr2", "ppr3",
                          'ppr4','ppr5')
  for(j in 1:K.ppr){
    train.ppr = AQ[folds != v,][folds.ppr != j,]
    valid.ppr = AQ[folds != v,][folds.ppr == j,2:6]
    Y.valid.ppr = AQ[folds != v,][folds.ppr == j,1]
    
    for(l in 1:max.terms){
      fit.ppr = ppr(Ozone ~ ., data = train.ppr, 
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
  fit.ppr.best = ppr(Ozone ~ ., data = AQ[folds != v,],
                     max.terms = max.terms, nterms = best.terms, sm.method = "gcvspline")
  pred.ppr.best = predict(fit.ppr.best, AQ[folds == v,2:6])
  MSPE.ppr.best = get.MSPE(AQ[folds == v,1], pred.ppr.best) # Our helper function
  
  MSPEs.cv[v, 8] = MSPE.ppr.best
  
  ### Regression tree ####
  pr.tree2 <- rpart(Ozone ~. , method="anova", data=AQ[folds != v,], cp=0)
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
  
  pred.tree <- predict(pr.tree2 ,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,9] = mean((AQ[folds==v,1] - pred.tree)^2)
  pred.prune.min <- predict(pr2.prune.min ,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,10] = mean((AQ[folds==v,1] - pred.prune.min)^2)
  pred.prune.1se <- predict(pr2.prune.1se ,newdata=AQ[folds==v,2:6])
  MSPEs.cv[v,11] = mean((AQ[folds==v,1] - pred.prune.1se)^2)
  
}


dev.off()
MSPEs.cv
MSPEs.cv.mean= apply(X=MSPEs.cv,MARGIN=2,FUN=mean)
MSPEs.cv.mean
par(mar=c(7,5,5,5)+.1)

boxplot(MSPEs.cv, las=2, ylim=c(0,1500),
        main="MSPE \n Cross-Validation")
low.c = apply(MSPEs.cv, 1, min) 
boxplot(MSPEs.cv/low.c, las=2,ylim=c(0.8,8),
        main="Relative MSPE \n Cross-Validation")
boxplot(MSPEs.cv/low.c, las=2,ylim=c(0.8,3),
        main="Focused Relative MSPE \n Cross-Validation")
#####################################

fit.tree = rpart(Ozone ~ Temp + Wind, data = AQ, cp = 0)

### Get the CP table
info.tree = fit.tree$cptable
info.tree
### We have to prune the tree manually. First, get the CP value with minimum
### CV error
ind.min = which.min(info.tree[,"xerror"])
# info.tree[,"xerror"]
CP.min.raw = info.tree[ind.min, "CP"]
CP.min.raw

### It's best to average the minimum CP value with the one from the row above 
### using the geometric mean (i.e. multiply them together, then square root). 
### If we implement this procedure directly, and the minimum CP value is in   
### the first row, our code will probably give an error. We should write our  
### code so that it can cope with this weird situation. This attitude is      
### called defensive programming, and it is a very good habit to practice.    

### Check if minimum CP value is in row 1. We can do this using an if-else
### statement. See this tutorial's video for details.
if(ind.min == 1){
  ### If minimum CP is in row 1, store this value
  CP.min = CP.min.raw
} else{
  ### If minimum CP is not in row 1, average this with the value from the
  ### row above it.
  
  ### Value from row above
  CP.above = info.tree[ind.min-1, "CP"]
  
  ### (Geometric) average
  CP.min = sqrt(CP.min.raw * CP.above)
}

### We now have our chosen CP value. We can prune our tree using the prune()
### function. The first input to prune() is the tree object, and we set "cp"
### equal to the CP value where we want to prune
fit.tree.min = prune(fit.tree, cp = CP.min)


### Next, we want to prune using the 1SE rule. Fortunately, the CP table
### gives us the CV standard error. First, find the minimum CV error plus 1 
### standard error
err.min = info.tree[ind.min, "xerror"]
se.min = info.tree[ind.min, "xstd"]
threshold = err.min + se.min

### Next, get the smallest tree with CV error below our threshold. 
### Note: We limit our search to only trees which are no larger than our min CV
###       error tree.
ind.1se = min(which(info.tree[1:ind.min,"xerror"] < threshold))

### Get the corresponding CP value, averaging if necessary
CP.1se.raw = info.tree[ind.1se, "CP"] ############ FIXXX !!!!!!!!!!!!
if(ind.1se == 1){
  ### If best CP is in row 1, store this value
  CP.1se = CP.1se.raw
} else{
  ### If best CP is not in row 1, average this with the value from the
  ### row above it.
  
  ### Value from row above
  CP.above = info.tree[ind.1se-1, "CP"]
  
  ### (Geometric) average
  CP.1se = sqrt(CP.1se.raw * CP.above)
}

### Prune the tree
fit.tree.1se = prune(fit.tree, cp = CP.1se)


###########################################################################
### A nice feature of tree models is that they make great plots. Let's  ###
### plot the full tree, and both pruned trees. We can plot trees using  ###
### the prp() function from the rpart.plot package.                     ###
###########################################################################

### The prp() function has many inputs We will just use two: type and extra.
### Setting both of these inputs to 1 gives a nice looking plot. Since
### prp() makes a plot, we can set the title using main. We also
### have to provide the fitted tree object which is being plotted.
x11(h=10, w=12, pointsize=11)
prp(fit.tree, type = 1, extra = 1, main = "Full Tree")

x11(h=10, w=12, pointsize=11)
par(mfrow=c(1,2))
prp(fit.tree.min, type = 1, extra = 1, main = "Pruned Tree - Min")
prp(fit.tree.1se, type = 1, extra = 1, main = "Pruned Tree - 1SE")

