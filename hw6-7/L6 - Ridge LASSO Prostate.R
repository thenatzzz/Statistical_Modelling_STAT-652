#########################################################################
# Ridge regression and LASSO on Prostate Data 
# Ridge using MASS::lm.ridge
# LASSO using glmnet package 
# Splitting the data in half and modeling each half separately.
rm(list = ls())

prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")
head(prostate)

# Splitting data in half using random uniform selection to make two "set"s.

set.seed(120401002) 
set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)

library(MASS) # for ridge
library(glmnet) # for LASSO

###############################################################
# Ridge regression using lm.ridge()
#
# Functions much like lm(), but need to specify lambda.
#   Can specify sequence of lambda values using seq(from= , to= , by=)
#     to generate sequence of numbers
#   Then use a version of CV to select best 
seq(from=1, to=10, by=1)
seq(1, 10, 1)
seq(0, 2, 0.2)

# In my experience, optimal lambda can be <1 sometimes
#   or as large as upper double digits.  Running sequence
#  from 0 to 100 by a small increment may be a little expensive
#  but probably covers range.
ridge1 <- lm.ridge(lpsa ~., lambda = seq(0, 100, .05), data=prostate[set==1,])
# Show coefficient path
plot(ridge1)

select(ridge1)
(coef.ri.best1 = coef(ridge1)[which.min(ridge1$GCV),])

# Unfortunately, there is no predict() function for lm.ridge
#  Have to compute predicted values manually using matrix multiplication
#  Formula is 
#  as.matrix(cbind(1,DATA)) %*% coef(RIDGE MODEL)
#  where DATA is the X-data for which you want predicted values, 
#  and RIDGE MODEL is the lm.ridge object you have created.
#  The "1" adds a column of 1's for the intercept term.
#
# Here I can use the optimal lambda to make predictions
#  on test data (set==2)

pred.ri1 = as.matrix(cbind(1,prostate[set==2,1:8])) %*% coef.ri.best1
MSPE1.ridge = mean((prostate[set==2,9]-pred.ri1)^2)

# Compare MSPE to MSPE from lm
mod.lm1 = lm(lpsa~., data=prostate[set==1,])
pred.lm1 = predict(mod.lm1, newdata=prostate[set==2,])
MSPE1.lm = mean((prostate[set==2,9]-pred.lm1)^2)

############
# Repeat for other half of data
ridge2 <- lm.ridge(lpsa ~., lambda = seq(0, 100, .05), data=prostate[set==2,])
# Show coefficient path
plot(ridge2)
select(ridge2)
(coef.ri.best2 = coef(ridge2)[which.min(ridge2$GCV),])

# Unfortunately, there is no predict() function for lm.ridge
#  Have to compute predicted values manually using matrix multiplication
#  Formula is 
#  as.matrix(cbind(1,DATA)) %*% coef(RIDGE MODEL)
#  where DATA is the X-data for which you want predicted values, 
#  and RIDGE MODEL is the lm.ridge object you have created.
#  The "1" adds a column of 1's for the intercept term.
#
# Here I can use the optimal lambda to make predictions
#  on training data (set==1) and test data (set==2)

pred.ri2 = as.matrix(cbind(1,prostate[set==1,1:8])) %*% coef.ri.best2
MSPE2.ridge = mean((prostate[set==1,9]-pred.ri2)^2)

# Compare MSPE to MSPE from lm
mod.lm2 = lm(lpsa~., data=prostate[set==2,])
pred.lm2 = predict(mod.lm2, newdata=prostate[set==1,])
MSPE2.lm = mean((prostate[set==1,9]-pred.lm2)^2)

# glmnet() requires x to be in matrix class, so saving out 
#   the separate variables to be used as Y and X.
#   glmnet() scales all columns of x in its calculations, but presents 
#   unscaled versions of coefficients.  


y.1 <- prostate[set==1,1]
x.1 <- as.matrix(prostate[set==1,c(2:9)])
y.2 <- prostate[set==2,1]
x.2 <- as.matrix(prostate[set==2,c(2:9)])

# Fit LASSO by glmnet(y=, x=). Gaussian is default, but other families are available  
#  Function produces series of fits for many values of lambda.  

# First half of data 
lasso.1 <- glmnet(y=y.1, x= x.1, family="gaussian")
#("%Dev" in output below is R-square in linear regression)
lasso.1
plot(lasso.1) # Plots coefficient path
##### Note that these are on original scales, even though LASSO scaled variables 
coef(lasso.1) # Lists out coefficients for each lambda
##### Note that glmnet() does not necessarily go all the way out to the LSEs
#####But they are close
# coef(lasso.1, s=lasso.1$lambda.min)
# cv.glmnet() uses crossvalidation to estimate optimal lambda
#  We haven't talked about this yet, so don't worry about it.

cv.lasso.1 <- cv.glmnet(y=y.1, x= x.1, family="gaussian")
cv.lasso.1
plot(cv.lasso.1) # Plot CV-MSPE
coef(cv.lasso.1) # Print out coefficients at optimal lambda 
coef(cv.lasso.1, s=cv.lasso.1$lambda.min) # Another way to do this.
# Using the "+1SE rule" (see later) produces a sparser solution
coef(cv.lasso.1, s=cv.lasso.1$lambda.1se) # Another way to do this.

# Predict both halves using first-half fit
pred.las1.min <- predict(cv.lasso.1, newx=x.2, s=cv.lasso.1$lambda.min)
pred.las1.1se <- predict(cv.lasso.1, newx=x.2, s=cv.lasso.1$lambda.1se)
pred.las1 <- predict(cv.lasso.1, newx=x.2) #1SE is the default!

MSPE1.las.min <- mean((y.2 - pred.las1.min)^2)
MSPE1.las.1se <- mean((y.2 - pred.las1.1se)^2)

# Repeat for second half of data
lasso.2 <- glmnet(y=y.2, x= x.2, family="gaussian")
#("%Dev" in output below is R-square in linear regression)
lasso.2
plot(lasso.2) # Plots coefficient path
##### Note that these are on original scales, even though LASSO scaled variables 
coef(lasso.2) # Lists out coefficients for each lambda
##### Note that glmnet() does not necessarily go all the way out to the LSEs
#####But they are close

# cv.glmnet() uses crossvalidation to estimate optimal lambda
#  We haven't talked about this yet, so don't worry about it.

cv.lasso.2 <- cv.glmnet(y=y.2, x= x.2, family="gaussian")
cv.lasso.2
plot(cv.lasso.2) # Plot CV-MSPE
coef(cv.lasso.2) # Print out coefficients at optimal lambda 
coef(cv.lasso.2, s=cv.lasso.2$lambda.min) # Another way to do this.
# Using the "+1SE rule" (see later) produces a sparser solution
coef(cv.lasso.2, s=cv.lasso.2$lambda.1se) # Another way to do this.

# Predict both halves using first-half fit
pred.las2.min <- predict(cv.lasso.2, newx=x.2, s=cv.lasso.2$lambda.min)
pred.las2.1se <- predict(cv.lasso.2, newx=x.2, s=cv.lasso.2$lambda.1se)
pred.las2 <- predict(cv.lasso.2, newx=x.2) #1SE is the default!

MSPE2.las.min <- mean((y.2 - pred.las2.min)^2)
MSPE2.las.1se <- mean((y.2 - pred.las2.1se)^2)

