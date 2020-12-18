#############################################################
# Demonstrating various different forms of explanatory variables.
#   1. Categorical/Factor variables
#   2. Transformations
#   3. Interactions and other functions
#
#############################################################

#############################################################
# 1. Categorical variables
#
# Below we create some data where X1 is categorical with 4
#   levels and X2 is numerical.
# The mean of Y is related to the 4 levels of X1 but not X2
# We explore the variables and see what regressions look like
#############################################################

# Creating some data, saving to data frame, "dat"
set.seed(3894270)
X1 = rep(x=c("A", "B", "C", "D"), each=5)
X2 = round(rnorm(20),2)
mean.y = rep(c(3,1,5,1), each=5)
epsilon = rnorm(20)
y = round(mean.y + epsilon, 2)
dat = data.frame(y,X1,X2)

cbind(dat[1:5,],dat[6:10,],dat[11:15,],dat[16:20,])

# Here is how to see the information in the variable X1
class(dat$X1)
levels(dat$X1)

# Fit a model in R using X1. 
#  See how it writes the parameter estimates.
mod.cat = lm(y ~ X1, data=dat)
summary(mod.cat)

# Add the numerical variable to see how that works.
#  Could add the interaction, X1:X2
mod.cat2 = lm(y ~ X1 + X2, data=dat)
summary(mod.cat2)

# Now showing how to create dummy (indicator) variables
#   because some functions only work with numerical vars
library(caret)
dv1 = dummyVars("~.", data=dat)
dv2 = predict(dv1, newdata=dat)
dat.dv = data.frame(dv2)
# Note that it createsd ALL Q variables, and does not drop any
head(dat.dv)

# See what happens when all the indicators are in the model together
mod.cat.dv = lm(y ~ ., data=dat.dv)
summary(mod.cat.dv)

#############################################################
# 2. Transformations
#  
# The main thing to know here is what functions and other math 
#  operations are possible in R.  Google is your friend.
# Some commone ones
#  Powers: X^2, X^3, etc.
#  Logs: Natural: log()    Base 10: log10()
#  Square roots: sqrt() or X^(1/2) (USE PARENTHESES AROUND POWER)!!!)
#  For other roots, use powers
#  Inverse: 1/X or X^-1
#
# You can either do calculations within lm or create the variables 
#   separately and include them in R. If you do it as a new variable
#   be sure to include it in the data frame.  If you do calculations
#   within lm(formula=), put them in I(...).  The I() function tells
#   R basically to treat the contents as a calculation.
#############################################################





# Probably shouldn't be taking a log here, but showing it for demo.
# Code below produces missing values due to negative X2
dat$logx2 = log(dat$X2)
head(dat)
lm(y~logx2, data=dat)

# Or can use log() directly in formula:
lm(y~log(X2), data=dat)

# Same ideas work for math operations
dat$invx2 = 1/X2
head(dat)

# Math operations within formula need to be done carefully
# Adding inverse of x within lm
# Without it fits the wrong model

mod.noI = lm(y~ 1/X2, data=dat)
summary(mod.noI)
# With I() does it right
mod.Internal = lm(y~ I(1/X2), data=dat)
summary(mod.Internal)

#############################################################
# 3. Interactions and other functions
#
# Crossproducts are usually handled by addind them to the formula
#   in lm().  The form is X1:X2.
# Other fuctions are easy: just create them as variables and 
#   add them to the model
#############################################################

# Creating another variable for our interactions and functions
dat$X3 = runif(n=20, min=10, max=20)
head(dat)

# Adding X2:X3 crossproduct
#  First show model without crossproduct
mod.23 = lm(y ~ X2 + X3, data=dat)
summary(mod.23)

# Look at a plot
x1. <- seq(from=-3, to=3, by=.1)
xy1 <- data.frame(expand.grid(X2=seq(from=-3, to=3, by=.1), 
                              X3=seq(from=10, to=20, by=.2)))
pred <- predict(mod.23 ,newdata=xy1)
surface = matrix(pred, nrow=length(x1.))

library(rgl)  
open3d()
persp3d(x = seq(from=-3, to=3, by=.1), y = seq(from=10, to=20, by=.2), 
        z = surface, col = "orange", xlab="X2", ylab="X3", 
        zlab="Predicted Y")
points3d(dat$y ~ dat$X2 + dat$X3, col="blue")


# Adding X2:X3 crossproduct
#   Now add crossproduct
mod.cp = lm(y ~ X2 + X3 + X2:X3, data=dat)
summary(mod.cp)

# Look at a plot
x1. <- seq(from=-3, to=3, by=.1)
xy1 <- data.frame(expand.grid(X2=seq(from=-3, to=3, by=.1), 
                              X3=seq(from=10, to=20, by=.2)))
pred <- predict(mod.cp ,newdata=xy1)
surface = matrix(pred, nrow=length(x1.))

library(rgl)  
open3d()
persp3d(x = seq(from=-3, to=3, by=.1), y = seq(from=10, to=20, by=.2), 
        z = surface, col = "orange", xlab="X2", ylab="X3", 
        zlab="Predicted Y")
points3d(dat$y ~ dat$X2 + dat$X3, col="blue")



# Now making a new variable as function of two variables, X3/(X2+X3)
dat$X4 = dat$X3 / (dat$X3+dat$X2)

#Adding these into model just treats them as regular variables
mod.func = lm(y~X1 + X2 + X3 + X4, data=dat)
summary(mod.func)



