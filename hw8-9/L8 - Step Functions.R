#########################################################################
# Step Functions shown on the Prostate Data 
# 
rm(list=ls(all=TRUE))

prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")
summary(prostate)

# The cut() function turns a numeric variable into a factor with 
#   levels created by the "breaks=".  Data that fall beyond the 
#   do not get assigned to a group!
# By default, intervals are open on the left and closed on the right.
#   Changing that here with "right=FALSE"
regions = cut(x=prostate$age, breaks=c(0, 50, 60, 70, 100), 
              right=FALSE)
regions[1:10]
head(data.frame(prostate$age,regions))

#  Plot of the data, with lines to indicate groups
# x11()
plot(x=prostate$age, y=prostate$lpsa, 
     main="lpsa vs. age groups for prostate data")
abline(v=c(49.5, 59.5, 69.5), col="red", lty="dotted")

# Run the regression on the age groups
mod.step = lm(prostate$lpsa~regions)
summary(mod.step)

# Get predicted values for age groups.
#  "newdata" has to be a data.frame with the vaiable name from 
#     the regression
#  levels() returns the levels of the factor in order, just 
#     what we want
age.means = predict(mod.step, 
                    newdata=data.frame(regions=levels(regions)))
# Add lines for means within each level
lines(x=c(0,50), y=rep(age.means[1],2), col="blue", lwd=2)
lines(x=c(50,60), y=rep(age.means[2],2), col="blue", lwd=2)
lines(x=c(60,70), y=rep(age.means[3],2), col="blue", lwd=2)
lines(x=c(70,100), y=rep(age.means[4],2), col="blue", lwd=2)

##################################################
# Two variables, lcavol and pgg45
#   Using median from each as cutpoint, 
#   so will create 4 means, Lo-Lo, Lo-Hi, Hi-Lo, and Hi-Hi
#
# With two groups, we can create indicators directly instead of factors with groupings.  

lcavol.hilo = (prostate$lcavol < median(prostate$lcavol))
pgg45.hilo = (prostate$pgg45 < median(prostate$pgg45))

median(prostate$lcavol)
median(prostate$pgg45)

head(data.frame(prostate$lcavol, lcavol.hilo, prostate$pgg45, pgg45.hilo))
tail(data.frame(prostate$lcavol, lcavol.hilo, prostate$pgg45, pgg45.hilo))

#No interaction
mod.2step = lm(prostate$lpsa ~ lcavol.hilo + pgg45.hilo)
# Could also run with interaction using X1*X2 for X1+X2

# Plot the data and both model prediction surfaces

library(rgl)  
open3d()
plot3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")

x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

xy1c = data.frame(lcavol.hilo = (xy1$lcavol < median(prostate$lcavol)),
                  pgg45.hilo = (xy1$pgg45 < median(prostate$pgg45)))

pred2 <- predict(mod.2step ,newdata=xy1c)
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")
