#########################################################################
# Generalized Additive Model on Prostate Data 
rm(list=ls(all=TRUE))

prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")
#head(prostate)

library(mgcv)

#  Generalized additive model on all variables
gam.all <- gam(data=prostate,
            formula=lpsa ~s(lcavol)+s(lweight) + svi + gleason + s(lcp) + s(age) + s(pgg45) + s(lbph), 
            family=gaussian(link=identity)) 
summary(gam.all)

# Plots of splines in each dimension
#  Dashed lines are 2-SE bounds
x11(h=15,w=15,pointsize=12)
par(mfrow=c(3,2))
plot(gam.all, main="GAM marginal splines")
#  Then creating matrix of estimated means by direct calculation.  Matrix form required by contour()  


#  Generalized additive model on just two vars so we can show plot
gam.2 <- gam(data=prostate,
               formula=lpsa ~s(lcavol)+ s(pgg45), 
               family=gaussian(link=identity)) 
summary(gam.2)

# Plots of splines in each dimension
#  Dashed lines are 2-SE bounds
x11(h=7,w=15,pointsize=12)
par(mfrow=c(1,2))
plot(gam.2, main="GAM marginal splines")

library(rgl)  

x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

pred2 <- predict(gam.2 ,newdata=xy1)
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")

