#########################################################################
# Projection Pursuit on Prostate Data 
rm(list=ls(all=TRUE))


prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")
#head(prostate)

library(rgl)  
# Create 3-D plot to rotate around
open3d()
plot3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")


####################################################################
## One term
####################################################################
#  "scale" rescale variances to SD=1 before computing weights
#    Makes weights interpretable as relative importance
# Also using smoothing spline (sm.method="gcvspline") 
#   rather than default smoother
ppr1 <- ppr(data=prostate, lpsa~scale(lcavol)+scale(pgg45), 
            nterms=1, sm.method="gcvspline") 
summary(ppr1)

# Plots of results
x11(h=7,w=6,pointsize=12)
plot(ppr1, main="Projection Pursuit Regression 1 term", col="orange", ylim=c(-2,4), xlab="Z-ppr")

library(rgl)  

x1 <- seq(from=-2, to=4, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(lcavol=x1, pgg45=x2))

pred2 <- predict(ppr1 ,newdata=xy1)
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")


####################################################################
## All Data, fit 6 terms (max.terms), select best 3
#  In practice, I would tune the nterms
####################################################################

ppr3 <- ppr(lpsa~., data=prostate, nterms=3, 
            max.terms=6, sm.method="gcvspline") 
summary(ppr3)

# Plots of results
x11(h=7,w=6,pointsize=12)
par(mfrow=c(1,3))
plot(ppr3, main="Projection Pursuit Regression 3 term", col="orange", ylim=c(-2,4), xlab="Z-ppr")

