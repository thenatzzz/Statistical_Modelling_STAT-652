prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")
round(head(prostate), digits=3)

x11()
pairs(prostate)

mod.vol = lm(lpsa ~ lcavol, data=prostate)
mod.pgg = lm(lpsa ~ pgg45, data=prostate)

x11(h=7, w=12)
par(mfrow=c(1,2))
plot(y=prostate$lpsa, x=prostate$lcavol, 
     main="lpsa vs. lcavol with fitted line")
abline(mod.vol, col="blue", lwd=2, lty="dotted")

plot(y=prostate$lpsa, x=prostate$pgg45, 
     main="lpsa vs. pgg45 with fitted line")
abline(mod.pgg, col="blue", lwd=2, lty="dotted")
mean0 = mean(prostate$lpsa[prostate$pgg45==0])
points(x=0, y=mean0, pch=18, col="red", cex=2)


library(rgl)  
open3d()
plot3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")

mod2 =  lm(lpsa ~ lcavol + pgg45, data=prostate)

x1 <- seq(from=-2, to=4, by=.05)
xy1 <- data.frame(expand.grid(lcavol=seq(from=-2, to=4, by=.05), 
                              pgg45=seq(from=0, to=100, by=.5)))
pred <- predict(mod2 ,newdata=xy1)
surface = matrix(pred, nrow=length(x1))

library(rgl)  
open3d()
persp3d(x = seq(from=-2, to=4, by=.05), y = seq(from=0, to=100, by=.5), 
        z = surface, col = "orange", xlab="lcavol", ylab="pgg45", 
        zlab="Predicted lpsa")
points3d(prostate$lpsa ~ prostate$lcavol + prostate$pgg45, col="blue")

# Compare estimated regression corfficients from separate regs vs. multiple

summary(mod.vol)
summary(mod.pgg)
summary(mod2)

cor(prostate)


