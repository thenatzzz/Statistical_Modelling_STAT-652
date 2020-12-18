# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(2928893) 



####### Application #################
help("airquality")

AQ_original = airquality
head(AQ_original,5)

# X= Solar.R, Wind, Temp
# Y = Ozone
AQ = AQ_original[,1:4]

AQ = na.omit(airquality[,1:4])
AQ$TWcp = AQ$Temp*AQ$Wind # cross-product
AQ$TWrat = AQ$Temp/AQ$Wind # ratio

head(AQ)

####1 median wind,temp
median(AQ$Wind)
median(AQ$Temp)

####2 

Wind.hilo = (AQ$Wind < median(AQ$Wind))
Wind.hilo

Temp.hilo = (AQ$Temp < median(AQ$Temp))
Temp.hilo

head(data.frame(AQ$Wind, Wind.hilo, AQ$Temp, Temp.hilo))
tail(data.frame(AQ$Wind, Wind.hilo, AQ$Temp, Temp.hilo))
data.frame(AQ$Wind, Wind.hilo, AQ$Temp, Temp.hilo)

## 3.
#No interaction
mod.2step = lm(AQ$Ozone ~ Wind.hilo + Temp.hilo)
summary(mod.2step)           

# c
library(rgl)  
open3d()
plot3d(AQ$Ozone ~ AQ$Wind + AQ$Temp, col="blue")

x1 <- seq(from=-2, to=25, by=.05)
# x1 <- seq(from=-2, to=50, by=.05)
x2 = seq(from=0, to=100, by=.5)
# x2 = seq(from=0, to=200, by=.5)
xy1 <- data.frame(expand.grid(Wind=x1, Temp=x2))

xy1c = data.frame(Wind.hilo = (xy1$Wind < median(AQ$Wind)),
                  Temp.hilo = (xy1$Temp < median(AQ$Temp)))

pred2 <- predict(mod.2step ,newdata=xy1c)
surface2 = matrix(pred2, nrow=length(x2))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(AQ$Ozone ~  AQ$Wind+  AQ$Temp, col="blue")

summary(AQ$Wind)
summary(AQ$Temp)

## 4. interaction variable
mod.2step = lm(AQ$Ozone ~ Wind.hilo+Temp.hilo+Wind.hilo * Temp.hilo)
summary(mod.2step)           
# 
# open3d()
# plot3d(AQ$Ozone ~ AQ$Wind + AQ$Temp, col="blue")

x1 <- seq(from=-2, to=25, by=.05)
x2 = seq(from=0, to=100, by=.5)
xy1 <- data.frame(expand.grid(Wind=x1, Temp=x2))

xy1c = data.frame(Wind.hilo = (xy1$Wind < median(AQ$Wind)),
                  Temp.hilo = (xy1$Temp < median(AQ$Temp)))

pred2 <- predict(mod.2step ,newdata=xy1c)
surface2 = matrix(pred2, nrow=length(x1))


open3d()
persp3d(x = x1, y = x2, 
        z = surface2, col = "orange", xlab="Wind", ylab="Temp", 
        zlab="Predicted Ozone")
points3d(AQ$Ozone ~  AQ$Wind+  AQ$Temp, col="blue")

summary(AQ$Wind)
summary(AQ$Temp)
# summary(AQ$TWcp)





######### hw 9 ###########
# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
set.seed(2928893) 

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

library(splines)
head(AQ)

x11(h=7, w=10)

plot(x=AQ$Temp, y=AQ$Ozone, main="Plot Ozone ~ Temp")
# Adding legend to the plot.  Note that "as.numeric(covid$date)"
#   shows that dates are represented numerically by numbers
#   ranging from 18287 to 18527, so putting legend corner at 18300
legend(x="topleft", y=0.92, 
       legend=c("Cubic poly", "Cubic spline 5 df", "Cubic spline 7 df", 'Cubic spline 9 df','Cubic spline 20 df'), lty="solid",
       col=colors()[c(24,121,145,84,33)], lwd=2)

# Add cubic polynomial to plot (3 df model)
poly3 <- lm(data=AQ, Ozone ~ poly(x=Temp, degree=3))
summary(poly3)
lines(AQ$Temp, predict(poly3, newdata=AQ), col=colors()[24], lwd=1)

cub.spl.5 <- lm(data=AQ, Ozone ~ bs(Temp,df=5))
summary(cub.spl.5) #Doesn't mean much
lines(x=AQ$Temp, y=predict(cub.spl.5, newdata=AQ), col=colors()[121], lwd=2)

cub.spl.7 <- lm(data=AQ, Ozone ~ bs(Temp,df=7))
summary(cub.spl.7) #Doesn't mean much
lines(x=AQ$Temp, y=predict(cub.spl.7, newdata=AQ), col=colors()[145], lwd=2)

cub.spl.9 <- lm(data=AQ, Ozone ~ bs(Temp,df=9))
summary(cub.spl.9) #Doesn't mean much
lines(x=AQ$Temp, y=predict(cub.spl.9, newdata=AQ), col=colors()[84], lwd=2)

cub.spl.20 <- lm(data=AQ, Ozone ~ bs(Temp,df=20))
summary(cub.spl.5) #Doesn't mean much
lines(x=AQ$Temp, y=predict(cub.spl.20, newdata=AQ), col=colors()[33], lwd=2)


### 2.
x11(h=7, w=10)

plot(x=AQ$Temp, y=AQ$Ozone, main="Plot Ozone ~ Temp")
legend(x="topleft", y=0.92, 
       legend=c( "Natural spline 5 df", "Natural spline 7 df", 'Natural spline 9 df'), lty="solid",
       col=colors()[c(121,145,84)], lwd=2)
nat.spl.5 <- lm(data=AQ, Ozone ~ ns(Temp,df=5))
summary(nat.spl.5) #Doesn't mean much
lines(x=AQ$Temp, y=predict(nat.spl.5, newdata=AQ), col=colors()[121], lwd=2)

nat.spl.7 <- lm(data=AQ, Ozone ~ ns(Temp,df=7))
summary(nat.spl.7) #Doesn't mean much
lines(x=AQ$Temp, y=predict(nat.spl.7, newdata=AQ), col=colors()[145], lwd=2)

nat.spl.9 <- lm(data=AQ, Ozone ~ ns(Temp,df=9))
summary(nat.spl.9) #Doesn't mean much
lines(x=AQ$Temp, y=predict(nat.spl.9, newdata=AQ), col=colors()[84], lwd=2)