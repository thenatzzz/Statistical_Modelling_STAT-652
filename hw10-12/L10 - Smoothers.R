# Fitting Smoothing splines and LOESS to Covid-19 data
rm(list=ls(all=TRUE))

covid <-  read.csv ("Covid BC 2020-09-22.csv", header=TRUE)
covid$date = as.Date(covid$Date, "%Y-%m-%d")
head(covid)
summary(covid)

#######################################################################
# Next try smoothing splines using the smooth.spline() function
#   Specify x and y and some other parameters to control the DF.
#   Default is to do GCV, but CV can be requested, too.

x11(h=7, w=10)

plot(x=covid$date, y=covid$Cases, type="l", col="gray", 
     main="Plot of Daily BC Covid-19 Cases over time")
legend(x=18300, y=170, legend=c( "Smoothing Spline 6 df", 
                                "Smoothing Spline 9 df", "Smoothing Spline, 12 df"), 
       lty="solid", col=colors()[c(121,145,84)], lwd=2)
# legend(x=18300, y=170, legend=c("Cubic poly", "Smoothing Spline 6 df", 
#                                 "Smoothing Spline 9 df", "Smoothing Spline, 12 df"), 
#        lty="solid", col=colors()[c(121,145,84)], lwd=2)

# Smoothing spline using smooth.spline()
# Note: Can specify DF.  
# If not specified, Generalized Crossvalidation is used to find "best" lambda 
#   and estimate equivalent DF,

# 6 DF spline
sm.spl.6 <- smooth.spline(x=covid$date, y=covid$Cases, df=6)
sm.spl.6
lines(sm.spl.6, col=colors()[121], lwd=2)

# 9 DF spline
sm.spl.9 <- smooth.spline(x=covid$date, y=covid$Cases, df=9)
sm.spl.9
lines(sm.spl.9, col=colors()[145], lwd=2)

# 12 DF spline
sm.spl.12 <- smooth.spline(x=covid$date, y=covid$Cases, df=12)
sm.spl.12
lines(sm.spl.12, col=colors()[84], lwd=2)


# Optimal Spline.  
#   "CV=TRUE" uses N-fold CV.  NOT RECOMMENDED IF DUPLICATE VALUES OF X EXIST
#   "CV=FALSE" uses generalized CV (GCV)

# IN THIS EXAMPLE, GCV Doesn't work well.  N-Fold *seems* to do something reasonable.
x11(h=7, w=10)

plot(x=covid$date, y=covid$Cases, type="l", col="gray", 
     main="Comparison of 'Optimum' Smoothing splines")
legend(x=18300, y=170, legend=c("N-Fold CV", "Generalized CV"), 
       lty="solid", col=colors()[c(121,91)], lwd=2)

sm.spl.opt <- smooth.spline(x=covid$date, y=covid$Cases, cv=TRUE)
sm.spl.opt
lines(sm.spl.opt, col=colors()[121], lwd=2)

sm.spl.opt2 <- smooth.spline(x=covid$date, y=covid$Cases, cv=FALSE)
sm.spl.opt2
lines(sm.spl.opt2, col=colors()[91], lwd=2)


###########################################################################
# Local polynomial regression using LOESS
#
#  Uses loess() from stats package.  
#     Default degree=2 (quadratic), span=.75, Tri-cube kernel
#  Can instead use approximate DF with "enp.target"

x11(h=7, w=10)

plot(x=covid$date, y=covid$Cases, type="l", col="gray", 
     main="Plot of Daily BC Covid-19 Cases over time")
legend(x=18300, y=170, legend=c("LOESS 6 df", 
                                "LOESS 9 df", "LOESS, 12 df"), 
       lty="solid", col=colors()[c(121,145,84)], lwd=2)

lo.6 <- loess(data=covid, formula=Cases~as.numeric(date), enp.target=6)
summary(lo.6)
lines(x=covid$date, y=predict(lo.6), col=colors()[121], lwd=2)

lo.9 <- loess(data=covid, formula=Cases~as.numeric(date), enp.target=9)
summary(lo.9)
lines(x=covid$date, y=predict(lo.9), col=colors()[145], lwd=2)

lo.12 <- loess(data=covid, formula=Cases~as.numeric(date), enp.target=12)
summary(lo.12)
lines(x=covid$date, y=predict(lo.12), col=colors()[84], lwd=2)


#######################################################################
# Comparison of 12df fits
x11(h=7, w=10)
library(splines)

plot(x=covid$date, y=covid$Cases, type="l", col="gray", 
     main="Comparison of 12-DF fits")
legend(x=18290, y=170, legend=c("Polynomial", "Cubic reg Spline", "Natural Spline", 
                                "Smoothing Spline", "LOESS"), 
       lty="solid", col=colors()[c(121,145,84,53,24)], lwd=2)

poly12 <- lm(data=covid, Cases ~ poly(x=date, degree=12))
lines(x=covid$date, y=predict(poly12, newdata=covid), col=colors()[121], lwd=2)

cub.spl.12 <- lm(data=covid, Cases ~ bs(date,df=12))
lines(x=covid$date, y=predict(cub.spl.12, newdata=covid), col=colors()[145], lwd=2)

nat.spl.12 <- lm(data=covid, Cases ~ ns(date,df=12))
lines(x=covid$date, y=predict(nat.spl.12, newdata=covid), col=colors()[84], lwd=2)

lines(sm.spl.12, col=colors()[53], lwd=2)
lines(x=covid$date, y=predict(lo.12), col=colors()[24], lwd=2)




x11(h=7, w=10)
library(splines)

plot(x=covid$date[1:150], y=covid$Cases[1:150], type="l", col="gray", 
     main="Comparison of 12-DF fits")
legend(x=18300, y=80, legend=c("Polynomial", "Cubic reg Spline", "Natural Spline", 
                                "Smoothing Spline", "LOESS"), 
       lty="solid", col=colors()[c(121,145,84,53,24)], lwd=2)

poly12 <- lm(data=covid, Cases ~ poly(x=date, degree=12))
lines(x=covid$date[1:150], y=predict(poly12, newdata=covid)[1:150], col=colors()[121], lwd=2)

cub.spl.12 <- lm(data=covid, Cases ~ bs(date,df=12))
lines(x=covid$date, y=predict(cub.spl.12, newdata=covid), col=colors()[145], lwd=2)

nat.spl.12 <- lm(data=covid, Cases ~ ns(date,df=12))
lines(x=covid$date[1:150], y=predict(nat.spl.12, newdata=covid)[1:150], col=colors()[84], lwd=2)

lines(sm.spl.12, col=colors()[53], lwd=2)
lines(x=covid$date, y=predict(lo.12), col=colors()[24], lwd=2)

