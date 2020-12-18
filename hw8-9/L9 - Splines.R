# Fitting various splines to Covid-19 data

covid <-  read.csv ("Covid BC 2020-09-22.csv", header=TRUE)
covid$date = as.Date(covid$Date, "%Y-%m-%d")
head(covid)
summary(covid)

# Plot of data
# x11(h=7, w=10)
plot(x=covid$date, y=covid$Cases, type="l", main="Plot of Daily BC Covid-19 Cases over time")

##################################################################
# Plot with added polynomials
x11(h=7, w=10)
plot(x=covid$date, y=covid$Cases, type="l", col="gray", 
     main="Plot of Daily BC Covid-19 Cases over time")

# Adding legend to the plot.  Note that "as.numeric(covid$date)"
#   shows that dates are represented numerically by numbers
#   ranging from 18287 to 18527, so putting legend corner at 18300
legend(x=18300, y=170, 
       legend=c("3rd order poly", "6th order poly", "9th order poly", '12th order poly'), lty="solid",
       col=colors()[c(24,121,145,84)], lwd=2)

# Add cubic polynomial to plot (3 df model)
poly3 <- lm(data=covid, Cases ~ poly(x=date, degree=3))
summary(poly3)
lines(x=covid$date, y=predict(poly3, newdata=covid), col=colors()[24], lwd=2)
# Add 6th order polynomial to plot (3 df model)
poly6 <- lm(data=covid, Cases ~ poly(x=date, degree=6))
#summary(poly6)
lines(x=covid$date, y=predict(poly6, newdata=covid), col=colors()[121], lwd=2)
# Add 9th polynomial to plot (9 df model)
poly9 <- lm(data=covid, Cases ~ poly(x=date, degree=9))
#summary(poly9)
lines(x=covid$date, y=predict(poly9, newdata=covid), col=colors()[145], lwd=2)
# Add 12th polynomial to plot (9 df model)
poly12 <- lm(data=covid, Cases ~ poly(x=date, degree=12))
#summary(poly12)
lines(x=covid$date, y=predict(poly12, newdata=covid), col=colors()[84], lwd=2)

###################################################################
# Now use splines.  Will try three different cubic splines:
#   6df (3 knots), 9df (6 knots), 12df (9 knots)
# First preparing a new plot
x11(h=7, w=10)
plot(x=covid$date, y=covid$Cases, type="l", col="gray", 
     main="Plot of Daily BC Covid-19 Cases over time")
legend(x=18300, y=170, legend=c("Cubic poly", "Cubic Spline 6 df", 
                             "Cubic Spline 9 df", "Cubic Spline, 12 df"), 
       lty="solid", col=colors()[c(24,121,145,84)], lwd=2)
lines(x=covid$date, y=predict(poly3, newdata=covid), col=colors()[24], lwd=2)

# Now start fitting splines.  
library(splines)
# Cubic regression spline using bs() to create the basis functions
# 6 DF spline
head(bs(covid$date,df=6))
cub.spl.6 <- lm(data=covid, Cases ~ bs(date,df=6))
summary(cub.spl.6) #Doesn't mean much
lines(x=covid$date, y=predict(cub.spl.6, newdata=covid), col=colors()[121], lwd=2)

# 9 DF spline
#head(bs(covid$date,df=9))
cub.spl.9 <- lm(data=covid, Cases ~ bs(date,df=9))
#summary(cub.spl.9) #Doesn't mean much
lines(x=covid$date, y=predict(cub.spl.9, newdata=covid), col=colors()[145], lwd=2)

# 12 DF spline
#head(bs(covid$date,df=12))
cub.spl.12 <- lm(data=covid, Cases ~ bs(date,df=12))
#summary(cub.spl.12) #Doesn't mean much
lines(x=covid$date, y=predict(cub.spl.12, newdata=covid), col=colors()[84], lwd=2)


####################################################################
# Repeat using natural splines
#   6df (3 knots), 9df (6 knots), 12df (9 knots)
# First preparing a new plot
x11(h=7, w=10)
plot(x=covid$date, y=covid$Cases, type="l", col="gray", 
     main="Plot of Daily BC Covid-19 Cases over time")
legend(x=18300, y=170, legend=c("Cubic poly", "Natural Spline 6 df", 
                                "Natural Spline 9 df", "Natural Spline, 12 df"), 
       lty="solid", col=colors()[c(24,121,145,84)], lwd=2)
lines(x=covid$date, y=predict(poly3, newdata=covid), col=colors()[24], lwd=2)

# NATURAL spline using ns() to create the basis functions
# 6 DF spline
head(ns(covid$date,df=6))
nat.spl.6 <- lm(data=covid, Cases ~ ns(date,df=6))
summary(nat.spl.6) #Doesn't mean much
lines(x=covid$date, y=predict(nat.spl.6, newdata=covid), col=colors()[121], lwd=2)

# 9 DF spline
#head(ns(covid$date,df=9))
nat.spl.9 <- lm(data=covid, Cases ~ ns(date,df=9))
#summary(nat.spl.9) #Doesn't mean much
lines(x=covid$date, y=predict(nat.spl.9, newdata=covid), col=colors()[145], lwd=2)

# 12 DF spline
#head(ns(covid$date,df=12))
nat.spl.12 <- lm(data=covid, Cases ~ ns(date,df=12))
#summary(nat.spl.12) #Doesn't mean much
lines(x=covid$date, y=predict(nat.spl.12, newdata=covid), col=colors()[84], lwd=2)

#######################################################################
# Next try smoothing splines using the smooth.spline() function
#   Specify x and y and some other parameters to control the DF.
#   Default is to do GCV, but CV can be requested, too.

x11(h=7, w=10)

plot(x=covid$date, y=covid$Cases, type="l", col="gray", 
     main="Plot of Daily BC Covid-19 Cases over time")
legend(x=18300, y=170, legend=c("Cubic poly", "Smoothing Spline 6 df", 
                                "Smoothing Spline 9 df", "Smoothing Spline, 12 df"), 
       lty="solid", col=colors()[c(24,121,145,84)], lwd=2)
lines(x=covid$date, y=predict(poly3, newdata=covid), col=colors()[24], lwd=2)
