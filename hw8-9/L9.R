### In this lecture we will again analyze the relationship between 
### density and alcohol in the wine dataset. This time, we will use
### various kinds of spline

### Run this new script to read in the relevant variables from the
### forest fire dataset
source("Read Wine Data.R")

### We will use functions from the splines package to fit spline models
library(splines)

#####################################################################
### First, fit some polynomial models of various degrees and look ###
### at the plots.                                                 ###
#####################################################################

### Fit polynomial regression models
fit.poly.1 = lm(alcohol ~ density, data = data)
fit.poly.2 = lm(alcohol ~ poly(density, degree = 2), data = data)
fit.poly.4 = lm(alcohol ~ poly(density, degree = 4), data = data)
fit.poly.12 = lm(alcohol ~ poly(density, degree = 12), data = data)

### Get predictions.
### Note: For plotting to work properly, we need to get predictions
###       on the sorted density values.
density.sort = data.frame(density = sort(data$density))
pred.poly.1 = predict(fit.poly.1, density.sort)
pred.poly.2 = predict(fit.poly.2, density.sort)
pred.poly.4 = predict(fit.poly.4, density.sort)
pred.poly.12 = predict(fit.poly.12, density.sort)

### Plot data and predictions on the same axes
with(data, plot(density, alcohol))
lines(density.sort$density, pred.poly.1)
lines(density.sort$density, pred.poly.2, col = "red")
lines(density.sort$density, pred.poly.4, col = "blue")
lines(density.sort$density, pred.poly.12, col = "green")


##################################################
### Let's see if we can do better with splines ###
##################################################

### First, fit basis splines
fit.basis.4 = lm(alcohol ~ bs(density, df = 4), data = data)
fit.basis.8 = lm(alcohol ~ bs(density, df=8), data = data)
fit.basis.12 = lm(alcohol ~ bs(density, df=12), data = data)

### Get predictions
pred.basis.4 = predict(fit.basis.4, density.sort)
pred.basis.8 = predict(fit.basis.8, density.sort)
pred.basis.12 = predict(fit.basis.12, density.sort)

### Plot data and splines
with(data, plot(density, alcohol))
lines(density.sort$density, pred.basis.4, col = "red")
lines(density.sort$density, pred.basis.8, col = "blue")
lines(density.sort$density, pred.basis.12, col = "green")



### Try again with natural splines
fit.nat.4 = lm(alcohol ~ ns(density, df = 4), data = data)
fit.nat.8 = lm(alcohol ~ ns(density, df=8), data = data)
fit.nat.12 = lm(alcohol ~ ns(density, df=12), data = data)

### Get predictions
pred.nat.4 = predict(fit.nat.4, density.sort)
pred.nat.8 = predict(fit.nat.8, density.sort)
pred.nat.12 = predict(fit.nat.12, density.sort)

### Plot data and splines
with(data, plot(density, alcohol))
lines(density.sort$density, pred.nat.4, col = "red")
lines(density.sort$density, pred.nat.8, col = "blue")
lines(density.sort$density, pred.nat.12, col = "green")
