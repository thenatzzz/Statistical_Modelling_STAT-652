###########################################################################
### We've got some new types of spline to try. Let's see how they do on ###
### the wine quality data. We focus on alcohol and density              ###
###########################################################################

### This script has some randomness in it, so set the seed
set.seed(24705131)

### Read in the wine data. Remove any repeated density values
source("Read Wine Data - Remove Duplicates.R")

### Let's start with smoothing splines. Fit various degrees of freedom.
### We use the smooth.spline function to fit a smoothing spline. This function
### takes x and y specified as separate vectors. We can also set degrees of
### freedom using the df input.
fit.smooth.4 = smooth.spline(x = data$density, y = data$alcohol, df = 4)
fit.smooth.8 = smooth.spline(x = data$density, y = data$alcohol, df = 8)
fit.smooth.12 = smooth.spline(x = data$density, y = data$alcohol, df = 12)

### We can also fit smoothing splines using CV and GCV. Set cv to TRUE for 
### CV and FALSE for GCV
fit.smooth.CV = smooth.spline(x = data$density, y = data$alcohol, cv=T)
fit.smooth.GCV = smooth.spline(x = data$density, y = data$alcohol, cv=F)


### Plot the data, and add a legend to distinguish our splines
### We create legends using the legend() function. The first input is the
### position, which can be numeric (this is hard to get right), or a name, like
### "topright" or "center". The next inputs specify what we want our legend to
### say. See this tutorial's video for more details.
with(data, plot(density, alcohol, 
  main = "Smoothing Splines for the Wine Quality Data"))
legend("topright", title = "Degrees of Freedom", legend = c("4", "8", "12"),
  col = c("red", "blue", "green"), lty = 1)

### Add splines to our plot. We can add smoothing splines using the lines()
### function. Remember to match colors to what we set in legend()
lines(fit.smooth.4, col = "red")
lines(fit.smooth.8, col = "blue")
lines(fit.smooth.12, col = "green")

### Create a new plot with "optimal" smoothing splines
with(data, plot(density, alcohol, 
  main = "Optimal Smoothing Splines for the Wine Quality Data"))
legend("topright", title = "Degrees of Freedom", legend = c("CV", "GCV"),
  lty = c(2, 3))
lines(fit.smooth.CV, lty = 2)
lines(fit.smooth.GCV, lty = 3)



### Now, let's move on to loess. We fit loess models using the loess() 
### function. This function uses data frame & formula syntax instead
### of x & y vectors syntax. We can specify how many degrees of freedom
### to use with the enp.target input.

fit.loess.4 = loess(alcohol ~ density, data = data,
  enp.target = 4)
fit.loess.8 = loess(alcohol ~ density, data = data,
  enp.target = 8)
fit.loess.12 = loess(alcohol ~ density, data = data,
  enp.target = 12)

### Get predicted values for our fits. First, construct a grid of ordered
### density values.
min.dens = min(data$density)
max.dens = max(data$density)
vals.dens.raw = seq(from = min.dens, to = max.dens, length.out = 100)
vals.dens = data.frame(density = vals.dens.raw)

pred.loess.4 = predict(fit.loess.4, vals.dens)
pred.loess.8 = predict(fit.loess.8, vals.dens)
pred.loess.12 = predict(fit.loess.12, vals.dens)

### Plot the data and add loess curves. Remember a legend.
with(data, plot(density, alcohol, 
  main = "LOESS Smoothers for the Wine Quality Data"))
legend("topright", title = "Degrees of Freedom", legend = c("4", "8", "12"),
  col = c("red", "blue", "green"), lty = 1)

lines(x = vals.dens$density, y = pred.loess.4, col = "red")
lines(x = vals.dens$density, y = pred.loess.8, col = "blue")
lines(x = vals.dens$density, y = pred.loess.12, col = "green")

