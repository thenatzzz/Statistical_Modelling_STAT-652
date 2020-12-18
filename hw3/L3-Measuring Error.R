#####################################################################
# Simulation showing properties of various ways of measuring error
#
#   Generating data from same structure we used for showing boas and various
#   Fitting models to the data 
#   Measuring the error of these models in various ways
#   Demonatrating the variability and other properties of error measures
#


# Set starting point for random number generation 
set.seed(26891074)

# Start with a population: 
# y = beta0 + beta1 x + beta2 x^2 + beta3 x^3 + beta4 x^4 + epsilon, epsilon~N(0,sigma^2)
#   Create the parameters of the population regression line
beta0 = 2
beta1 = 13/5
beta2 = 8/15
beta3 = -8/5
beta4 = -8/15
sigma = .3
n=10

x = runif(n, min=-1, max=1)
epsilon = rnorm(n=n, mean=0, sd=sigma)
y = beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4 + epsilon

###### Part 1: What we'd like to do is generate a giant new sample
# Fit a model
# Generate enormous test data from same population structure
# Compute mean squared error from model fit


x11(h=6, w=10)
par(mfrow=c(1,2))

# Estimating the regression line
mod1 = lm(y~x)
pred = predict(mod1)
sMSE = mean((y - pred)^2)

# Plot this pop structure, data, model fit
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
    ylim=c(min(y)-.2, max(y)+.2),
     col="red", lwd=3, xlab="X", ylab="Y",  
     main=paste("Structure, Sample, and Model \nRootsMSE=",round(sqrt(sMSE),2)))
points(x=x, y=y, pch="x", col="blue")
abline(mod1, col="blue", lwd=3)


#  Test data: n.t obs

n.t=10000
x.t = runif(n.t, min=-1, max=1)
epsilon.t = rnorm(n=n.t, mean=0, sd=sigma)
y.t = beta0 + beta1*x.t + beta2*x.t^2 + beta3*x.t^3 + beta4*x.t^4 + epsilon.t

# Predicted values
pred.t = predict(mod1, newdata=data.frame(x=x.t))
MSPE.t = mean((y.t - pred.t)^2)

pred = predict(mod1)
sMSE = mean((y - pred)^2)

#####
# Add sampled points and an estimated regression line
#####
plot(x=x.t, y=y.t, pch=16, cex=.5, col="lightblue", xlab="X", ylab="Y",
     main = paste0("Test data \nRoot MSPE=", round(sqrt(MSPE.t),2)))
# Estimating the regression line
abline(mod1, col="blue", lwd=3)


################################################
# Repeat for a quadratic
#
#


x11(h=6, w=10)
par(mfrow=c(1,2))

mod2 = lm(y~x+I(x^2))
predfun=function(x){predict(mod2,data.frame(x))}
pred = predict(mod2)
sMSE = mean((y - pred)^2)

# Plot this pop structure, data, model fit
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      ylim=c(min(y)-.2, max(y)+.2),
      col="red", lwd=3, xlab="X", ylab="Y",  
      main=paste("Structure, Sample, and Model \nRootsMSE=",round(sqrt(sMSE),2)))
points(x=x, y=y, pch="x", col="blue")
# Estimating the regression curve
curve(expr=predfun(x), add=TRUE, col="blue", lwd=3)

# Predicted values
pred.t = predict(mod2, newdata=data.frame(x=x.t))
MSPE.t = mean((y.t - pred.t)^2)


#####
# Add sampled points and an estimated regression line
#####
plot(x=x.t, y=y.t, pch=16, cex=.5, col="lightblue", xlab="X", ylab="Y",
     main = paste0("Test data \nRoot MSPE=", round(sqrt(MSPE.t),2)))
# Estimating the regression curve
curve(expr=predfun(x), add=TRUE, col="blue", lwd=3)





################################################
# Repeat for 4th order
#
#


x11(h=6, w=10)
par(mfrow=c(1,2))

mod4 = lm(y~x+I(x^2)+I(x^3)+I(x^4))
predfun=function(x){predict(mod4,data.frame(x))}
pred = predict(mod4)
sMSE = mean((y - pred)^2)

# Plot this pop structure, data, model fit
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      ylim=c(min(y)-.2, max(y)+.2),
      col="red", lwd=3, xlab="X", ylab="Y",  
      main=paste("Structure, Sample, and Model \nRootsMSE=",round(sqrt(sMSE),2)))
points(x=x, y=y, pch="x", col="blue")
# Estimating the regression curve
curve(expr=predfun(x), add=TRUE, col="blue", lwd=3)

# Predicted values
pred.t = predict(mod4, newdata=data.frame(x=x.t))
MSPE.t = mean((y.t - pred.t)^2)


#####
# Add sampled points and an estimated regression line
#####
plot(x=x.t, y=y.t, pch=16, cex=.5, col="lightblue", xlab="X", ylab="Y",
     main = paste0("Test data \nRoot MSPE=", round(sqrt(MSPE.t),2)))
# Estimating the regression curve
curve(expr=predfun(x), add=TRUE, col="blue", lwd=3)



################################################
# Repeat for 7th order
#
#


x11(h=6, w=10)
par(mfrow=c(1,2))

mod7 = lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6)+I(x^7))
pred = predict(mod7)
predfun=function(x){predict(mod7,data.frame(x))}
sMSE = mean((y - pred)^2)

# Plot this pop structure, data, model fit
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      ylim=c(min(y)-.2, max(y)+.2),
      col="red", lwd=3, xlab="X", ylab="Y",  
      main=paste("Structure, Sample, and Model \nRootsMSE=",round(sqrt(sMSE),2)))
points(x=x, y=y, pch="x", col="blue")
# Estimating the regression curve
curve(expr=predfun(x), add=TRUE, col="blue", lwd=3)

# Predicted values
pred.t = predict(mod7, newdata=data.frame(x=x.t))
MSPE.t = mean((y.t - pred.t)^2)


#####
# Add sampled points and an estimated regression line
#####
plot(x=x.t, y=y.t, pch=16, cex=.5, col="lightblue", xlab="X", ylab="Y",
     main = paste0("Test data \nRoot MSPE=", round(sqrt(MSPE.t),2)))
# Estimating the regression curve
curve(expr=predfun(x), add=TRUE, col="blue", lwd=3)

####################################
# Rerun models of all sizes, compute sMSE and MSPE
# Plot and compare these values

errmat = matrix(NA, nrow=n, ncol=3)

mod=lm(y~1)

pred = predict(mod)
sMSE = mean((y - pred)^2)
pred.t = predict(mod, newdata=data.frame(x=x.t))
MSPE.t = mean((y.t - pred.t)^2)

errmat[1,] = c(0, sMSE, MSPE.t)

for(r in 1:(n-1)){
  mod=lm(y~poly(x,r))
  pred = predict(mod)
  sMSE = mean((y - pred)^2)
  pred.t = predict(mod, newdata=data.frame(x=x.t))
  MSPE.t = mean((y.t - pred.t)^2)
  
  errmat[r+1,] = c(r, sMSE, MSPE.t)
}

errmat

x11()
plot(x=errmat[,1], y=sqrt(errmat[,2]), type="l", col=colors()[121], lwd=2, 
     main = paste0("Comparison of sMSE and MSPE for \npolynomial models for n=",n))
lines(x=errmat[,1], y=sqrt(errmat[,3]), col=colors()[145], lwd=2)
legend(x=2, y=1, legend=c("sMSE","MSPE"), lwd=2, col=colors()[c(121,145)])




