#####################################################################
# Simulation of some properties of linear regression
#
# Demonstrating Bias and Variance for Linear Regression
#  What happens when the population is not a perfectly linear trend?
#  Examples of slightly nonlinear and highly nonlinear
# In exercises, change n, sigma and see what happens


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
# n=10
n=50

x = runif(n, min=-1, max=1)
epsilon = rnorm(n=n, mean=0, sd=sigma)
y = beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4 + epsilon

x11()
par(mfrow=c(2,2))

# Plot this line
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
    ylim=c(min(y)-.2, max(y)+.2),
     col="red", lwd=3, xlab="X", ylab="Y",  
     main=paste("Population truth and \n one sample of n=",n))
points(x=x, y=y, pch="x", col="blue")


#####
# Add sampled points and an estimated regression line
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      ylim=c(min(y)-.2, max(y)+.2),  #c(0.5,3.8),
      col="red", lwd=1, xlab="X", ylab="Y", lty="dotted" ,
      main=paste("Estimated regression line vs. truth"))
#####
points(x=x, y=y, pch="x", col="blue")
# Estimating the regression line
mod1 = lm(y~x)
abline(mod1, col="blue", lwd=3)

####################################
# Evaluate the regression process
# Repeat for 100 samples 

curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      ylim=c(min(y)-4*sigma, max(y)+4*sigma),  #c(0.5,3.8),
      col="red", lwd=3, xlab="X", ylab="Y",  
      main=paste("Comparing many regression lines \nfrom 100 different samples of n=",n))
#####

betas = matrix(NA, nrow=100, ncol=2)

for(jj in 1:100){
  x = runif(n=n, min=-1, max=1)
  epsilon = rnorm(n=n, mean=0, sd=sigma)
  y =  beta0 + beta1*x + beta2*x^2 + beta3*x^3  + beta4*x^4+ epsilon
  mod = lm(y~x)
  betas[jj,] = coef(mod)
  abline(mod, col="lightblue")
}

curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      col="red", lwd=2, add=TRUE) 
      
###############################
# Focus on the comparison between true mean and average regression

curve(expr=mean(betas[,1]) + mean(betas[,2])*x, from=-1, to=1, 
      col="blue", lwd=2, xlab="X", ylab="Y",  
      main=paste("How well does regression estimate \ntrue mean on average?"))
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      col="red", lwd=2, add=TRUE)  


#curve(expr=mean(betas[,1]) + mean(betas[,2])*x, from=-1, to=1, 
#      col="blue", lwd=2, xlab="X", ylab="Y",  
#      main=paste("Bias: how well does a straight line fit a curve?"))
#curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
#      col="red", lwd=2, add=TRUE)  

##################################################################################
##################################################################################
##################################################################################
# What if I fit the right model? Fitting quartic

# Start with a population: 
# y = beta0 + beta1 x + beta2 x^2 + beta3 x^3  + beta4*x^4+ epsilon, epsilon~N(0,sigma^2)
#   Create the parameters of the population regression line
set.seed(26891074)

x = runif(n, min=-1, max=1)
epsilon = rnorm(n=n, mean=0, sd=sigma)
y = beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4 + epsilon

x11()
par(mfrow=c(2,2))

# Plot this line
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      ylim=c(0.5,3.8),
      col="red", lwd=3, xlab="X", ylab="Y",  
      main=paste("Population truth and \n one sample of n=",n))
points(x=x, y=y, pch="x", col="blue")


#####
# Add sampled points and an estimated regression CURVE
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      ylim=c(min(y)-.2, max(y)+.2),  #c(0.5,3.8),
      col="red", lwd=1, xlab="X", ylab="Y", lty="dotted" ,
      main=paste("Estimated regression line vs. truth"))
#####
points(x=x, y=y, pch="x", col="blue")
######
# Estimating the regression line
mod = lm(y~x+I(x^2)+I(x^3)+I(x^4))
#abline(mod1, col="brown", lwd=2)
predfun=function(x){predict(mod,data.frame(x))}
curve(expr=predfun, col="blue", lwd=3, add=TRUE)

####################################
# Evaluate the regression process
# Repeat for 100 samples 

curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      ylim=c(min(y)-6*sigma, max(y)+6*sigma),  #c(0.5,3.8),
      col="red", lwd=3, xlab="X", ylab="Y",  
      main=paste("Comparing many regression curves \nfrom 1000 different samples of n=",n))
#####

betas = matrix(NA, nrow=100, ncol=5)

for(jj in 1:100){
  x = runif(n=n, min=-1, max=1)
  epsilon = rnorm(n=n, mean=0, sd=sigma)
  y =  beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4 + epsilon
  mod = lm(y~x+I(x^2)+I(x^3)+I(x^4))
  betas[jj,] = coef(mod)
  curve(expr=predfun, col="lightblue", add=TRUE)
}

curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      col="red", lwd=2, add=TRUE) 

###############################
# Focus on the comparison between true mean and average regression

avbet = apply(betas, 2, mean)

curve(expr=avbet[1] + avbet[2]*x + avbet[3]*x^2 + avbet[4]*x^3 + avbet[5]*x^4, from=-1, to=1, 
      col="blue", lwd=2, xlab="X", ylab="Y",  
      main=paste("How well does regression estimate \ntrue mean on average?"))
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
      col="red", lwd=2, add=TRUE)  



#avbet = apply(betas, 2, mean)

#curve(expr=avbet[1] + avbet[2]*x + avbet[3]*x^2 + avbet[4]*x^3 + avbet[5]*x^4, from=-1, to=1, 
#      col="blue", lwd=2, xlab="X", ylab="Y",  
#      main=paste("Bias might be reduced if we fit a better model"))
#curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, from=-1, to=1, 
#      col="red", lwd=2, add=TRUE)  



#
# Clear plots
dev.off()  # But only if there IS a plot

# clear variable
# rm(list=ls(all=TRUE))
