#########################################################################
# Demonatration of the utility of bootstrap aggregation
# 
### Using our 4-th order polynomial model from earlier lectures
#
# Creating 100-obs training set and 1000-obs test set
# Plotting data with a regression tree
# Showing how variable regression tree is in 100 bootstrap resamples
#   Aggregating into 

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
n=100
# Training data
x = runif(n, min=-1, max=1)
epsilon = rnorm(n=n, mean=0, sd=sigma)
y = beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4 + epsilon

x11(h=7, w=7)
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, 
      from=-1, to=1, col="red", lwd=2, ylim=c(0,4.2), ylab = "y",
      main="100 points from 4-ord poly with reg tree")
points(x=x, y=y, col="gray")

#  Test data: n.t obs
x.t = seq(from=-1, to=1, by=1/500)
n.t=length(x.t)
epsilon.t = rnorm(n=n.t, mean=0, sd=sigma)
y.t = beta0 + beta1*x.t + beta2*x.t^2 + beta3*x.t^3 + beta4*x.t^4 + epsilon.t


## Now fit regression tree and add to plot
x.tree <- rpart(y ~ x, method="anova", cp=0)
cpt = x.tree$cptable

# Find location of minimum error
minrow <- which.min(cpt[,4])
# Take geometric mean of cp values at min error and one step up 
cplow.min <- cpt[minrow,1]
cpup.min <- ifelse(minrow==1, yes=1, no=cpt[minrow-1,1])
cp.min <- sqrt(cplow.min*cpup.min)

# Do pruning
pr.prune.min <- prune(x.tree, cp=cp.min)


# Plot this line
preds = predict(pr.prune.min, newdata=data.frame(x=x.t))
points(x=x.t, y=preds, cex=0.2, col="blue")


# Now bootstrap that 100 times
reps=100
pred.tree = matrix(NA, nrow=n.t, ncol=reps)
pred.lm = matrix(NA, nrow=n.t, ncol=reps)

for(i in 1:reps){
  samp = sample.int(n, size=n, replace=TRUE)
  train = data.frame(x=x[samp], y=y[samp])

  pr.tree1 <- rpart(y~x, method="anova", data=train, cp=0)
  cpt1 = pr.tree1$cptable
  
  # Find location of minimum error
  minrow1 <- which.min(cpt1[,4])
  # Take geometric mean of cp values at min error and one step up 
  cplow.min1 <- cpt1[minrow1,1]
  cpup.min1 <- ifelse(minrow1==1, yes=1, no=cpt1[minrow1-1,1])
  cp.min1 <- sqrt(cplow.min1*cpup.min1)
  
  # Do pruning each way
  pr.prune.min1 <- prune(pr.tree1, cp=cp.min1)
  
  # Plot these points line
  preds1 = predict(pr.prune.min1, newdata=data.frame(x=x.t))
  
  pred.tree[,i] = preds1
  pred.lm[,i] = predict(lm(y ~ x, data=train), newdata=data.frame(x=x.t))
}


x11(h=7, w=12)
par(mfrow = c(1,2))
plot(x=x, y=y, col="gray", main="Reg Tree on Bootstrap Poly-4 Data")
for(i in 1:reps){
  points(x=x.t, y=pred.tree[,i], cex=0.2, col="lightblue")
}
points(x=x, y=y, col="gray")
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, 
      from=-1, to=1, col="red", lwd=2, ylim=c(0,4.2), add=TRUE)

plot(x=x, y=y, col="gray", main="Lin Reg on Bootstrap Poly-4 Data")
for(i in 1:reps){
  points(x=x.t, y=pred.lm[,i], cex=0.2, col="lightblue")
}
points(x=x, y=y, col="gray")
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, 
      from=-1, to=1, col="red", lwd=2, ylim=c(0,4.2), add=TRUE)

x11(h=7, w=12)
par(mfrow = c(1,2))
plot(x=x, y=y, col="gray", main="Bootstrap Aggregated Reg Tree")
for(i in 1:reps){
  points(x=x.t, y=pred.tree[,i], cex=0.2, col="lightblue")
}
points(x=x, y=y, col="gray")
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, 
      from=-1, to=1, col="red", lwd=2, ylim=c(0,4.2), add=TRUE)

mean.tree = apply(pred.tree, 1, mean)
points(x=x.t, y=mean.tree, col="blue", cex=0.5)

plot(x=x, y=y, col="gray", main="Bootstrap Aggregated Lin reg")
for(i in 1:reps){
  points(x=x.t, y=pred.lm[,i], cex=0.2, col="lightblue")
}
points(x=x, y=y, col="gray")
curve(expr=beta0 + beta1*x + beta2*x^2 + beta3*x^3 + beta4*x^4, 
      from=-1, to=1, col="red", lwd=2, ylim=c(0,4.2), add=TRUE)

mean.lm = apply(pred.lm, 1, mean)
points(x=x.t, y=mean.lm, col="blue", cex=0.5)


#Compute MSPEs on test set for true polynimial
#  Lin Model, Tree, Boot-LM, and Boot-tree
(rMSPE.tree = sqrt(mean((preds-y.t)^2)))
(rMSPE.boot.tree = sqrt(mean((mean.tree-y.t)^2)))
(rMSPE.lm = sqrt(mean((predict(lm(y~x), data.frame(x=x.t))-y.t)^2)))
(rMSPE.boot.lm = sqrt(mean((mean.lm-y.t)^2)))
(rMSPE.poly = sqrt(mean((predict(lm(y~poly(x, 4)), data.frame(x=x.t))-y.t)^2)))


