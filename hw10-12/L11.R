#############################################################################
### In this tutorial, we will use GAM to fit the wine quality data. To    ###
### make the presentation easier, we will just use density, pH, sulphates ###
### and type as predictors.                                               ###
#############################################################################

### This script contains randomness, so we need to set the seed
set.seed(60597418)

### Read-in full wine quality dataset
source("Read Wine Data - All Vars.R")

### It will be convenient to have access to our helper functions from
### previous lectures
source("Helper Functions.R")

### We will use the gam() function from the mgcv package. Load this package
library(mgcv)

### Let's do a single 75/25 split on the data and get the validation set MSPE
n = nrow(data)
p.train = 0.75
n.train = round(n * p.train)
n.valid = n - n.train
sets = c(rep(1, times = n.train), rep(2, times = n.valid))
sets.rand = shuffle(sets) # Our helper function

data.train = data[sets.rand == 1,]
data.valid = data[sets.rand == 2,]
Y.valid = data.valid$alcohol

### The gam() function has similar syntax to lm(). Specify a model formula
### and a data frame, but for each predictor, you can optionally put it
### inside a function called s(). See below for a demonstration.
fit.gam = gam(alcohol ~ s(density) + s(pH) + s(sulphates) + type,
  data = data.train)

### Get information about the GAM fit using the summary() function.
summary(fit.gam)

### We can get plots of the smoothers fit to each predictor using the plot()
### function. 
### It would be nice to see all three smoothers simultaneously. You can get
### multiple plots in a grid using the par() function. Inside par(), set
### mfrow equal to a vector of two numbers: the number of rows in your grid,
### then the number of columns in your grid.
par(mfrow = c(2,2))
plot(fit.gam)

### Let's also plot alcohol vs type for reference
with(data, boxplot(alcohol ~ type))

### Make sure to reset your plot grid to 1x1 when you're done
par(mfrow = c(1,1))


### Now, let's compute the validation set MSPE
pred.gam = predict(fit.gam, data.valid)
MSPE.valid = get.MSPE(Y.valid, pred.gam) # Our helper function
print(MSPE.valid)
