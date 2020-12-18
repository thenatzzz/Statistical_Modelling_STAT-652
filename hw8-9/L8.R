#######################################################################
### In this lecture, we will use step functions to predict alcohol  ###
### content using density and sulphates.                            ###
#######################################################################

### First, load the packages we will need
library(dplyr)
library(rgl)

### Run a new script for reading the data
source("Read Wine Data.R")

#######################################################################
###                 Single Variable Step Function                   ###
### For now, let's focus on just using density to predict alcohol   ###
### We will divide density into 5 groups, and fit a constant value  ###
### within each group.                                              ###
#######################################################################

### Choose breakpoints and construct regions. We use the cut() function
### to construct a categorical variable based on regions for a numeric
### variable.
breaks = c(0, 0.99, 0.993, 0.996, 0.999, 1.1)
regions = cut(data$density, breaks)
data.regions = data.frame(density = data$density, region = regions)
head(data.regions)

### Plot relationship between density and alcohol
with(data, plot(density, alcohol))

### Add breakpoints
abline(v = breaks[2:5], lty = 2)


### Fit a piecewise constant model with our breakpoints
data.breaks = data.frame(alcohol = data$alcohol, Region = regions)
fit.piece = lm(alcohol ~ Region, data = data.breaks)

### Get the predicted value for each region
region.levels = unique(regions)
regions.sorted = sort(region.levels)
data.pred = data.frame(Region = regions.sorted)
fitted.vals = predict(fit.piece, data.pred)

### Add predicted values to the plot
for(i in 1:length(fitted.vals)){
  a = breaks[i]
  b = breaks[i+1]
  preds = rep(fitted.vals[i], times = 2)
  
  lines(c(a,b), preds, col = "red")
}


###################################################################
###                 2 Predictor Step Functions                  ###
### We will now construct a step function using two predictors: ###
### density and sulphates.                                      ###
###################################################################

### Let's just use two levels for each predictor. This will give us
### four separate groups. It probably makes sense to split at the median.
### First, we'll need to get the medians for these two predictors
density.med = median(data$density)
sulphates.med = median(data$sulphates)

### Construct indicators for each variable, checking whether a point is
### above or below the median
density.split.med = with(data, density >= density.med)
sulphates.split.med = with(data, sulphates >= sulphates.med)

### Put the response and the indicators in a data frame for our analysis.
### Let's also include the original variable values for reference
data.split.med = data.frame(alcohol = data$alcohol, density = data$density,
  sulphates = data$sulphates, dens.split = density.split.med, 
  sulp.split = sulphates.split.med)

### Fit a regression model to predict alcohol using step
### functions based on density and sulphates
fit.step.2 = lm(alcohol ~ dens.split + sulp.split, data = data.split.med)

### Plot both predictors, along with alcohol
with(data, plot3d(alcohol ~ density + sulphates))

### Construct a grid of values for the two explanatories
vals.density = seq(from = 0.9870, to = 1.002, by = 0.0002)
vals.sulphates = seq(from = 0.26, to = 1.14, by = 0.01)
vals.grid = expand.grid(density = vals.density, sulphates = vals.sulphates)

### Convert this grid of values to a grid of indicators
### The transmute function lets us define new variables, and drops all
### the variables we started with.
split.grid = transmute(vals.grid, dens.split = density >= density.med,
  sulp.split = sulphates >= sulphates.med)

### Get predicted values on the grid of indicators
pred.step.2 = predict(fit.step.2, split.grid)

### Plot predicted values and points
persp3d(x = vals.density, y = vals.sulphates, z = pred.step.2,
  xlab = "density", ylab = "sulphates", zlab = "Body Fat %", col = "orange")
with(data, points3d(alcohol ~ density + sulphates, col = "blue"))
