###############################################################################
### In this tutorial, we will use regression trees to predict alcohol using ###
### some of the other variables in the wine quality dataset.                ###
###############################################################################

library(rpart)
library(rpart.plot)

set.seed(8598176)

source("Read Wine Data.R")


### Let's use a regression tree to predict alcohol using density and pH. We
### fit regression tree models using the rpart() function from the rpart
### package. This function uses model formula + data frame syntax. The 
### default complexity parameter value tends to underfit. We can set the CP
### value using the cp input.
fit.tree = rpart(alcohol ~ density + pH, data = data, cp = 0)

### Get the CP table
info.tree = fit.tree$cptable

### We have to prune the tree manually. First, get the CP value with minimum
### CV error
ind.min = which.min(info.tree[,"xerror"])
CP.min.raw = info.tree[ind.min, "CP"]


### It's best to average the minimum CP value with the one from the row above 
### using the geometric mean (i.e. multiply them together, then square root). 
### If we implement this procedure directly, and the minimum CP value is in   
### the first row, our code will probably give an error. We should write our  
### code so that it can cope with this weird situation. This attitude is      
### called defensive programming, and it is a very good habit to practice.    

### Check if minimum CP value is in row 1. We can do this using an if-else
### statement. See this tutorial's video for details.
if(ind.min == 1){
  ### If minimum CP is in row 1, store this value
  CP.min = CP.min.raw
} else{
  ### If minimum CP is not in row 1, average this with the value from the
  ### row above it.
  
  ### Value from row above
  CP.above = info.tree[ind.min-1, "CP"]
  
  ### (Geometric) average
  CP.min = sqrt(CP.min.raw * CP.above)
}

### We now have our chosen CP value. We can prune our tree using the prune()
### function. The first input to prune() is the tree object, and we set "cp"
### equal to the CP value where we want to prune
fit.tree.min = prune(fit.tree, cp = CP.min)


### Next, we want to prune using the 1SE rule. Fortunately, the CP table
### gives us the CV standard error. First, find the minimum CV error plus 1 
### standard error
err.min = info.tree[ind.min, "xerror"]
se.min = info.tree[ind.min, "xstd"]
threshold = err.min + se.min

### Next, get the smallest tree with CV error below our threshold. 
### Note: We limit our search to only trees which are no larger than our min CV
###       error tree.
ind.1se = min(which(info.tree[1:ind.min,"xerror"] < threshold))

### Get the corresponding CP value, averaging if necessary
CP.1se.raw = info.tree[ind.1se, "CP"]
if(ind.1se == 1){
  ### If best CP is in row 1, store this value
  CP.1se = CP.1se.raw
} else{
  ### If best CP is not in row 1, average this with the value from the
  ### row above it.
  
  ### Value from row above
  CP.above = info.tree[ind.1se-1, "CP"]
  
  ### (Geometric) average
  CP.1se = sqrt(CP.1se.raw * CP.above)
}

### Prune the tree
fit.tree.1se = prune(fit.tree, cp = CP.1se)


###########################################################################
### A nice feature of tree models is that they make great plots. Let's  ###
### plot the full tree, and both pruned trees. We can plot trees using  ###
### the prp() function from the rpart.plot package.                     ###
###########################################################################

### The prp() function has many inputs We will just use two: type and extra.
### Setting both of these inputs to 1 gives a nice looking plot. Since
### prp() makes a plot, we can set the title using main. We also
### have to provide the fitted tree object which is being plotted.
prp(fit.tree, type = 1, extra = 1, main = "Full Tree")
prp(fit.tree.min, type = 1, extra = 1, main = "Pruned Tree - Min")
prp(fit.tree.1se, type = 1, extra = 1, main = "Pruned Tree - 1SE")
