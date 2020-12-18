###############################################################
# Interactive demo on 1-dimensional regression trees
##############################################################
# Function tree.demo{TeachingDemos} has just two parameters, x and y.

library(TeachingDemos)

carord <- mtcars[order(mtcars$wt),]
x11()
tree.demo(x=carord$wt,y=carord$mpg)
