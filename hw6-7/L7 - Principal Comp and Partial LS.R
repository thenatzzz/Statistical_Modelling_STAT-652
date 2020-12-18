#########################################################################
# Principal Components on Prostate Data 
# Partial Least Squares 

prostate <-  read.table("Prostate.csv", 
                        header=TRUE, sep=",", na.strings=" ")
head(prostate)


#set.seed(120401002) 
#set <- ifelse(runif(n=nrow(prostate))>0.5, yes=2, no=1)
#vehdata <-  read.table("C:\\Users\\tom loughin\\sfuvault\\852 Modern Applied Methods\\R\\vehicle3.txt",header=TRUE,sep=" ")

#head(vehdata)
#dim(vehdata)

# Scatterplot
# win.graph(h=15, w=15)
# pairs(x=vehdata)


# Create 2 sets again: 
# Permuting the numbers 1:n and using these to partition sets

#set.seed(46685326)
#perm <- sample(x=nrow(vehdata))
#set1 <- vehdata[which(perm <= 3*nrow(vehdata)/4), -20]
#set2 <- vehdata[which(perm > 3*nrow(vehdata)/4), -20]

###############################################################
## Principal Component Analysis analysis via stats::prcomp()
## Uses singular value decomposition on X instead of eigen-decomposition
##   on X'X for efficiency.  [princomp() uses eigen on X'X]
##  Default is to work with un-standardized data; scale.=TRUE fixes this
###############################################################

pc <-  prcomp(x=prostate[,2:9], scale.=TRUE)
summary(pc)

# Default plot is a scree plot as a histogram (why???)
# I'll make my own using points
plot(pc)

evals <- pc$sdev^2
csum = cumsum(evals)
x11(h=7,w=12)
par(mfrow=c(1,2))
plot(y=evals, x=c(1:(ncol(prostate)-1)), xlab="PC#", 
     main="Variance explained by PCA", ylab="Variance Explained")
abline(a=0,b=0)
abline(a=1,b=0)

plot(y=c(0,csum/max(csum)), x=c(0:(ncol(prostate)-1)), xlab="PC#", ylim=c(0,1),
     main="Cumulative Variance explained by PCA", ylab="Pct Explained")

# Look at eigenvectors to see how variables contribute to PC's
pc$rotation

############################################################
# Partial Least Squares Regression using plsr() from the pls package.
#   The code is pretty similar to lm() except that I can tell it 
#   (a) Max number of components in model
#   (b) The option to use validation to choose number of components
#       "validation=" can use "none", "CV", or "LOO" (leave one out CV)
#        If "CV" is used, V=10 unless changed with "segments="

library(pls)

mod.pls = plsr(lpsa~., data=prostate, ncomp=8, validation="CV")
summary(mod.pls)

validationplot(mod.pls)

