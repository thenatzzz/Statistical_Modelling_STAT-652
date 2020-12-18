# Generalized Additive Model on Wheat Data
rm(list=ls(all=TRUE))

##########
# Enter data and do some processing
wheat <-  read.csv("wheat.csv")
head(wheat)
#summary(wheat)

# Variable "type" is the response variable.  "class" is another explanatory.
class(wheat$type)
wheat$type = as.factor(wheat$type)
wheat$class = as.factor(wheat$class)
#summary(wheat)

# Create a numerical version of "class" for methods that need numbers
wheat$classnum <- as.numeric((wheat$class))
# Remove "id"
wheat = wheat[,-1]
#summary(wheat)

############

# multinom() requires numerical explanatories, so remove factor class 
# Creating TWO sets: 200 train, 75 test
set.seed(67982193)
perm <- sample(x=nrow(wheat))
set1 <- wheat[which(perm <= 200),-1]
set2 <- wheat[which(perm>200),-1]


###############################################################
# gam() can fit binomial model to 2-class problem easily
#  Just change to "family=binomial(link=logit)".  
#  Everything else acts the same.
#  Have so specify variables in s() for splines
#  Can add linear terms without s(), especially if indicators
#    or discrete with (I think) < 8 levels
###############################################################
#
# Will demonstrate on special binary variable where Healthy = 1
#
Hbin1 = as.numeric(set1$type=="Healthy")
Hbin2 = as.numeric(set2$type=="Healthy")

library(mgcv)

#  Generalized additive model as alternative to multivariate splines
# One variable for demonstration
gam1 <- gam(data=set1, Hbin1~s(density), family=binomial(link=logit)) 
summary(gam1)

# Plots of results: Spline in logit scale
x11(h=8,w=7,pointsize=12)
plot(gam1, main="GAM Spline in logit scale")

# All variables
gam2 <- gam(data=set1, Hbin1~s(density) + s(hardness) + 
            s(size) + s(weight) + s(moisture) + classnum, 
            family=binomial(link=logit)) 
summary(gam2)

# Plot marginals
x11(h=12,w=18,pointsize=12)
par(mfrow=c(2,3))
plot(gam2, main="GAM Spline in logit scale")

x11(h=12,w=18,pointsize=12)
par(mfrow=c(2,3))
plot(gam2, main="GAM Spline in logit scale", se=FALSE)

x11(h=12,w=18,pointsize=12)
par(mfrow=c(2,3))
plot(gam2, main="GAM Spline in logit scale", se=FALSE, 
     scale=0)

pred.prob2.1 <- predict(gam2, newdata=set1, type="response")
pred.class2.1 <- as.numeric(predict(gam2, newdata=set1, type="link") > 0)
head(cbind(round(pred.prob2.1, digits=3), pred.class2.1))

pred.prob2.2 <- predict(gam2, newdata=set2, type="response")
pred.class2.2 <- as.numeric(predict(gam2, newdata=set2, type="link") > 0)

# Error rates not comparable to full multinomial problem
(misclass2.train <- mean(ifelse(pred.class2.1 == Hbin1, yes=0, no=1)))
(misclass2.test <- mean(ifelse(pred.class2.2 == Hbin2, yes=0, no=1)))


##########################
# gam() can fit multinomial models, but response must be written
#  as numerical 0,1,..,K-1.  The syntax requires formula as 
#  "list(y~ MODEL1, ~MODEL2, ~MODEL3)" where there is a separate 
#  baseline logit model using category 0 as baseline.  They have 
#  to be given for each logit, even if they are the same model.  
#  the family is "multinom(K=...)" where "K" is the number of *logits* 

levels(set1$type)
set1$type0 <- as.numeric(set1$type) - 1
# Healthy will be our baseline class

# Fit full model, all variables in each logit
gam.m <- gam(data=set1, list(type0
  ~s(density) + s(hardness) + s(size) 
  + s(weight) + s(moisture) + classnum,
  ~s(density) + s(hardness) + s(size) 
  + s(weight) + s(moisture) + classnum),
            family=multinom(K=2)) 
summary(gam.m)

pred.prob.m <- predict(gam.m, newdata=set1, type="response")
pred.class.m <- apply(pred.prob.m,1,function(x) which(max(x)==x)[1])-1

head(cbind(round(pred.prob.m, digits=3), pred.class.m))

pred.prob.2m <- predict(gam.m, newdata=set2, type="response")
pred.class.2m <- apply(pred.prob.2m,1,function(x) which(max(x)==x)[1])-1

(misclassm.train <- mean(pred.class.m != as.numeric(set1$type)-1))
(misclassm.test <- mean(pred.class.2m != as.numeric(set2$type)-1))

# Confusion Matrix
table(set2$type, pred.class.2m,  dnn=c("Observed","Predicted"))
