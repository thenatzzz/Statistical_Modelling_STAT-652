# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
# set.seed(46685326,kind="Mersenne-Twister")
set.seed(5,kind="Mersenne-Twister")

################## read data ###################
# read data for training
df <-  read.csv("P2Data2020.csv")
df$Y = factor(df$Y)

# delete some column
df[,c("X3","X5","X13",'X15')] <- list(NULL)

# shuffle data
rows <- sample(nrow(df))
df <- df[rows, ]
head(df)
######## temporary
perm <- sample(x=nrow(df))
set1 <- df[which(perm <= 3*nrow(df)/4),]
set2 <- df[which(perm >  3*nrow(df)/4),]


######### prepare dataset ##############
# Prepare matrix for XGBoost algorithm
training_matrix <-model.matrix(Y ~.-1, data = set1)
validation_matrix <-model.matrix(Y ~.-1, data = set2)
dtrain <- xgb.DMatrix(data = training_matrix, label = set1$Y)
dvalid <- xgb.DMatrix(data = validation_matrix, label = set2$Y)

# training_matrix <-model.matrix(Y ~.-1, data = df)
# validation_matrix <-model.matrix(Y ~.-1, data = set2)
# dtrain <- xgb.DMatrix(data = training_matrix, label = df$Y) 
# dvalid <- xgb.DMatrix(data = validation_matrix, label = set2$Y)

library(dplyr)
# convert Y categories to numerical
must_convert<-sapply(set2,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
Y<-sapply(set2[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
set2_new<-cbind(set2[,!must_convert],Y)        # complete data.frame with all variables put together
head(set2_new,5)


############# build model##########################
params <- list(booster = "gbtree",
               max_depth = 5,
               eta = 0.1789921,
               subsample = 0.7167523,
               colsample_bytree = 0.8935017,
               min_child_weight = 2)
params
xgb_tuned <- xgb.train(params = params,
                       data = dtrain,
                       nrounds =1000,
                       print_every_n = 10,
                       eval_metric = "merror",
                       num_class = 6,
                       early_stopping_rounds = 30,
                       watchlist = list(train= dtrain, val= dvalid))

y_pred <- predict(xgb_tuned, dvalid)
mean(y_pred == set2_new$Y)

table(set2_new[,'Y'],y_pred, dnn=c("Observed","Predicted"))

########## CREATE new prediction (result) ################
# read data for prediction
data_test <-  read.table("P2Data2020testX.csv", 
                         header=TRUE, sep=",", na.strings=" ")
data_test[,c("X3","X5","X13",'X15')] <- list(NULL)

# convert to matrix format
test_matrix <-model.matrix( ~.-1, data = data_test)
data_test <- xgb.DMatrix(data = test_matrix)

predictions=predict(xgb_tuned, data_test)
predictions
library(car)
result=recode(predictions, "1='A'; 2='B';3='C';4='D';5='E';")
result= as.factor(result)
result
# write.table(predictions, 'prediction.csv', sep = ",", row.names = F, col.names =F)


















##################
# clear var
rm(list=ls(all=TRUE))

# set.seed(46685326,kind="Mersenne-Twister")
set.seed(5,kind="Mersenne-Twister")

################## read data ###################
# read data for training
df <-  read.csv("P2Data2020.csv")
df$Y = factor(df$Y)

# delete some column
df[,c("X3","X5","X13",'X15')] <- list(NULL)

# shuffle data
rows <- sample(nrow(df))
df <- df[rows, ]
head(df)
######## temporary test model ###########
# perm <- sample(x=nrow(df))
# set1 <- df[which(perm <= 3*nrow(df)/4),]
# set2 <- df[which(perm >  3*nrow(df)/4),]


######### prepare dataset ##############
# Prepare matrix for XGBoost algorithm
# training_matrix <-model.matrix(Y ~.-1, data = set1)
# validation_matrix <-model.matrix(Y ~.-1, data = set2)
# dtrain <- xgb.DMatrix(data = training_matrix, label = set1$Y)
# dvalid <- xgb.DMatrix(data = validation_matrix, label = set2$Y)
# library(dplyr)
# # convert Y categories to numerical
# must_convert<-sapply(set2,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
# Y<-sapply(set2[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
# set2_new<-cbind(set2[,!must_convert],Y)        # complete data.frame with all variables put together
# head(set2_new,5

training_matrix <-model.matrix(Y ~.-1, data = df)
dtrain <- xgb.DMatrix(data = training_matrix, label = df$Y) 

############# build model##########################
params <- list(booster = "gbtree",
               max_depth = 5,
               eta = 0.1789921,
               subsample = 0.7167523,
               colsample_bytree = 0.8935017,
               min_child_weight = 2)
params
xgb_tuned <- xgb.train(params = params,
                       data = dtrain,
                       nrounds =1000,
                       print_every_n = 10,
                       eval_metric = "merror",
                       num_class = 6,
                       early_stopping_rounds = 30,
                       
                       watchlist = list(train= dtrain))
# watchlist = list(train= dtrain, val= dvalid))

# y_pred <- predict(xgb_tuned, dvalid)
# mean(y_pred == set2_new$Y)
# table(set2_new[,'Y'],y_pred, dnn=c("Observed","Predicted"))

########## CREATE new prediction (result) ################
# read data for prediction
data_test <-  read.table("P2Data2020testX.csv", 
                         header=TRUE, sep=",", na.strings=" ")
data_test[,c("X3","X5","X13",'X15')] <- list(NULL)

# convert to matrix format
test_matrix <-model.matrix( ~.-1, data = data_test)
data_test <- xgb.DMatrix(data = test_matrix)

predictions=predict(xgb_tuned, data_test)
predictions
library(car) # for recode function
result=recode(predictions, "1='A'; 2='B';3='C';4='D';5='E';")
result= as.factor(result)
result
write.table(result, 'prediction.csv', sep = ",", row.names = F, col.names =F)





