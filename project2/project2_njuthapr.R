## Nattapat Juthaprachakul, 301350117, njuthapr@sfu.ca
# STAT-652 Project 2

# clear var
rm(list=ls(all=TRUE))

set.seed(46685326,kind="Mersenne-Twister")

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
# split 90/10 :train/test
# perm <- sample(x=nrow(df))
# set1 <- df[which(perm <= 9*nrow(df)/10),]
# set2 <- df[which(perm >  9*nrow(df)/10),]
# set1 <- df[which(perm <= 3*nrow(df)/4),]
# set2 <- df[which(perm >  3*nrow(df)/4),]

############# build model##########################
# set.seed(46685326,kind="Mersenne-Twister")
# rf.tun <- randomForest(data=set1, Y~., mtry=2, nodesize=5,ntree=1000,
                          # importance=TRUE, keep.forest=TRUE)
rf.tun <- randomForest(data=df, Y~., mtry=2, nodesize=5,ntree=1000,
                          importance=TRUE, keep.forest=TRUE)

# # Predict results of classification. 
# pred.rf.train.tun <- predict(rf.tun, newdata=set1, type="response")
# pred.rf.test.tun <- predict(rf.tun, newdata=set2, type="response")
#"vote" gives proportions of trees voting for each class
# pred.rf.vtrain.tun <- predict(rf.tun, newdata=set1, type="vote")
# pred.rf.vtest.tun <- predict(rf.tun, newdata=set2, type="vote")
# head(cbind(pred.rf.test.tun,pred.rf.vtest.tun))
# 
# (misclass.train.rf.tun <- mean(ifelse(pred.rf.train.tun == set1$Y, yes=0, no=1)))
# (misclass.test.rf.tun <- mean(ifelse(pred.rf.test.tun == set2$Y, yes=0, no=1)))
# 
# table(set2[,'Y'],pred.rf.test.tun, dnn=c("Observed","Predicted"))


########## CREATE new prediction (result) ################
# read data for prediction
data_test <-  read.table("P2Data2020testX.csv", 
                         header=TRUE, sep=",", na.strings=" ")
# remove some features
data_test[,c("X3","X5","X13",'X15')] <- list(NULL)

predictions=predict(rf.tun, data_test)
predictions

# write.table(predictions, 'prediction_rf2.csv', sep = ",", row.names = F, col.names =F)



