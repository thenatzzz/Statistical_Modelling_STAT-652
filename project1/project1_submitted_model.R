# clear var
rm(list=ls(all=TRUE))
# Clear plots
dev.off()  # But only if there IS a plot
# set.seed(1234,kind="Mersenne-Twister")

################## read data ###################
########### Train model #######################
source("Helper Functions.R") # TA code
library(gbm)          # basic implementation

########### train GBM ##################
# read data for training
data <-  read.table("Data2020.csv", 
                    header=TRUE, sep=",", na.strings=" ")
head(data)

# shuffle data
rows <- sample(nrow(data))
data <- data[rows, ]
head(data)

# build model
gbm.fit.final <- gbm(
  formula = Y ~ .,
  distribution = "gaussian",
  data = data,
  n.trees = 213,
  interaction.depth = 3,
  shrinkage = 0.1,
  n.minobsinnode = 15,
  bag.fraction = 0.8,
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)

########## CREATE new prediction (result) ################
# read data for prediction
data_test <-  read.table("Data2020testX.csv", 
                         header=TRUE, sep=",", na.strings=" ")
predictions=predict(gbm.fit.final, data_test, n.trees = gbm.fit.final$n.trees)
predictions

write.table(predictions, 'prediction_.csv', sep = ",", row.names = F, col.names =F)


