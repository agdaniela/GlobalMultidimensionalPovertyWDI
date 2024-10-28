
#### EXPERIMENT 2 SCRIPT FILE ####

# The experiment depends mainly on the data set used for the
# experiment (see Table in Appendix A) and the target variable (MPI,H,A).
# In this script we have the example of data set 1 and MPI as the
# target. To reproduce the code just change the desired data frame
# and target variable.

source("./main_function/main_function_exp2.R")

# Run if want to create all datasets ------------------------------------------------

#all_datas = list()
#for (i in seq(1,29)){
#  nombre <- paste("dataset", i, sep="_")
#  all_datas[[nombre]] = selectdfs(data,i)
#}

# Load an specific dataset  ------------------------------------------------------------

all_datas = readRDS("Experiment_2/all_datasets.Rdata") 

# Load dataset ----------------------------------------------------------

df_fold = all_datas[[2]]
 
# Code for experiment 2 - Dataset 2 ----------------------------------------------------------

# Creating folds
df_fold$h_Other = df_fold$h_Other /100 #express in decimals
df_fold$a_Other = df_fold$a_Other /100 #express in decimals
df_fold$mpi_Other = df_fold$mpi_Other + 0.0000000001 #avoid zero value
df_fold$h_Other = df_fold$h_Other + 0.0000000001 #avoid zero value
df_fold$a_Other = df_fold$a_Other + 0.0000000001 #avoid zero value

df_fold  <- df_fold[sample(nrow(df_fold)),]

folds_index <- cut(seq(1,nrow(df_fold)),breaks=10,labels=FALSE) 

# Predicted values for MPI

predicted_mpi = list()
for(i in 1:10){
  testIndexes <- which(folds_index == i,arr.ind=TRUE)
  testData <- df_fold[testIndexes, ]
  trainData <- df_fold[-testIndexes, ]
  name <- paste("yhats", i, sep="_")
  predicted_mpi[[name]] = main_function_exp2(trainData,testData,"mpi_Other", corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}
 
# Predicted values for A

predicted_a = list()
for(i in 1:10){
  testIndexes <- which(folds_index == i,arr.ind=TRUE)
  testData <- df_fold[testIndexes, ]
  trainData <- df_fold[-testIndexes, ]
  name <- paste("ahats", i, sep="_")
  predicted_a[[name]] = main_function_exp2(trainData,testData,"a_Other", corte=0.5, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}

# Predicted values for H

predicted_h = list()
for(i in 1:10){
  testIndexes <- which(folds_index == i,arr.ind=TRUE)
  testData <- df_fold[testIndexes, ]
  trainData <- df_fold[-testIndexes, ]
  name <- paste("hhats", i, sep="_")
  predicted_h[[name]] = main_function_exp2(trainData,testData,"h_Other", corte=0.2, link_phi = "log", link_mu = "logit", distancia = "hellinger")
}

 
 




