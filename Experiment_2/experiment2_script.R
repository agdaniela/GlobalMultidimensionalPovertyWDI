
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

# Load datasets ----------------------------------------------------------

df_fold_1 = all_datas[[1]]
df_fold_2 = all_datas[[2]]
df_fold_13 = all_datas[[13]]


# Function for predicting in each fold ------------------------------------------------------

#' @param df Dataset for spliting into folds
#' @param nfolds Number of folds
#' @param indexes Index for spliting into folds
#' @param param Target variable ("mpi_Other","h_Other","a_Other")
#' @param cut Parameter for beta-tree models
#' 
#' @return a list with predicted values and evaluation metrics for each fold  

predicted_exp2 = function(df, nfolds, indexes, param, cut){
  predicted_list = list()
  if (param == "mpi_Other") {
    prefix <- "yhats"
  } else if (param == "h_Other") {
    prefix <- "hhats"
  } else {
    prefix <- "ahats"
  }
  
  for(i in 1:nfolds){
    testIndexes <- which(indexes == i,arr.ind=TRUE)
    testData <- df[testIndexes, ]
    trainData <- df[-testIndexes, ]
    
    name <- paste(prefix, i, sep="_")
    predicted_list[[name]] = main_function_exp2(trainData,testData,param, corte=cut, link_phi = "log", link_mu = "logit", distancia = "hellinger")
  }
  return(predicted_list) 
}


# Code for Dataset 1 ------------------------------------------------------

# Creating folds
df_fold_1$h_Other = df_fold_1$h_Other /100 #express in decimals
df_fold_1$a_Other = df_fold_1$a_Other /100 #express in decimals
df_fold_1$mpi_Other = df_fold_1$mpi_Other + 0.0000000001 #avoid zero value
df_fold_1$h_Other = df_fold_1$h_Other + 0.0000000001 #avoid zero value
df_fold_1$a_Other = df_fold_1$a_Other + 0.0000000001 #avoid zero value

df_fold_1  <- df_fold_1[sample(nrow(df_fold_1)),]

folds_index_1 <- cut(seq(1,nrow(df_fold_1)),breaks=10,labels=FALSE) 

# Predicted values for MPI

predicted_mpi_df1 = predicted_exp2(df_fold_1, 10, folds_index_1, "mpi_Other", cut =0.2)
 
# Predicted values for A

predicted_a_df1 = predicted_exp2(df_fold_1, 10, folds_index_1, "a_Other", cut =0.5)

# Predicted values for H

predicted_h_df1 = predicted_exp2(df_fold_1, 10, folds_index_1, "h_Other", cut =0.2)


# Code for Dataset 2 ----------------------------------------------------------

# Creating folds
df_fold_2$h_Other = df_fold_2$h_Other /100 #express in decimals
df_fold_2$a_Other = df_fold_2$a_Other /100 #express in decimals
df_fold_2$mpi_Other = df_fold_2$mpi_Other + 0.0000000001 #avoid zero value
df_fold_2$h_Other = df_fold_2$h_Other + 0.0000000001 #avoid zero value
df_fold_2$a_Other = df_fold_2$a_Other + 0.0000000001 #avoid zero value

df_fold_2  <- df_fold_2[sample(nrow(df_fold_2)),]

folds_index_2 <- cut(seq(1,nrow(df_fold_2)),breaks=10,labels=FALSE) 

# Predicted values for MPI

predicted_mpi_df2 = predicted_exp2(df_fold_2, 10, folds_index_2, "mpi_Other", cut =0.2)

# Predicted values for A

predicted_a_df2 = predicted_exp2(df_fold_2, 10, folds_index_2, "a_Other", cut =0.5)

# Predicted values for H

predicted_h_df2 = predicted_exp2(df_fold_2, 10, folds_index_2, "h_Other", cut =0.2)
 

# Code for Dataset 13 -----------------------------------------------------

# Creating folds
df_fold_13$h_Other = df_fold_13$h_Other /100 #express in decimals
df_fold_13$a_Other = df_fold_13$a_Other /100 #express in decimals
df_fold_13$mpi_Other = df_fold_13$mpi_Other + 0.0000000001 #avoid zero value
df_fold_13$h_Other = df_fold_13$h_Other + 0.0000000001 #avoid zero value
df_fold_13$a_Other = df_fold_13$a_Other + 0.0000000001 #avoid zero value

df_fold_13  <- df_fold_13[sample(nrow(df_fold_13)),]

folds_index_13 <- cut(seq(1,nrow(df_fold_13)),breaks=10,labels=FALSE) 

# Predicted values for MPI

predicted_mpi_df13 =  predicted_exp2(df_fold_13, 10, folds_index_13, "mpi_Other", cut =0.2)

# Predicted values for A

predicted_a_df13 = predicted_exp2(df_fold_13, 10, folds_index_13, "a_Other", cut =0.5)
 
# Predicted values for H

predicted_h_df13 = predicted_exp2(df_fold_13, 10, folds_index_13, "h_Other", cut =0.2)


 


 