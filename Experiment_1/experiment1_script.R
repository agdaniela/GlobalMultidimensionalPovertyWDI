
#### EXPERIMENT 1 SCRIPT FILE ####

# The experiment depends mainly on the data set used for the
# experiment (see Table in Appendix A) and the target variable (MPI,H,A).
# In this script we have the example of data set 1 and MPI as the
# target. To reproduce the code just change the desired data frame
# and target variable.

source("./main_function/main_function_exp1.R")

# Run if want to create all datasets ------------------------------------------------

#all_datas = list()
#for (i in seq(1,29)){
#  nombre <- paste("dataset", i, sep="_")
#  all_datas[[nombre]] = selectdfs(data,i)
#}

# Load an specific dataset  ------------------------------------------------------------

all_datas = readRDS("Experiment_1/all_datasets.Rdata") 

# Load datasets ----------------------------------------------------------

df_1 = all_datas[[1]]
df_2 = all_datas[[2]]
df_13 = all_datas[[13]]

# Code for Dataset 1 ----------------------------------------------------------

df_1$h_Other = df_1$h_Other /100
df_1$a_Other = df_1$a_Other /100
df_1$mpi_Other = df_1$mpi_Other + 0.0000000001
df_1$h_Other = df_1$h_Other + 0.0000000001
df_1$a_Other = df_1$a_Other + 0.0000000001


# MPI
rep_mpi_df1 = repetitions(df_1,"mpi_Other", corte=0.2,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)

#H
rep_h_df1 = repetitions(df_1,target = "h_Other", corte=0.2,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)
 
#A
rep_a_df1 = repetitions(df_1,target = "a_Other", corte=0.5,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)
 


# Code for Dataset 2 ----------------------------------------------------------

df_2$h_Other = df_1$h_Other /100
df_2$a_Other = df_1$a_Other /100
df_2$mpi_Other = df_1$mpi_Other + 0.0000000001
df_2$h_Other = df_1$h_Other + 0.0000000001
df_2$a_Other = df_1$a_Other + 0.0000000001


# MPI
rep_mpi_df2 = repetitions(df_2,"mpi_Other", corte=0.2,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)

#H
rep_h_df2 = repetitions(df_2,target = "h_Other", corte=0.2,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)

#A
rep_a_df2 = repetitions(df_2,target = "a_Other", corte=0.5,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)



# Code for Dataset 13----------------------------------------------------------


df_13$h_Other = df_1$h_Other /100
df_13$a_Other = df_1$a_Other /100
df_13$mpi_Other = df_1$mpi_Other + 0.0000000001
df_13$h_Other = df_1$h_Other + 0.0000000001
df_13$a_Other = df_1$a_Other + 0.0000000001


# MPI
rep_mpi_df13 = repetitions(df_13,"mpi_Other", corte=0.2,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)

#H
rep_h_df13 = repetitions(df_13,target = "h_Other", corte=0.2,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)

#A
rep_a_df13 = repetitions(df_13,target = "a_Other", corte=0.5,link_phi = "log", link_mu = "logit",distancia = "hellinger",nreps=50)




 
