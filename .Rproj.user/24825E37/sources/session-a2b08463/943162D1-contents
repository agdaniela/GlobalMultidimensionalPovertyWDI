
# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)


# Load Results ------------------------------------------------------------------

# df1
rep_mpi_df1 = readRDS("Experiment_1/rep_mpi_df1.Rdata")
rep_a_df1 = readRDS("Experiment_1/rep_a_df1.Rdata")
rep_h_df1 = readRDS("Experiment_1/rep_h_df1.Rdata")
# df2
rep_mpi_df2 = readRDS("Experiment_1/rep_mpi_df2.Rdata")
rep_a_df2 = readRDS("Experiment_1/rep_a_df2.Rdata")
rep_h_df2 = readRDS("Experiment_1/rep_h_df2.Rdata")
# df13
rep_mpi_df13 = readRDS("Experiment_1/rep_mpi_df13.Rdata")
rep_a_df13 = readRDS("Experiment_1/rep_a_df13.Rdata")
rep_h_df13 = readRDS("Experiment_1/rep_h_df13.Rdata")


# Tables ------------------------------------------------------------------


tabla_exp1 = function(lista, df){
  
  average = lapply(lista, function(x) round(mean(na.omit(x)),4))
  deviation = lapply(lista, function(x) round(sd(na.omit(x)),4))
  #swich the return between average and deviation
  return(average)
}

 

reps_mpi = data.frame(cbind(tabla_exp1(rep_mpi_df1),tabla_exp1(rep_mpi_df2),tabla_exp1(rep_mpi_df13)))
colnames(reps_mpi) = c("df1","df2","df13")

reps_a = data.frame(cbind(tabla_exp1(rep_a_df1),tabla_exp1(rep_a_df2),tabla_exp1(rep_a_df13)))
colnames(reps_a) = c("df1","df2","df13")

reps_h = data.frame(cbind(tabla_exp1(rep_h_df1),tabla_exp1(rep_h_df2),tabla_exp1(rep_h_df13)))
colnames(reps_h) = c("df1","df2","df13")

# Swich to deviation
reps_mpi_sd = data.frame(cbind(tabla_exp1(rep_mpi_df1),tabla_exp1(rep_mpi_df2),tabla_exp1(rep_mpi_df13)))
colnames(reps_mpi) = c("df1","df2","df13")

reps_a_sd = data.frame(cbind(tabla_exp1(rep_a_df1),tabla_exp1(rep_a_df2),tabla_exp1(rep_a_df13)))
colnames(reps_a) = c("df1","df2","df13")

reps_h_sd = data.frame(cbind(tabla_exp1(rep_h_df1),tabla_exp1(rep_h_df2),tabla_exp1(rep_h_df13)))
colnames(reps_h) = c("df1","df2","df13")

 

# Graphs ------------------------------------------------------------------

graph_data_errors = function(lista){
  errors_df = data.frame(matrix(unlist(lista), nrow=50, byrow=F))
  colnames(errors_df) = names(lista)
  return(errors_df)
}

 
theme_set(
  theme_light(base_size = 18) +
    theme(
      axis.line.x = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      axis.line.y = element_line(
        colour = "black",
        size = 0.5,
        linetype = "solid"
      ),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(colour = "gray", linetype = "dotted", size = 0.5),
      panel.grid.major.x = element_line(colour = "gray", linetype = "dotted", size = 0.5) ,
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_text(size = 18), #
      
    )
)


# Dataset 1 ---------------------------------------------------------------


#MSE
#mpi -df1
errors = graph_data_errors(rep_mpi_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

# Plot
ggplot(data = errors_graph, aes(x = Methods, y = MSE)) +
  geom_boxplot(aes(fill = Methods)) + 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels = c("Linear-PLS", "Beta-PLS", "Beta-Tree-PLS", "Elastic Net", "Beta (elastic)", "Beta-Tree (elastic)", "XGBoost", "Betaboost")) +
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")

#a - df1
errors = graph_data_errors(rep_a_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  #ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")

#h - df1
errors = graph_data_errors(rep_h_df1)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")


# Dataset 2 ---------------------------------------------------------------


#MSE
#mpi -df2
errors = graph_data_errors(rep_mpi_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")   


#a - df2
errors = graph_data_errors(rep_a_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")

#h - df2
errors = graph_data_errors(rep_h_df2)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")

# Dataset 13 --------------------------------------------------------------


#MSE
#mpi -df13
errors = graph_data_errors(rep_mpi_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.03) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")


#a - df13
errors = graph_data_errors(rep_a_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.01) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")

#h - df13
errors = graph_data_errors(rep_h_df13)
errors_graph = reshape2::melt(errors[,c(1:8)])
names(errors_graph) = c("Methods","MSE")

ggplot(data = errors_graph, aes(x=Methods, y=MSE)) +
  geom_boxplot(aes(fill=Methods))+ 
  ylim(-0.0001, 0.08) +
  scale_x_discrete(labels= c("Linear-PLS","Beta-PLS","Beta-Tree-PLS","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","XGBoost","Betaboost") )+
  scale_fill_manual(
    values = c("#B2DCEB", "#68B5D2", "#4196B6", "#545387", "#8470A1", "#A68FC0", "#D68EB0", "#EFB6C6")
  ) +
  guides(linetype = "none")


 