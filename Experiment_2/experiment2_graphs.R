# Libraries ---------------------------------------------------------------

library(dplyr)
library(ggplot2)


# Load Results ------------------------------------------------------------------


# Dataset 1
df_fold_1 = readRDS("Experiment_2/df_fold_1.Rdata")
predicted_mpi_df1 = readRDS("Experiment_2/predicted_mpi_df1.Rdata")
predicted_a_df1 = readRDS("Experiment_2/predicted_a_df1.Rdata")
predicted_h_df1 = readRDS("Experiment_2/predicted_h_df1.Rdata")

# Dataset 2
df_fold_2 = readRDS("Experiment_2/df_fold.Rdata")
predicted_mpi_df2 = readRDS("Experiment_2/predicted_mpi_df2.Rdata")
predicted_a_df2 = readRDS("Experiment_2/predicted_a_df2.Rdata")
predicted_h_df2 = readRDS("Experiment_2/predicted_h_df2.Rdata")

# Dataset 13
df_fold_13 = readRDS("Experiment_2/df_fold_13.Rdata")
predicted_mpi_df13 = readRDS("Experiment_2/predicted_mpi_df13.Rdata")
predicted_a_df13 = readRDS("Experiment_2/predicted_a_df13.Rdata")
predicted_h_df13 = readRDS("Experiment_2/predicted_h_df13.Rdata")


#  Helping function for Tables and Plots ------------------------------------------------------


graph_data = function(df, lista){
  predicted = data.frame()
  for (i in 1:10) {
    preds = lista[[i]]$predicted
    predicted = rbind(predicted, preds)
  }
  index = rownames(predicted)
  df$year_trend <- as.numeric(as.character(df$year_Other))
  df$year_trend <- df$year_trend - min(df$year_trend)
  data = df[,c(1:33,ncol(df))] #choose columns
  data = data[index,] #filter by rows
  data_graph = cbind(data,predicted) 
  names(data_graph)[names(data_graph) == 's0'] <- 'yhat.elastic_tc'
  #names(data_graph)[names(data_graph) == 's0.1'] <- 'yhat.elastic_tc'
  
  return(data_graph)
}


# Tables ------------------------------------------------------------------

# Distances for MPI

# Dataset 1 
folds_df1 = graph_data(df_fold_1, predicted_mpi_df1)
dist_mpi_df1 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_df1$y_test, from = 0,to = 1,)$y,density(na.omit(folds_df1[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# Dataset 2   
folds_df2 = graph_data(df_fold_2, predicted_mpi_df2)
dist_mpi_df2 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_df2$y_test, from = 0,to = 1,)$y,density(folds_df2[,x],from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# Dataset 13
folds_df13 = graph_data(df_fold_13, predicted_mpi_df13)
dist_mpi_df13 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_df13$y_test, from = 0,to = 1,)$y,density(na.omit(folds_df13[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# Distances for A

# Dataset 1
folds_a_df1 = graph_data(df_fold_1, predicted_a_df1)
dist_a_df1 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_a_df1$y_test, from = 0.3,to = 1,)$y,density(na.omit(folds_a_df1[,x]),from=0.3,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# Dataset 2
folds_a_df2 = graph_data(df_fold_2, predicted_a_df2)
dist_a_df2 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_a_df2$y_test, from = 0.3,to = 1,)$y,density(folds_a_df2[,x],from=0.3,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# Dataset 13 
folds_a_df13 = graph_data(df_fold_13, predicted_a_df13)
dist_a_df13 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_a_df13$y_test, from = 0.3,to = 1,)$y,density(na.omit(folds_a_df13[,x]),from=0.3,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# Distances for H

# Dataset 1
folds_h_df1 = graph_data(df_fold_1, predicted_h_df1)
dist_h_df1 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_h_df1$y_test, from = 0,to = 1,)$y,density(na.omit(folds_h_df1[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# Dataset 2
folds_h_df2 = graph_data(df_fold_2, predicted_h_df2)
dist_h_df2 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_h_df2$y_test, from = 0,to = 1,)$y,density(na.omit(folds_h_df2[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))

# Dataset 13
folds_h_df13 = graph_data(df_fold_13, predicted_h_df13)
dist_h_df13 = sapply(36:43, function(x) as.numeric(philentropy::distance(rbind(density(folds_h_df13$y_test, from = 0,to = 1,)$y,density(na.omit(folds_h_df13[,x]),from=0,to=1)$y),est.prob = "empirical",method = "hellinger",mute.message = TRUE)))


tabla_exp2 = function(lista, df){
  results = data.frame()
  for (i in 1:10) {
    res = lista[[i]]$results
    results = rbind(results, res)
    average = sapply(results, function(x) mean(na.omit(x)))
    average$n = nrow(df)
    average$p = length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))])
    average$Total.de.paises = length(unique(df$iso))
    
  }
  return(average)
}


results_mpi = data.frame(cbind(tabla_exp2(predicted_mpi_df1,df_fold_1),tabla_exp2(predicted_mpi_df2,df_fold_2),tabla_exp2(predicted_mpi_df13,df_fold_13)))
colnames(results_mpi) = c("df1","df2","df13")
results_mpi[9:16,] = cbind(dist_mpi_df1,dist_mpi_df2,dist_mpi_df13)

results_a = data.frame(cbind(tabla_exp2(predicted_a_df1,df_fold_1),tabla_exp2(predicted_a_df2,df_fold_2),tabla_exp2(predicted_a_df13,df_fold_13)))
colnames(results_a) = c("df1","df2","df13")
results_a[9:16,] = cbind(dist_a_df1,dist_a_df2,dist_a_df13)

results_h = data.frame(cbind(tabla_exp2(predicted_h_df1,df_fold_1),tabla_exp2(predicted_h_df2,df_fold_2),tabla_exp2(predicted_h_df13,df_fold_13)))
colnames(results_h) = c("df1","df2","df13")
results_h[9:16,] = cbind(dist_h_df1,dist_h_df2,dist_h_df13)



# Plots for Dataset 2 -----------------------------------------------------
 
folds = folds_df2

# Densities  
data_plot_all = plot_data(folds, "none")

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
      axis.text = element_text(size = 18),
      legend.position = "bottom",
      legend.text = element_text(size = 18), #
      
    )
)


# Linear Methods ----------------------------------------------------------

plot_data = function(df, division){
  
  data_plot = df
  
  # choose methods by comment
  
  #data_plot$yhat.pls_tc = NULL
  #data_plot$yhat.beta_tc_cr = NULL
  #data_plot$yhat.beta_tc_tree_cr = NULL
  
  data_plot$yhat.elastic_tc = NULL
  data_plot$yhat.beta_tc_ela = NULL
  data_plot$yhat.beta_tc_tree_ela= NULL
  
  
  data_plot$yhat.xgb_tc = NULL
  data_plot$yhat.betaboost_tc = NULL
  
  ######################################
  
  if (division == "region"){
    data_plot = data_plot[,c(3,34:ncol(data_plot))]
    data_plot$year_trend = NULL  
    data_plot = reshape2::melt(data_plot,id.vars = "region_Other")
    
    
  } else {
    data_plot = data_plot[,c(1,35:ncol(data_plot))]
    data_plot = reshape2::melt(data_plot, id.vars = "iso_Other")
    
  }
  
  
  return(data_plot)
}


# Plot for A
folds_a = folds_a_df2
data_plot_a = plot_data(folds_a,"none")
 
densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("A-true", "Linear-PLS", "Beta-PLS", "Beta-Tree-PLS")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))

# Plot for H
folds_h = folds_h_df2
data_plot_h = plot_data(folds_h,"none")
 
densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  xlim(-.001, 1) +
  labs(x = "H", y = "Density", color = "") +
  scale_color_discrete(labels = c("H-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("H-true", "Linear-PLS", "Beta-PLS", "Beta-Tree-PLS")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))


# Plot for MPI  HXA


# Y-hat A*H
 
A_hat = folds_a_df2
A_hat = A_hat[,35:43]
H_hat = folds_h_df2
H_hat = H_hat[,35:43]
MPI_hat_hxa = A_hat * H_hat 
 
data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,2,3,4)])
 
densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("HxA-true", "Linear-PLS", "Beta-PLS", "Beta-Tree-PLS")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))

# Plot for MPI

folds = folds_df2
data_plot_all = plot_data(folds, "none")
 
densities_plot = ggplot(data_plot_all, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Ytrue","Linear-PLS","Beta-PLS","Beta-Tree-PLS"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("Ytrue", "Linear-PLS", "Beta-PLS", "Beta-Tree-PLS")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))



# Variable Selection Methods ----------------------------------------------

plot_data = function(df, division){
  
  data_plot = df
  
  # choose methods by comment
  
  data_plot$yhat.pls_tc = NULL
  data_plot$yhat.beta_tc_cr = NULL
  data_plot$yhat.beta_tc_tree_cr = NULL
  
  #data_plot$yhat.elastic_tc = NULL
  #data_plot$yhat.beta_tc_ela = NULL
  #data_plot$yhat.beta_tc_tree_ela= NULL
  
  
  data_plot$yhat.xgb_tc = NULL
  data_plot$yhat.betaboost_tc = NULL
  
  ######################################
  
  if (division == "region"){
    data_plot = data_plot[,c(3,34:ncol(data_plot))]
    data_plot$year_trend = NULL  
    data_plot = reshape2::melt(data_plot,id.vars = "region_Other")
    
    
  } else {
    data_plot = data_plot[,c(1,35:ncol(data_plot))]
    data_plot = reshape2::melt(data_plot, id.vars = "iso_Other")
    
  }
  
  
  return(data_plot)
}


# Plot for A
data_plot_a = plot_data(folds_a,"none")

densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("A-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))

 
# Plot for H
data_plot_h = plot_data(folds_h,"none")

densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  xlim(-.001, 1) +
  labs(x = "H", y = "Density", color = "") +
  # xlim(0.32, 1) +
  scale_color_discrete(labels = c("H-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("H-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))



# Plot for MPI  HXA

# Y-hat A*H

data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,5,6,7)])

densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)","a","b","c"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("HxA-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))


# Plot for MPI
data_plot_mpi = plot_data(folds,"none")

densities_plot_mpi = ggplot(data_plot_mpi, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_mpi + 
  labs(x = "H", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Y-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("Y-true","Elastic Net","Beta (elastic)","Beta-Tree (elastic)")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))


# Boosting Methods --------------------------------------------------------

plot_data = function(df, division){
  
  data_plot = df
  
  # choose methods by comment
  
  data_plot$yhat.pls_tc = NULL
  data_plot$yhat.beta_tc_cr = NULL
  data_plot$yhat.beta_tc_tree_cr = NULL
  
  data_plot$yhat.elastic_tc = NULL
  data_plot$yhat.beta_tc_ela = NULL
  data_plot$yhat.beta_tc_tree_ela= NULL
  
  #data_plot$yhat.xgb_tc = NULL
  #data_plot$yhat.betaboost_tc = NULL
  
  ######################################
  
  if (division == "region"){
    data_plot = data_plot[,c(3,34:ncol(data_plot))]
    data_plot$year_trend = NULL  
    data_plot = reshape2::melt(data_plot,id.vars = "region_Other")
    
    
  } else {
    data_plot = data_plot[,c(1,35:ncol(data_plot))]
    data_plot = reshape2::melt(data_plot, id.vars = "iso_Other")
    
  }
  
  
  return(data_plot)
}


# Plot for A
data_plot_a = plot_data(folds_a,"none")
 
densities_plot_A = ggplot(data_plot_a, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_A + 
  labs(x = "A", y = "Density", color = "") +
  xlim(0.32, 1) +
  scale_color_discrete(labels = c("A-true","XGBoost","Betaboost"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("A-true","XGBoost","Betaboost")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))

 
# Plot for H
data_plot_h = plot_data(folds_h,"none")
#unique(data_plot_h$variable)

densities_plot_H = ggplot(data_plot_h, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot_H + 
  labs(x = "H", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("H-true","XGBoost","Betaboost"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("H-true","XGBoost","Betaboost")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))



# Plot for MPI  HXA


# Y-hat A*H 

data_plot_hxa = reshape2::melt(MPI_hat_hxa[,c(1,8,9)])
 
densities_plot = ggplot(data_plot_hxa, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI (HxA)", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("HxA-true","XGBoost","Betaboost"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("HxA-true","XGBoost","Betaboost")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))


# MPI

data_plot_all = plot_data(folds, "none")
 
densities_plot = ggplot(data_plot_all, aes(x=value, color = variable,linetype = variable))  +
  geom_density(lwd = 1) 

densities_plot + 
  labs(x = "MPI", y = "Density", color = "") +
  xlim(-.001, 1) +
  scale_color_discrete(labels = c("Ytrue","XGBoost","Betaboost"))+
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted", "dotdash"),
    labels = c("Ytrue","XGBoost","Betaboost")
  ) +
  guides(color = guide_legend(title = ""), linetype = guide_legend(title = ""))


