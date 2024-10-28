#' Function used for experiment 1
#' 
#' @param df Which of the datasets in Appendix A is preferred to used
#' @param target Target variable ("mpi_Other","h_Other","a_Other")
#' @param nfolds Number of folds used in K-Fold Cross-Validation for hyperparameter selection
#' @param corte Parameter for beta-tree models
#' @param link_phi Parameter for beta model
#' @param link_mu  Parameter for beta model
#' @param distancia Distance measure selected
#'
#' @return a list with evaluation metrics of each method  
 
# Calling auxiliary functions
function_files <- list.files(path = "./auxiliary_functions", pattern = "\\.R$", full.names = TRUE)
lapply(function_files, source)


main_function_exp1 = function(df, target, corte , link_phi, link_mu, distancia){
  
  #  
  # 
  results = data.frame()
  nfolds <- 5
  
  df$year_trend <- as.numeric(as.character(df$year_Other))
  df$year_trend <- df$year_trend - min(df$year_trend)
  
  # Train and test datasets
  data <- random.split(df, 0.8)
  ytrain <- data$data_train[, target] 
  Ttrain = data$data_train[, c(13:33)] #dummies de tiempo
  Rtrain = data$data_train[, c(8:12)] #regiones
  Xtrain <- data$data_train[,-c(1:33)]
  
  ytest <- data$data_test[, target]
  Ttest = data$data_test[, c(13:33)]
  Rtest = data$data_test[, c(8:12)]
  Xtest <- data$data_test[,-c(1:33)]
  # data <- as.data.frame(cbind(ytrain, Xtrain))
  pls.directions = 30 
  
  
  
  ##################################################################################################### ----
  # Continuous time
  
  # 1.  Using dimension reduction
  
  hyperparam_pls <- kfoldCV.pls(Xtrain, ytrain, nfolds, pls.directions)
  hyperparam_beta <- kfoldCV.plsbeta(Xtrain, ytrain, nfolds, pls.directions)
  d_pls = hyperparam_pls$d.min
  d_plsbeta = hyperparam_beta$d.min
  #  
  
  # 1.1 PLS with optimal $d$ using linear predictor. 
  #  Data with dimension reduction
  pls.projections_tc_cr <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_pls, scale = FALSE)
  data_tc_cr <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_tc_cr$W))
  newdata_tc_cr <- data.frame(as.matrix(Xtest) %*% pls.projections_tc_cr$W)
  colnames(newdata_tc_cr) <- colnames(data_tc_cr)[-1]
  
  # Train and test data with dimension reduction and dummies of time and region - formula of fit
  data_tc_cr_dum = data.frame(data_tc_cr, Rtrain)
  formu_tc_cr = as.formula(paste("ytrain", paste(names(data_tc_cr_dum)[-1], collapse=" + "), sep=" ~ ")) 
  newdata_tc_cr_dum = data.frame(newdata_tc_cr,Rtest)
  
  # Fit
  pls.fit <- tryCatch(lm(ytrain ~ . , data_tc_cr_dum), error= function(e) {return(NA)}  )
  # Predict over Xtest
  ytest_pred.pls_tc  <- tryCatch(predict(pls.fit, newdata_tc_cr_dum), error= function(e) {return(NA)}  )
  # Distance
  dist_pls_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.pls_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # 
  # 1.3 PLS with optimal $d$ using beta regression (For hyperparameter, training with the prediction model)
  pls.projections_tc_dbeta <- chemometrics::pls1_nipals(Xtrain, ytrain, a = d_plsbeta, scale = FALSE)
  data_tc_dbeta <- as.data.frame(cbind(ytrain, as.matrix(Xtrain) %*% pls.projections_tc_dbeta$W))
  newdata_tc_dbeta <- data.frame(as.matrix(Xtest) %*% pls.projections_tc_dbeta$W)
  colnames(newdata_tc_dbeta) <- colnames(data_tc_dbeta)[-1]
  
  # Fit
  formu_tc_dbeta = as.formula(paste("ytrain", paste(names(data_tc_dbeta)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_cr <- tryCatch(betareg::betareg(formu_tc_dbeta, data = data_tc_dbeta, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  # Predict over Xtest
  ytest_pred.beta_tc_cr = tryCatch(betareg::predict(beta.fit_tc_cr, newdata_tc_dbeta), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  # 1.4 PLS with optimal $d$ using beta tree model for prediction.
  
  # Fit
  dummy_corte_train = ifelse(ytrain<=corte, 1, 0)
  dummy_corte_test = as.vector(ifelse(ytest<=corte, 1, 0))
  
  data_beta_tc_tree_cr = data.frame(data_tc_dbeta,dummy_corte_train)
  formu_tc_dbeta_tree = as.formula(paste("ytrain", paste(names(data_beta_tc_tree_cr)[-1], collapse=" + "), sep=" ~ ")) 
  
  beta.fit_tc_tree_cr <- tryCatch(betareg::betatree(formu_tc_dbeta_tree, ~ dummy_corte_train, data = data_beta_tc_tree_cr ,  link.phi = link_phi, link = link_mu ), error= function(e) {return(NA)}  )
  #predict
  newdata_beta_tc_tree_cr = data.frame(newdata_tc_dbeta, dummy_corte_test)
  names(newdata_beta_tc_tree_cr)[length(names(newdata_beta_tc_tree_cr))]  = "dummy_corte_train"
  ytest_pred.beta_tc_tree_cr =  tryCatch(predict(beta.fit_tc_tree_cr, newdata_beta_tc_tree_cr), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_tree_cr = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_tree_cr)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ) , error= function(e) {return(NA)}  )
  
  
  
  # 2. Based on Regularized Regresion/Variable Selection
  # Data without dimension reduction
  data_tc_sr = data.frame(Xtrain, Rtrain)
  newdata_tc_sr <- data.frame(Xtest, Rtest)
  
  # 2.1 Elastic Net.
  # Fit 
  hyperparam <- kfoldCV.elastic(data_tc_sr, ytrain, nfolds)
  elastic.fit_tc <- glmnet::glmnet(x = data_tc_sr, y = ytrain, family = "gaussian", alpha = hyperparam$best.alpha, lambda = hyperparam$best.lambda)
  # Predict over Xtest
  ytest_pred.elastic_tc <- glmnet::predict.glmnet(elastic.fit_tc, as.matrix(newdata_tc_sr))
  # distance
  dist_elastic_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.elastic_tc)$y), est.prob = "empirical", method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # 2.2 Beta regression using selected covariates with elastic net.
  tmp_coeffs_tc <- coef(elastic.fit_tc, s = "lambda.min")
  var_selected_tc = tmp_coeffs_tc@Dimnames[[1]][tmp_coeffs_tc@i +1]
  num_chosen_ela =  length(var_selected_tc)
  Xtrain_tc_ela = Xtrain[,colnames(Xtrain) %in% var_selected_tc]
  Xtest_tc_ela <- Xtest[,colnames(Xtest) %in% var_selected_tc]
  # Fit
  data_beta_tc_ela = data.frame(Xtrain_tc_ela, Rtrain)
  formu_tc_ela = as.formula(paste("ytrain", paste(names(data_beta_tc_ela)[-1], collapse=" + "), sep=" ~ ")) 
  beta.fit_tc_ela <- tryCatch(betareg::betareg(formu_tc_ela , data = data_beta_tc_ela, link.phi = link_phi, link = link_mu), error= function(e) {return(NA)}  )
  # Predict over Xtest
  newdata_beta_tc_ela = data.frame(Xtest_tc_ela, Rtest)
  ytest_pred.beta_tc_ela = tryCatch(predict(beta.fit_tc_ela, newdata_beta_tc_ela), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # 2.3 Beta tree using selected covariates with elastic net.
  # Fit
  data_beta_tc_tree_ela = data.frame(data_beta_tc_ela, dummy_corte_train)
  formu_tc_tree_ela = as.formula(paste("ytrain", paste(names(data_beta_tc_tree_ela)[-1], collapse=" + "), sep=" ~ "))
  beta.fit_tc_tree_ela <- tryCatch(betareg::betatree(formu_tc_tree_ela, ~ dummy_corte_train, data = data_beta_tc_tree_ela,  link.phi = link_phi, link = link_mu   ), error= function(e) {return(NA)}  )
  # predict
  newdata_beta_tc_tree_ela = data.frame(newdata_beta_tc_ela, dummy_corte_test )
  names(newdata_beta_tc_tree_ela)[length(names(newdata_beta_tc_tree_ela))]  = "dummy_corte_train"
  ytest_pred.beta_tc_tree_ela =  tryCatch(predict(beta.fit_tc_tree_ela, newdata_beta_tc_tree_ela), error= function(e) {return(NA)}  )
  # distance
  dist_beta_tc_tree_ela = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.beta_tc_tree_ela)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  # 3 Based on Model Selection
  
  # 3.1 XGBoost lineal.
  # Fit
  hyperparam_xgb_tc = kfoldCV.xgboost(data_tc_sr, ytrain, nfolds)
  # Predict over Xtest
  ytest_pred.xgb_tc <- tryCatch(predict(hyperparam_xgb_tc$xgb.model, newdata_tc_sr), error= function(e) {return(NA)}  )
  # distance
  dist_xgb_tc = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.xgb_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  # 3.2 Beta Boost.
  # Fit
  formu_boost_tc = as.formula(paste("ytrain", paste(names(data_tc_sr)[-1], collapse=" + "), sep=" ~ ")) 
  betaboost.fit_tc <- tryCatch(mboost::glmboost(formu_boost_tc, data = data_tc_sr, family = betaboost::BetaReg()), error= function(e) {return(NA)}  )
  # Predict over Xtest
  ytest_pred.betaboost_tc = tryCatch(predict(betaboost.fit_tc, newdata = newdata_tc_sr, type = "response"), error= function(e) {return(NA)}  )
  dist_betaboost_tc  = tryCatch(as.numeric(philentropy::distance(rbind(density(ytest)$y, density(ytest_pred.betaboost_tc)$y), est.prob = "empirical",  method = distancia, mute.message = TRUE) ), error= function(e) {return(NA)}  )
  
  
  
  
  #########################################################################
  
  # Saving results
  results = data.frame( 
    
    
    
    "MSE pls_tc" = mean((ytest-ytest_pred.pls_tc)^2),
    "MSE beta_tc_cr" = mean((ytest-ytest_pred.beta_tc_cr)^2),
    "MSE beta_tc_tree_cr" = mean((ytest-ytest_pred.beta_tc_tree_cr)^2),
    
    "MSE elastic_tc" = mean((ytest-ytest_pred.elastic_tc)^2),
    "MSE beta_tc_ela" = mean((ytest-ytest_pred.beta_tc_ela)^2),
    "MSE beta_tc_tree_ela" = mean((ytest-ytest_pred.beta_tc_tree_ela)^2),
    
    "MSE xgb_tc" = mean((ytest-ytest_pred.xgb_tc)^2),
    "MSE betaboost_tc" = mean((ytest-ytest_pred.betaboost_tc)^2),
    
    "nas pls_tc" = sum(length(which(is.na(ytest_pred.pls_tc)))),
    "nas beta_tc_cr" = sum(length(which(is.na(ytest_pred.beta_tc_cr)))),
    "nas beta_tc_tree_cr" = sum(length(which(is.na(ytest_pred.beta_tc_tree_cr)))),
    
    "nas elastic_tc" = sum(length(which(is.na(ytest_pred.elastic_tc)))),
    "nas beta_tc_ela" = sum(length(which(is.na(ytest_pred.beta_tc_ela)))),
    "nas beta_tc_tree_ela" = sum(length(which(is.na(ytest_pred.beta_tc_tree_ela)))),
    
    "nas xgb_tc" = sum(length(which(is.na(ytest_pred.xgb_tc)))),
    "nas betaboost_tc" = sum(length(which(is.na(ytest_pred.betaboost_tc)))),
    
    
    
    
    "dist pls_tc" = dist_pls_tc,
    "dist beta_tc_cr" = dist_beta_tc_cr,
    "dist beta_tc_tree_cr" = dist_beta_tc_tree_cr,
    
    "dist elastic_tc" = dist_elastic_tc,
    "dist beta_tc_ela" = dist_beta_tc_ela,
    "dist beta_tc_tree_ela" = dist_beta_tc_tree_ela,
    
    
    "dist xgb_tc" = dist_xgb_tc,
    "dist betaboost_tc" = dist_betaboost_tc,
    
    "d_pls" = d_pls,
    "d_pls_beta" = d_plsbeta,
    "nro variables ela" = num_chosen_ela,
    "n" = nrow(df),  
    "p"= length(colnames(df)[!((colnames(df) %in% c(colnames(df)[1:33])))]),
    "Total de paises" = length(unique(df$iso))
    
    
    
  )
  
  return(results)
  
}

