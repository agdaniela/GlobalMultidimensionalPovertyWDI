
# Auxiliary function for repeating Experiment 1 ---------------------------



#funciones = list(main_function_tcyd = main_function_tcyd  )

repetitions = function(df, target, corte,link_phi,link_mu, distancia, nreps){
  results = list()
  nreps = nreps
  
  
  for (rep in 1:nreps) {
    print(paste("rep = ",rep))  

    main <- tryCatch(main_function_exp1(df,target,corte,link_phi,link_mu,distancia) , error= function(e) {return(list())}  )
    if (length(main) != 0) {
      print("ok con la rep")

       
      results$MSE.pls_tc[rep] = main$MSE.pls_tc
      results$MSE.beta_tc_cr[rep] = main$MSE.beta_tc_cr
      results$MSE.beta_tc_tree_cr[rep] = main$MSE.beta_tc_tree_cr
      
      results$MSE.elastic_tc[rep] = main$MSE.elastic_tc
      results$MSE.beta_tc_ela[rep] = main$MSE.beta_tc_ela
      results$MSE.beta_tc_tree_ela[rep] = main$MSE.beta_tc_tree_ela
      
      
      results$MSE.xgb_tc[rep] = main$MSE.xgb_tc
      results$MSE.betaboost_tc[rep] = main$MSE.betaboost_tc
      
       
     
      
      
      #distancias
      
      
      results$dist.pls_tc[rep] = main$dist.pls_tc
      results$dist.beta_tc_cr[rep] = main$dist.beta_tc_cr
      results$dist.beta_tc_tree_cr[rep] = main$dist.beta_tc_tree_cr
      
      results$dist.elastic_tc[rep] = main$dist.elastic_tc
      results$dist.beta_tc_ela[rep] = main$dist.beta_tc_ela
      results$dist.beta_tc_tree_ela[rep] = main$dist.beta_tc_tree_ela
      
      results$dist.xgb_tc[rep] = main$dist.xgb_tc
      results$dist.betaboost_tc[rep] = main$dist.betaboost_tc
      
      results$d_pls[rep] = main$d_pls
      results$d_pls_beta[rep] = main$d_pls_beta
      results$nro.variables.ela[rep] = main$nro.variables.ela
      results$n[rep] = main$n
      results$p[rep] = main$p
      results$Total.de.paises[rep] = main$Total.de.paises
       
      results$nas.pls_tc[rep] = main$nas.pls_tc
      results$nas.beta_tc_cr[rep] = main$nas.beta_tc_cr
      results$nas.beta_tc_tree_cr[rep] = main$nas.beta_tc_tree_cr
      
      results$nas.elastic_tc[rep] = main$nas.elastic_tc
      results$nas.beta_tc_ela[rep] = main$nas.beta_tc_ela
      results$nas.beta_tc_tree_ela[rep] = main$nas.beta_tc_tree_ela
      
      
      results$nas.xgb_tc[rep] = main$nas.xgb_tc
      results$nas.betaboost_tc[rep] = main$nas.betaboost_tc
      
    }else{
      paste("Algo está mal con la rep", i)
      
      


      next
    }


    
  }
  return(results)
}  

















