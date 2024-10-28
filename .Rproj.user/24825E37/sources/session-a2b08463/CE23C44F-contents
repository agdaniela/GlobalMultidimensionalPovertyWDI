
####################################################################################################

categoriavar = function(df){
  vars = read.csv("auxiliary_functions/dataframes_variables.csv")
  vars = vars[!duplicated(vars$variable),] #saco dupes
  vars = data.frame(lapply(vars, function(x) {gsub(",", ".", x)})) #saco comas de mas
  vars = data.frame(lapply(vars, function(x) {gsub(":", ".", x)})) #saco : de mas
  vars = data.frame(lapply(vars, function(x) {gsub("\\+", ".", x)})) #saco : de mas
  vars = data.frame(lapply(vars, function(x) {gsub("\\/", ".", x)})) #saco : de mas
  vars = data.frame(lapply(vars, function(x) {gsub("\\'", ".", x)})) #saco : de mas
  #cambio algunos raros
  vars[29,1] = colnames(df)[29]  
  vars[835,1] = "women.making.their.own.informed.decisions.regarding.sexual.relations..contraceptive.use.and.reproductive.health.care.....of.women.age.15.49."
  vars[1309,1] = "statistical.performance.indicators..spi...pillar.3.data.products.score...scale.0.100." 
  vars[141,1] = "disaster.risk.reduction.progress.score..1.5.scale..5.best."
  #le saco las mayus
  vars$variable <- tolower(vars$variable)
  colnames(df) <- tolower(colnames(df))
  #busco variables similares en vars y les asigno la misma categoria porque si
  faltantesvars = data.frame(variable = c("net.oda.provided..total..constant.2020.us..","net.oda.provided..total....of.gni.", "net.oda.provided..total..current.us..", "net.oda.provided.to.the.least.developed.countries....of.gni.", "net.oda.provided..to.the.least.developed.countries..current.us..",
                                          "control.of.corruption..standard.error", "control.of.corruption..percentile.rank..upper.bound.of.90..confidence.interval" , "control.of.corruption..percentile.rank..lower.bound.of.90..confidence.interval", 
                                          "control.of.corruption..percentile.rank", "control.of.corruption..number.of.sources", "control.of.corruption..estimate",
                                          "government.effectiveness..standard.error","government.effectiveness..percentile.rank..upper.bound.of.90..confidence.interval" ,"government.effectiveness..percentile.rank..lower.bound.of.90..confidence.interval",                             
                                          "government.effectiveness..percentile.rank","government.effectiveness..number.of.sources", "government.effectiveness..estimate",
                                          "regulatory.quality..standard.error","regulatory.quality..percentile.rank..upper.bound.of.90..confidence.interval","regulatory.quality..percentile.rank..lower.bound.of.90..confidence.interval" ,                                  
                                          "regulatory.quality..percentile.rank","regulatory.quality..number.of.sources","regulatory.quality..estimate",                                                                                  
                                          "rule.of.law..standard.error","rule.of.law..percentile.rank..upper.bound.of.90..confidence.interval","rule.of.law..percentile.rank..lower.bound.of.90..confidence.interval",                                          
                                          "rule.of.law..percentile.rank","rule.of.law..number.of.sources","rule.of.law..estimate",                                                                                         
                                          "political.stability.and.absence.of.violence.terrorism..standard.error","political.stability.and.absence.of.violence.terrorism..percentile.rank..upper.bound.of.90..confidence.interval","political.stability.and.absence.of.violence.terrorism..percentile.rank..lower.bound.of.90..confidence.interval",
                                          "political.stability.and.absence.of.violence.terrorism..percentile.rank","political.stability.and.absence.of.violence.terrorism..number.of.sources","political.stability.and.absence.of.violence.terrorism..estimate",
                                          "voice.and.accountability..standard.error","voice.and.accountability..percentile.rank..upper.bound.of.90..confidence.interval","voice.and.accountability..percentile.rank..lower.bound.of.90..confidence.interval",
                                          "voice.and.accountability..percentile.rank","voice.and.accountability..number.of.sources","voice.and.accountability..estimate",
                                          "firms.visited.or.required.meetings.with.tax.officials....of.firms."),
                             tipo = c(rep("Aid.Effectiveness",5), rep("Public.Sector",36), "Private.Sector" ) ) 
  vars = rbind(vars,faltantesvars)
  # ahora creo un dataframe que saca las de vars en colnames plsdata
  variables = vars[(vars$variable %in% colnames(df)),]  
  # cambio el orden para que quede igual que el de la base
  variables = variables[match(colnames(df), variables$variable),]
  # ahora queda con _, reeemplazo los nombres de las columnas
  colnames(df) = paste(colnames(df), variables$tipo, sep = "_")
  return(df)
}

 
dummiesregion = function(df){
  
  
  df$region_Other = as.factor(df$region_Other) 
  region_dummies = model.matrix( ~ df$region_Other )
  region_dummies = region_dummies[,-1]
  df = data.frame(df, region_dummies)
  
  df <- df[, c(1:7,1486:1490,8:1485)]
  return(df)
}

# year 
dummiesyear = function(df){
  df$year_Other = as.factor(df$year_Other) 
  year_dummies = model.matrix( ~ df$year_Other )
  year_dummies = year_dummies[,-1]
  
  df = data.frame(df, year_dummies)
  df = df[, c(1:12, 1491:1511,13:1490)]
  return(df)
}



 
