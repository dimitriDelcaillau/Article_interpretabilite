#########################################################################################
################# Ajustement des modèles dé fréquence et de coût ########################
#########################################################################################

# Dans ce code, on met en place des fonctions pour pouvoir ajuster les modèles en fréquence
# et en coût. On a également implémenté des fonctions d'erreur pour juger la qualité du modèle
# Exemple : NRMSD (RMSE normalisé), ou la déviance de poisson
# On a également proposé deux implémentations utilisant soit la librairie classique,
# soit une autre librairie (mlr pour le GLM, caret pour le XGBoost).
# Certaines méthodes d'interprétation implémentée ne fonctionnent que pour certains packages

###############################################################################################
################################################ 1 - GLM ######################################
###############################################################################################

# A - Ajustement GLM fréquence -------------------
target_freq = function(data = data){
  #Input : 
  #       data: base de données de sinistralité (fréquence, coût)
  #        [data.frame]
  #Output : rapport du nombre de sinistres sur l'exposition
  # [vector] de [numeric]
  return(data$ClaimNb/data$Exposure)
}
y_freq = target_freq(data_test)

glm_frequence = function(base_app = data_app, var = var_freq){
  #Input : 
  #       base_app : base de données de sinistralité (fréquence, coût)
  #        [data.frame]
  #       var : listes des noms de variables
  #       [character (vector)]
  #Output : modèle glm (si var == NULL => renvoie le modèle GLM "moyen" sans var. explicative)
  if (!is.null(var)){
    variables_freq=paste(var,collapse="+")    
    formule_freq=as.formula(paste("ClaimNb~offset(log(Exposure))+",variables_freq))
    return(glm(formule_freq,data=base_app,family=poisson(link="log")))
  }
   else{ 
    return(glm(ClaimNb~offset(log(Exposure)),data = base_app,family=poisson(link="log")))
    }
}
glm_frequence_mlr = function(base_app = data_app, var = var_freq){
  #Input : 
  #       base_app : base de données de sinistralité (fréquence, coût)
  #        [data.frame]
  #       var : listes des noms de variables
  #       [character (vector)]
  #Output : modèle glm (si var == NULL => renvoie le modèle GLM "moyen" sans var. explicative)
  # Utile pour ne pas avoir "Exposure" comme variable explicative dans le modèle
  # Servira pour l'interprétation des modèles dans la suite 
  if (!is.null(var)){
    variables_freq=paste(var,collapse="+")    
    formule_freq=as.formula(paste("ClaimNb/Exposure~",variables_freq))
    return(glm(formule_freq,data=base_app, weights = base_app$Exposure,family=poisson(link="log")))
  }
  else{ 
    return(glm(ClaimNb/Exposure~1,weights = base_app$Exposure,data = base_app,family=poisson(link="log")))
  }
}

# variables_freq=paste(var_freq,collapse="+")    
# formule_freq=as.formula(paste("ClaimNb~offset(log(Exposure))+",variables_freq))
# fit_freq_moy = glm_frequence(data_app, NULL )
# fit_freq_complet = glm_frequence(data_app,var_freq)

pred_freq_glm = function(model_glm = fit_freq_complet, test = data_test){
#Input : 
  #       model_glm : modèle GLM fréquence 
  #        [output de la fonction glm]
  #       test : base de données de test
  #       [data.frame]
  #Output :prediction du modèle GLM  (ClaimNb/Exposure : nombre de sinistres "annuels")
  # [vector] de [numeric]
  return(predict(model_glm, test, type="response")/test$Exposure)
}

nrmsd = function(theo, pred, type="min_max"){
  # source : https://en.wikipedia.org/wiki/Root-mean-square_deviation
  # Input :
  #        theo : vecteur des valeurs théoriques
  #        [numeric]
  #        pred : vecteur des prédictions
  # Output : renvoie le nrmsd (RMSE normalisé) suivant un des critères "min-max", "mean", "Q1-Q3"
  # [numeric]
  rmse = Metrics::rmse(theo, pred)
  if (type=="min_max"){
    return(rmse/(max(theo)-min(theo)))
  }
  if (type=="mean"){
    return(rmse/mean(theo))
  }
  if (type=="Q1_Q3"){
    q1 = quantile(theo,0.25)
    q3 = quantile(theo,0.75)
    return(rmse/(q3-q1))
  }
}

#Déviance
Poisson.Deviance = function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}

err_test_freq_glm = function(err_func = "mse", model_glm = fit_freq_complet, test = data_test){
  #Input : 
  #       err_func : fonction d'erreur (entre mse et mae pour l'instant)
  #       [character] ("mae", "mse" , "min_max", "mean" ou "dev_pois")
  #       model_glm : modèle GLM fréquence 
  #        [output de la fonction glm]
  #       test : base de données de test
  #       [data.frame]
  #Output : errreur sur la base de test
  # [numeric]
  pred = pred_freq_glm(model_glm, test)
  y_theo = target_freq(test)
  if (err_func=="mse"){
    return(mse(y_theo, pred))
  }
  if (err_func=="mae"){
    return(mae(y_theo, pred))
  }
  if (err_func=="min_max"){
    return(nrmsd(y_theo, pred, type="min_max"))
  }
  if (err_func=="mean"){
    return(nrmsd(y_theo, pred, type="mean"))
  }
  if (err_func=="dev_pois"){
    return(Poisson.Deviance(pred, y_theo))
  }
}

# B - Ajustement GLM coût ----------------------------------
target_cout = function(dat = data_sinistres){
  #Input : 
  #       dat: base de données de sinistralité (fréquence, coût)
  #        [data.frame] (doit contenir aucun zéro dans la colonne ClaimNb (ou ClaimAmount))
  #Output : rapport du montant des sinistres sur le nombre de sinistres 
  # [vector] de [numeric] 
  return(dat$ClaimAmount/dat$ClaimNb)
}
# y_cout = target_cout()

glm_cout = function(base_app = data_sinistres(data_app), var = var_cout){
  #Input : 
  #       base_app : base de données de sinistralité (fréquence, coût)
  #        [data.frame]
  #       var : listes des noms de variables
  #       [character (vector)]
  #Output : modèle glm (si var == NULL => renvoie le modèle GLM "moyen" sans var. explicative)
  if (!is.null(var)){
    variables_cout=paste(var,collapse="+")    
    formule_cout=as.formula(paste("ClaimAmount/ClaimNb~",variables_cout))
    return(glm(formule_cout,data=base_app,weights = base_app$ClaimNb,family=Gamma(link="log")))
  }
  else{ 
    return(glm(ClaimAmount/ClaimNb~1,data = base_app,weights = base_app$ClaimNb,family=Gamma(link="log")))
  }
}
pred_cout_glm = function(model_glm = fit_cout_complet, test = data_test, ecret = 300){
  #Input : 
  #       model_glm : modèle GLM coût 
  #        [output de la fonction glm]
  #       test : base de données de test
  #       [data.frame]
  #       ecret : valeur de l'ecrêtement
  #       [numeric]
  #Output :prediction du modèle GLM 
  # [vector] de [numeric]
  return(predict(model_glm, test, type = "response")+ecret)
}
err_test_cout_glm = function(err_func = "mse", model_glm = fit_freq_complet, test = data_test, ecret = ecret){
  #Input : 
  #       err_func : fonction d'erreur (entre mse et mae pour l'instant)
  #       [character] ("mae" ou "mse")
  #       model_glm : modèle GLM cout 
  #        [output de la fonction glm]
  #       test : base de données de test
  #       [data.frame]
  #Output : errreur sur la base de test
  # [numeric]
  pred = pred_cout_glm(model_glm, test, ecret)
  y_theo = target_cout(test)
  if (err_func=="mse"){
    return(mse(y_theo, pred))
  }
  if (err_func=="mae"){
    return(mae(y_theo, pred))
  }
  if (err_func=="nrmsd_min_max"){
    return(nrmsd(y_theo, pred, type="min_max"))
  }
  if (err_func=="mean"){
    return(nrmsd(y_theo, pred, type="mean"))
  }
}

# C - GLM coût x fréquence ----------
pred_total_glm = function(model_freq_glm, model_cout_glm, test = data_test, ecret = 300){
  #Input : 
  #       model_glm : modèle GLM coût 
  #        [output de la fonction glm]
  #       test : base de données de test
  #       [data.frame]
  #       ecret : valeur de l'ecrêtement
  #       [numeric]
  #Output :prediction du modèle GLM 
  # [vector] de [numeric]
  return(pred_freq_glm(model_freq_glm, test)* pred_cout_glm(model_cout_glm, test, ecret))
}
target_total = function(dat = data_sinistres){
  #Input : 
  #       dat: base de données de sinistralité (fréquence, coût)
  #        [data.frame] (doit contenir aucun zéro dans la colonne ClaimNb (ou ClaimAmount))
  #Output : rapport du montant des sinistres sur l'exposition
  # [vector] de [numeric] 
  return(dat$ClaimAmount/dat$Exposure)
}
err_test_total_glm = function(err_func = "mse", model_freq_glm, model_cout_glm, test = data_test, ecret){
  #Input : 
  #       err_func : fonction d'erreur (entre mse et mae pour l'instant)
  #       [character] ("mae" ou "mse")
  #       model_glm : modèle GLM cout 
  #        [output de la fonction glm]
  #       test : base de données de test
  #       [data.frame]
  #Output : errreur sur la base de test
  # [numeric]
  pred = pred_total_glm(model_freq_glm, model_cout_glm, test, ecret)
  y_theo = target_total(test)
  if (err_func=="mse"){
    return(mse(y_theo, pred))
  }
  if (err_func=="mae"){
    return(mae(y_theo, pred))
  }
  if (err_func=="nrmsd_min_max"){
    return(nrmsd(y_theo, pred, type="min_max"))
  }
  if (err_func=="mean"){
    return(nrmsd(y_theo, pred, type="mean"))
  }
}



###########################################################################################
########################################### 2 - XGBoost ###################################
###########################################################################################

# A - XGBoost fréquence ----------
# Cross validation XGboost
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$RMSE, probs = probs), min(x$results$RMSE))) +
    theme_bw()
}

# tune_grid = expand.grid( nrounds = c(50, 100, 150,250), eta = c(0.01 ,0.1, 0.3),
#   max_depth = c(2, 4, 6), gamma = 0, colsample_bytree = 1, min_child_weight = 1,subsample = 1)

xgboost_cv_param = function(dat_app, tune_grid = tune_grid, type="total", objective = "reg:linear"){
  # Input :
  #        dat_app : base de données utilisées pour entraîner le XGBoost
  #        [data.frame]
  #        tune_grid : une grille de paramètres (nrounds, eta, max_depth, gamma, colsample_bytree,min_child_weight, subsample)
  #        ["data.frame"] (appel avec la fonction expand.grid)
  #        type : indiquant si on calibre un XGBoost sur la fréquence (ClaimNb/Exposure), cout(ClaimAmount/ClaimNb) ou total (ClaimAmount/Exposure)
  #        [char] ("total", "frequence" ou "cout")
  #        objective : fonction objectif dans l'optimisation du xgb
  #        [char] ("reg:linear" (total, cout ou frequence), "count:poisson" (fréquence) ou "reg:gamma" (cout))
  # Ouput : le "meileur" XGBoost en réalisant des validations croisées ($bestTune : meilleurs paramètres)
  if (type=="frequence"){
    data_app = dat_app
    variables=paste(var_freq,collapse="+")    
    formule=as.formula(paste("ClaimNb/Exposure~",variables))
    poids = dat_app$Exposure
    # objective = "count:poisson"
  }
  if (type=="cout"){
    data_app = data_sinistres(dat_app)
    variables=paste(var_freq,collapse="+")    
    formule=as.formula(paste("ClaimAmount/ClaimNb~",variables))
    poids = dat_app$ClaimNb
    # objective = "reg:gamma"
    
  }
  if (type=="total"){
    data_app =dat_app
    variables=paste(var_freq,collapse="+")    
    formule=as.formula(paste("ClaimAmount/Exposure~",variables))
    poids = dat_app$Exposure
    # objective = 'reg:linear'
  }
  tune_control <- caret::trainControl(
    method = "cv", # cross-validation
    number = 5, # with n folds
    #index = createFolds(tr_treated$Id_clean), # fix the folds
    verboseIter = FALSE, # no training log
    allowParallel = TRUE # FALSE for reproducible results
  ) 
    # Step 1
    xgb_tune = caret::train(formule, data = data_app, method = "xgbTree",
                            weight = poids, tuneGrid = tune_grid,
                            verbose=F, trControl = tune_control, objective = objective )
    g1 = tuneplot(xgb_tune)
    
    # Step 2
    tune_grid2 <- expand.grid(
      nrounds = c(50,100,150),
      eta = xgb_tune$bestTune$eta,
      max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                         c(xgb_tune$bestTune$max_depth:4),
                         xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
      gamma = 0,
      colsample_bytree = 1,
      min_child_weight = c(1, 2, 3),
      subsample = 1
    )
    
    xgb_tune2 = caret::train(formule, data = data_app, method = "xgbTree",
                             weight = poids, tuneGrid = tune_grid2,
                             verbose=F, trControl = tune_control, objective = objective )
    g2 = tuneplot(xgb_tune2)
    # Step 3 
    tune_grid3 <- expand.grid(
      nrounds = c(50,100,150),
      eta = xgb_tune$bestTune$eta,
      max_depth = xgb_tune2$bestTune$max_depth,
      gamma = 0,
      colsample_bytree = c(0.4, 0.8, 1.0),
      min_child_weight = xgb_tune2$bestTune$min_child_weight,
      subsample = c(0.5, 0.75, 1.0)
    )
    
    xgb_tune3 = caret::train(formule, data = data_app, method = "xgbTree",
                             weight = poids, tuneGrid = tune_grid3,
                             verbose=F, trControl = tune_control, objective = objective )
    g3 = tuneplot(xgb_tune3)
    # Step 4
    tune_grid4 <- expand.grid(
      nrounds = c(50,100, 150),
      eta = xgb_tune$bestTune$eta,
      max_depth = xgb_tune2$bestTune$max_depth,
      gamma = c(0, 0.3, 0.7 ,1.0),
      colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
      min_child_weight = xgb_tune2$bestTune$min_child_weight,
      subsample = xgb_tune3$bestTune$subsample
    )
    
    xgb_tune4 = caret::train(formule, data = data_app, method = "xgbTree",
                             weight = poids, tuneGrid = tune_grid4,
                             verbose=F, trControl = tune_control, objective = objective )
    
    g4 = tuneplot(xgb_tune4)
    # Step 5
    tune_grid5 <- expand.grid(
      nrounds = c(50,100,250,400,500),
      eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
      max_depth = xgb_tune2$bestTune$max_depth,
      gamma = xgb_tune4$bestTune$gamma,
      colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
      min_child_weight = xgb_tune2$bestTune$min_child_weight,
      subsample = xgb_tune3$bestTune$subsample
    )
    xgb_tune5 = caret::train(formule, data = data_app, method = "xgbTree",
                             weight = poids, tuneGrid = tune_grid5,
                             verbose=F, trControl = tune_control, objective = objective )
    g5 = tuneplot(xgb_tune5)
    # Modèle final
    final_grid <- expand.grid(
      nrounds = xgb_tune5$bestTune$nrounds,
      eta = xgb_tune5$bestTune$eta,
      max_depth = xgb_tune5$bestTune$max_depth,
      gamma = xgb_tune5$bestTune$gamma,
      colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
      min_child_weight = xgb_tune5$bestTune$min_child_weight,
      subsample = xgb_tune5$bestTune$subsample
    )
    #
    xgb_final = caret::train(formule, data = data_app, method = "xgbTree",
                             weight = poids, tuneGrid = final_grid,
                             verbose=F, trControl = caret::trainControl(method="none") ,objective = objective)
    return(list(mod = xgb_final, g1 = g1, g2 = g2, g3 = g3, g4 = g4, g5 = g5))
}


# freqq = xgboost_cv_param(app_1, tune_grid, "frequence")
# coutt = xgboost_cv_param(app_1, tune_grid, "cout")
# complett =freqq = xgboost_cv_param(app_1, tune_grid, "complet")

# caret::train(formule, data = data_app, method = "xgbTree",
#              weight = app_1$Exposure, tuneGrid = freqq$bestTune,
#              verbose=F, trControl = caret::trainControl(method="none") ,objective = "count:poisson")

# caret_xgb_param <- expand.grid(nrounds = 100, eta = 0.1, max_depth = 4,
#   gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 1)
fit_xgb_freq_caret = function(dat_app, caret_xgb_param = caret_xgb_param, vars = var_freq, objective = "count:poisson", seed = 2019){
  # Input :
  #       dat_app : base d'apprentissage du modèle fréquence 
  #       [data.frame]
  #       caret_xgb_param : grille de paramètres (avec une valeur par paramètre) :
  #       [data.frame] : à créer avec la fonction "expand.grid"
  #       vars : liste des noms des variables utilisées pour l'ajustement
  #       [char] (vector)
  #       objective : fonction objectif du XGBoost : 
  #       [char] : soit "reg:linear" ou "count:poisson" (par défaut)
  #       seed : graine pour la reproductibilité des résultats
  #       [integer]
  # Output : modèle XGBoost ajusté avec la fonction train de caret
  variables=paste(vars,collapse="+")    
  formule=as.formula(paste("ClaimNb/Exposure~",variables))
  set.seed(seed)
  return(suppressWarnings(caret::train(formule, data = dat_app, method = "xgbTree",
               weight = dat_app$Exposure, tuneGrid = caret_xgb_param,
               verbose=F, trControl = caret::trainControl(method="none") ,objective = objective)))
  
}
pred_xgb_freq_caret = function(model, test){
  # Input :
  #       model : modèle XGBoost ajusté avec caret::train
  #       [xgb.Booster] (créé avec fit_xgb_freq_caret par exemple)
  #       test : base de test pour laquelle on va réaliser les prédictions
  #       [data.frame]
  # Output : prédiction réalisée par le modèle "model" sur la base de test
  #         [numeric] (vector)
  return(predict(model,test))
}

# xgboost_xgb_param <-  list(eta = 0.1, colsample_bytree=0.4,max_depth=6,
#                         gamma=1, min_child_weight=2, subsample = 0.5)

fit_xgb_freq_xgboost = function(dat_app, xgboost_xgb_param = xgboost_xgb_param, nrounds = 100, vars = var_freq, objective = "count:poisson" , verbose = F, seed = 2019){
  # Input :
  #       dat_app : base d'apprentissage du modèle fréquence 
  #       [data.frame]
  #       caret_xgb_param : grille de paramètres (avec une valeur par paramètre) :
  #       [list] 
  #       nrounds : nombre d'itérations dans l'algo XGBoost
  #       [integer] (par défaut : 100)
  #       vars : liste des noms des variables utilisées pour l'ajustement
  #       [char] (vector)
  #       objective : fonction objectif du XGBoost : 
  #       [char] : soit "reg:linear" ou "count:poisson" (par défaut)
  #       verbose : indique si on affiche les résultats au fur et à mesure des itérations ou non
  #       [bool] (par défaut : False)
  #       seed : graine pour la reproductibilité des résultats
  #       [integer]
  # Output : modèle XGBoost ajusté avec la fonction xgboost du package xgboost
  variables=paste(vars,collapse="+")    
  formule=as.formula(paste("~",variables))
  sparse_matrix <- sparse.model.matrix(formule, data=dat_app , weight = dat_app$Exposure)
  set.seed(seed)
  return(xgboost(data = sparse_matrix, label = dat_app$ClaimNb/dat_app$Exposure,nrounds=nrounds
                 ,params=xgboost_xgb_param, objective=objective,verbose = verbose))
}
# fit_xgboost_freq_caret(app_1, caret_xgb_param, var_freq, "count:poisson")

pred_xgb_freq_xgboost = function(model, test, vars = var_freq){
  # Input :
  #       model : modèle XGBoost ajusté avec xgboost
  #       [xgb.Booster] (créé avec fit_xgb_freq_xgboost par exemple)
  #       test : base de test pour laquelle on va réaliser les prédictions
  #       [data.frame]
  #       vars : liste des noms des variables utilisées pour l'ajustement
  #       [char] (vector)
  # Output : prédiction réalisée par le modèle "model" sur la base de test
  #         [numeric] (vector)
  variables=paste(vars,collapse="+")    
  formule=as.formula(paste("~",variables))
  sparse_matrix <- sparse.model.matrix(formule, data=test , weight = test$Exposure)
  return(predict(model,sparse_matrix))
}

# B - XGBoost Coût ----------

fit_xgb_cout_caret = function(dat_app, caret_xgb_param = caret_xgb_param, vars = var_cout, objective = "reg:gamma", seed = 2019){
  # Input :
  #       dat_app : base d'apprentissage du modèle coût 
  #       [data.frame]
  #       caret_xgb_param : grille de paramètres (avec une valeur par paramètre) :
  #       [data.frame] : à créer avec la fonction "expand.grid"
  #       vars : liste des noms des variables utilisées pour l'ajustement
  #       [char] (vector)
  #       objective : fonction objectif du XGBoost : 
  #       [char] : soit "reg:linear" ou "reg:gamma" (par défaut)
  #       seed : graine pour la reproductibilité des résultats
  #       [integer] (par défaut : 2019)
  # Output : modèle XGBoost ajusté avec la fonction train de caret
  variables=paste(vars,collapse="+")    
  formule=as.formula(paste("ClaimAmount/ClaimNb~",variables))
  set.seed(seed)
  return(suppressWarnings(caret::train(formule, data = dat_app, method = "xgbTree",
                                       weight = dat_app$ClaimNb, tuneGrid = caret_xgb_param,
                                       verbose=F, trControl = caret::trainControl(method="none") ,objective = objective)))
  
}

pred_xgb_cout_caret = function(model, test, ecret = 300){
  #Input : 
  #       model : modèle XGB coût 
  #        [output de la fonction caret::train]
  #       test : base de données de test (pas écrétée)
  #       [data.frame]
  #       ecret : valeur de l'ecrêtement
  #       [numeric]
  #Output :prediction du modèle XGB
  # [vector] de [numeric]
  return(predict(model, test)+ecret)
}

fit_xgb_cout_xgboost = function(dat_app, xgboost_xgb_param = xgboost_xgb_param, nrounds = 1000, vars = var_cout, objective = "reg:gamma" , verbose = F, seed = 2019){
  # Input :
  #       dat_app : base d'apprentissage du modèle cout 
  #       [data.frame]
  #       caret_xgb_param : grille de paramètres (avec une valeur par paramètre) :
  #       [list] 
  #       nrounds : nombre d'itérations dans l'algo XGBoost
  #       [integer] (par défaut : 100)
  #       vars : liste des noms des variables utilisées pour l'ajustement
  #       [char] (vector)
  #       objective : fonction objectif du XGBoost : 
  #       [char] : soit "reg:linear" ou "reg:gamma" (par défaut)
  #       verbose : indique si on affiche les résultats au fur et à mesure des itérations ou non
  #       [bool] (par défaut : False)
  #       seed : graine pour la reproductibilité des résultats
  #       [integer]
  # Output : modèle XGBoost ajusté avec la fonction xgboost du package xgboost
  variables=paste(vars,collapse="+")    
  formule=as.formula(paste("~",variables))
  sparse_matrix <- sparse.model.matrix(formule, data=dat_app , weight = dat_app$ClaimNb)
  set.seed(seed)
  return(xgboost(data = sparse_matrix, label = dat_app$ClaimAmount/dat_app$ClaimNb,nrounds=nrounds
                 ,params=xgboost_xgb_param, objective=objective,verbose = verbose))
}

pred_xgb_cout_xgboost = function(model, test, vars = var_cout, ecret = 300){
  # Input :
  #       model : modèle XGBoost ajusté avec xgboost
  #       [xgb.Booster] (créé avec fit_xgb_freq_xgboost par exemple)
  #       test : base de test pour laquelle on va réaliser les prédictions
  #       [data.frame]
  #       vars : liste des noms des variables utilisées pour l'ajustement
  #       [char] (vector)
  #       ecret : valeur de l'ecretement
  #       [numeric]
  # Output : prédiction réalisée par le modèle "model" sur la base de test
  #         [numeric] (vector)
  variables=paste(vars,collapse="+")    
  formule=as.formula(paste("~",variables))
  sparse_matrix <- sparse.model.matrix(formule, data=test , weight = test$ClaimNb)
  return(predict(model,sparse_matrix)+ecret)
}
