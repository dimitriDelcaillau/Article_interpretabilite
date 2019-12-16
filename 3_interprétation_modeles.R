############################################################################################
################### Interprétation des modèles de tarification##############################
############################################################################################

# Dans ce code, on propose plusieurs fonctions pour mettre en place des méthodes
# d'interprétations, de différents packages, en les adaptant à la base de tarification 
# que l'on étudie.

target_freq = function(a){a$ClaimNb}
########################################################################################
############################# 1 - PDP ##################################################
########################################################################################

# A) PDP utilisant 1 variable ---------

pred_func = function(object,newdata) {predict(object,newdata,type="response")}
pdp_func = function(model, ind_var){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  # Output : fonction de dépendance partielle associé à la variable d'indice ind_var
  # [data.frame]
  k=ind_var
  return(model %>% partial(pred.var = var_freq[k]))
}

pdp_all_var = function(model = fit_freq_complet){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  # Output: listes des fonctions de dépendance partielle de chaque variable explicative
  # [list]
  lapply(1:length(var_freq),pdp_func,model=model)
}
pdp_plot = function(model, ind_var,type = "other"){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  #       - type : character indiquant si on veut le type "response" de la prédiction GLM ou non
  #         [type] "response" ou "other"
  # Output : graphique de dépendance partielle (PDP) associé à la variable d'indice ind_var
  # [ggplot]
  pdp = pdp_func(model,ind_var)
  pdp$x = pdp[,1]
  pdp <- pdp %>%
    mutate(x = fct_reorder(x, yhat))
  # gg=ggplot(pdp,aes_string(x=names(pdp)[1],y="yhat"))+geom_point()+ggtitle("PDP")
  if (ind_var ==6){
    lab = as.character(pdp$x)
    lab[lab=="R23_31_72_54"] = "R_"
    pdp$x = as.factor(lab)
    pdp <- pdp %>%
      mutate(x = fct_reorder(x, yhat))
  }
  ifelse(type=="response", pdp$yhat <- exp(pdp$yhat), pdp$yhat <- pdp$yhat)
  gg=ggplot(pdp,aes(x=x,y=yhat, fill=x))+geom_col()+ggtitle("PDP", var_freq[ind_var])+theme_gdocs()+scale_color_gdocs()+guides(fill=FALSE)
  return(gg)
}

pdp_smooth_plot = function(model, ind_var){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  # Output : graphique de dépendance partielle (PDP) associé à la variable d'indice ind_var, lissée
  # [ggplot]
  # utile que si il y a des variables numériques
  model %>%  
    partial(pred.var = var_freq[ind_var]) %>%
    autoplot(smooth = TRUE, ylab = paste("f(",var_freq[ind_var],")",sep="")) +
    theme_light() +
    ggtitle(paste("PDP lissée",var_freq[ind_var]))
}



pdp_plot2 = function(model, ind_var){
  # utile que si il y a des variables numériques
  g=model %>%  
    partial(pred.var = var_freq[ind_var]) %>%
    autoplot(smooth = TRUE, ylab = paste("f(",var_freq[ind_var],")",sep="")) +
    theme_light() +
    ggtitle(paste("PDP",var_freq[ind_var]))
  g+geom_line(data=g$data, aes_string(x=var_freq[ind_var], y="yhat", group=1))+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
    theme(axis.text.x = element_text(size=14))
}

# Avec le package pdp ------- 
func_pdp = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - variable : nom de la variable étudiée
  #       - base_app : base d'apprentissage
  #       - n_sample : nombre de points utilisés pour le calcul : très utile pour réduire temps de calcul avec des variables numériques
  # Output : fonction de dépendance partielle associé à la variable
  # [data.frame]
  # k=ind_var
  set.seed(seed)
  app_sample = base_app[sample(1:nrow(base_app),n_sample),]
  return(model %>% partial(pred.var = variable, train = app_sample))
}

func_pdp_plot_cat = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019){
  # PDP pour les variables catégorielles sans ordre
  partialD = func_pdp(model, variable, base_app, n_sample, seed)
  partialD$xx = partialD[,1]
  ggplot(partialD%>%mutate(xx = fct_reorder(xx, yhat)), aes(x = xx, y = yhat))+geom_col(fill = "cornflowerblue", col='black')+xlab(variable)
}

func_pdp_plot_cat_ordered = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019){
  # PDP pour les variables catégorielles mais qui ont un ordre (ex : CarAgeGLM, DriverAgeGLM....)
  partialD = func_pdp(model, variable, base_app, n_sample, seed)
  partialD[,1] = as.integer(as.character(partialD[,1]))
  ggplot(partialD, aes_string(x = variable, y = 'yhat'))+geom_col(fill = "cornflowerblue", col='black')
}

func_pdp_plot_num = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019, x_lim = NULL){
  # x_lim : vecteur pour donner les limites de l'abscisse (utile lorsque peu de points représentés dans une zone : ex CarAge)
  # si x_lim = NULL ==> graphique entier affiché
  partialD = func_pdp(model, variable, base_app, n_sample, seed)
  g = ggplot(partialD, aes_string(x = variable, y = 'yhat'))+geom_line(col = "cornflowerblue", size = 2)+geom_point()
  if (!is.null(x_lim)){
    return(g+xlim(x_lim))
  }
  return(g)
}

# B) PDP utilisant 2 variables -----------------
pdp_func_2var = function(model, k = 1, j = 2){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - k : indice de la variable 1 dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  #       - j : indice de la variable 2 dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  # Output : fonction de dépendance partielle associé aux variables d'indices k et j 
  # [data.frame]
  pd <- partial(model, pred.var = c(var_freq[k],var_freq[j]))
  return(pd)
}
pdp_2var_plot = function(model, k, j){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - k : indice de la variable 1 dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  #       - j : indice de la variable 2 dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  # Output : graphiques de fonction de dépendance partielle associé aux variables d'indices k et j 
  # [data.frame]
 
  return(plotPartial(pdp_func_2var(model, k, j)))
}

############################################################################################
################################ 2 - Coefficients du GLM ###################################
############################################################################################

# Les coefficients du GLM permettent une analyse intrinsèque (modèle modulaire et simulable)

coef_var_glm_plot = function(model, variable){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) 
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  # Output : graphique correspondant aux coefficients dans le GLM des modalités de la variable
  # [ggplot]
  coef = model$coefficients[-1]
  y = c(0,coef[stringr::str_detect(names(coef),variable)])
  x = levels(model$data[,variable])
  dff=data.frame(x=x, y=y)
  # dff <- dff %>%
  #   mutate(x = fct_reorder(x, y))
  pp = ggplot(dff,aes(x=x,y=y))+geom_col(fill="cornflowerblue")+xlab(variable)+ggtitle("Coefficients du GLM")+scale_color_gdocs()
  return(pp)
}

##########################################################################################
############################### 3 - Courbes ICES #########################################
##########################################################################################
ICE_func = function(model, ind_var = 1, dat_app, x_interest = 1, n_sample = 30, seed = 2019){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) 
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  #       - dat_app : base de données (d'apprentissage ou non)
  #         [data.frame]
  #       - x_interest : numéro de la ligne dans la base dat_app
  #         [integer]
  #       - n_sample : nombre de points utilisés sur la courbe ICE (séparation en quantiles) 
  #                  : utile que si variable numérique
  #         [integer]
  # Output : data.frame avec l'abscisse et l'ordonnée de la courbe ICE
  # [data.frame]
  if (class(dat_app[,var_freq[ind_var]])%in%c("factor","character")){
    lev = levels(dat_app[,var_freq[ind_var]])
  }
  else{
    set.seed(seed)
    lev = quantile(dat_app[,var_freq[ind_var]], probs = seq(0,1,length.out = n_sample))
    # lev = unique(lev)[-length(lev)]
  }
  ICE = NULL
  for (k in 1:length(lev)){
    xx = dat_app[x_interest,]
    xx[,var_freq[ind_var]] = lev[k]      
    ICE[k] = predict(model,xx)
  }
  
  return(data.frame(x = lev, y = ICE))
}
ICE_courbes_freMTPL2 = function(nb_curves = 10 , model, ind_var = 1, dat_app, n_sample = 10, seed = 2019, alp = 0.2){
  # Input : 
  #       - nb_curves : nombre de courbes ICE tracées
  #         [integer]
  #       - model : modèle ajusté
  #         ["glm"] (glm) 
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  #       - dat_app : base de données (d'apprentissage ou non)
  #         [data.frame]
  #       - n_sample : nombre de points utilisés sur la courbe ICE (séparation en quantiles) 
  #                  : utile que si variable numérique
  #         [integer]
  #       - seed : graine pour la reproductibilité
  #         [integer]
  #       - se : paramètre graphique pour afficher l'enveloppe (écart-type) de la courbe
  #         [bool]
  #      - alp : paramètre graphique alpha de ggplot pour la transparence des courbes
  #         [numeric] (entre 0 et 1)
  # Output : nb_curves courbes ICE , : adapté uniquement pour la base freMTPL2
  # [ggplot]
  set.seed(seed)
  x_interests = sample(1:nrow(dat_app), nb_curves)
  temp = lapply(x_interests,ICE_func,model=model, ind_var = ind_var, dat_app = dat_app, n_sample = ,n_sample, seed = seed)
  mean = do.call(rbind, temp)
  df = as.data.frame(temp)
  xx = df$x
  dff = df[,!stringr::str_detect(colnames(df),"x")]
  colnames(dff) = NULL
  melt = melt(as.matrix(dff))
  melt$x = xx
  # +geom_smooth(method="auto",
  #              data=melt, aes(x = x, y = value, group = Var2), 
  #              alpha=alp, col='grey', se=F)
  # On charge les fonction de dépendance partielle (PDP) sauvegardés au préalable
  # file = get(load(paste("C:\\Users\\dimitri.delcaillau\\Desktop\\Dimitri Delcaillau\\Images\\freMTPL2_memoire\\PDP_data\\",var_freq[ind_var],".RData",sep="")))
  ggplot()+
    geom_point(data=melt, aes(x = x, y = value, group = Var2), alpha = alp, size = 2, col='red')+
    geom_line(data=melt, aes(x = x, y = value, group = Var2), alpha = alp, size = 0.5, col='red')+
    # geom_line(data=file,aes_string(x=var_freq[ind_var], y='yhat'), size = 1, color='black')+geom_point(data=file,aes_string(x=var_freq[ind_var], y='yhat'), color="black")+
    ggtitle("PDP (noir) et courbes ICE (rouge)", var_freq[ind_var])+xlab(var_freq[ind_var])
}

ICE_pdp_courbes = function(nb_curves = 10 , model, ind_var = 1, dat_app, n_sample = 10, seed = 2019, alp = 0.2){
  # Input : 
  #       - nb_curves : nombre de courbes ICE tracées
  #         [integer]
  #       - model : modèle ajusté
  #         ["glm"] (glm) 
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  #       - dat_app : base de données (d'apprentissage ou non)
  #         [data.frame]
  #       - n_sample : nombre de points utilisés sur la courbe ICE (séparation en quantiles) 
  #                  : utile que si variable numérique
  #         [integer]
  #       - seed : graine pour la reproductibilité
  #         [integer]
  #       - se : paramètre graphique pour afficher l'enveloppe (écart-type) de la courbe
  #         [bool]
  #      - alp : paramètre graphique alpha de ggplot pour la transparence des courbes
  #         [numeric] (entre 0 et 1)
  # Output : nb_curves courbes ICE 
  # [ggplot]
  set.seed(seed)
  x_interests = sample(1:nrow(dat_app), nb_curves)
  temp = lapply(x_interests,ICE_func,model=model, ind_var = ind_var, dat_app = dat_app, n_sample = ,n_sample, seed = seed)
  mean = do.call(rbind, temp)
  df = as.data.frame(temp)
  xx = df$x
  dff = df[,!stringr::str_detect(colnames(df),"x")]
  colnames(dff) = NULL
  melt = melt(as.matrix(dff))
  melt$x = xx
  pd = pdp_func(model, ind_var)
  df_pd = data.frame(x = pd[,1], y = pd[,2])
  ggplot()+geom_smooth(method="auto",
                       data=melt, aes(x = x, y = value, group = Var2), 
                       alpha=alp, col='grey', se=F)+
    geom_point(data=melt, aes(x = x, y = value, group = Var2), alpha = alp, size = 2, col='red')+
    geom_line(data=melt, aes(x = x, y = value, group = Var2), alpha = alp, size = 0.5, col='red')+
    theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
    theme(axis.text.x = element_text(size=14))+
    geom_smooth(method="auto",data=mean,aes(x=x, y=y),se =F)+geom_point(data=df_pd,aes(x=x, y=y), size=3)+geom_line(data=df_pd,aes(x=x, y=y,group=1), size=1.5)
  
}

ICE_pdp_courbes_all_var = function(nb_curves = 10 , model, dat_app, n_sample = 10, seed = 2019, alp = 0.2){
  # Input : 
  #       - nb_curves : nombre de courbes ICE tracées
  #         [integer]
  #       - model : modèle ajusté
  #         ["glm"] (glm) 
  #       - dat_app : base de données (d'apprentissage ou non)
  #         [data.frame]
  #       - n_sample : nombre de points utilisés sur la courbe ICE (séparation en quantiles) 
  #                  : utile que si variable numérique
  #         [integer]
  #       - seed : graine pour la reproductibilité
  #         [integer]
  #       - se : paramètre graphique pour afficher l'enveloppe (écart-type) de la courbe
  #         [bool]
  #      - alp : paramètre graphique alpha de ggplot pour la transparence des courbes
  #         [numeric] (entre 0 et 1)
  # Output : nb_curves courbes ICE pour chaque variable de var_freq
  # [ggplot]
  ll = lapply(1:7, ICE_pdp_courbes, nb_curves = 100 , model = model, dat_app = dat_app, n_sample = n_sample,alp = alp, seed = seed)
  grid.arrange(ll[[1]], ll[[2]],ll[[3]], ll[[4]],ll[[5]], ll[[6]],ll[[7]])
}


# a7 = ggplotly(ICE_col(5,3,nb_curves = 500))


##########################################################################################
############################### 4 - ALE ##################################################
##########################################################################################


# !!!! Attention avec deux variables, le temps de calcul est très important !!!!
ALE_plot_2var = function(model, dat_app, var1, var2){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour l'apprentissage du modèle
  #         [data.frame]
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  # Output : graphique ALE associé à la variable d'indice ind_var
  # [NULL]
  pred_func=function(X.model,newdata){
    return(predict(X.model,newdata))
  }
  J = which(colnames(dat_app)==var1)
  J2 = which(colnames(dat_app)==var2)
  if(class(dat_app[,var1])%in%c("integer","numeric")& class(dat_app[,var2])%in%c("integer","numeric")){
    ALE = ALEPlot(X = dat_app, X.model = ,model,pred.fun = pred_func,J=c(J,J2))
    return(ALE)
    }

  if(class(dat_app[,var1])=="factor"&class(dat_app[,var2])=="factor"){
    return("erreur : 2 factors")
  }
  if(class(dat_app[,var2])=="factor"){
    J2 = which(colnames(dat_app)==var1)
    J = which(colnames(dat_app)==var2)
  }
  ALE = ALEPlot(X = dat_app, X.model = ,model,pred.fun = pred_func,J=c(J,J2))
  x1 = rep(ALE$x.values[[1]], dim(ALE$f.values)[2])
  y = as.vector(ALE$f.values)
  x2 = as.vector(t(matrix(rep(ALE$x.values[[2]],dim(ALE$f.values)[1]), ncol = dim(ALE$f.values)[1])))
  df = data.frame(x1 = x1, x2 = x2, y = y)
  g = ggplot(head(df, length(df$x1)-dim(ALE$f.values)[1]), aes(x = x2, y = y))+geom_line()+geom_smooth()+facet_wrap(~x1)
  g+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
    theme(axis.text.x = element_text(size=14))
}

ALE_plot2 = function(model, dat_app, variable){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour l'apprentissage du modèle
  #         [data.frame]
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  # Output : graphique ALE associé à la variable d'indice ind_var
  # [NULL]
  pred_func=function(X.model,newdata){
    return(predict(X.model,newdata))
  }
  J = which(colnames(dat_app)==variable)
  aa=ALEPlot(dat_app,model,pred_func,J=J, NA.plot = F)
  dff = data.frame(x=aa$x.values, y=aa$f.values)
  ggplot(dff, aes(x=x, y=y))+geom_col()+
    theme(axis.text.y = element_text(size=18))+
    theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
    theme(axis.text.x = element_text(size=14))+ggtitle(paste("ALE", variable))
  
}
ALE_plot2_cont = function(model, dat_app, variable){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour l'apprentissage du modèle
  #         [data.frame]
  #       - ind_var : indice de la variable dans le vecteur des variables explicatives var_freq 
  #         [integer] (entre 1 et length(var_freq))
  # Output : graphique ALE associé à la variable d'indice ind_var
  # [NULL]
  pred_func=function(X.model,newdata){
    return(predict(X.model,newdata))
  }
  J = which(colnames(dat_app)==variable)
  aa=ALEPlot(dat_app,model,pred_func,J=J, NA.plot = F)
  dff = data.frame(x=aa$x.values, y=aa$f.values)
  ggplot(dff, aes(x=x, y=y))+geom_line()+
    theme(axis.text.y = element_text(size=18))+
    theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
    theme(axis.text.x = element_text(size=14))+ggtitle(paste("ALE", variable))
  
}

#########################################################################################
############################5 - Importance des variables #################################
##########################################################################################

imp_var = function(model){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  # Output : vecteur d'importance des variables pour chaque modalité des variables
  # [numeric] (vecteur)
  if(class(model)[1]=="glm"){
    imp = caret::varImp(model)
    ord = order(imp$Overall)
    res = imp$Overall[ord]
    names(res) = rownames(imp)[ord]
  }
  else{
    imp = caret::varImp(model)
    ord = order(imp$importance)
    res = imp$importance$Overall[ord]
    names(res) = rownames(imp$importance)[ord]
  }
  
  return(res)
}
imp_var_plot = function(model){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  # Output : graphique d'importance des variables pour chaque modalité des variables
  # [NULL] (vecteur)
  aa = imp_var(model)
  df=data.frame(var = names(aa), y = aa) 
  df <- df %>%
    mutate(var = fct_reorder(var, y))
  ggplot(df, aes(x=var, y=y))+geom_col()+ coord_flip()
}

imp_var_dalex = function(model, dat_app, type="frequence", n_sample = 100000 ,seed = 2019){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - type : indique si il s'agit du modèle fréquence ou coût
  #         [character] ("frequence" ou "cout")
  #       - n_sample : nombre d'échantillons utilisés
  #         [integer]
  # Output : vecteur d'importance des variables (uniquement 1 valeur par variable, pas par modalité)
  # [numeric]
  if(type=="frequence"){
    yy = target_freq(dat_app)
  }
  else{
    yy = target_cout(dat_app)
  }  
  set.seed(seed)
  explainer = DALEX::explain(model, data = dat_app[,var_freq], y = yy)
  variable_importance(explainer, n_sample = n_sample)
}
imp_var_dalex_2 = function(model, dat_app, type="frequence", n_sample = 100000, seed = 2019){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - type : indique si il s'agit du modèle fréquence ou coût
  #         [character] ("frequence" ou "cout")
  #       - n_sample : nombre d'échantillons utilisés
  #         [integer]
  # Output : graphique d'importance des variables (uniquement 1 valeur par variable, pas par modalité)
  # [numeric]
  
  temp = imp_var_dalex(model, dat_app, type="frequence", n_sample = n_sample, seed = seed)
  temp = temp[-nrow(temp),]
  temp <- temp %>%
    mutate(imp = abs(dropout_loss-temp$dropout_loss[1]))
  t = temp[-1,]
  t <- t %>%
    mutate(variable = fct_reorder(variable, imp))
  return(t)
  }

imp_var_dalex_plot = function(model, dat_app, type="frequence", n_sample = 100000, seed = 2019){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - type : indique si il s'agit du modèle fréquence ou coût
  #         [character] ("frequence" ou "cout")
  #       - n_sample : nombre d'échantillons utilisés
  #         [integer]
  # Output : graphique d'importance des variables (uniquement 1 valeur par variable, pas par modalité)
  # [numeric]
  plot(imp_var_dalex(model, dat_app, type="frequence", n_sample = n_sample, seed = seed))
}
imp_var_dalex_plot_2 = function(model, dat_app, type="frequence", n_sample = 100000, seed = 2019){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - type : indique si il s'agit du modèle fréquence ou coût
  #         [character] ("frequence" ou "cout")
  #       - n_sample : nombre d'échantillons utilisés
  #         [integer]
  # Output : graphique d'importance des variables (uniquement 1 valeur par variable, pas par modalité)
  # [numeric]
  
  temp = imp_var_dalex(model, dat_app, type="frequence", n_sample = n_sample, seed = seed)
  temp = temp[-nrow(temp),]
  temp <- temp %>%
            mutate(imp = abs(dropout_loss-temp$dropout_loss[1]))
  t = temp[-1,]
  t <- t %>%
         mutate(variable = fct_reorder(variable, imp))
  ggplot(t, aes(x=variable, y =imp))+geom_col()+coord_flip()
}

imp_var_dalex_stabilite = function(model, dat_app, type="frequence", n_sample = 10000, Nsimu = 30){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage 
  #         [data.frame]
  #       - type : indique si il s'agit du modèle fréquence ou coût
  #         [character] ("frequence" ou "cout")
  #       - n_sample : nombre d'échantillons utilisés
  #         [integer]
  #       - Nsimu : nombre de simulations
  #         [integer]
  # Output : boxplot d'analyse d'importance des variables 
  # [plot]
  if(type=="frequence"){
    yy = target_freq(dat_app)
  }
  else{
    yy = target_cout(dat_app)
  }  
  explainer = DALEX::explain(model, data = dat_app[,var_freq], y = yy)
  varImp = variable_importance(explainer, n_sample = n_sample)
  vec=varImp[-c(1,9),-3] 
  vec$ord = (vec$dropout_loss)
  vec$dropout_loss=NULL
  vec$variable = as.character(vec$variable)
  for (k in 2:Nsimu){
    varImp = variable_importance(explainer,n_sample = n_sample)
    temp=varImp[-c(1,9),-3]
    temp$ord = (temp$dropout_loss)
    temp$dropout_loss = NULL
    vec = rbind(vec, temp)
    vec$variable = as.character(vec$variable)
  }
  vec$variable= factor(vec$variable)
  df_sorted <- vec %>%
    mutate(variable = fct_reorder(variable, ord))
  avg <- df_sorted %>%
    summarize(avg = mean(ord, na.rm = T)) %>%
    pull(avg)
  g <- ggplot(df_sorted, aes(variable, ord, color=variable)) + geom_boxplot()+
    coord_flip() +
    labs(x = NULL, y = "Score d'importance") +theme_gdocs()+scale_color_gdocs()+geom_point(size=3,alpha=0.05)
    # geom_hline(aes(yintercept = avg), color = "black", size = 1)+guides(fill=F)
  return(g)
}

imp_var_dalex_stabilite_2 = function(model, dat_app, type="frequence", n_sample = 10000, Nsimu = 30){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage 
  #         [data.frame]
  #       - type : indique si il s'agit du modèle fréquence ou coût
  #         [character] ("frequence" ou "cout")
  #       - n_sample : nombre d'échantillons utilisés
  #         [integer]
  #       - Nsimu : nombre de simulations
  #         [integer]
  # Output : boxplot d'analyse d'importance des variables 
  # [plot]
  
  # explainer = DALEX::explain(model, data = dat_app[,var_freq], y = yy)
  varImp = imp_var_dalex_2(model, dat_app, type, n_sample, seed = sample(1:100000,1))
  vec=varImp[,-3] 
  vec$ord = (vec$imp)
  vec$dropout_loss=NULL
  vec$variable = as.character(vec$variable)
  for (k in 2:Nsimu){
    varImp = imp_var_dalex_2(model, dat_app, type, n_sample, seed = sample(1:100000,1))
    temp=varImp[,-3]
    temp$ord = (temp$imp)
    temp$dropout_loss = NULL
    vec = rbind(vec, temp)
    vec$variable = as.character(vec$variable)
  }
  vec$variable= factor(vec$variable)
  df_sorted <- vec %>%
    mutate(variable = fct_reorder(variable, ord))
  avg <- df_sorted %>%
    summarize(avg = mean(ord, na.rm = T)) %>%
    pull(avg)
  g <- ggplot(df_sorted, aes(variable, ord, color=variable)) + geom_boxplot()+
    coord_flip() +
    labs(x = NULL, y = "Score d'importance") +scale_color_gdocs()+theme_minimal()+geom_point(size=3,alpha=0.05)
  # geom_hline(aes(yintercept = avg), color = "black", size = 1)+guides(fill=F)
  return(g)
}

# !!!! Cette fonction est généralement très coûteuse en temps de calcul !!!!!
imp_var_iml = function(model, dat_app, type="frequence", n_rep = 5, compare = "ratio"){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base ayant servi à l'apprentissage du modèle
  #         [data.frame]
  #       - type : indique si il s'agit du modèle fréquence ou sévérité qui est modélisé
  #         [character] ("frequence" ou "cout")
  #       - n_rep : nombre de fois l'opération de mélange des variables est répétée
  #         [integer]
  #       - compare : calcul de l'importance basé sur "ratio" ou "difference"
  #         [character(1)] ("ratio" ou "difference")
  # Output : vecteur d'importance des variables pour chaque modalité des variables
  # [numeric] (vecteur)
  y = target_freq(dat_app)
  X = dat_app[,var_freq]  
  if(type=="cout"){
    dat_app = data_sinistres(dat_app)
    y = target_cout(dat_app)
    X = dat_app[,var_freq]                         
  }
  predictor = Predictor$new(model, data = X, y = y)
  FeatureImp$new(predictor, "mse", compare = compare, n.repetitions = n_rep)
}
imp_var_iml_plot = function(model, dat_app, type="frequence", n_rep = 5, compare = "ratio"){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base ayant servi à l'apprentissage du modèle
  #         [data.frame]
  #       - type : indique si il s'agit du modèle fréquence ou sévérité qui est modélisé
  #         [character] ("frequence" ou "cout")
  #       - n_rep : nombre de fois l'opération de mélange des variables est répétée
  #         [integer]
  #       - compare : calcul de l'importance basé sur "ratio" ou "difference"
  #         [character(1)] ("ratio" ou "difference")
  # Output : plot d'importance des variables pour chaque modalité des variables
  # [plot] 
  temp = imp_var_iml(model = model, dat_app = dat_app, type = type, n_rep = n_rep, compare = compare)
  temp$plot()
}


##########################################################################################
###################################### 6 - LIME ##########################################
##########################################################################################

# Utilisant le package 'lime'
lime_func = function(model, dat_app, ind_obs = 1, n_permutations = 5000, dist_fun = "gower", kernel_width = 1, n_features = 4, seed = 2019){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - ind_obs : indice de l'observation que l'on cherche à expliquer avec LIME
  #         [integer] (entre 1 et 7)
  #       - n_features : nombre de variables utilisées pour ajuster le surrogate
  #         [integer] (entre 1 et 7)
  #       - n_permutations : nombre de permutations utilisées pour ajuster le surrogate
  #         [integer]
  #       - dist_fun : fonction utiliser pour mesurer la distance d'une observation 
  #         [character] (par défaut : gower)
  #       - kernel_width : largeur du noyau utilisé
  #         [numeric >0] (inutile lorsque dist_fun=="gower")
  # Output : explication fournie l'algorithme LIME (modèle linéaire LASSO)
  # [data.frame] (output de la fonction 'lime::explain')
  
  # Cette fonction ne fonctionne que pour les modèles issus de : 
  # train from caret 
  # WrappedModel from mlr
  # xgb.Booster from xgboost
  # H2OModel from h2o
  # keras.engine.training.Model from keras
  # lda from MASS (used for low-dependency examples)
  
  explanation_lime = lime(dat_app[,var_freq], model, dist_fun = dist_fun, kernel_width = kernel_width)
  res = lime::explain(dat_app[ind_obs,var_freq], n_permutations=n_permutations, explanation_lime, n_features = n_features)
  return(res)
}
lime_plot = function(model, dat_app, ind_obs = 1, n_features = 4, n_permutations = 5000, dist_fun = "gower", kernel_width = 1 ,type = "features"){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - ind_obs : indice de l'observation que l'on cherche à expliquer avec LIME
  #         [integer] (entre 1 et 7)
  #       - n_features : nombre de variables utilisées pour ajuster le surrogate
  #         [integer] (entre 1 et 7)
  #       - n_permutations : nombre de permutations utilisées pour ajuster le surrogate
  #         [integer]
  #       - dist_fun : fonction utiliser pour mesurer la distance d'une observation 
  #         [character] (par défaut : gower)
  #       - kernel_width : largeur du noyau utilisé
  #         [numeric >0] (inutile lorsque dist_fun=="gower")
  #       - type : type de graphique renvoyé par la fonction
  #         [character] ("features" ou "explanations")
  # Output : plot de l'explication fournie l'algorithme LIME 
  # [plot] 
  plot_features(lime_func(model, dat_app, ind_obs, n_permutations , 
                                     dist_fun, kernel_width, n_features))
}
lime_stabilite = function(model, dat_app, ind_obs = 1, n_permutations = 5000, dist_fun = "gower", kernel_width = 1, n_features = 4, Nsimu = 100){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - ind_obs : indice de l'observation que l'on cherche à expliquer avec LIME
  #         [integer] (entre 1 et 7)
  #       - n_features : nombre de variables utilisées pour ajuster le surrogate
  #         [integer] (entre 1 et 7)
  #       - n_permutations : nombre de permutations utilisées pour ajuster le surrogate
  #         [integer]
  #       - dist_fun : fonction utiliser pour mesurer la distance d'une observation 
  #         [character] (par défaut : gower)
  #       - kernel_width : largeur du noyau utilisé
  #         [numeric >0] (inutile lorsque dist_fun=="gower")
  #       - Nsimu : nombre de simulations
  #         [integer]
  # Output : boxplot des explications fournies par l'algorithme LIME (modèle linéaire LASSO)
  # [plot] 
  dat = NULL
  for (k in 1:Nsimu){
    ex = lime_func(model, dat_app, ind_obs, n_permutations , 
                   dist_fun, kernel_width, n_features)
    dat = c(dat,ex$feature)
  }
  # barplot(summary(factor(dat)),col="red")
  df = data.frame(x = dat)
  df$x = fct_infreq(df$x)
  ggplot(df, aes(x, fill=x))+geom_bar()+scale_color_gdocs()+xlab(NULL)+ylab("% utilisation par Lime")+ggtitle("Variables utilisées par Lime selon les simulations",paste("Policy_ID :",dat_app$PolicyID[ind_obs]))
}

lime_stabilite_2 = function(model, dat_app, ind_obs = 1, n_permutations = 5000, dist_fun = "gower", kernel_width = 1, n_features = 4, Nsimu = 100){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - ind_obs : indice de l'observation que l'on cherche à expliquer avec LIME
  #         [integer] (entre 1 et 7)
  #       - n_features : nombre de variables utilisées pour ajuster le surrogate
  #         [integer] (entre 1 et 7)
  #       - n_permutations : nombre de permutations utilisées pour ajuster le surrogate
  #         [integer]
  #       - dist_fun : fonction utiliser pour mesurer la distance d'une observation 
  #         [character] (par défaut : gower)
  #       - kernel_width : largeur du noyau utilisé
  #         [numeric >0] (inutile lorsque dist_fun=="gower")
  #       - Nsimu : nombre de simulations
  #         [integer]
  # Output : boxplot des explications fournies par l'algorithme LIME (modèle linéaire LASSO)
  # [plot] 
  dat = NULL
  for (k in 1:Nsimu){
    ex = lime_func(model, dat_app, ind_obs, n_permutations , 
                   dist_fun, kernel_width, n_features)
    dat = rbind(dat,data.frame(feature = ex$feature, poids = ex$feature_weight))
  }
  # barplot(summary(factor(dat)),col="red")
  # df = data.frame(x = dat)
  dat = dat %>%
    mutate(feature = fct_reorder(feature, poids))
  # avg <- dat %>%
  #   summarize(avg = mean(poids, na.rm = T)) %>%
  # pull(avg)
  # g <- ggplot(df_sorted, aes(variable, ord, color=variable)) + geom_boxplot()+
  #   coord_flip() +
  #   labs(x = NULL, y = "Contribution") +theme_gdocs()+scale_color_gdocs()+geom_point(size=3,alpha=0.15)
  
  g <- ggplot(dat, aes(feature, poids, color=feature)) + geom_boxplot()+
    coord_flip() +
    labs(x = NULL, y = "Poids attribué par Lime") +theme_bw()+scale_fill_stata()+geom_point(size=3,alpha=0.15)+ggtitle("Stabilité de l'interprétation fournie par Lime",paste("Policy_ID :",dat_app$PolicyID[ind_obs]))
  return(g)
}

# Utilisant le package 'iml'
lime_iml_func = function(model, dat_app, type="frequence", ind_obs = 1, dist.fun = "gower", kernel.width=NULL, n_features=3){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - type : indique si le modèle ajuste la fréquence ou le coût
  #         [character] ("frequence" ou "cout")
  #       - ind_obs : indice de la variable que l'on cherche à expliquer avec LIME
  #         [integer] (entre 1 et 7)
  #       - dist.fun : fonction utiliser pour mesurer la distance d'une observation 
  #         [character] (par défaut : gower)
  #       - kernel.width : largeur du noyau utilisé
  #         [numeric >0] (inutile lorsque dist_fun=="gower")
  #       - n_features : nombre de variables utilisées pour ajuster le surrogate
  #         [integer] (entre 1 et 7)
  # Output : explication fournie l'algorithme LIME (modèle linéaire LASSO)
  # [data.frame] (output de la fonction 'lime::LocalModel')
   y =target_freq(dat_app)
  if(type=="cout"){
    y = target_cout(dat_app)
  }
  X = dat_app[,var_freq]
  predictor = Predictor$new(model, data = X)
  LocalModel$new(predictor, x.interest = X[ind_obs,], dist.fun = dist.fun,  
                        kernel.width = NULL, k = n_features)
}
lime_iml_plot = function(model, dat_app, type="frequence", ind_obs = 1, dist.fun = "gower", kernel.width=NULL, n_features=3){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - type : indique si le modèle ajuste la fréquence ou le coût
  #         [character] ("frequence" ou "cout")
  #       - ind_obs : indice de la variable que l'on cherche à expliquer avec LIME
  #         [integer] (entre 1 et 7)
  #       - dist.fun : fonction utiliser pour mesurer la distance d'une observation 
  #         [character] (par défaut : gower)
  #       - kernel.width : largeur du noyau utilisé
  #         [numeric >0] (inutile lorsque dist_fun=="gower")
  #       - n_features : nombre de variables utilisées pour ajuster le surrogate
  #         [integer] (entre 1 et 7)
  # Output : explication fournie l'algorithme LIME (modèle linéaire LASSO)
  # [plot] 
  temp = lime_iml_func(model, dat_app, type, ind_obs, dist.fun, kernel.width, n_features)
  suppressWarnings(temp$plot())
}

##########################################################################################
###################################### 7 - SHAP ##########################################
##########################################################################################

shap_iml_func = function(model, dat_app, type="frequence", ind_obs = 1, sample_size = 1000){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - type : indique si le modèle ajuste la fréquence ou le coût
  #         [character] ("frequence" ou "cout")
  #       - ind_obs : indice de la variable que l'on cherche à expliquer avec SHAP
  #         [integer] (entre 1 et 7)
  #       - sample_size : taille de l'échantillon utilisé pour calculer les valeurs de shapley
  #         [integer] (entre 1 et 7)
  # Output : explication fournie l'algorithme SHAP 
  # [data.frame] 
  y =target_freq(dat_app)
  if(type=="cout"){
    y = target_cout(dat_app)
  }
  X = dat_app[,var_freq]
  predictor = Predictor$new(model, data = X)
  Shapley$new(predictor, x.interest = X[ind_obs,],sample.size = sample_size)
}
shap_iml_plot = function(model, dat_app, type="frequence", ind_obs = 1, sample_size = 1000){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - type : indique si le modèle ajuste la fréquence ou le coût
  #         [character] ("frequence" ou "cout")
  #       - ind_obs : indice de la variable que l'on cherche à expliquer avec SHAP
  #         [integer] (entre 1 et 7)
  #       - sample_size : taille de l'échantillon utilisé pour calculer les valeurs de shapley
  #         [integer] (entre 1 et 7)
  # Output : graphique de l'explication fournie l'algorithme SHAP 
  # [plot] 
  temp = shap_iml_func(model, dat_app, type, ind_obs, sample_size)
  temp$plot()
}
shap_iml_stabilite = function(model, dat_app, type="frequence", ind_obs = 1, sample_size=100, Nsimu=20){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - dat_app : base d'apprentissage utilisée pour entraîner le modèle
  #         [data.frame]
  #       - type : indique si le modèle ajuste la fréquence ou le coût
  #         [character] ("frequence" ou "cout")
  #       - ind_obs : indice de la variable que l'on cherche à expliquer avec SHAP
  #         [integer] (entre 1 et 7)
  #       - sample_size : taille de l'échantillon utilisé pour calculer les valeurs de shapley
  #         [integer] (entre 1 et 7)
  #       - Nsimu : nombre de simulations réalisées de l'algorithme LIME
  #         [integer]
  # Output : graphique sous forme de boîte à moustaches des poids renvoyés par Lime pour chaque variable
  # [plot]
  # [plot]
  y =target_freq(dat_app)
  if(type=="cout"){
    y = target_cout(dat_app)
  }
  X = dat_app[,var_freq]
  predictor = Predictor$new(model, data = X)
  shapley = Shapley$new(predictor, x.interest = X[ind_obs,],sample.size = sample_size)
  # shapley = Shapley$new(predictor, x.interest = X[ind_interest,],sample.size = n_sample)
  vec = shapley$results[,1:2] 
  vec$ord = (shapley$results$phi)
  for (k in 2:Nsimu){
    shapley = Shapley$new(predictor, x.interest = X[ind_obs,],sample.size = sample_size)
    temp = shapley$results[,1:2]
    temp$ord = (shapley$results$phi)
    vec = rbind(vec, temp)
  }
  vec$variable= factor(vec$feature)
  vec$feature=NULL
  df_sorted <- vec %>%
    mutate(variable = fct_reorder(variable, ord))
  avg <- df_sorted %>%
    summarize(avg = mean(ord, na.rm = T)) %>%
    pull(avg)
  g <- ggplot(df_sorted, aes(variable, ord, color=variable)) + geom_boxplot()+
    coord_flip() +
    labs(x = NULL, y = "Contribution") +theme_gdocs()+scale_color_gdocs()+geom_point(size=3,alpha=0.15)
    # geom_hline(aes(yintercept = avg), color = "black", size = 1) 
  return(g)
  # return(boxplot(ord~feature,vec, xlab = "Feature", ylab = "Contribution", las = 1, horiz =T))
}

##########################################################################################
#################################### 8 - BreakDown #######################################
##########################################################################################

breakdown_func = function(model, dat_app, type="frequence", x_interest = 1, direction = "up"){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - type : indique si il s'agit du modèle fréquence ou coût
  #         [character] ("frequence" ou "cout")
  #       - x_interest : ligne de l'instance d'intérêt dans le data.frame dat_app
  #         [integer]
  #       - direction : direction utilisée dans Breakdown "up" ou "down"
  #         [character] ("up" ou "down")
  # Output : résultat fournis par BreakDown
  # [data.frame]
  expl = DALEX::explain(model, data = dat_app[,var_freq], y = target_freq(dat_app))
  if (type=="cout"){
    expl = DALEX::explain(model, data = dat_app[,var_freq], y = target_cout(dat_app))
  }
  return(prediction_breakdown(expl,dat_app[x_interest,var_freq],direction = direction))
}
breakdown_plot = function(model, dat_app, type="frequence", x_interest = 1, direction = "up"){
  # Input : 
  #       - model : modèle ajusté
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - type : indique si il s'agit du modèle fréquence ou coût
  #         [character] ("frequence" ou "cout")
  #       - x_interest : ligne de l'instance d'intérêt dans le data.frame dat_app
  #         [integer]
  #       - direction : direction utilisée dans Breakdown "up" ou "down"
  #         [character] ("up" ou "down")
  # Output : plot des résultat fournis par BreakDown
  # [ggplot]
  return(plot(breakdown_func(model, dat_app, type, x_interest, direction)))
}

##########################################################################################
#################################### 9 - H-statistique ###################################
##########################################################################################

H_stat_func = function(model,j=1, k=2){
  t1 = pdp_func_2var(model, j, k)
  t2 = pdp_func(model, j)
  t3 = pdp_func(model, k)
  aa = rep(t2$yhat,length(t3$yhat))
  bb = as.vector(t(matrix(rep(t3$yhat,length(t2$yhat)), ncol= length(t2$yhat))))
  sum((t1$yhat - aa - bb)^2)/sum(t1$yhat^2)
}
 
