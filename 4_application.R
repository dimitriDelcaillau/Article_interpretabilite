##############################################################################################
###################################### Application ###########################################
##############################################################################################

# Ce code a pour objectif d'utiliser les différentes fonctions issues des codes :
# '1_retraitement_analyse_preliminaire'
# '2_ajustement_modeles_freq_cout'
# '3_interpretation_modeles'

# Les principales étapes seront :
# Retraitement de la base de données et analyse préliminaire
# Ajustement des modèles : GLM et XGBoost 
# Interprétation des deux modèles, avec l'accent sur le XGBoost nécessitant les méthodes post-hoc

# Remarques importantes : 
# 
# On charge les codes précédents avec les fonctions utiles à l'analyse
# !! à changer par l'utilisateur !!
wd = "C:\\Users\\dimitri.delcaillau\\Desktop\\Dimitri Delcaillau\\Article_Interprétabilite\\Code\\"
source(paste(wd, "1_retraitement_analyse_preliminaire.R", sep =""))
source(paste(wd, "2_ajustement_modeles_freq_cout.R", sep =""))
# source(paste(wd, "3_interprétation_modeles.R", sep =""))

# Import des packages
Packages = c("CASdatasets", "dplyr", "data.table", "ggplot2","gridExtra", "stringr","MASS","Metrics", "RColorBrewer",
             "DescTools","plotrix", "reshape2", "xgboost","ranger","caret","mlr","Metrics","Matrix","forcats","rpart", 
             "rpart.plot","lime", "iml", "shapleyR", "ALEPlot","breakDown","condvis","DALEX","iBreakDown","ICEbox","plotly",
             "breakDown", "ICEbox", "ingredients", "localModel", "pdp","shapper", "vip", "xtable", "ggthemes")
a=suppressMessages(suppressWarnings(lapply(Packages, library, character.only = TRUE)))

# Import des données
data("freMTPL2freq")
freq=freMTPL2freq

# Changement de noms des colonnes de la base freMTPL2freq 
# (pour coïncider avec ceux de la base freMTPLfreq utilisée en premier lieu)
freq2 = freq%>%mutate(PolicyID = IDpol, Power = VehPower, DriverAge = DrivAge, Brand = VehBrand,
                      Gas = VehGas, CarAge = VehAge)
freq2$VehPower =NULL;freq2$VehAge=NULL; freq2$VehBrand = NULL; freq2$VehGas = NULL; freq2$DrivAge = NULL;freq2$IDpol = NULL
# Variables explicatives
var_freq = colnames(freq2)[-c(1,2,7)]
# Variables numériques
var_freq_num = c("BonusMalus", "Density", "Power", "DriverAge", "CarAge")
# Variables catégorielles
var_freq_cat = c("Area", "Region", "Brand", "Gas")

############################################################################################
####################### 0 - Statistiques descriptive #######################################
############################################################################################

# Nombres de polices, et exposition par Nombre de Claims
freq2%>%group_by(ClaimNb)%>%dplyr::summarise( NbPolices = n(), TotalExpo = sum(Exposure))

# Réparition des sinistres
freq2%>%group_by(ClaimNb)%>%dplyr::summarise(NbPolices = n(), Proportion = n()/nrow(freq), TotalExposition = sum(Exposure))
# Fréquence moyenne de sinistres : envrion 10%
sum(freq2$ClaimNb)/sum(freq2$Exposure)
# Nombre de polices à plus de 4 sinistres (srictement) : 9
freq%>%filter(ClaimNb>4)%>%nrow()
# Pourcentage d'exposition > 1 : 0.18%
mean(freq$Exposure>1)*100
# Pourcentage de BM > 150 : 0.03%
mean(freq$BonusMalus>150)*100

# On remarque que la majorité des assurés n'ont pas eu de sinistres (environ 95%).
mean(freq2$ClaimNb>0)

# Analyse exposition et fréquences de sinistres
g1=ggplot(data.frame(x=freq2$Exposure), aes(x=x))+geom_histogram(bins=20)+xlab("Exposition")+ylab("Nombre de polices")+ggtitle("Histogramme Exposure")
g2=ggplot(data.frame(x=freq2$Exposure), aes(y=x))+geom_boxplot()+ggtitle("Boxplot Exposure")
g3 = ggplot(data.frame(x=freq2$ClaimNb), aes(x=x))+geom_histogram(bins=20)+xlab("ClaimNb")+ylab("Nombre de polices")+ggtitle("Histogramme ClaimNb")
grid.arrange(g1,g2,g3,ncol=3)

# Region
suppressMessages(suppressWarnings( hist_func(freq2, "Region")))
# Power
suppressMessages(suppressWarnings( hist_func(freq2, "Power")))
# CarAge (restreint à [0, 25])
suppressMessages(suppressWarnings( hist_func(freq2, "CarAge",c(0,25))))
# DriverAge (restreint à [18,90])
suppressMessages(suppressWarnings( hist_func(freq2, "DriverAge",c(18,90))))
# Brand
suppressMessages(suppressWarnings( hist_func(freq2, "Brand")))
# Area : on observe une croissance
suppressMessages(suppressWarnings( hist_func(freq2, "Area"))) 
# Gas
suppressMessages(suppressWarnings( hist_func(freq2, "Gas"))) 
# Densité : on prend la partie entière du log de la densité (cf article)
freq_bis = freq2
freq_bis$Density = round(log(freq2$Density))
suppressMessages(suppressWarnings( hist_func(freq_bis, "Density")))


# Analyse des corrélations
# On calcule les corrélations via le V de Cramer (pour gérer les variables catégorielles)
mat_cor = mat_correlation(freq2, var_freq)
# plot de la matrice de corrélation
mat_correlation_plot_2(mat_cor)

# Densité et région très corrélées (cohérent)
# Gas et power pas mal corrélées également


############################################################################################
####################### 1 - Retraitement des données #######################################
############################################################################################
#Premier retraitement : on cappe les valeurs de ClaimNb à 4, le BonusMalus à 150 et l'exposition à 1
# Suite aux courbes de la partie 0
freq2$ClaimNb = pmin(freq$ClaimNb, 4)
freq2$BonusMalus = pmin(freq$BonusMalus,150)
freq2$Exposure = pmin(freq$Exposure, 1)

# Ajout des variables retraitées (dans le nom GLM à la fin)
freq2 = retraitement_base_freq2(freq2)

# Base d'apprentissage et de test ----------------------
n=nrow(freq2)
n_app=floor(90/100*n) 
set.seed(100)
A=sample(1:n,n_app)
app = freq2[A,]
test = freq2[-A,]
# app2 = temp[A,c(var_freq,"Exposure", "ClaimNb")]
# test2 = temp[-A,c(var_freq,"Exposure", "ClaimNb")]

# Analyse Ba et Bt
app%>%group_by(ClaimNb)%>%summarise(prop = n()/nrow(app))
test%>%group_by(ClaimNb)%>%summarise(prop = n()/nrow(test))
# fréquence empirique test et app : léger biais 
sum(app$ClaimNb)/sum(app$Exposure)
sum(test$ClaimNb)/sum(test$Exposure)


############################################################################################
######################################## 2 - Modèles #######################################
############################################################################################

# A) Modèle GLM --------- 
# a) Modèle GLM 1 : complet avec les variables numériques
variables=paste(var_freq,collapse="+")    
formule=as.formula(paste("ClaimNb","~",variables))
glm_freq_num = glm(formule,data=app,family=poisson(), offset = log(Exposure))
summary(glm_freq_num)
# toutes les variables sont significatives : pas de meilleur modèle possible
# résultat modèle
app$fit_glm_num = fitted(glm_freq_num)
test$fit_glm_num = predict(glm_freq_num, newdata=test, type="response")

# Taux d'erreur du modèle (déviance, MSE, MAE)
res_glm_num = func_resume_modele(app$fit_glm_num, app$ClaimNb, test$fit_glm_num, test$ClaimNb)

# b) Modèle GLM 2 : complet avec les variables retraitées
variables=paste(paste(var_freq,"GLM",sep=""),collapse="+")    
formule=as.formula(paste("ClaimNb","~",variables))
glm_freq_cat = glm(formule,data=app,family=poisson(), offset = log(Exposure))
summary(glm_freq_cat)
# toutes les variables sont significatives : pas de meilleur modèle possible
# résultat modèle
app$fit_glm_cat = fitted(glm_freq_cat)
test$fit_glm_cat = predict(glm_freq_cat, newdata=test, type="response")

# Taux d'erreur du modèle (déviance, MSE, MAE)
res_glm_cat = func_resume_modele(app$fit_glm_cat, app$ClaimNb, test$fit_glm_cat, test$ClaimNb)

# Le GLM catégoriel est meilleur : cohérent avec le fait qu'une monotonie est imposée avec les variables numériques

# B) Modèle CART ---------
# a) Cart avec variables numériques
variables=paste(var_freq,collapse="+")    
formule=as.formula(paste("cbind(Exposure,ClaimNb)","~",variables))

tree_num <- rpart(formule,app, method="poisson", control=rpart.control(xval=1, minbucket=10000, cp=0.0005))     

rpart.plot(tree_num, min.auto.cex=0.5)
tree_num                   # show tree with all binary splits
printcp(tree_num)           # cost-complexit statistics

app$fit_tree_num = predict(tree_num)*app$Exposure
test$fit_tree_num = predict(tree_num, newdata=test)*test$Exposure
res_tree_num = func_resume_modele(app$fit_tree_num, app$ClaimNb, test$fit_tree_num, test$ClaimNb)

# b) CART avec variables catégorielles

tree_cat <- rpart(cbind(Exposure,ClaimNb) ~ Power+CarAge+DriverAge+Brand+Gas+Region+Density+Area+BonusMalus, 
                  app, method="poisson", control=rpart.control(xval=1, minbucket=10000, cp=0.0005))     

rpart.plot(tree_cat, min.auto.cex=0.5)
tree_cat                   # show tree with all binary splits
printcp(tree_cat)           # cost-complexit statistics

app$fit_tree_cat = predict(tree_cat)*app$Exposure
test$fit_tree_cat = predict(tree_cat, newdata=test)*test$Exposure
res_tree_cat = func_resume_modele(app$fit_tree_cat, app$ClaimNb, test$fit_tree_cat, test$ClaimNb)


# C) XGBoost ------

# Via caret
# Paramètres à optimiser via CV : la fonction "xgboost_cv_param" peut le faire 
# Mais très long à exécuter : searchGrid beaucoup de combinaisons testées

# a) XGBoost avec avec les variables numériques
caret_xgb_param_freq_1 <- expand.grid(nrounds = 150, eta = 0.1, max_depth = 4,
                                      gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
set.seed(2019)
variables=paste(var_freq ,collapse="+")    
formule=as.formula(paste("ClaimNb/Exposure","~",variables))
xgb_freq_num = caret::train(formule, data = app, method = "xgbTree",
                          tuneGrid = caret_xgb_param_freq_1, weights= app$Exposure,
                          verbose=F, trControl = caret::trainControl(method="none") ,objective = "count:poisson")

app$fit_xgb_num = predict(xgb_freq_num, app)*app$Exposure
test$fit_xgb_num = predict(xgb_freq_num, test)*test$Exposure
res_xgb_num= func_resume_modele(app$fit_xgb_num, app$ClaimNb, test$fit_xgb_num, test$ClaimNb)

# b) XGBoost avec avec les variables catégorielles
caret_xgb_param_freq_2 <- expand.grid(nrounds = 150, eta = 0.1, max_depth = 4,
                                      gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
variables=paste(paste(var_freq,"GLM",sep=""),collapse="+")    
formule=as.formula(paste("ClaimNb/Exposure","~",variables))
set.seed(2019)
xgb_freq_cat = caret::train(formule, data = app, method = "xgbTree",
                            tuneGrid = caret_xgb_param_freq_2, weights= app$Exposure,
                            verbose=F, trControl = caret::trainControl(method="none") ,objective = "count:poisson")

app$fit_xgb_cat = predict(xgb_freq_cat, app)*app$Exposure
test$fit_xgb_cat = predict(xgb_freq_cat, test)*test$Exposure
res_xgb_cat = func_resume_modele(app$fit_xgb_cat, app$ClaimNb, test$fit_xgb_cat, test$ClaimNb)

# Comparaison des différents modèles
# Valeurs réelles
rbind(res_glm_num,res_glm_cat, res_tree_num, res_tree_cat, res_xgb_num, res_xgb_cat)
# Valeurs relatives au GLM catégoriel (en %)
temp = do.call(rbind,lapply(1:6,function(a){return(res_glm_cat)}))
-(rbind(res_glm_num,res_glm_cat, res_tree_num, res_tree_cat, res_xgb_num, res_xgb_cat)-temp)/temp*100


############################################################################################
####################### 3 - Interprétation des résultats####################################
############################################################################################

# 1) Importance des variables ------
# a) avec caret : renvoie une valeur par coefficient (et non par variable : variables Dummy)! ------

func_imp_var_caret = function(model){
  e = varImp(model)
  if (class(e)=="varImp.train"){
    return(e$importance)
  }
  else{
    return(e)
  }
}
func_imp_var_caret_plot = function(model, nb_vars = NULL){
  # model : le modèle ajusté (avec la fonction train de caret)
  # nb_vars : nombre de variables retenues pour le plot (si NULL ou pas spécifiée : toutes les variables sont utilisées)
  a = func_imp_var_caret(model)
  if (is.null(nb_vars)){
    nb_vars = nrow(a)
  }
  data.frame(vars = rownames(a), Overall = a)%>%head(nb_vars)%>%
    mutate(vars = fct_reorder(vars,Overall))%>%
    ggplot(aes(x = vars, y = Overall))+
    geom_col(fill = "cornflowerblue")+coord_flip()
}
# Pour le GLM ==> caret utilise la t-stat
g1 = func_imp_var_caret_plot(glm_freq_num,10)
g2 = func_imp_var_caret_plot(glm_freq_cat,10)
g3 = func_imp_var_caret_plot(xgb_freq_num,10)
g4 = func_imp_var_caret_plot(xgb_freq_cat,10)
grid.arrange(g1+ggtitle("GLM num"),g2+ggtitle("GLM cat"),g3+ggtitle("XGB num"),g4+ggtitle("XGB cat"))


# b) Avec vip  ------
# i) GLM
# !!!! à faire : le glm doit être fait avec caret ... !!!
# ii) XGB
imp_xgb_cat = (vi(xgb_freq_cat))
g5 = vip(imp_xgb_cat)

imp_xgb_num = (vi(xgb_freq_num))
g6 = vip(imp_xgb_num)
g6
# g7 et g8 pour le GLM : à faire

# Comparaison caret et vip !!!
grid.arrange(g3,g5)
grid.arrange(g4,g6)

#3),Avec,DALEX
expl_glm_cat=DALEX::explain(glm_freq_cat,data=app[,c(paste(var_freq,"GLM",sep=""),"Exposure")],y=app$ClaimNb,predict_function=function(a,b){return(predict(a,b)*b$Exposure)})
imp_glm_cat_dalex=DALEX::variable_importance(expl_glm_cat)
g9 = plot(imp_glm_cat_dalex)

expl_glm_num=DALEX::explain(glm_freq_num,data=app[,c(var_freq,"Exposure")],y=app$ClaimNb,predict_function=function(a,b){return(predict(a,b)*b$Exposure)})
imp_glm_num_dalex=DALEX::variable_importance(expl_glm_num)
g10 = plot(imp_glm_num_dalex)

expl_xgb_cat=DALEX::explain(glm_freq_cat,data=app[,c(paste(var_freq,"GLM",sep=""),"Exposure")],y=app$ClaimNb,predict_function=function(a,b){return(predict(a,b)*b$Exposure)})
imp_xgb_cat_dalex=DALEX::variable_importance(expl_xgb_cat)
g11 = plot(imp_xgb_cat_dalex)

expl_xgb_num=DALEX::explain(glm_freq_num,data=app[,c(var_freq,"Exposure")],y=app$ClaimNb,predict_function=function(a,b){return(predict(a,b)*b$Exposure)})
imp_xgb_num_dalex=DALEX::variable_importance(expl_xgb_num)
g12 = plot(imp_xgb_num_dalex)

grid.arrange(g3,g5,g11)
 
# avec iml (PFI : permutation feature importance)
n_sample = 10000
set.seed(2019)
app_sample= app[1:nrow(app), n_sample,]
predictor = Predictor$new(xgb_freq_num, data = app[sample(1:nrow(app), n_sample),c(var_freq,"ClaimNb")], y = "ClaimNb")
imp = FeatureImp$new(predictor, "mae", compare = "ratio", n.repetitions = 5)
plot(imp)

# 2) PDP et ALE --------
pred_func = function(object,newdata) {predict(object,newdata,type="response")}
 
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
# Quelques exemples
# PDP CarAge sans limite et avec limite
func_pdp_plot_num(xgb_freq_num, "CarAge", app, 10000)
func_pdp_plot_num(xgb_freq_num, "CarAge", app, 10000,,c(0,20))

func_pdp_plot_cat(xgb_freq_cat, "RegionGLM", app, 10000,)
func_pdp_plot_cat_ordered(xgb_freq_cat, "CarAgeGLM", app, 10000,)
func_pdp_plot_cat_ordered(xgb_freq_cat, "DriverAgeGLM", app, 10000,)

# ICE
func_ice = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019){
  set.seed(seed)
  app_sample = base_app[sample(1:nrow(base_app),n_sample),]
  model %>% partial(pred.var = variable, train  = app_sample, 
                    pred.fun = function(object, newdata){predict(object,newdata)*newdata$Exposure}) 
}
# ex
func_ice(xgb_freq_num, "CarAge",app, 1000, 2019)

func_ice_plot = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019, alpha = 0.1, col = "cornflowerblue"){
  a = func_ice(model, variable, base_app, n_sample, 2019)
  a%>%ggplot(aes_string(x = variable, y = "yhat", group = "yhat.id" ))+geom_line(alpha = alpha, col = col)
}
func_ice_plot(xgb_freq_num, "CarAge",app, 1000, 2019, 0.4)

# Avec iml ----

# 3) Analyse interactions : ICE, H-stat, PDP 2 var, PDP par groupe
# avec iml
#temps estimé pour n_sample = 1 000 (et grid.size =20 par défaut) pour une variable (CarAge): environ 10 secondes)
# ça devient vite très long en temps de calcul
n_sample = 1000
set.seed(2019)
mod = Predictor$new(xgb_freq_num, data = app[sample(1:nrow(app), n_sample),var_freq]) 
t1 = Sys.time()
# H_stat avec 1 var
H_stat1 = Interaction$new(mod, feature = "CarAge")
H_stat1$results
t1 = Sys.time() - t1
t1
# H_stat avec toutes les var
H_stat_all = Interaction$new(mod)

# On observe des H-stat > 1 : à cause des variables catégorielles (comme on l'avait noté dans le mémoire)
# Alors que ça devrait être entre 0 et 1

# 4) Lime, Shap, Breakdown, Live
