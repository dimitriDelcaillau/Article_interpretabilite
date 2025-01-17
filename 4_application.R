##############################################################################################
###################################### Application ###########################################
##############################################################################################

# Ce code a pour objectif d'utiliser les diff�rentes fonctions issues des codes :
# '1_retraitement_analyse_preliminaire'
# '2_ajustement_modeles_freq_cout'
# '3_interpretation_modeles'

# Les principales �tapes seront :
# Retraitement de la base de donn�es et analyse pr�liminaire
# Ajustement des mod�les : GLM et XGBoost 
# Interpr�tation des deux mod�les, avec l'accent sur le XGBoost n�cessitant les m�thodes post-hoc

# Remarques importantes : 
# 
# On charge les codes pr�c�dents avec les fonctions utiles � l'analyse
# !! � changer par l'utilisateur !!
wd = "C:\\Users\\dimitri.delcaillau\\Desktop\\Dimitri Delcaillau\\Article_Interpr�tabilite\\Code\\"
source(paste(wd, "1_retraitement_analyse_preliminaire.R", sep =""))
source(paste(wd, "2_ajustement_modeles_freq_cout.R", sep =""))
source(paste(wd, "3_interpr�tation_modeles.R", sep =""))

# Import des packages
Packages = c("CASdatasets", "dplyr", "data.table", "ggplot2","gridExtra", "stringr","MASS","Metrics", "RColorBrewer",
             "DescTools","plotrix", "reshape2", "xgboost","ranger","caret","mlr","Metrics","Matrix","forcats","rpart", 
             "rpart.plot","lime", "iml", "shapleyR", "ALEPlot","breakDown","condvis","DALEX","iBreakDown","ICEbox","plotly",
             "breakDown", "ICEbox", "ingredients", "localModel", "pdp","shapper", "vip", "xtable", "ggthemes")
a=suppressMessages(suppressWarnings(lapply(Packages, library, character.only = TRUE)))

# Import des donn�es
data("freMTPL2freq")
freq=freMTPL2freq

# Changement de noms des colonnes de la base freMTPL2freq 
# (pour co�ncider avec ceux de la base freMTPLfreq utilis�e en premier lieu)
freq2 = freq%>%mutate(PolicyID = IDpol, Power = VehPower, DriverAge = DrivAge, Brand = VehBrand,
                      Gas = VehGas, CarAge = VehAge)
freq2$VehPower =NULL;freq2$VehAge=NULL; freq2$VehBrand = NULL; freq2$VehGas = NULL; freq2$DrivAge = NULL;freq2$IDpol = NULL
# Variables explicatives
var_freq = colnames(freq2)[-c(1,2,7)]
# Variables num�riques
var_freq_num = c("BonusMalus", "Density", "Power", "DriverAge", "CarAge")
# Variables cat�gorielles
var_freq_cat = c("Area", "Region", "Brand", "Gas")

############################################################################################
####################### 0 - Statistiques descriptive #######################################
############################################################################################

# Nombres de polices, et exposition par Nombre de Claims
freq2%>%group_by(ClaimNb)%>%dplyr::summarise( NbPolices = n(), TotalExpo = sum(Exposure))

# R�parition des sinistres
freq2%>%group_by(ClaimNb)%>%dplyr::summarise(NbPolices = n(), Proportion = n()/nrow(freq), TotalExposition = sum(Exposure))
# Fr�quence moyenne de sinistres : envrion 10%
sum(freq2$ClaimNb)/sum(freq2$Exposure)
# Nombre de polices � plus de 4 sinistres (srictement) : 9
freq%>%filter(ClaimNb>4)%>%nrow()
# Pourcentage d'exposition > 1 : 0.18%
mean(freq$Exposure>1)*100
# Pourcentage de BM > 150 : 0.03%
mean(freq$BonusMalus>150)*100

# On remarque que la majorit� des assur�s n'ont pas eu de sinistres (environ 95%).
mean(freq2$ClaimNb>0)

# Analyse exposition et fr�quences de sinistres
g1=ggplot(data.frame(x=freq2$Exposure), aes(x=x))+geom_histogram(bins=20)+xlab("Exposition")+ylab("Nombre de polices")+ggtitle("Histogramme Exposure")
g2=ggplot(data.frame(x=freq2$Exposure), aes(y=x))+geom_boxplot()+ggtitle("Boxplot Exposure")
g3 = ggplot(data.frame(x=freq2$ClaimNb), aes(x=x))+geom_histogram(bins=20)+xlab("ClaimNb")+ylab("Nombre de polices")+ggtitle("Histogramme ClaimNb")
grid.arrange(g1,g2,g3,ncol=3)

# Region
suppressMessages(suppressWarnings( hist_func(freq2, "Region")))
# Power
suppressMessages(suppressWarnings( hist_func(freq2, "Power")))
# CarAge (restreint � [0, 25])
suppressMessages(suppressWarnings( hist_func(freq2, "CarAge",c(0,25))))
# DriverAge (restreint � [18,90])
suppressMessages(suppressWarnings( hist_func(freq2, "DriverAge",c(18,90))))
# Brand
suppressMessages(suppressWarnings( hist_func(freq2, "Brand")))
# Area : on observe une croissance
suppressMessages(suppressWarnings( hist_func(freq2, "Area"))) 
# Gas
suppressMessages(suppressWarnings( hist_func(freq2, "Gas"))) 
# Densit� : on prend la partie enti�re du log de la densit� (cf article)
freq_bis = freq2
freq_bis$Density = round(log(freq2$Density))
suppressMessages(suppressWarnings( hist_func(freq_bis, "Density")))


# Analyse des corr�lations
# On calcule les corr�lations via le V de Cramer (pour g�rer les variables cat�gorielles)
mat_cor = mat_correlation(freq2, var_freq)
# plot de la matrice de corr�lation
mat_correlation_plot_2(mat_cor)

# Densit� et r�gion tr�s corr�l�es (coh�rent)
# Gas et power pas mal corr�l�es �galement


############################################################################################
####################### 1 - Retraitement des donn�es #######################################
############################################################################################
#Premier retraitement : on cappe les valeurs de ClaimNb � 4, le BonusMalus � 150 et l'exposition � 1
# Suite aux courbes de la partie 0
freq2$ClaimNb = pmin(freq$ClaimNb, 4)
freq2$BonusMalus = pmin(freq$BonusMalus,150)
freq2$Exposure = pmin(freq$Exposure, 1)

# Ajout des variables retrait�es (dans le nom GLM � la fin)
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
# fr�quence empirique test et app : l�ger biais 
sum(app$ClaimNb)/sum(app$Exposure)
sum(test$ClaimNb)/sum(test$Exposure)


############################################################################################
######################################## 2 - Mod�les #######################################
############################################################################################

# A) Mod�le GLM --------- 
# a) Mod�le GLM 1 : complet avec les variables num�riques
variables=paste(var_freq,collapse="+")    
formule=as.formula(paste("ClaimNb","~",variables))
glm_freq_num = glm(formule,data=app,family=poisson(), offset = log(Exposure))
summary(glm_freq_num)
# toutes les variables sont significatives : pas de meilleur mod�le possible
# r�sultat mod�le
app$fit_glm_num = fitted(glm_freq_num)
test$fit_glm_num = predict(glm_freq_num, newdata=test, type="response")

# Taux d'erreur du mod�le (d�viance, MSE, MAE)
res_glm_num = func_resume_modele(app$fit_glm_num, app$ClaimNb, test$fit_glm_num, test$ClaimNb)

# b) Mod�le GLM 2 : complet avec les variables retrait�es
variables=paste(paste(var_freq,"GLM",sep=""),collapse="+")    
formule=as.formula(paste("ClaimNb","~",variables))
glm_freq_cat = glm(formule,data=app,family=poisson(), offset = log(Exposure))
summary(glm_freq_cat)
# toutes les variables sont significatives : pas de meilleur mod�le possible
# r�sultat mod�le
app$fit_glm_cat = fitted(glm_freq_cat)
test$fit_glm_cat = predict(glm_freq_cat, newdata=test, type="response")

# Taux d'erreur du mod�le (d�viance, MSE, MAE)
res_glm_cat = func_resume_modele(app$fit_glm_cat, app$ClaimNb, test$fit_glm_cat, test$ClaimNb)

# Le GLM cat�goriel est meilleur : coh�rent avec le fait qu'une monotonie est impos�e avec les variables num�riques

# B) Mod�le CART ---------
# a) Cart avec variables num�riques
variables=paste(var_freq,collapse="+")    
formule=as.formula(paste("cbind(Exposure,ClaimNb)","~",variables))

tree_num <- rpart(formule,app, method="poisson", control=rpart.control(xval=1, minbucket=10000, cp=0.0005))     

rpart.plot(tree_num, min.auto.cex=0.5)
tree_num                   # show tree with all binary splits
printcp(tree_num)           # cost-complexit statistics

app$fit_tree_num = predict(tree_num)*app$Exposure
test$fit_tree_num = predict(tree_num, newdata=test)*test$Exposure
res_tree_num = func_resume_modele(app$fit_tree_num, app$ClaimNb, test$fit_tree_num, test$ClaimNb)

# b) CART avec variables cat�gorielles

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
# Param�tres � optimiser via CV : la fonction "xgboost_cv_param" peut le faire 
# Mais tr�s long � ex�cuter : searchGrid beaucoup de combinaisons test�es

# a) XGBoost avec avec les variables num�riques
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

# b) XGBoost avec avec les variables cat�gorielles
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

# Comparaison des diff�rents mod�les
# Valeurs r�elles
rbind(res_glm_num,res_glm_cat, res_tree_num, res_tree_cat, res_xgb_num, res_xgb_cat)
# Valeurs relatives au GLM cat�goriel (en %)
temp = do.call(rbind,lapply(1:6,function(a){return(res_glm_cat)}))
(rbind(res_glm_num,res_glm_cat, res_tree_num, res_tree_cat, res_xgb_num, res_xgb_cat)-temp)/temp*100


############################################################################################
####################### 3 - Interpr�tation des r�sultats####################################
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
  # model : le mod�le ajust� (avec la fonction train de caret)
  # nb_vars : nombre de variables retenues pour le plot (si NULL ou pas sp�cifi�e : toutes les variables sont utilis�es)
  a = func_imp_var_caret(model)
  if (is.null(nb_vars)){
    nb_vars = nrow(a)
  }
  data.frame(vars = rownames(a), Overall = a)%>%head(nb_vars)%>%
    mutate(vars = fct_reorder(vars,Overall))%>%
    ggplot(aes(x = vars, y = Overall))+
    geom_col(fill = "cornflowerblue", col='black')+coord_flip()
}
# Pour le GLM ==> caret utilise la t-stat
g1 = func_imp_var_caret_plot(glm_freq_num,10)
g1
g2 = func_imp_var_caret_plot(glm_freq_cat,10)
g3 = func_imp_var_caret_plot(xgb_freq_num,10)
g4 = func_imp_var_caret_plot(xgb_freq_cat,10)
grid.arrange(g1+ggtitle("GLM num"),g2+ggtitle("GLM cat"),g3+ggtitle("XGB num"),g4+ggtitle("XGB cat"))


# b) Avec vip  ------
# i) GLM
variables=paste(paste(var_freq,"GLM",sep=""),collapse="+")    
formule=as.formula(paste("ClaimNb","~",variables))
glm_freq_cat_caret = caret::train(formule, data = app, method="glm", family=poisson, weights = app$Exposure,
                                  trControl = caret::trainControl(method="none"))

imp_glm_cat = (vi(glm_freq_cat_caret))
g5 = vip(imp_glm_cat)
# !!!! � faire : le glm doit �tre fait avec caret ... !!!
# ii) XGB
imp_xgb_cat = (vi(xgb_freq_cat))
g5 = vip(imp_xgb_cat)

imp_xgb_num = (vi(xgb_freq_num))
g6 = vip(imp_xgb_num)
g6
# g7 et g8 pour le GLM : � faire

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
  #       - model : mod�le ajust�
  #         ["glm"] (glm) ou ["train"] (caret::train)
  #       - variable : nom de la variable �tudi�e
  #       - base_app : base d'apprentissage
  #       - n_sample : nombre de points utilis�s pour le calcul : tr�s utile pour r�duire temps de calcul avec des variables num�riques
  # Output : fonction de d�pendance partielle associ� � la variable
  # [data.frame]
  # k=ind_var
  set.seed(seed)
  app_sample = base_app[sample(1:nrow(base_app),n_sample),]
  return(model %>% partial(pred.var = variable, train = app_sample))
}

func_pdp_plot_cat = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019){
  # PDP pour les variables cat�gorielles sans ordre
  partialD = func_pdp(model, variable, base_app, n_sample, seed)
  partialD$xx = partialD[,1]
  ggplot(partialD%>%mutate(xx = fct_reorder(xx, yhat)), aes(x = xx, y = yhat))+geom_col(fill = "cornflowerblue", col='black')+xlab(variable)
}

func_pdp_plot_cat_ordered = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019){
  # PDP pour les variables cat�gorielles mais qui ont un ordre (ex : CarAgeGLM, DriverAgeGLM....)
  partialD = func_pdp(model, variable, base_app, n_sample, seed)
  partialD[,1] = as.integer(as.character(partialD[,1]))
  ggplot(partialD, aes_string(x = variable, y = 'yhat'))+geom_col(fill = "cornflowerblue", col='black')
}

func_pdp_plot_num = function(model, variable, base_app, n_sample = nrow(base_app), seed = 2019, x_lim = NULL){
  # x_lim : vecteur pour donner les limites de l'abscisse (utile lorsque peu de points repr�sent�s dans une zone : ex CarAge)
  # si x_lim = NULL ==> graphique entier affich�
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


# Comparaison
variable = "Region"
# Variable purement cat�gorielle (Region, Area (si on ne la transforme pas en num.), Gas, Brand)
app%>%group_by_(variable)%>%summarise(gg = sum(ClaimNb)/sum(Exposure))%>%ggplot(aes_string(x = "Region", y = "gg"))+geom_col()
app%>%group_by_("Brand")%>%summarise(gg = sum(ClaimNb)/sum(Exposure))%>%ggplot(aes_string(x = "Brand", y = "gg"))+geom_col()
app%>%group_by_("Area")%>%summarise(gg = sum(ClaimNb)/sum(Exposure))%>%ggplot(aes_string(x = "Area", y = "gg"))+geom_col()

# Variable cat�gorielle ordonn�e
variable = "VehPower"
freq%>%group_by_(variable)%>%summarise(gg = sum(ClaimNb)/sum(Exposure))%>%ggplot(aes_string(x = variable, y = "gg"))+geom_col()


# Avec iml ----

# 3) Analyse interactions : ICE, H-stat, PDP 2 var, PDP par groupe
# a) H STAT -----
# avec iml
#temps estim� pour n_sample = 1 000 (et grid.size =20 par d�faut) pour une variable (CarAge): environ 10 secondes)
# �a devient vite tr�s long en temps de calcul
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

# On observe des H-stat > 1 : � cause des variables cat�gorielles (comme on l'avait not� dans le m�moire)
# Alors que �a devrait �tre entre 0 et 1

# b) Courbes ICES : choisir les courbes les plus pertinentes ------
# Courbes pas translat�es entre elles ==> interaction
ICE_courbes_freMTPL2(10, xgb_freq_num, which(var_freq=="Gas"), dat_app = app)
ICE_courbes_freMTPL2(100, xgb_freq_num, which(var_freq=="CarAge"), dat_app = app)+xlim(c(0,20))
ICE_courbes_freMTPL2(100, xgb_freq_num, which(var_freq=="Power"), dat_app = app)

# ICE : courbes pas translat�es entre elles ==> interaction
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


# 4) Lime, Shap, Breakdown, Live------

#a) LIME
# avec package lime
# Sur exemple particulier (1 ere observation)
lime_func(xgb_freq_num, dat_app= app, 1, 5000, n_features = 4)
lime_plot(xgb_freq_num, dat_app= app, 1, 5000, n_features = 4)
# On peut analyser un des extremats : et comparer GLM et XGBoost
# max : tr�s bizarre : l'analyse de LIME tend plut�t � dire que la valeur pr�dite devrat �tre faible
# BONUS MALUS semble fair ebaisser le risque de sinistres
app[which.max(app$fit_xgb_num),]
lime_plot(xgb_freq_num, dat_app= app, which.max(app$fit_xgb_num), 20000, n_features = 8)

# autre exemple : plus coh�rent indice 230339 dans la table app (PolicyID = 2127799)
set.seed(2019)
obs = app[app$fit_glm_num>quantile(app$fit_glm_num,0.99),]%>%sample_n(1)
lime_plot(xgb_freq_num, dat_app= app, which(app$PolicyID==obs$PolicyID), 5000, n_features = 4)

# min : coh�rent (tout est en rouge quasiment)
lime_plot(xgb_freq_num, dat_app= app, which.min(app$fit_xgb_num), 20000, n_features = 7)

#Analyse de la stabilit� des r�sulats fournis (plusieurs simulations et boxplot)
lime_stabilite_2(xgb_freq_num, dat_app= app, 1, 5000, n_features = 4, Nsimu = 20)
# LIME doone des r�sultats tr�s stables
#~On pourrait analyser choix du noyau ou de la distance (gower ou autre)

# avec package iml 
#(qui utilise une variante  qui 
# utilise les donn�es d'apprentissage pas des donn�es simul�es via Loi normale comme LIME)
# int�ressant de les comparer
lime_iml_func(xgb_freq_num, app, , 1)
lime_iml_plot(xgb_freq_num, app, , 1)


# b) SHAP : plus long !!
shap_iml_func(xgb_freq_num, app, , 1)
shap_iml_plot(xgb_freq_num, app, , 1, sample_size = 100)
# Analyse stablit�
shap_iml_stabilite(xgb_freq_num, app, , 1, sample_size = 100, Nsimu = 10)

# A tester avec le package shappe (qui utilise DALEX : fonction individual_variable_effect)

# C) breakdown
breakdown_func(xgb_freq_num, app, , x_interest = 1, direction = "up")
breakdown_func(xgb_freq_num, app, , x_interest = 1, direction = "down")
breakdown_plot(xgb_freq_num, app, , x_interest = 1, direction = "down")
