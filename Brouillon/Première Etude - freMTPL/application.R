##############################################################################################
###################################### Application ###########################################
##############################################################################################

# Ce code a pour objectif d'utiliser les différentes fonctions issues des codes :
# '1_retraitement_analyse_preliminaire'
# '2_ajustement_modeles_freq_cout'
# '3_interpretation_modeles.R'

# Les principales étapes seront :
# Retraitement de la base de données et analyse préliminaire
# Ajustement des modèles : GLM et XGBoost 
# Interprétation des deux modèles, avec l'accent sur le XGBoost nécessitant les méthodes post-hoc

#############################################################################################
####################### 1 - Retraitement des données #######################################
#############################################################################################

# !!! Choix des paramètres : à définir avant suivant la base !!!

# Exposition minimale de chaque assuré (exposition = max(exposition,expo))
expo = 0.25
# Variables explicatives pour la fréquence
var_freq=c("Power","CarAge","DriverAge","Brand","Gas","Region","Density") 
# Variables explicatives pour le coût
var_cout=c("Power","CarAge","DriverAge","Brand","Gas","Region","Density") 

data("freMTPLfreq")  # fréquence
data("freMTPLsev")   # sévérité
freq=freMTPLfreq
sev=freMTPLsev
# pour la base de sévérité, on conserve uniquement la somme des sinistres par PolicyID
temp=sev%>%group_by(PolicyID)%>%mutate(ClaimAmount=sum(ClaimAmount))
sev = unique(temp)
# On  renomme les noms des marques de voiture (par des lettres) :
letters=c("D","F","E","C","G","A","B") 
levels(freq$Brand)=letters

# Fusion des bases freq et sev
data = merge_freq_sev(freq, sev, "PolicyID")

# Changement de l'exposition : (capée à 0.25)
data_exp = change_expo_assure(data, expo_min = expo)
# Retraitement de la base:
data_r = retraitement_base(data_exp)

# Séparation Ba et Bt
app_test = separation_base_test_app(data_r,prop_app = 0.9,seed=2015)
app = app_test$app
test = app_test$test

# Ecretement (à 10K) de Ba
app_ecr = ecretement_base_app(app,ecret = 10000)

app_1 = app_ecr
test_1 = test
# Base des "sinsitres" 
app_sin_1 =data_sinistres(app_1)
app_sin_1_avant_ecret = data_sinistres(app)
# Différence entre la valeur moyenne des sinistres de la Ba avant et après écretement
ecret = mean(app_sin_1_avant_ecret$ClaimAmount/app_sin_1_avant_ecret$ClaimNb) - mean(app_sin_1$ClaimAmount/app_sin_1$ClaimNb) 


# Sans catégoriser les variables 
sep = separation_base_test_app(data_exp,seed=2015)
app_cont = sep$app
test_con = sep$test
app_1_continue = ecretement_base_app(app_cont)
test_1_continue = test_con


########################################################################################### 
########################## 2) Analyse préliminaire ########################################
###########################################################################################

# PLOT sinsitres
tr = data_sinistres(data)
f1=ggplot()+geom_point(data=data.frame(x=1:nrow(tr),y=tr$ClaimAmount), aes(x=x,y=y))+theme_bw()+scale_fill_stata()+ggtitle("Montant des sinistres")+
  theme(axis.title.x = element_text(size=14), title = element_text(size=16), axis.text.y = element_text(size=18), axis.text.x = element_text(size=14))+
  geom_point(data=data.frame(x=head(order(tr$ClaimAmount,decreasing = T),2), y = head(sort(tr$ClaimAmount,decreasing = T),2)), aes(x=x,y=y), size=3,col="red")
tr2 = tr[tr$ClaimAmount<100000,]
f2=ggplot()+geom_point(data=data.frame(x=1:nrow(tr2),y=tr2$ClaimAmount), aes(x=x,y=y))+theme_bw()+scale_fill_stata()+ggtitle("Montant des sinistres (sauf des deux plus gros)")+
  theme(axis.title.x = element_text(size=14), title = element_text(size=16), axis.text.y = element_text(size=18), axis.text.x = element_text(size=14))
grid.arrange(f1,f2,ncol=2)

# Analyse préliminaire avant retraitement
sinistres_par_modalite_variable2(data, 1)
sinistres_par_modalite_variable2(data, 4)
sinistres_par_modalite_variable2(data, 5)
sinistres_par_modalite_variable2(data, 6)

# Analyse préliminaire après retraitement
sinistres_par_modalite_variable2(data_r, 1)
sinistres_par_modalite_variable2(data_r, 4)
sinistres_par_modalite_variable2(data_r, 5)
sinistres_par_modalite_variable2(data_r, 6)

# Distribution Exposition avant et après retraitement
g1=ggplot(data%>%dplyr::select(Exposure)%>%filter(Exposure <=1), aes(x = Exposure))+
  geom_density()+ggtitle("Distribution de l'exposition avant retraitement") #av retraitement
g2=ggplot(data_exp%>%dplyr::select(Exposure)%>%filter(Exposure <=1), aes(x = Exposure))+
  geom_density()+ggtitle("Distribution de l'exposition après retraitement") # ap retraitement
grid.arrange(g1,g2,ncol=2)

# Montant des sinistres
data_sin_exp = data_sinistres(data_exp)
g3 = ggplot()+geom_point(data= data.frame(x = 1:nrow(data_sin_exp), r = data_sin_exp$ClaimAmount), aes(x=x, y = r))+xlab("Assurés")+ylab('Montant total de sinistres')+ylim(c(0,tail(sort(data_exp$ClaimAmount),5)[3]))
g3 + geom_hline(yintercept = 10000, col='red')
g4 = ggplot()+geom_point(data= data.frame(x = 1:nrow(data_sin_exp), r = data_sin_exp$ClaimAmount), aes(x=x, y = r))+xlab("Assurés")+ylab('Montant total de sinistres')+geom_point(data=data.frame(ind = head(order(data_sin_exp$ClaimAmount,decreasing = T),2),val =head(sort(data_sin_exp$ClaimAmount,decreasing = T),2) ), aes(x=ind,y =val), col='red')
grid.arrange(g4+ggtitle("Sinistres de la base"), g3+ggtitle("Sinistres de la base (sans les deux plus importants)"),ncol=2)


# Corrélation :
# Avant retraitement
mat_av = mat_correlation(data, var_freq)
mat_corelation_plot_2(mat_av)

# Après retraitement
mat_ap = mat_correlation(data_r, var_freq)
mat_corelation_plot_2(mat_ap)

# Analyse univariée des sinistres par modalité
par(mfrow=c(2,3))
sinistres_par_modalite_variable2(data_r, 1)
sinistres_par_modalite_variable2(data_r, 2)
sinistres_par_modalite_variable2(data_r, 3)
sinistres_par_modalite_variable2(data_r, 4)
sinistres_par_modalite_variable2(data_r, 5)
sinistres_par_modalite_variable2(data_r, 6)
sinistres_par_modalite_variable2(data_r, 7)
par(mfrow=c(1,1))


###########################################################################################
#################################### 3) Ajustement du GLM #################################
###########################################################################################

# A - GLM fréquence  ---------
# i) GLM fréquence trivial  ------

fit_freq_moy = glm_frequence(app_1, NULL)
# Prédiction (Bt)
pred_freq_moy = pred_freq_glm(fit_freq_moy, test_1) 
# Erreur (Bt)
mse_freq_moy = err_test_freq_glm("mse", fit_freq_moy, test_1)
mae_freq_moy = err_test_freq_glm("mae", fit_freq_moy, test_1)

# ii) GLM fréquence complet (= meilleur modèle) ------
fit_freq_complet = glm(ClaimNb~offset(log(Exposure))+Power+CarAge+DriverAge+Gas+Brand+Region+Density,
                       data = app_1,family=poisson(link="log"))
# Utile pour pas avoir de problème avec "Exposure" comme variable dans l'interprétation
fit_freq_complet_2 = glm(ClaimNb/Exposure~Power+CarAge+DriverAge+Gas+Brand+Region+Density,
                       data = app_1, weights = app_1$Exposure,family=poisson(link="log"))

# GLM freq : ajustement avec caret(utile pour après)
train_control = caret::trainControl(method="none")

modelLog = caret::train(ClaimNb/Exposure ~ Power + CarAge + DriverAge + 
                          Brand + Gas + Region + Density, data=app_1, weights = app_1$Exposure, method="glm", family=poisson, trControl=train_control)

# Prédiction (Bt)
pred_freq_complet = pred_freq_glm(fit_freq_complet, test_1)
# Erreur(Bt)
mse_freq_complet = err_test_freq_glm("mse", fit_freq_complet, test_1)
mae_freq_complet = err_test_freq_glm("mae", fit_freq_complet, test_1)
# stepAIC(fit_freq_complet)

# B) GLM coût --------------
# i) GLM coût trivial --------

# Ajustement
fit_cout_moy = glm_cout(app_1, NULL)
test_sin_1 = data_sinistres(test_1)
# Prédiction (Bt)
pred_cout_moy = pred_cout_glm(fit_cout_moy, test_sin_1, ecret = ecret)
# Erreur(Bt)
mse_cout_moy = err_test_cout_glm("mse", fit_cout_moy, data_sinistres(test_1),ecret = ecret)
mae_cout_moy = err_test_cout_glm("mae", fit_cout_moy, data_sinistres(test_1),ecret = ecret)


# ii) GLM coût complet --------------

fit_cout_complet = glm(ClaimAmount/ClaimNb~Power+CarAge+DriverAge+Gas+Brand+Region+Density, weights = app_sin_1$ClaimNb,
                       data = app_sin_1, family=Gamma(link="log"))
pred_cout_complet = pred_cout_glm(fit_cout_complet, test_sin_1, ecret = ecret)
mse_cout_complet = err_test_cout_glm("mse", fit_cout_complet, data_sinistres(test_1),ecret = ecret)
mae_cout_complet = err_test_cout_glm("mae", fit_cout_complet, data_sinistres(test_1),ecret)


# iii) Best GLM coût ----------

variables_cout=paste(var_cout,collapse="+")    
formule_cout=as.formula(paste("ClaimAmount/ClaimNb~",variables_cout))
gg = glm(formule_cout,data=app_1,weights = app_1$ClaimNb,family=Gamma(link="log"))
stepAIC = stepAIC(gg, direction ="backward")
stepAIC$anova
# On retire "Region"
fit_cout_best = glm(ClaimAmount/ClaimNb ~ Power + CarAge + DriverAge +Brand+Gas+ Density, data = app_1, weights = app_1$ClaimNb,family=Gamma(link="log"))
summary(fit_cout_best)
pred_cout_best = pred_cout_glm(fit_cout_best, test_sin_1, ecret = ecret)
# Erreur (Bt)
mse_cout_best = err_test_cout_glm("mse",  fit_cout_best, data_sinistres(test_1), ecret)
mae_cout_best = err_test_cout_glm("mae",  fit_cout_best, data_sinistres(test_1), ecret)

# C) Modèle total : coût x fréquence -------------------
pred_total_glm_best = pred_total_glm(fit_freq_complet, fit_cout_best, test_1, ecret)
mse_total_moy = err_test_total_glm("mse", fit_freq_moy, fit_cout_moy, test_1, ecret)
mse_total_complet = err_test_total_glm("mse", fit_freq_complet, fit_cout_complet, test_1, ecret)
mse_total_best = err_test_total_glm("mse", fit_freq_complet, fit_cout_best, test_1, ecret)
pred_total_glm_moy = pred_total_glm(fit_freq_moy, fit_cout_moy, test_1, ecret)

mae_total_moy = err_test_total_glm("mae", fit_freq_moy, fit_cout_moy, test_1, ecret)
mae_total_complet = err_test_total_glm("mae", fit_freq_complet, fit_cout_complet, test_1, ecret)
mae_total_best = err_test_total_glm("mae", fit_freq_complet, fit_cout_best, test_1, ecret)


###########################################################################################
################################ 3) Ajustement du XGboost #################################
###########################################################################################

# A - XGBoot fréquence -------------------------
# caret_xgb_param <- expand.grid(nrounds = 100, eta = 0.1, max_depth = 4,
#  gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 1)

# a) XGBoost 1 : avec variables retraitées --------
# i) Ajustement avec caret -----
caret_xgb_param_freq_1 <- expand.grid(nrounds = 120, eta = 0.1, max_depth = 4,
                                      gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
fit_freq_xgb_caret1 = fit_xgb_freq_caret(app_1, caret_xgb_param_freq_1, var_freq,"count:poisson")
pred_freq_xgb1 = pred_xgb_freq_caret(fit_freq_xgb_caret1, test_1) 
mse_freq_xgb1 = mse(pred_freq_xgb1, target_freq(test_1))
mae_freq_xgb1 = mae(pred_freq_xgb1, target_freq(test_1))
# Gain (si >0) ou perte (si <0) par rapport au meilleur GLM correspondant
gain_mse_freq_xgb = (mse_freq_complet-mse_freq_xgb1)/mse_freq_complet*100
gain_mae_freq_xgb = (mae_freq_complet-mae_freq_xgb1)/mae_freq_complet*100

# ii) Ajustement avec xgboost ----
xgboost_xgb_param <-  list(eta = 0.1, max_depth = 4,
                           gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
fit_freq_xgb_xgboost1 =fit_xgb_freq_xgboost(app_1, xgboost_xgb_param, nrounds = 120, var_freq, verbose=F)

dev_freq_xgb1 =  Poisson.Deviance(pred_freq_xgb1*test_1$Exposure, test_1$ClaimNb)
dev_freq_glm_complet =  Poisson.Deviance(pred_freq_complet*test_1$Exposure, test_1$ClaimNb)

## b) XGBoost 2 : en gardant variables continues ----------
# Ajustement en gardant variables numériques inchangées
retraitement_base_2 = function(data = data){
  # Input:
  #       data : base de données initiale que l'on va retraiter
  #       [data.frame]
  #Output : base de données retraitées
  #         [data.frame]
  
  # Premier regroupement :
  data_2 = data
  # Power
  data_2$Power = as.character(data_2$Power)
  data_2$Power[data$Power=='e' | data$Power=='f'] = ("e-f")
  data_2$Power[data$Power=='g' | data$Power=='h'] = "g-h"
  data_2$Power[data$Power=='j' | data$Power=='k'] = "j-k"
  data_2$Power[data$Power=='m' | data$Power=='n'] = "m-n"
  data_2$Power = factor(data_2$Power,ordered = F)
  # 
  # #DriverAge
  # data_2$DriverAge[data$DriverAge<27] = "18-26"
  # data_2$DriverAge[data$DriverAge>26 & data$DriverAge<43] = "27-42"
  # data_2$DriverAge[data$DriverAge>42 & data$DriverAge<56] = "43-55"
  # data_2$DriverAge[data$DriverAge>55 & data$DriverAge<70] = "56-69"
  # data_2$DriverAge[data$DriverAge>69 & data$DriverAge<86] = "70-85"
  # data_2$DriverAge[data$DriverAge>85] = "86+"
  # data_2$DriverAge = factor(data_2$DriverAge, ordered = F)
  # 
  # # CarAge
  # data_2$CarAge[data$CarAge<1] = "0"
  # data_2$CarAge[data$CarAge>0 & data$CarAge<4] = "1-3"
  # data_2$CarAge[data$CarAge>3 & data$CarAge<9] = "4-8"
  # data_2$CarAge[data$CarAge>8 & data$CarAge<13] = "9-12"
  # data_2$CarAge[data$CarAge>12 & data$CarAge<21] = "13-20"
  # data_2$CarAge[data$CarAge>20] = "20+"
  # data_2$CarAge = factor(data_2$CarAge, levels = c("0", "1-3", "4-8", "9-12", "13-20", "20+"),ordered = F)
  # 
  # # Density
  # data_2$Density[data$Density<=100] = "0-100"
  # data_2$Density[data$Density>100 & data$Density<501] = "101-500"
  # data_2$Density[data$Density>500 & data$Density<2001] = "501-2000"
  # data_2$Density[data$Density>2000 & data$Density<10001] = "2001-10000"
  # data_2$Density[data$Density>10000 & data$Density<20000] = "10001-19999"
  # data_2$Density[data$Density>=20000] = "20000+"
  # data_2$Density=factor(data_2$Density,levels=c("0-100","101-500", "501-2000", "2001-10000", "10001-19999","20000+"),ordered="T")
  # 
  data3 =data_2
  # Deuxième regroupement :
  # Power
  data3$Power = as.character(data3$Power)
  data3$Power[data_2$Power%in%levels(data_2$Power)[-c(1:3)]] = "3"
  data3$Power[data_2$Power%in%levels(data_2$Power)[2:3]] = "2"
  data3$Power[data_2$Power=="d"]="1"
  data3$Power=factor(data3$Power,ordered = F)
  # 
  # # CarAge
  # data3$CarAge = as.character(data_2$CarAge)
  # data3$CarAge[data_2$CarAge%in%levels(data_2$CarAge)[5:6]] = "13+"
  # data3$CarAge=factor(data3$CarAge, levels=c("0","1-3", "4-8","9-12","13+"),ordered = F)
  # 
  # # DriverAge
  # data3$DriverAge = as.character(data_2$DriverAge)
  # data3$DriverAge[data_2$Driver%in%levels(data_2$DriverAge)[4:5]] = "56-85"
  # data3$DriverAge=factor(data3$DriverAge,ordered = F)
  # 
  # Brand
  data3$Brand = as.character(data_2$Brand)
  data3$Brand[data_2$Brand!="F"] = "!F"
  data3$Brand = factor(data3$Brand)
  
  # Region
  data3$Region = as.character(data_2$Region)
  data3$Region[data_2$Region%in%c('R23', 'R31', 'R72','R54')] = "R23_31_72_54"
  data3$Region = factor(data3$Region)
  
  # Density
  # data3$Density = as.character(data_2$Density)
  # data3$Density[data3$Density%in%levels(data_2$Density)[2:5]]="101-19999"
  # data3$Density=factor(data3$Density,ordered = F,levels=c("0-100","101-19999","20000+"))
  # data3$Density = as.character(data3$Density)
  # data3$Density[data3$Density%in%levels(data3$Density)[2:3]]="101-19999"
  # data3$Density=factor(data3$Density,ordered = F)
  return(data3)
}

# Fusion des bases freq et sev
data = merge_freq_sev(freq, sev, "PolicyID")

# Changement de l'exposition : (capée à 0.25)
data_exp = change_expo_assure(data, expo_min = expo)
# Retraitement de la base:
data_r_2 = retraitement_base_2(data_exp)

# Séparation Ba et Bt
app_test_2 = separation_base_test_app(data_r_2,seed=2015)
app_2b = app_test_2$app
test_2 = app_test_2$test

# Ecretement (à 10K) de Ba
app_ecr_2 = ecretement_base_app(app_2b)

app_2 = app_ecr_2
test_2 = test_2
# Base des "sinsitres" 
app_sin_2 =data_sinistres(app_2)
app_sin_2_avant_ecret = data_sinistres(app_2b)
# Différence entre la valeur moyenne des sinistres de la Ba avant et après écretement
ecret_2 = mean(app_sin_2_avant_ecret$ClaimAmount/app_sin_2_avant_ecret$ClaimNb) - mean(app_sin_2$ClaimAmount/app_sin_2$ClaimNb) 



bst_gr = expand.grid(nrounds = 500, eta = 0.05, max_depth =2,
                     gamma = 0.3, colsample_bytree = 1, min_child_weight = 2, subsample = 0.5)
fit_freq_xgb_caret2_cont_best = fit_xgb_freq_caret(app_2, bst_gr, var_freq,"count:poisson")
pred_freq_xgb2 = predict(fit_freq_xgb_caret2_cont_best, test_2)
mse_freq_xgb2 = mse(pred_freq_xgb2, test_2$ClaimNb/test_2$Exposure)

# 
# caret_xgb_param_freq_1_cont <- expand.grid(nrounds = 120, eta = 0.1, max_depth = 4,
#                                            gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
# fit_freq_xgb_caret1_cont = fit_xgb_freq_caret(app_1_continue, caret_xgb_param_freq_1_cont, var_freq,"count:poisson")
# pred_freq_xgb1_cont = pred_xgb_freq_caret(fit_freq_xgb_caret1_cont, test_1_continue)
# mse_freq_xgb1_cont = mse(pred_freq_xgb1_cont, target_freq(test_1_continue))
# mae_freq_xgb1_cont = mae(pred_freq_xgb1_cont, target_freq(test_1_continue))
# 
# caret_xgb_param_cout_1_cont <- expand.grid(nrounds = 120, eta = 0.1, max_depth = 4,
#                                            gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
# fit_cout_xgb_caret1_cont = fit_xgb_cout_caret(data_sinistres(app_1_continue), caret_xgb_param_cout_1_cont, var_freq,"reg:gamma")
# pred_cout_xgb1_cont = pred_xgb_freq_caret(fit_cout_xgb_caret1_cont, data_sinistres(test_1_continue))
# mse_cout_xgb1_cont = mse(pred_cout_xgb1_cont, target_cout(test_1_continue[test_1_continue$ClaimAmount>0,]))
# mae_cout_xgb1_cont = mae(pred_cout_xgb1_cont, target_cout(test_1_continue[test_1_continue$ClaimAmount>0,]))

# cv_cont_freq = xgboost_cv_param(app_1_continue, tune_grid = tune_grid, type="frequence", objective = "count:poisson")
# cv_cont_cout = xgboost_cv_param(app_1_continue, tune_grid = tune_grid, type="cout", objective = "reg:gamma")
# bst_gr = expand.grid(nrounds = 500, eta = 0.05, max_depth =2,
#                      gamma = 0.3, colsample_bytree = 1, min_child_weight = 2, subsample = 0.5)
# fit_freq_xgb_caret1_cont_best = fit_xgb_freq_caret(app_1_continue, bst_gr, var_freq,"count:poisson")


# b) XGBoost coût ---------------------
# i) Ajustement avec caret -------
caret_xgb_param_cout_1 <- expand.grid(nrounds = 171, eta = 0.1, max_depth = 2,
                                      gamma = 0.2, colsample_bytree = 0.9, min_child_weight = 4, subsample = 0.4)
fit_cout_xgb_caret1 = fit_xgb_cout_caret(app_sin_1, caret_xgb_param_cout_1, var_freq,"reg:gamma")
pred_cout_xgb1 = pred_xgb_cout_caret(fit_cout_xgb_caret1, test_sin_1,ecret) 
mse_cout_xgb1 = mse(pred_cout_xgb1, target_cout(test_sin_1))
mae_cout_xgb1 = mae(pred_cout_xgb1, target_cout(test_sin_1))
# Gain (si >0) ou perte (si <0) par rapport au meilleur GLM correspondant
gain_mse_cout_xgb = -(mse_cout_xgb1-mse_cout_best)/mse_cout_best*100
gain_mae_cout_xgb = -(mae_cout_xgb1-mae_cout_best)/mae_cout_best*100

# ii) Ajustement avec xgboost ---------
xgboost_xgb_param = list(eta = 0.1, max_depth = 2,
                         gamma = 0.2, colsample_bytree = 0.9, min_child_weight = 4, subsample = 0.4)
fit_cout_xgb_xgboost1 =fit_xgb_cout_xgboost(app_sin_1, xgboost_xgb_param, nrounds = 171, var_freq, verbose=F)

# c) XGBoost total --------
pred_total_xgb1 = pred_freq_xgb1*pred_xgb_cout_caret(fit_cout_xgb_caret1, test_1, ecret)
mse_total_xgb1 = mse(pred_total_xgb1, test_1$ClaimAmount/test_1$Exposure)
mae_total_xgb1 = mae(pred_total_xgb1, test_1$ClaimAmount/test_1$Exposure)

###############################################################################################
######################## 5 - Interprétation des modèles #######################################
###############################################################################################

###############################################################################################
############################################ A - PDP ##########################################
###############################################################################################

# XGB -----
pdp_plot_all_var(fit_freq_xgb_caret1)
pdp_plot_all_var(fit_cout_xgb_caret1)

# GLM ------
pdp_plot_all_var(fit_freq_complet_2)
pdp_plot_all_var(fit_cout_best)
pdp_plot_all_var(fit_cout_complet)

# Comparaison avec les coeffs du GLM ----------
coef_all_var_glm_plot(fit_freq_complet_2)
coef_all_var_glm_plot(fit_cout_complet)
coef_all_var_glm_plot(fit_cout_best)


# Comparaison PDP des modèles XGB1 et XGB2 --------
# #var. cat
a1 = pdp_func(fit_freq_xgb_caret1, 1)
a2 = pdp_func(fit_freq_xgb_caret2_cont_best, 1)
g1 = ggplot()+geom_col(data=a1, aes(x=Power, y=yhat), alpha = 0.3, fill='green')+geom_col(data=a2, aes(x=Power,y=yhat), fill = 'blue',alpha = 0.2)+ggtitle("PDP")

d1 = pdp_func(fit_freq_xgb_caret1, 4)
d2 = pdp_func(fit_freq_xgb_caret2_cont_best, 4)
g4 = ggplot()+geom_col(data=d1, aes(x=Brand, y=yhat), alpha = 0.3, fill='green')+geom_col(data=d2, aes(x=Brand,y=yhat), fill = 'blue',alpha = 0.2)+ggtitle("PDP")

e1 = pdp_func(fit_freq_xgb_caret1, 5)
e2 = pdp_func(fit_freq_xgb_caret2_cont_best, 5)
g5 = ggplot()+geom_col(data=e1, aes(x=Gas, y=yhat), alpha = 0.3, fill='green')+geom_col(data=e2, aes(x=Gas,y=yhat), fill = 'blue',alpha = 0.2)+ggtitle("PDP")

f1 = pdp_func(fit_freq_xgb_caret1, 6)
f2 = pdp_func(fit_freq_xgb_caret2_cont_best, 6)
g6 = ggplot()+geom_col(data=f1, aes(x=Region, y=yhat), alpha = 0.3, fill='green')+geom_col(data=f2, aes(x=Region,y=yhat), fill = 'blue',alpha = 0.2)+ggtitle("PDP")

# Num.
# t = pdp_func(fit_freq_xgb_caret1,3)
aa = c(rep(0.11498132, 9), rep(0.06447068,16), rep(0.07132519,13), rep(0.06243255,30), rep(0.09734413,15))
c1 = data.frame(DriverAge = 18:100, yhat =aa)
c2 = pdp_func(fit_freq_xgb_caret2_cont_best,3)
g3 = ggplot()+geom_col(data=c1, aes(x=DriverAge, y=yhat), col="green")+geom_line(data=c2, aes(x=DriverAge,y=yhat),col='blue')

x = 0:50
y = c(0.06469860, rep(0.06794396,3), rep(0.07239219,5),rep( 0.08174252,4),rep(0.06425800, 38))
b1 = data.frame(CarAge=x,yhat = y)
b2=pdp_func(fit_freq_xgb_caret2_cont_best, 2)
g2=ggplot()+geom_col(data=b1, aes(x=CarAge, y=yhat), col="green")+geom_line(data=b2, aes(x=CarAge,y=yhat),col='blue')

# dens
x =c(seq(0,100,length.out= 30), seq(101,19999,length.out=30), seq(20000,30000,length.out=30))
rr=pdp_func(fit_freq_xgb_caret2_cont_best, 7)
ee = pdp_func(fit_freq_xgb_caret1, 7)

x =c(seq(0,100,length.out= 300),seq(101,19999,length.out=300), seq(20000,30000,length.out=300))
yy = c(rep(0.06031274, 300),rep(0.07541138,300), rep(0.09073102,300))
ee=data.frame(Density =x, yhat = yy)
g7=ggplot()+geom_col(data=ee, aes(x=Density, y=yhat), col="green")+geom_line(data=rr, aes(x=Density,y=yhat),col='blue')

grid.arrange(g1,g2,g3,g4,g5,g6,g7)
x2 =c(seq(0,100,length.out= 1000),seq(101,200,length.out=1000))
yy2 = c(rep(0.06031274, 1000),rep(0.07541138,1000))

ee2 = data.frame(Density =x2, yhat = yy2)
g8 = ggplot()+geom_col(data=ee2, aes(x=Density, y=yhat), fill="green", alpha =1)+ggtitle("PDP Densité XGB frequence 1","Zoom sur la zone [0,200] ")

grid.arrange(g1,g2,g3,g4)
grid.arrange(g5,g6,g7,g8)


###############################################################################################
############################################ B - ALE ##########################################
###############################################################################################

par(mfrow=c(3,3))
ALE_plot_all_var(fit_freq_xgb_caret1, app_1)
ALE_plot_all_var(fit_freq_complet_2, app_1)
# Importance des variables
# Avec caret
imp_var_plot(fit_freq_xgb_caret)
imp_var_plot(fit_freq_complet_2)
# Avec DALEX
imp_var_dalex(fit_freq_xgb_caret, app_1, "frequence", 10000)
imp_var_dalex_plot(fit_freq_xgb_caret, app_1, "frequence", 10000)
imp_var_dalex_stabilite(fit_freq_xgb_caret, app_1, "frequence", 10000, 30)


###############################################################################################
########################### C - Comparaison 2 assurés #########################################
###############################################################################################

# On prend deux assurés aléatoirement
assure = data.frame(Power = factor(2), CarAge = factor("1-3"),
                    DriverAge = factor("43-55"), Brand = factor("!F"), Gas = factor("Diesel"),
                    Region = factor("R23_31_72_54"), Density = factor("101-19999"), Exposure =1)
assure2=assure
assure2$Power=factor(3)
aa=app_1%>%filter(Power==2, CarAge=="1-3", DriverAge=="43-55" & Brand=="!F" & Gas=="Diesel" & Region == "R23_31_72_54"& Density=="101-19999")
ind_ass1=which(app_1$PolicyID== aa$PolicyID[5])
bb=app_1%>%filter(Power==3, CarAge=="1-3", DriverAge=="43-55" & Brand=="!F" & Gas=="Diesel" & Region == "R23_31_72_54"& Density=="101-19999")
ind_ass2  = which(app_1$PolicyID== bb$PolicyID[1]) # indice de l'assuré 2 (avec expo=1)

# Prédiction assuré 1
# FREQ
pred_ass_1_freq_glm = predict(fit_freq_complet_2, assure,type="response")
pred_ass_1_freq_xgb = predict(fit_freq_xgb_caret1, assure)

# COUT
pred_ass_1_cout_glm = predict(fit_cout_best, assure,type="response")
pred_ass_1_cout_xgb = predict(fit_cout_xgb_caret1, assure)

# Total
pred_ass_1_total_glm = pred_ass_1_freq_glm*pred_ass_1_cout_glm
pred_ass_1_total_xgb = pred_ass_1_freq_xgb*pred_ass_1_cout_xgb

# Prédiction assuré 2 :
# FREQ
pred_ass_2_freq_glm = predict(fit_freq_complet_2, assure2,type="response")
pred_ass_2_freq_xgb = predict(fit_freq_xgb_caret1, assure2)

# COUT
pred_ass_2_cout_glm = predict(fit_cout_best, assure2,type="response")
pred_ass_2_cout_xgb = predict(fit_cout_xgb_caret1, assure2)

# Total
pred_ass_2_total_glm = pred_ass_2_freq_glm*pred_ass_2_cout_glm
pred_ass_2_total_xgb = pred_ass_2_freq_xgb*pred_ass_2_cout_xgb

###############################################################################################
##################################### D - H-statistique #######################################
###############################################################################################

# !!! temps d'exécution long !!! 

ap_0 = app_1[sample(1:nrow(app_1),10000),]
fit_0_caret = fit_xgb_freq_caret(ap_0, caret_xgb_param_freq_1, var_freq,"count:poisson")

pr = Predictor$new(fit_0_caret, ap_0[,var_freq], y = ap_0$ClaimNb/ap_0$Exposure)
ia = Interaction$new(pr)
ia$plot()
ia2 = Interaction$new(pr, feature=var_freq[1], grid = 10)

app_1[ind_ass1,]
app_1[ind_ass2,]

lime_plot(fit_freq_xgb_caret1, app_1, ind_ass1,10000, n_features = 4)
lime_stabilite_2(fit_freq_xgb_caret1, app_1, ind_ass1,10000, n_features = 4)

x <- data.frame(glm=pred_total_glm_best, xgb1 = pred_total_xgb1)

grid.arrange(g1,g2,ncol=2)


###############################################################################################
################################ E - Sensibilité découpage de la base #########################
###############################################################################################

seed_new = 2018
# Pour avoir les variables numériques inchangées : 
# prendre data_r = data_r_2 ( = retraitement_base_2(data_exp) )
# Pour avoir les données retraitées : 
# prendre data_r = data_r ( = retraitement_base(data_exp) )
separation_func = function(seed_new = 2018, dat = data_r){
  app_test_new = separation_base_test_app(dat,seed=seed_new)
  app_new = app_test_new$app
  test_new = app_test_new$test
  
  # Ecretement (à 10K) de Ba
  app_ecr_new = ecretement_base_app(app_new)
  app_1_new = app_ecr_new
  test_1_new = test_new
  # Base des "sinsitres" 
  app_sin_1_new =data_sinistres(app_1_new)
  app_sin_1_avant_ecret_new = data_sinistres(app_new)
  # Différence entre la valeur moyenne des sinistres de la Ba avant et après écretement
  ecret_new = mean(app_sin_1_avant_ecret_new$ClaimAmount/app_sin_1_avant_ecret_new$ClaimNb) - mean(app_sin_1_new$ClaimAmount/app_sin_1_new$ClaimNb) 
  return(list(ecret = ecret_new, app_new = app_1_new, test_new = test_1_new))
}
temp = separation_func(seed_new, data_r)
ecret_new = temp$ecret
app_new = temp$app_new
test_new = temp$test_new

all_model_new = function(seed_new = 2018, dat = data_r){
  temp = separation_func(seed_new, dat)
  ecret_new = temp$ecret
  app_new = temp$app_new
  test_new = temp$test_new
  mat = matrix(0, ncol = 6, nrow = 9)
  rownames(mat) = c("XGB freq", "XGB Coût ","XGB Total",
                    "GLM freq Trivial","GLM Coût Trivial", "GLM Total Trivial", 
                    "GLM freq Best","GLM Coût Best", "GLM Total Best")
                    
  colnames(mat) = c("MAE", "MSE", "RMSE", "RMSE_mean", "RMSE_min_max", "RMSE_q")
  yy = test_new$ClaimNb/test_new$Exposure
  zz = data_sinistres(test_new)$ClaimAmount/data_sinistres(test_new)$ClaimNb
  ww = test_new$ClaimAmount/test_new$Exposure
  # XGB freq
  bst_gr = expand.grid(nrounds = 120, eta = 0.1, max_depth = 4,
                       gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5) 
  fit_freq_xgb_caret2_cont_best_new = fit_xgb_freq_caret(app_new, bst_gr, var_freq,"count:poisson")
  pr = predict(fit_freq_xgb_caret2_cont_best_new, test_new)
  mse = mse(pr,yy)
  mae = mae(pr,yy)
  rmse = rmse(pr,yy)
  nrmsd_min_max = nrmsd(yy, pr, "min_max")
  nrmsd_q = nrmsd(yy, pr, "Q1_Q3")
  nrmsd_mean = nrmsd(yy, pr, "mean")
  mat[1,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)
  
  # XGB cout
  caret_xgb_param_cout_new <- expand.grid(nrounds = 171, eta = 0.1, max_depth = 2,
                                        gamma = 0.2, colsample_bytree = 0.9, min_child_weight = 4, subsample = 0.4)
  
  fit_cout_xgb_caret1_new = fit_xgb_cout_caret(data_sinistres(app_new), caret_xgb_param_cout_new, var_freq,"reg:gamma")
  pr2 = pred_xgb_cout_caret(fit_cout_xgb_caret1_new, data_sinistres(test_new),ecret_new)
  mse = mse(pr2,zz)
  mae = mae(pr2,zz)
  rmse = rmse(pr2,zz)
  nrmsd_min_max = nrmsd(zz, pr2, "min_max")
  nrmsd_q = nrmsd(zz, pr2, "Q1_Q3")
  nrmsd_mean = nrmsd(zz, pr2, "mean")
  mat[2,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)
  ## 
  pr2 = pred_xgb_cout_caret(fit_cout_xgb_caret1_new, test_new,ecret_new)
  mse = mse(pr2*pr,ww)
  mae = mae(pr2*pr,ww)
  rmse = rmse(pr2*pr,ww)
  nrmsd_min_max = nrmsd(ww, pr2*pr, "min_max")
  nrmsd_q = nrmsd(ww, pr2*pr, "Q1_Q3")
  nrmsd_mean = nrmsd(ww, pr2*pr, "mean")
  mat[3,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)
  # GLM freq 
  #trivial
  fit_freq_moy_new = glm(ClaimNb/Exposure~1,
                               data = app_new, weights = app_new$Exposure,family=poisson(link="log"))
  pr = predict(fit_freq_moy_new, test_new, type="response")
  mse = mse(pr,yy)
  mae = mae(pr,yy)
  rmse = rmse(pr,yy)
  nrmsd_min_max = nrmsd(yy, pr, "min_max")
  nrmsd_q = nrmsd(yy, pr, "Q1_Q3")
  nrmsd_mean = nrmsd(yy, pr, "mean")
  mat[4,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)
  #GLM Cout
  fit_cout_moy_new = glm(ClaimAmount/ClaimNb~1, weights = data_sinistres(app_new)$ClaimNb,
                         data = data_sinistres(app_new), family=Gamma(link="log"))
  pr2 = pred_cout_glm(fit_cout_moy_new, data_sinistres(test_new), ecret_new)
  mse = mse(pr2,zz)
  mae = mae(pr2, zz)
  rmse = rmse(pr2,zz)
  nrmsd_min_max = nrmsd(zz, pr2, "min_max")
  nrmsd_q = nrmsd(zz, pr2, "Q1_Q3")
  nrmsd_mean = nrmsd(zz, pr2, "mean")
  mat[5,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)
  # Total
  pr2 = pred_cout_glm(fit_cout_moy_new, test_new, ecret_new)
  mse = mse(pr2*pr,ww)
  mae = mae(pr2*pr,ww)
  rmse = rmse(pr2*pr,ww)
  nrmsd_min_max = nrmsd(ww, pr2*pr, "min_max")
  nrmsd_q = nrmsd(ww, pr2*pr, "Q1_Q3")
  nrmsd_mean = nrmsd(ww, pr2*pr, "mean")
  mat[6,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)
  #complet
  fit_freq_complet_2_new = glm(ClaimNb/Exposure~Power+CarAge+DriverAge+Gas+Brand+Region+Density,
                           data = app_new, weights = app_new$Exposure,family=poisson(link="log"))
  pr = predict(fit_freq_complet_2_new, test_new, type="response")
  mse = mse(pr,yy)
  mae = mae(pr,yy)
  rmse = rmse(pr,yy)
  nrmsd_min_max = nrmsd(yy, pr, "min_max")
  nrmsd_q = nrmsd(yy, pr, "Q1_Q3")
  nrmsd_mean = nrmsd(yy, pr, "mean")
  mat[7,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)
  fit_cout_best_new = glm(ClaimAmount/ClaimNb~Power+CarAge+DriverAge+Gas+Brand+Density, weights = data_sinistres(app_new)$ClaimNb,
                          data = data_sinistres(app_new), family=Gamma(link="log"))
  pr2 = pred_cout_glm(fit_cout_best_new,data_sinistres(test_new), ecret_new)
  mse = mse(pr2,zz)
  mae = mae(pr2,zz)
  rmse = rmse(pr2,zz)
  nrmsd_min_max = nrmsd(zz, pr2, "min_max")
  nrmsd_q = nrmsd(zz, pr2, "Q1_Q3")
  nrmsd_mean = nrmsd(zz, pr2, "mean")
  mat[8,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)
  # Total
  pr2 = pred_cout_glm(fit_cout_best_new,test_new, ecret_new)
  mse = mse(pr2*pr,ww)
  mae = mae(pr2*pr,ww)
  rmse = rmse(pr2*pr,ww)
  nrmsd_min_max = nrmsd(ww, pr2*pr, "min_max")
  nrmsd_q = nrmsd(ww, pr2*pr, "Q1_Q3")
  nrmsd_mean = nrmsd(ww, pr2*pr, "mean")
  mat[9,] = c(mae, mse, rmse, nrmsd_mean, nrmsd_min_max, nrmsd_q)

  # fit_cout_complet_new = glm(ClaimAmount/ClaimNb~Power+CarAge+DriverAge+Gas+Brand+Region+Density, weights = data_sinistres(app_new)$ClaimNb,
  #                        data = data_sinistres(app_new), family=Gamma(link="log"))
  # pr = pred_cout_glm(fit_cout_complet_new, data_sinistres(test_new), ecret_new)
  # mse = mse(pr,zz)
  # mae = mae(pr,zz)
  # mat[7,1:2] = c(mse,mae)
  
  mat
  
}
temp = all_model_new(2019, data_r)
fr = temp[stringr::str_detect(rownames(temp), "freq"),]
cou = temp[stringr::str_detect(rownames(temp), "Coût"),]
tot = temp[stringr::str_detect(rownames(temp), "Total"),]
# Calcul le gain par rapport à un modèle de référence 
gain_tab = function(seed_new =2019, dat  = data_r, ref = "GLM_trivial"){
  temp = all_model_new(seed_new, dat)
  temp = temp[,-ncol(temp)]
  fr = temp[stringr::str_detect(rownames(temp), "freq"),]
  cou = temp[stringr::str_detect(rownames(temp), "Coût"),]
  tot = temp[stringr::str_detect(rownames(temp), "Total"),]
  ref2 = ifelse(ref=="GLM_trivial",2,3)
  temp2 = rbind(fr,cou,tot)
  # if(ref =="GLM_trivial"){
  #   ref2 = 2
  # }
  reff = rbind(rbind(fr[ref2,],fr[ref2,], fr[ref2,]),
    rbind(cou[ref2,],cou[ref2,],cou[ref2,]),
    rbind(tot[ref2,], tot[ref2,], tot[ref2,]))
  res=(reff-temp2)/temp2
  colnames(res) = colnames(temp2)
  rownames(res) = rownames(temp2)
  return(formattable::percent(round(1+res,4)))
}

# r3=all_model_new(5,data_r)
# xtable(r3[c(1,4,7,2,5,8,3,6,9),-ncol(r3)], digits=4)
# t3=gain_tab(8, data_r, "GLM_trivial")
# xtable(as.character(t3[,-c(4,5)]))

tab = cbind(all_model_new(2014, data_r),all_model_new(2015, data_r), all_model_new(2016, data_r),
            all_model_new(2017, data_r), all_model_new(2018, data_r))
vec = as.vector(tab) 
c1 = rep(c(rep("XGB",3), rep("GLM Trivial",3), rep("GLM Best",3)), ncol(tab))
c2 = rep(rep(c("freq", "cout", "total"),3),ncol(tab))
c3 = c(rep("mse", 9), rep("mae", 9), rep("mse", 9), rep("mae", 9), rep("mse", 9), rep("mae", 9),
       rep("mse", 9), rep("mae", 9), rep("mse", 9), rep("mae", 9))
c4 = c(rep(1,9), rep(2, 9), rep(3, 9), rep(4,9), rep(5,9))
df = data.frame(res = vec, glm_xgb = c1, freq_cout_total = c2, mse_mae = c3, num = c4)     
ggplot(df, aes(x = num, y = vec, col = glm_xgb ))+geom_point()+ facet_wrap(~freq_cout_total)+facet_grid(~mse_mae)

df_1 = df[df$mse_mae=="mse"& df$freq_cout_total =="freq",]
df_1$res[df_1$glm_xgb=="GLM Best" | df_1$glm_xgb=="GLM Trivial"] = df_1$res[df_1$glm_xgb=="GLM Best"| df_1$glm_xgb=="GLM Trivial"] + rep(0.001,ncol(df_1[df_1$glm_xgb=="GLM Best",]))
g1=ggplot(df_1, aes(x = num, y = res, col = glm_xgb))+geom_point()+geom_line()+xlab("Echantillonnage de la base")+ylab("MSE fréquence")
df_1_mae = df[df$mse_mae=="mae"& df$freq_cout_total =="freq",]
g2=ggplot(df_1_mae, aes(x = num, y = res, col = glm_xgb))+geom_point()+geom_line()+ylim(c(0.11,0.15))+xlab("Echantillonnage de la base")+ylab("MAE fréquence")
grid.arrange(g1,g2)
df_2 = df[df$mse_mae=="mse"& df$freq_cout_total =="cout",]
ggplot(df_2, aes(x = num, y = res, col = glm_xgb))+geom_point()+geom_line()
df_3 = df[df$mse_mae=="mse"& df$freq_cout_total =="total",]
ggplot(df_3, aes(x = num, y = res, col = glm_xgb))+geom_point()+geom_line()
xtable(r4[,-ncol(r4)], digits=4)


###############################################################################################
################################ F - Comparaison des 2 modèles ################################
###############################################################################################

# Densité des prédictions réalisées ---------------

library(ggplot2);library(reshape2)
data<- melt(x)
g1 = ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+ggtitle("Densité des prédictions totales faîtes par les différents modèles")
# ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)
g2 = ggplot(data,aes(x=variable, y=value, fill=variable)) + geom_boxplot()+ggtitle("Boxplot des prédictions totales des différents modèles")
# grid.arrange(g1,g2,ncol=2)

x2 <- data.frame(glm=pred_cout_best, xgb1 = pred_cout_xgb1)
data<- melt(x2)
g3 = ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+ggtitle("Densité des prédictions de sévérité faîtes par les différents modèles")
# ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)
g4 = ggplot(data,aes(x=variable, y=value, fill=variable)) + geom_boxplot()+ggtitle("Boxplot des prédictions de sévérité des différents modèles")

x3 <- data.frame(glm=pred_freq_complet, xgb1 = pred_freq_xgb1)
data<- melt(x3)
g5 = ggplot(data,aes(x=value, fill=variable)) + geom_density(alpha=0.25)+ggtitle("Densité des prédictions en fréquence faîtes par les différents modèles")
# ggplot(data,aes(x=value, fill=variable)) + geom_histogram(alpha=0.25)
g6 = ggplot(data,aes(x=variable, y=value, fill=variable)) + geom_boxplot()+ggtitle("Boxplot des prédictions en fréquence des différents modèles")

grid.arrange(g5,g6, g3,g4, g1,g2, ncol= 2, nrow=3)

# Primes pures --------------------------------

dff=data.frame(x = 1:nrow(test_1),r1 = sort(pred_total_glm_best), r2 = (pred_total_xgb1)[order(pred_total_glm_best)])
gt1 = ggplot()+geom_point(data=dff, aes(x=x, y =r1),col='blue', size = 1.5)+
  geom_point(data=dff, aes(x=x, y =r2),col='red', alpha = 0.5)+
  xlab("Assurés")+ylab("Prime pure estimée")+ggtitle("Prime pures estimées par les modèles GLM et XGBoost")

# geom_violin
f = (pred_total_glm_best - pred_total_xgb1)/pred_total_glm_best
g = data.frame(f=f)
g$x = rep(factor(1), nrow(g))
gt2 = ggplot(g, aes(x = x, y = f))+geom_violin()+ylab("Erreur Relative")+xlab("")+ggtitle("Erreur relative entre les primes pures du GLM et du XGBoost")

df2 = data.frame(group = rep(factor(1),2*nrow(test)), type = c(rep("GLM", nrow(test)),rep("XGB(1)", nrow(test))), val = c(pred_total_glm_best, pred_total_xgb1))
dodge <- position_dodge(width = 1)
gt3 = ggplot(df2, aes(x = group, y = val, fill = type))+geom_violin(position = dodge)+
  geom_boxplot(width=.1, outlier.colour=NA, position = dodge) +xlab("Modèle")+ylab("Prime Pure")+ggtitle("Primes Pures")

# Analyse différence GLM et XGB ------
gg = test_1%>%mutate(pred_total_xgb1 = pred_total_xgb1, pred_total_glm_best = pred_total_glm_best)%>%mutate (diff = (pred_total_xgb1 - pred_total_glm_best)/pred_total_glm_moy)
g1 = ggplot(gg, aes(x = Power, y = diff))+geom_violin()
g2 = ggplot(gg, aes(x = CarAge, y = diff))+geom_violin()
g3 = ggplot(gg, aes(x = DriverAge, y = diff))+geom_violin()
g4 = ggplot(gg, aes(x = Brand, y = diff))+geom_violin()
g5 = ggplot(gg, aes(x = Gas, y = diff))+geom_violin()
g6 = ggplot(gg, aes(x = Region, y = diff))+geom_violin()
g7 = ggplot(gg, aes(x = Density, y = diff))+geom_violin()


#####################################################################################
#######################G - Stabilité BreakDown et SHAP ##############################
#####################################################################################

# Sur les assurés 1 et 2
# SHAP ------
expl = DALEX::explain(fit_freq_xgb_caret1, data = app_1[,var_freq], y = target_freq(app_1))
a1 = iBreakDown::shap(expl, app_1[ind_ass1,var_freq], B=25)
plot(a1)

a2 = iBreakDown::shap(expl, app_1[ind_ass2,var_freq], B=15)

# Breakdown --------

# !!! temps d'exécution long !!! 

b1 = break_down_uncertainty(expl, app_1[ind_ass1,var_freq], B = 10)
plot(b1)
b2 = break_down_uncertainty(expl, app_1[ind_ass2,var_freq], B = 10)
plot(b2)

#####################################################################################
####################################### I - LIME ####################################
#####################################################################################

# Lime avec le pcakge iml
grid.arrange(lime_iml_plot(fit_freq_complet_2, app_1, ind_obs=ind_ass1, n_features = 4)+theme(axis.text.y = element_text(size=18))+
               theme(axis.title.x = element_text(size=14))+
               theme(axis.text.x = element_text(size=14)),lime_iml_plot(fit_freq_complet_2, app_1, ind_obs=ind_ass2, n_features = 4)+theme(axis.text.y = element_text(size=18))+
               theme(axis.title.x = element_text(size=14))+
               theme(axis.text.x = element_text(size=14)), ncol= 2)

# LIME avec le package lime
# Assuré 1
a1=lime_plot(fit_freq_xgb_caret1, app_1, ind_ass1,n_features = 4)+theme_bw()+scale_fill_stata()+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
  theme(axis.text.x = element_text(size=14))
a2 = lime_iml_plot(fit_freq_xgb_caret1, app_1, "frequence", ind_ass1,n_features=4)+theme_bw()+scale_fill_stata()+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
  theme(axis.text.x = element_text(size=14))
a3 = lime_stabilite(fit_freq_xgb_caret1, app_1, ind_ass1,n_features = 4)+theme_bw()+scale_fill_stata()+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
  theme(axis.text.x = element_text(size=14))
a4 = lime_stabilite_2(fit_freq_xgb_caret1, app_1, ind_ass1,n_features = 4)+theme_bw()+scale_fill_stata()+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
  theme(axis.text.x = element_text(size=14))
grid.arrange(a1, a2, a3, a4)

# Assuré 2
a1=lime_plot(fit_freq_xgb_caret1, app_1, ind_ass2,n_features = 4)+theme_bw()+scale_fill_stata()+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
  theme(axis.text.x = element_text(size=14))
a2 = lime_iml_plot(fit_freq_xgb_caret1, app_1, "frequence", ind_ass2,n_features=4)+theme_bw()+scale_fill_stata()+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
  theme(axis.text.x = element_text(size=14))
a3 = lime_stabilite(fit_freq_xgb_caret1, app_1, ind_ass2,n_features = 4)+theme_bw()+scale_fill_stata()+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
  theme(axis.text.x = element_text(size=14))
a4 = lime_stabilite_2(fit_freq_xgb_caret1, app_1, ind_ass2,n_features = 4)+theme_bw()+scale_fill_stata()+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
  theme(axis.text.x = element_text(size=14))
grid.arrange(a1, a2, a3, a4)


#####################################################################################
################ I - Etude Lime, SHAP, Breakdown sur des assurés proches ############
#####################################################################################
similar_func = function(ap, ind, ass2){
  # Fonction permettant de trouver deux assurés ayant toutes les mêmes caractéristiques sauf 1
  if(sum(!ap[ind,var_freq]==ass2[1:7])==1){
    return(ind)
  }
  else{
    return (NULL)}
}

unlist(lapply(c(1:10000),similar_func, ap = app_1[1:10000,], ass2 = assure2))
# Comparaison des méthodes LIME et SHAP sur deux assurés proches

grid.arrange(lime_plot(fit_freq_xgb_caret1, app_1, ind_ass2, n_features = 7)+
               theme_bw()+scale_fill_stata()+ggtitle("Assuré 2")+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
               theme(axis.text.x = element_text(size=14)),
             lime_plot(fit_freq_xgb_caret1, app_1, 8851, n_features = 7)+theme_bw()+
               scale_fill_stata()+ggtitle("Assuré 3")+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
               theme(axis.text.x = element_text(size=14)),
             lime_stabilite_2(fit_freq_xgb_caret1, app_1, ind_ass2, n_features = 7)+
               theme_bw()+scale_fill_stata()+stat_summary(fun.y=mean, colour="darkred", geom="point",
                                                          shape=18, size=3,show_guide = FALSE)+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
               theme(axis.text.x = element_text(size=14)),
             lime_stabilite_2(fit_freq_xgb_caret1, app_1, 8851, n_features = 7)+
               theme_bw()+scale_fill_stata()+stat_summary(fun.y=mean, colour="darkred", geom="point",
                                                          shape=18, size=3,show_guide = FALSE)+theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+
               theme(axis.text.x = element_text(size=14)) ,ncol=2)

# Comparaison des méthodes SHAP et Breakdown sur deux assurés proches

# !!! temps d'exécution long !!! 

expl = DALEX::explain(fit_freq_xgb_caret1, data = app_1[,var_freq], y = target_freq(app_1))
a1 = break_down_uncertainty(expl, app_1[ind_ass1,var_freq], B=10)
a2 = iBreakDown::shap(expl, app_1[ind_ass1,var_freq], B=10)
b1 = break_down_uncertainty(expl, app_1[ind_ass2,var_freq], B=10)
b2 = iBreakDown::shap(expl, app_1[ind_ass2,var_freq], B=10)

t1=plot(a1)+theme_bw()+scale_fill_stata()+ggtitle("BreakDown Assuré 1")+
  theme(axis.title.x = element_text(size=14), title = element_text(size=16), axis.text.y = element_text(size=18), axis.text.x = element_text(size=14))

t2=plot(a2)+scale_fill_stata()+ggtitle("SHAP Assuré 1")+
  theme(axis.title.x = element_text(size=14), title = element_text(size=16), axis.text.y = element_text(size=18), axis.text.x = element_text(size=14))

t3=plot(a1)+theme_bw()+scale_fill_stata()+ggtitle("BreakDown Assuré 2")+
  theme(axis.title.x = element_text(size=14), title = element_text(size=16), axis.text.y = element_text(size=18), axis.text.x = element_text(size=14))

t4=plot(a2)+scale_fill_stata()+ggtitle("SHAP Assuré 2")+
  theme(axis.title.x = element_text(size=14), title = element_text(size=16), axis.text.y = element_text(size=18), axis.text.x = element_text(size=14))
grid.arrange(t1,t2, t3, t4,ncol=2)

#####################################################################################
################ J - Comparaison temps d'exécution Lime et Shap ####################
#####################################################################################
temps_execution_shap = function(model, dat_app, type="frequence", ind_obs = 1, sample_size=100){
  aa = Sys.time()
  shap_iml_func(model, dat_app, type, ind_obs, sample_size)
  return(as.numeric(Sys.time()-aa))
}

temps_execution_shap(fit_freq_xgb_caret1, app_1, ind_obs=ind_ass2,sample_size = 100)
nn = c(10, 20, 50, 80, 100, 200, 500,1000, 2000, 5000)
vec = unlist(lapply(nn, temps_execution_shap, model = fit_freq_xgb_caret1, type="frequence", dat_app = app_1, ind_obs = 1))
g2 = ggplot(data.frame(x=nn[-length(nn)],y=vec[-length(vec)]), aes(x=x, y=y))+geom_point()+geom_smooth()+
  xlab("Taille de l'échantillon")+ylab("Temps d'exécution (secondes)")+ggtitle("Temps d'exécution methode SHAP")

temps_execution_lime = function(model, dat_app, ind_obs = 1, 
                                n_permutations = 5000, dist_fun = "gower", kernel_width = 1, n_features = 4, Nsimu = 100){
  aa = Sys.time()
  lime_stabilite_2(model, dat_app, ind_obs, n_permutations, dist_fun, kernel_width, n_features, Nsimu )
  return(as.numeric(Sys.time()-aa))
}


#########################################################################################
################################# CONCURRENCE ###########################################
#########################################################################################
# Dans cette partie on se place dans un marché concurrentiel entre deux assureurs
# Le premier assureur utilise le GLM pour tarifer l'assurance auto
# Le deuxième utilie le XGBoost (1)
# On analyse le Chiffres d'affaires, le Loss ratio, etc. obtenu suivant le critère retenu
# pour le choix des assurés pour aller chez un des deux assureurs
# On retient : l'assuré va chez le moins cher

pred_ass_1 = pred_total_glm_best
pred_ass_2 = pred_total_xgb1
# test_1 = get(load("test_1.RData"))
# Renvoie le montat total réel des sinistres des assurés
sinistres = function(test){
  return(sum(test$ClaimAmount/test$Exposure))
}




# Renvoie le chiffre d'affaires théorique d'un assureur si il n'y avait pas de concurrence
CA_sans_concurrence = function(pred){
  sum(pred)
}

# renvoie les indices des assurés qui ont choisi l'assureur 1 , et de ceux qui ont choisi l'assureur 2
# suivant un des critères suivants:
# - moins_cher : l'assuré va chez l'assureur le moins cher
# - plus_cher : l'assuré va chez l'assureur le plus proche en prédiction de la valeur théorique 
# de ClaimAmount/Exposure en valeur absolue 
ou_va_assure_func = function(pred_ass_1, pred_ass_2, test = test_1, critere = c("plus_proche", "moins_cher")){
  y_th = test$ClaimAmount/test$Exposure
  if(critere=="moins_cher"){
    min = pmin(pred_ass_1,pred_ass_2)
    ind_1 = which(min==pred_ass_1)
    ind_2 = which(min==pred_ass_2)
  }
  if(critere=="plus_proche"){
    min = pmin(abs(pred_ass_1-y_th), abs(pred_ass_2-y_th))
    ind_1 = which(min==abs(pred_ass_1-y_th))
    ind_2 = which(min==abs(pred_ass_2-y_th))
  }
  list(ind_1 = ind_1, ind_2 = ind_2)
}

###
inds = ou_va_assure_func(pred_ass_1, pred_ass_2, test_1, "moins_cher")
ind_1 = inds$ind_1
ind_2 = inds$ind_2
###
dff = data.frame(ass1 = length(ind_1), ass2 = length(ind_2))

# renvoie Chiffre d'affaires, Loss ratio, résultats, sinistres des deux assureurs
CA_LR_res_sin_func = function(pred_ass_1,ind_1, pred_ass_2, ind_2, test = test_1){
  sin = c(sum(sinistres(test[ind_1,])),sum(sinistres(test[ind_2,])))
  CA = c(sum(pred_ass_1[ind_1]),sum(pred_ass_2[ind_2]))
  loss_ratio = sin/CA
  res = CA - sin
  data.frame(sin = sin, CA = CA, loss_ratio = loss_ratio, res = res)
}

###
df_resu = CA_LR_res_sin_func(pred_ass_1, ind_1, pred_ass_2, ind_2, test_1)
df_resu


# Analyse la répartition des assurés chez les deux assureurs
analyse_repartition_concurrence = function(test, ind_1, ind_2,pred_1,pred_2, ind_var){
  # ind_1 : indice des assurés dans "test" pris par l'assureur 1
  # ind_2 : indice des assurés dans "test" pris par l'assureur 2
  # test : base de test
  # ind_var : indice de la variable d'intérêt (si ind_var = 1 => "Power")
  var = var_freq[ind_var]
  a1=test%>%group_by_(var_freq[ind_var])%>%summarise(n=mean(ClaimAmount/Exposure))
  vec1 = c(a1$n,mean(test_1$ClaimAmount/test_1$Exposure))
  df2 = data.frame(var=test[,which(colnames(test)==var)], pred_ass_1 = pred_1, pred_ass_2 = pred_2)
  a2=df2%>%group_by(var)%>%summarise(n=mean(pred_ass_1))
  vec2=c(a2$n, mean(df2$pred_ass_1))
  a3=df2%>%group_by(var)%>%summarise(n=mean(pred_ass_2))
  vec3=c(a3$n, mean(df2$pred_ass_2))
  
  brand_1=summary(as.factor(test[ind_1,which(colnames(test)==var)]))
  brand_2=summary(as.factor(test[ind_2,which(colnames(test)==var)]))
  vec4 = c(brand_1, sum(brand_1))
  vec5 = c(brand_2, sum(brand_2))
  vec6=vec4/(vec4+vec5)*100
  vec7=vec5/(vec4+vec5)*100
  mat = rbind(vec1, vec2, vec3, vec4, vec5,vec6,vec7)
  colnames(mat) = c(names(brand_1), "total")
  rownames(mat) = c("CM théo", "CM pred_ass1", "CM pred_ass2", "Nb assureur 1", "Nb assureur 2", "répartition(%) par modalité ass1","répartition(%) par modalité ass2")
  round(mat,1)
}
test_1_ind_1 = test_1[ind_1,]
test_1_ind_2 = test_1[ind_2,]
target_total = function(test){
  return(test$ClaimAmount/test$Exposure)
}
# On ordonne selon les prédictions les plus proches de la réalité
ord_1 = order(abs(pred_ass_1[ind_1] - target_total(test_1_ind_1)))
ord_2 = order(abs(pred_ass_2[ind_2] - target_total(test_1_ind_2)))
ord_glm = order(abs(pred_ass_1-target_total(test_1)))
ord_xgb = order(abs(pred_ass_2-target_total(test_1)))

test_1[head(ord_xgb),]%>%mutate(y = ClaimAmount/Exposure)
pred_ass_1[head(ord_xgb)]
pred_ass_2[head(ord_xgb)]

# Application de lime sur les assurés les mieux prédits par le XGB
lime_plot(fit_cout_xgb_caret1, test_1, ord_xgb[1:2])


