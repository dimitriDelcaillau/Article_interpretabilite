#### Chargement des données et des packages ####

#Packages
Packages <- c("CASdatasets", "dplyr", "data.table", "ggplot2","gridExtra", "stringr","MASS","Metrics",
              "DescTools","plotrix", "reshape2", "xgboost","ranger","caret","mlr","Metrics","Matrix","forcats",
              "lime", "iml", "shapleyR", "ALEPlot","breakDown","condvis","DALEX","iBreakDown","ICEbox","plotly",
              "breakDown", "ICEbox", "ingredients", "localModel", "pdp","shapper", "vip", "xtable", "ggthemes")
a=lapply(Packages, library, character.only = TRUE)

Packages = c("CASdatasets", "dplyr", "ggplot2", "gridExtra", "cartography", "RColorBrewer", "reshape2", "forcats", "rpart", "rpart.plot", "Hmisc")
a=suppressMessages(suppressWarnings(lapply(Packages, library, character.only = TRUE)))

# Données
data("freMTPL2freq")
str(freMTPL2freq)
freq=freMTPL2freq
freq2 = freq%>%mutate(PolicyID = IDpol, Power = VehPower, DriverAge = DrivAge, Brand = VehBrand,
                      Gas = VehGas, CarAge = VehAge)
freq2$VehPower =NULL;freq2$VehAge=NULL; freq2$VehBrand = NULL; freq2$VehGas = NULL; freq2$DrivAge = NULL;freq2$IDpol = NULL
var_freq = colnames(freq2)[-c(1,2,7)]
#Variables explicatives
# 1. IDpol [PolicyID]: policy number (unique identifier);
# 2. ClaimNb: number of claims on the policy;
# 3. Exposure: total exposure in yearly units;
# 4. Area: area code (categorical);
# 5. VehPower [Power]: power of the car (categorical, ordered);
# 6. VehAge[Age]: age of the car in years;
# 7. DrivAge[DriverAge]: age of the driver in years;
# 8. BonusMalus: bonus-malus level between 50 and 230 (with reference level 100);
# 9. VehBrand[Brand]: car brand (categorical);
# 10. VehGas[Gas]: diesel or regular fuel car (binary);
# 11. Density: density of inhabitants per km2 in the city of the living place of the driver;
# 12. Region: regions in France (prior to 2016) (categorical)

# Statistiques descriptive -----------------------------
# Nombres de polices, et exposition par Nombre de Claims
freq2%>%group_by(ClaimNb)%>%dplyr::summarise( NbPolices = n(), TotalExpo = sum(Exposure))
 
# Analyse préliminaire : description des données 



# Réparition des sinistres
freq2%>%group_by(ClaimNb)%>%dplyr::summarise(NbPolices = n(), Proportion = n()/nrow(freq), TotalExposition = sum(Exposure))
# Fréquence moyenne de sinistres : envrion 10%
sum(freq2$ClaimNb)/sum(freq2$Exposure)
 

#Premier retraitement : on cappe les valeurs de ClaimNb à 4
freq2$ClaimNb = pmin(freq$ClaimNb, 4)
freq2$BonusMalus = pmin(freq2$BonusMalus,150)

 
# On remarque que la majorité des assurés n'ont pas eu de sinistres (environ 95%). Ensuite, lorsque les assurés ont été sinistrés, ils ont eu au maximum  16 sur la période d'exposition. On remarque que moins de 20 assurés ont eu plus de 4 sinistres. On cappe la valeur à 4.
 
# Analyse exposition et fréquences de sinistres
g1=ggplot(data.frame(x=freq2$Exposure), aes(x=x))+geom_histogram(bins=20)+xlab("Exposition")+ylab("Nombre de polices")+ggtitle("Histogramme Exposure")
g2=ggplot(data.frame(x=freq2$Exposure), aes(y=x))+geom_boxplot()+ggtitle("Boxplot Exposure")
g3 = ggplot(data.frame(x=freq2$ClaimNb), aes(x=x))+geom_histogram(bins=20)+xlab("ClaimNb")+ylab("Nombre de polices")+ggtitle("Histogramme ClaimNb")
grid.arrange(g1,g2,g3,ncol=3)

# Fonction renvoyant deux graphiques :
# - l'exposition totale par modalité de la variable choisie
# - fréquence de sinistres par modalité de la variable choisie
# On peut ajouter un paramètre de limite en abscisse pour plus de lisibilité des graphiques
hist_func = function(dat = freq2, mod = "Power", xlim = NULL){
  temp = dat%>%group_by_(mod)%>%dplyr::summarise(Expo = sum(Exposure), frequency = sum(ClaimNb)/sum(Exposure))
  temp2 = temp$Expo; names(temp2) = temp[,1]
  g1 = ggplot(temp, aes_string(x=mod, y = "Expo"))+geom_col()+ggtitle(paste("Exposition par",mod))
  # barplot(temp2, xlab =mod, ylab = "Exposition")
  g2 = ggplot(temp,aes_string(x=mod,y="frequency", group=1))+geom_point()+geom_line()+geom_smooth()+ylim(c(0,0.35))+
    ggtitle(paste("Fréquence par", mod))
  if (is.null(xlim)){
    grid.arrange(g1,g2,ncol=2)  
  }
  else{
    grid.arrange(g1+xlim(xlim),g2+xlim(xlim),ncol=2)  
  }
}
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

#BonusMalus : croissance avec la valeur

# Density (on utilise la partie entière du log) : on observe une croissance
freq_bis = freq2
freq_bis$Density = round(log(freq2$Density))
suppressMessages(suppressWarnings( hist_func(freq_bis, "Density")))

# # Analyse Régions plus approfondie [à compléter]
# dat = freq%>%group_by(Region)%>%dplyr::summarise(Expo = sum(Exposure), frequency = sum(ClaimNb)/sum(Exposure))
# dat$id = c("FR10","FR23","FR24","FR25","FR30","FR51","FR52","FR53","FR61","FR63")
# # id français des régions
# df = NULL
# df$id = nuts2.df$id[111:132]
# df=data.frame(df)
# df$val = 0
# df$val[df$id%in%dat$id] = dat$frequency
# df$val[!df$id%in%dat$id] = NA
# # Cartographie
# df$val=df$val*100
# choroLayer(spdf = nuts2.spdf, df = df, var = 'val',
#            breaks=dat$frequency[order(dat$frequency)]*100,
#            col = viridis::inferno(11)[11:1])
# title("Proportion de sinistres par région (en %)")
 


## Corrélation 
# On calcule les corrélations pour les variables numériques en se basant sur les méthodes de Sperman et Pearson.

# Corrélation entre les variables numériques : on suppose Power numérique (ordonnée), et Density -> log Density
# retraitement 
freq2$Power = freq2$Power-3
# Area :
letter2num <- function(x) {utf8ToInt(x) - utf8ToInt("a") + 1L}
freq2$Area = unlist(lapply(as.character(freq$Area), letter2num))+32
# Density : 
freq2$Density = log(freq2$Density)

# Corrélation
freq_2 = freq2
var_freq_num = c("Area","BonusMalus","Power", "CarAge", "DriverAge", "Density")
mat_corr1 = cor(freq_2[,var_freq_num], method="pearson")
mat_corr2 = cor(freq_2[,var_freq_num], method="spearman")
diag(mat_corr1)=diag(mat_corr2)=0

#Fonction pour afficher matrice de corrélation avec des couleurs
mat_correlation_plot = function(mat_corr = mat_corr){
  # Input :
  #        mat_corr : matrice de corrélation
  #        [matrix]
  # Output : graphique de la matrice de corrélation avec nuance de couleurs
  # [plot]
  neworder=hclust(dist(mat_corr))$order
  res2=mat_corr[neworder,neworder]
  
  # Matrice de corrélation avec nuance de couleurs
  return(qplot(x=Var1, y=Var2, data=melt(res2), fill=value, geom="tile") +
           scale_fill_gradient2(low="red",high="blue",limits=c(-1, 1)) +
           theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
           xlab(NULL)+ylab(NULL)+labs(fill="Corrélation"))
}
mat_correlation_plot(mat_corr1)
mat_correlation_plot(mat_corr2)



# Retraitement ------------
temp = freq2%>%group_by(Brand)%>%dplyr::summarise(Expo = sum(Exposure), frequency = sum(ClaimNb)/sum(Exposure))
temp = temp%>%mutate(Brand= fct_reorder(Brand, frequency))
g2 = ggplot(temp,aes(x=Brand,y=frequency, group=1))+geom_point()+geom_line()+geom_smooth()+ylim(c(0,0.35))+
  ggtitle(paste("Fréquence par", "Brand"))
temp2 = temp$Expo; names(temp2) = temp[,1]
g1 = ggplot(temp, aes(x=Brand, y = Expo))+geom_col()+ggtitle(paste("Exposition par","Brand"))
grid.arrange(g1,g2)

temp = freq2%>%group_by(Region)%>%dplyr::summarise(Expo = sum(Exposure), frequency = sum(ClaimNb)/sum(Exposure))
temp = temp%>%mutate(Region= fct_reorder(Region, frequency))
g2 = ggplot(temp,aes(x=Region,y=frequency, group=1))+geom_point()+geom_line()+geom_smooth()+ylim(c(0,0.35))+
  ggtitle(paste("Fréquence par", "Region"))
temp2 = temp$Expo; names(temp2) = temp[,1]
g1 = ggplot(temp, aes(x=Region, y = Expo))+geom_col()+ggtitle(paste("Exposition par","Region"))
grid.arrange(g1,g2)

retraitement_base_freq2 = function(dat){
  # Input:
  #       data : base de données initiale que l'on va retraiter
  #       [data.frame]
  #Output : base de données retraitées
  #         [data.frame]
  
  # POWER :
  dat2 <- dat
  dat2$AreaGLM <- as.integer(dat2$Area)
  dat2$PowerGLM <- as.factor(pmin(dat2$Power,6))
  AgeGLM <- cbind(c(0:110), c(1, rep(2,10), rep(3,100)))
  dat2$CarAgeGLM <- as.factor(AgeGLM[dat2$CarAge+1,2])
  dat2[,"CarAgeGLM"] <-relevel(dat2[,"CarAgeGLM"], ref="2")
  DriverAgeGLM <- cbind(c(18:100), c(rep(1,21-18), rep(2,26-21), rep(3,31-26), rep(4,41-31), rep(5,51-41), rep(6,71-51), rep(7,101-71)))
  dat2$DriverAgeGLM <- as.factor(DriverAgeGLM[dat2$DriverAge-17,2])
  dat2[,"DriverAgeGLM"] <-relevel(dat2[,"DriverAgeGLM"], ref="5")
  dat2$BonusMalusGLM <- as.integer(pmin(dat2$BonusMalus, 150))
  dat2$DensityGLM <- (dat2$Density)
  dat2[,"Region"] <-relevel(dat2[,"Region"], ref="R24")
  dat2$AreaGLM <- as.integer(dat2$Area)
  dat2$GasGLM = dat2$Gas
  return(dat2)
}
# Base retraitée
temp = retraitement_base_freq2(freq2)
freq_r = data.frame(ClaimNb = freq2$ClaimNb, Exposure = freq2$Exposure, Area = temp$AreaGLM,
                    BonusMalus = temp$BonusMalusGLM, Density = temp$DensityGLM, 
                    Region = temp$Region, Power = temp$PowerGLM, DriverAge = temp$DriverAgeGLM,
                    Brand = temp$Brand, Gas = freq2$Gas, CarAge = temp$CarAgeGLM)
tt =freq_r$ClaimNb
tr=unlist(lapply(tt, function(r){r}))
freq_r$ClaimNb = tr
str(freq_r)
#Déviance
  Poisson.Deviance = function(pred, obs){
    2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
  }
# Base d'apprentissage et de test ----------------------
n=nrow(freq_r)
n_app=floor(90/100*n) 
set.seed(100)
A=sample(1:n,n_app)
app = freq_r[A,]
test = freq_r[-A,]
# app et test sont retraitées
# app2 et test2 ne sont pas retraitées
app2 = temp[A,c(var_freq,"Exposure", "ClaimNb")]
app2$Gas = factor(app2$Gas)
test2 = temp[-A,c(var_freq,"Exposure", "ClaimNb")]
test2$Gas = factor(test2$Gas)
app2$Power = freMTPL2freq[A,]$VehPower - 3
test2$Power = freMTPL2freq[-A,]$VehPower - 3

# Analyse Ba et Bt
app%>%group_by(ClaimNb)%>%summarise(prop = n()/nrow(app))
test%>%group_by(ClaimNb)%>%summarise(prop = n()/nrow(test))
# fréquence empirique test et app : léger biais (10.07% contre 9.96%)
sum(app$ClaimNb)/sum(app$Exposure)
sum(test$ClaimNb)/sum(test$Exposure)


# Modèle GLM ---------
#Modèle 1 : complet
glm_freq_1 = glm(ClaimNb~Area+BonusMalus+Power+CarAge+DriverAge+Brand+Gas+Region+Density,
                 data=app,family=poisson(), offset = log(Exposure))
summary(glm_freq_1)
# toutes les variables sont significatives : pas de meilleur modèle possible

# résultat modèle
app$fit_glm = fitted(glm_freq_1)
test$fit_glm = predict(glm_freq_1, newdata=test, type="response")

# in-sample and out-of-sample losses (in 10^(-2))
in1=(insampleGLM <- 100*Poisson.Deviance(app$fit_glm, app$ClaimNb))
out1=100*Poisson.Deviance(test$fit_glm, test$ClaimNb)
mse_in1 = Metrics::mse(app$fit_glm, app$ClaimNb)
mse_out1 = Metrics::mse(test$fit_glm, test$ClaimNb)
mae_in1 = Metrics::mae(app$fit_glm, app$ClaimNb)
mae_out1 = Metrics::mae(test$fit_glm, test$ClaimNb)

res_glm_1 = c(glm_freq_1$aic, in1, out1, mse_in1, mse_out1, mae_in1, mae_out1)
names(res_glm_1) = c("AIC", "Deviance app", "Deviance test", "MSE app", "MSE test", "MAE app", "MAE test")
res_glm_1
# Autre GLM : En retirant Area
glm_freq_2 = glm(ClaimNb~BonusMalus+Power+CarAge+DriverAge+Brand+Gas+Region+Density,
                 data=app,family=poisson(), offset = log(Exposure))
summary(glm_freq_2)
# résultat modèle

app$fit_glm2 = fitted(glm_freq_2)
test$fit_glm2 = predict(glm_freq_2, newdata=test, type="response")

# in-sample and out-of-sample losses (in 10^(-2))
in2=(insampleGLM <- 100*Poisson.Deviance(app$fit_glm2, app$ClaimNb))
out2=100*Poisson.Deviance(test$fit_glm2, test$ClaimNb)
mse_in2 = Metrics::mse(app$fit_glm2, app$ClaimNb)
mse_out2 = Metrics::mse(test$fit_glm2, test$ClaimNb)
mae_in2= Metrics::mae(app$fit_glm2, app$ClaimNb)
mae_out2 = Metrics::mae(test$fit_glm2, test$ClaimNb)

res_glm_2 = c(glm_freq_2$aic, in2, out2,  mse_in2, mse_out2, mae_in2, mae_out2)
names(res_glm_2) = c("AIC", "Deviance app", "Deviance test", "MSE app", "MSE test", "MAE app", "MAE test")


# GLM 3 : en retirant Area et Brand
glm_freq_3 = glm(ClaimNb~BonusMalus+Power+CarAge+DriverAge+Gas+Region+Density,
                 data=app,family=poisson(), offset = log(Exposure))
summary(glm_freq_3)
# résultat modèle

app$fit_glm3 = fitted(glm_freq_3)
test$fit_glm3 = predict(glm_freq_3, newdata=test, type="response")

# in-sample and out-of-sample losses (in 10^(-2))
in3=(insampleGLM <- 100*Poisson.Deviance(app$fit_glm3, app$ClaimNb))
out3=100*Poisson.Deviance(test$fit_glm3, test$ClaimNb)
mse_in3 = Metrics::mse(app$fit_glm3, app$ClaimNb)
mse_out3 = Metrics::mse(test$fit_glm3, test$ClaimNb)
mae_in3= Metrics::mae(app$fit_glm3, app$ClaimNb)
mae_out3 = Metrics::mae(test$fit_glm3, test$ClaimNb)

res_glm_3 = c(glm_freq_3$aic, in3, out3,  mse_in3, mse_out3, mae_in3, mae_out3)
names(res_glm_3) = c("AIC", "Deviance app", "Deviance test", "MSE app", 
                     "MSE test", "MAE app", "MAE test")
res1_2_3 = rbind(res_glm_1,res_glm_2, res_glm_3)
rownames(res1_2_3) =c("GLM1", "GLM2(sans Region)","GLM3 (sans Region ni Brand)")
res1_2_3

tree_2 <- rpart(ClaimNb/Exposure ~ Power+CarAge+DriverAge+Brand+Gas+Region+Density+Area+BonusMalus, 
                mtpl2_app, method="poisson", weights = mtpl2_app$Exposure,
               control=rpart.control(xval=1, minbucket=10000, cp=0.0005))

tree1 <- rpart(cbind(Exposure,ClaimNb) ~ Power+CarAge+DriverAge+Brand+Gas+Region+Density+Area+BonusMalus, 
               app2, method="poisson",
               control=rpart.control(xval=1, minbucket=10000, cp=0.0005))     

rpart.plot(tree1, min.auto.cex=0.5)
tree1                    # show tree with all binary splits
printcp(tree1)           # cost-complexit statistics

app2$fit_tree = predict(tree1)*app2$Exposure
test2$fit_tree = predict(tree1, newdata=test2)*test2$Exposure
in_tree1 = 100*Poisson.Deviance(app2$fit_tree, app2$ClaimNb)
out_tree1 = 100*Poisson.Deviance(test2$fit_tree, test2$ClaimNb)
mse_in_tree1 = Metrics::mse(app2$fit_tree, app2$ClaimNb)
mse_out_tree1 = Metrics::mse(test2$fit_tree, test2$ClaimNb)
mae_in_tree1= Metrics::mae(app2$fit_tree, app2$ClaimNb)
mae_out_tree1 = Metrics::mae(test2$fit_tree, test2$ClaimNb)
vec1 = c(in_tree1, out_tree1, mse_in_tree1, mse_out_tree1, mae_in_tree1, mae_out_tree1)

#Cross-validation and élagage de l'arbre
K <- 10                 # K-fold cross-validation value
set.seed(2019)
xgroup <- rep(1:K, length = nrow(app2))
xfit <- xpred.rpart(tree1, xgroup)
(n_subtrees <- dim(tree1$cptable)[1])
std1 <- numeric(n_subtrees)
err1 <- numeric(n_subtrees)
err_group <- numeric(K)
for (i in 1:n_subtrees){
  for (k in 1:K){
    ind_group <- which(xgroup ==k)
    err_group[k] <- Poisson.Deviance(app2[ind_group,"Exposure"]*xfit[ind_group,i],app2[ind_group,"ClaimNb"])
  }
  err1[i] <- mean(err_group)
  std1[i] <- sd(err_group)
}

x1 <- log10(tree1$cptable[,1])
xmain <- "cross-validation error plot"
xlabel <- "cost-complexity parameter (log-scale)"
ylabel <- "CV error (in 10^(-2))"
errbar(x=x1, y=err1*100, yplus=(err1+std1)*100, yminus=(err1-std1)*100, xlim=rev(range(x1)), col="blue", main=xmain, ylab=ylabel, xlab=xlabel)
lines(x=x1, y=err1*100, col="blue")
abline(h=c(min(err1+std1)*100), lty=1, col="orange")
abline(h=c(min(err1)*100), lty=1, col="magenta")
abline(h=c(in1), col="green", lty=2)
legend(x="topright", col=c("blue", "orange", "magenta", "green"), lty=c(1,1,1,2), lwd=c(1,1,1,1), pch=c(19,-1,-1,-1), legend=c("tree1", "1-SD rule", "min.CV rule", "Model GLM1"))

# prune to appropriate cp constant
printcp(tree1)
tree2 <- prune(tree1, cp=0.00003)
printcp(tree2)

app2$fit_tree2<- predict(tree2)*app2$Exposure
test2$fit_tree2 <- predict(tree2, newdata=test2)*test2$Exposure
100*Poisson.Deviance(app2$fit_tree2, app2$ClaimNb)
100*Poisson.Deviance(test2$fit_tree2, test2$ClaimNb)
in_tree2 = 100*Poisson.Deviance(app2$fit_tree2, app2$ClaimNb)
out_tree2 = 100*Poisson.Deviance(test2$fit_tree2, test2$ClaimNb)
mse_in_tree2 = Metrics::mse(app2$fit_tree2, app2$ClaimNb)
mse_out_tree2 = Metrics::mse(test2$fit_tree2, test2$ClaimNb)
mae_in_tree2 = Metrics::mae(app2$fit_tree2, app2$ClaimNb)
mae_out_tree2 = Metrics::mae(test2$fit_tree2, test2$ClaimNb)
vec2 = c(in_tree2, out_tree2, mse_in_tree2, mse_out_tree2, mae_in_tree2, mae_out_tree2)

# Paramètres à optimiser via CV
caret_xgb_param_freq_1 <- expand.grid(nrounds = 150, eta = 0.1, max_depth = 4,
                                      gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
# opération bizarre pour mettre ClaimNb au bon format
app2$ClaimNb = unlist(lapply(app2$ClaimNb,function(r){r}))
set.seed(2019)

xgb_freq_1 = caret::train(ClaimNb/Exposure ~ Power+CarAge+DriverAge+Brand+Gas+Region+Density+Area+BonusMalus, data = app2, method = "xgbTree",
                          tuneGrid = caret_xgb_param_freq_1, weights= app2$Exposure,
                          verbose=F, trControl = caret::trainControl(method="none") ,objective = "count:poisson")


# sparse_matrix <- sparse.model.matrix(ClaimNb/Exposure ~ Power+CarAge+DriverAge+Brand+Gas+Region+Density+Area+BonusMalus, data=app2 , weight = app2$Exposure)
# set.seed(2019)
# xgboost_xgb_param <-  list(eta = 0.1, eta = 0.1, max_depth = 4,
#                            gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
# 
# xgb_xgboost_freq_1 = (xgboost(data = sparse_matrix, label = app2$ClaimNb/app2$Exposure,nrounds=150
#                ,params=xgboost_xgb_param, objective="count:poisson",verbose = T))

app2$fit_xgb = predict(xgb_freq_1, app2)*app2$Exposure
test2$fit_xgb = predict(xgb_freq_1, test2)*test2$Exposure
in1_xgb = 100*Poisson.Deviance(app2$fit_xgb, app2$ClaimNb)
out1_xgb = 100*Poisson.Deviance(test2$fit_xgb, test2$ClaimNb)

in_xgb1 = 100*Poisson.Deviance(app2$fit_xgb, app2$ClaimNb)
out_xgb1 = 100*Poisson.Deviance(test2$fit_xgb, test2$ClaimNb)
mse_in_xgb1 = Metrics::mse(app2$fit_xgb, app2$ClaimNb)
mse_out_xgb1 = Metrics::mse(test2$fit_xgb, test2$ClaimNb)
mae_in_xgb1= Metrics::mae(app2$fit_xgb, app2$ClaimNb)
mae_out_xgb1 = Metrics::mae(test2$fit_xgb, test2$ClaimNb)
vec_xgb1 = c(in_xgb1, out_xgb1, mse_in_xgb1, mse_out_xgb1, mae_in_xgb1, mae_out_xgb1)

# Fonction renvoyant MSE, MAE, etc.
perf_func = function(model, appe, teste){
  pred_app = predict(model, appe)*appe$Exposure
  pred_test =  predict(model, teste)*teste$Exposure
  in_xgb2 = 100*Poisson.Deviance(pred_app, appe$ClaimNb)
  out_xgb2 = 100*Poisson.Deviance(pred_test, teste$ClaimNb)
  mse_in_xgb2 = Metrics::mse(pred_app, appe$ClaimNb)
  mse_out_xgb2 = Metrics::mse(pred_test, teste$ClaimNb)
  mae_in_xgb2= Metrics::mae(pred_app, appe$ClaimNb)
  mae_out_xgb2 = Metrics::mae(pred_test, teste$ClaimNb)
  vec_xgb2 = c(in_xgb2, out_xgb2, mse_in_xgb2, mse_out_xgb2, mae_in_xgb2, mae_out_xgb2)
  return(vec_xgb2)
}

# Autres XGB
imp_var_plot(xgb_freq_1)
imp_var_dalex_plot_2(xgb_freq_1, app2, "frequence")
# Impvar ! Donne idée de XGB sans region 
caret_xgb_param_freq_1 <- expand.grid(nrounds = 150, eta = 0.1, max_depth = 4,
                                      gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
xgb_freq_2 = caret::train(ClaimNb/Exposure ~ Power+CarAge+DriverAge+Brand+Gas+Density+Area+BonusMalus, data = app2, method = "xgbTree",
                          tuneGrid = caret_xgb_param_freq_1, weights= app2$Exposure,
                          verbose=F, trControl = caret::trainControl(method="none") ,objective = "count:poisson")
app2$fit_xgb2 = predict(xgb_freq_2, app2)*app2$Exposure
test2$fit_xgb2 = predict(xgb_freq_2, test2)*test2$Exposure
vec_xgb2 = perf_func(xgb_freq_2,app2, test2 )

app_b = app2
app_b$Region = as.character(app2$Region)
app_b$Region[app_b$Region%in%c("R11","R21","R22","R94")] = "R11_21_22_94"
app_b$Region[app_b$Region!="R11_21_22_94"] = "R_other"
app_b$Region = factor(app_b$Region)
test_b= test2
test_b$Region = as.character(test2$Region)
test_b$Region[test_b$Region%in%c("R11","R21","R22","R94")] = "R11_21_22_94"
test_b$Region[test_b$Region!="R11_21_22_94"] = "R_other"
test_b$Region = factor(test_b$Region)

xgb_freq_3 = caret::train(ClaimNb/Exposure ~ Power+CarAge+DriverAge+Brand+Gas+Region+Density+Area+BonusMalus, data = app_b, method = "xgbTree",
                          tuneGrid = caret_xgb_param_freq_1, weights= app2$Exposure,
                          verbose=F, trControl = caret::trainControl(method="none") ,objective = "count:poisson")
perf_func(xgb_freq_3, app_b,test_b)


## Résumé de tous les modèles
res = rbind(res1_2_3[,-1], vec1,vec2, vec_xgb1)
rownames(res)=c("GLM1", "GLM2","GLM3", "Tree1","Tree2" ,"XGBoost")
res

# Comparaison avec le GLM trivial

glm_freq_triv = glm(ClaimNb~1,
                 data=app,family=poisson(), offset = log(Exposure))

# toutes les variables sont significatives : pas de meilleur modèle possible

# résultat modèle
app$fit_glm_triv = fitted(glm_freq_triv)
test$fit_glm_triv = predict(glm_freq_triv, newdata=test, type="response")

# in-sample and out-of-sample losses (in 10^(-2))
in1=(insampleGLM <- 100*Poisson.Deviance(app$fit_glm_triv, app$ClaimNb))
out1=100*Poisson.Deviance(test$fit_glm_triv, test$ClaimNb)
mse_in1 = Metrics::mse(app$fit_glm_triv, app$ClaimNb)
mse_out1 = Metrics::mse(test$fit_glm_triv, test$ClaimNb)
mae_in1 = Metrics::mae(app$fit_glm_triv, app$ClaimNb)
mae_out1 = Metrics::mae(test$fit_glm_triv, test$ClaimNb)



# Analyse performance finale  -------
res_glm_triv = c(glm_freq_1$aic, in1, out1, mse_in1, mse_out1, mae_in1, mae_out1)
names(res_glm_triv) = c("AIC", "Deviance app", "Deviance test", "MSE app", "MSE test", "MAE app", "MAE test")
res2 = rbind(res_glm_triv[-1], res)
res
gain_res = -(res2 -t(matrix(rep(res2[1,],7),nrow=6)))/t(matrix(rep(res2[1,],7),nrow=6))*100
gain_res #gain par rapport au GLM1 en %
mat_fin = gain_res[c(1,2,5,7),]+100
maxmin = rbind(rep(110,6),rep(90,6)); colnames(maxmin) = names(data.frame(mat_fin))
fmsb::radarchart(rbind(maxmin,data.frame(mat_fin)), axistype=1, seg=5, plty=1, maxmin = T, plwd = 2,
                 caxislabels=c("","100%",rep('',3),"105%"), cglcol = 1:5)
# fmsb::radarchart(rbind(maxmin,data.frame(mat_fin)), axistype=1, seg=5, plty=1, maxmin = T)
legend(1, 1.3, legend=c("XGB", "Tree", "GLM", "GLM trivial"),
       col=c("blue", "green","red","black"), lty=c(1,1,1), cex=0.7)

# Interprétationn modèle XGB ----------
# 1) Imp Var
# a) avec caret
zoom_func = function(){return(theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+theme(axis.text.x = element_text(size=14)))}

imp_var_plot(xgb_freq_1)
imp_var_plot(xgb_freq_2)
#b) DALEX
explainer = DALEX::explain(xgb_freq_1, data = app2[,var_freq], y = app2$ClaimNb)
plot(variable_importance(explainer, n_sample = 10000))

explainer2 = DALEX::explain(xgb_freq_3, data = app_b[,var_freq], y = app2$ClaimNb)
plot(variable_importance(explainer2, n_sample = 10000))


# 2) PDP et ICE

## PDP
# plutôt long à exécuter : data enregistrer dans le dossier images/freMTPL2_memoire
# for (k in 5:length(var_freq)){
  # ae = pdp_func(xgb_freq_1,k)
  # save(ae, file = paste("C:\\Users\\dimitri.delcaillau\\Desktop\\Dimitri Delcaillau\\Images\\freMTPL2_memoire\\",var_freq[k],".RData",sep=""))
  # ggplot(ae, aes_string(x=var_freq[k],y ='yhat'))+geom_point()+geom_smooth()+
  # ggtitle("PDP - XGBoost frequency", var_freq[k])+zoom_func()
# }
# var_freq

## ICEs

# try = lapply(1:length(var_freq), ICE_courbes_freMTPL2, nb_curves = 100, 
#              model = xgb_freq_1, dat_app = app2, n_sample = 15, seed = 2019, alp = 0.2) 
# plot_ly::ggplotly(try[[1]])

nb_curves = 10000
model = xgb_freq_1
ind_var = 2;  var1 = ind_var
dat_app = app2
n_sample = 10
seed = 2019
alp = 0.2
set.seed(seed)
var2 = 6
x_interests = sample(1:nrow(dat_app), nb_curves)
###################A modifier suivant var2 et la condition souhaitée#########################
a1 = dat_app[x_interests,var_freq[var2]]
a1 = a1<26
##########################################################################################
temp = lapply(x_interests,ICE_func,model=model, ind_var = ind_var, dat_app = dat_app, n_sample = ,n_sample, seed = seed)
mean = do.call(rbind, temp)
df = as.data.frame(temp)
xx = df$x
dff = df[,!stringr::str_detect(colnames(df),"x")]
colnames(dff) = NULL
melt = melt(as.matrix(dff))
melt$x = xx
melt$var = as.vector(t(matrix(rep(a1,n_sample), ncol= n_sample)))

# +geom_smooth(method="auto",
#              data=melt, aes(x = x, y = value, group = Var2), 
#              alpha=alp, col='grey', se=F)
# On charge PDP
file = get(load(paste("C:\\Users\\dimitri.delcaillau\\Desktop\\Dimitri Delcaillau\\Images\\freMTPL2_memoire\\PDP_data\\",var_freq[ind_var],".RData",sep="")))
gg = ggplot()+
  geom_point(data=melt, aes(x = x, y = value, group = Var2,col=var), alpha = alp, size = 0)+
  geom_line(data=melt, aes(x = x, y = value, group = Var2, col=var), alpha = alp, size = 0.9)+
  geom_line(data=file,aes_string(x=var_freq[ind_var], y='yhat'), size = 1, color='black')+geom_point(data=file,aes_string(x=var_freq[ind_var], y='yhat'), color="black")+
  ggtitle("PDP (noir) et courbes ICE", var_freq[ind_var])+xlab(var_freq[ind_var])


# 3) ALE
# for (k in 1:length(var_freq)){
#   ALE_plot(xgb_freq_1,app2, k)
# }
  
# SHAP
n_sample = 1000
ind = 1
X = app2[,var_freq]
predictor = Predictor$new(xgb_freq_1, data = X)
a = Sys.time()
Shap = Shapley$new(predictor, x.interest = X[ind,],sample.size = n_sample)
Sys.time()-a

# Lime
n_perm = 5000
n_feat = 8
a = Sys.time()
explanation_lime = lime::lime(app2[,var_freq], xgb_freq_1)
Lime = lime::explain(app2[ind,var_freq], explanation_lime, n_features = n_feat, n_permutations=n_perm)
t_l = Sys.time() - a
t_l
plot_features(Lime)+scale_fill_stata()

# Stabilité SHAP
X = dat_app[,var_freq]
model = xgb_freq_1
predictor = Predictor$new(model, data = X)
ind_obs = 1
sample_size = 10
Nsimu = 10
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
avg <- df_sorted %>% group_by(variable)
summarize(avg = mean(ord, na.rm = T)) %>%
  pull(avg)
g <- ggplot(df_sorted, aes(variable, ord, color=variable)) + geom_boxplot()+
  coord_flip() +
  labs(x = NULL, y = "Contribution") +scale_color_gdocs()+geom_point(size=3,alpha=0.15)+ggtitle("Stabilité Interprétation fournie par SHAP")

# Analyse concurrence

temp = test2%>%mutate(fit_glm = test$fit_glm) %>% dplyr::select(fit_xgb, fit_glm)
mi = pmin(temp$fit_xgb,temp$fit_glm)
ind_glm = which(temp$fit_glm==mi)
ind_xgb = which(temp$fit_xgb==mi)
length(ind_glm)+length(ind_xgb)
pie(x = c(length(ind_glm),length(ind_xgb)), labels = c("GLM", "XGB"), main = "Part de Marché - marché concurrentiel GLM et XGB")
testglm = test[ind_glm,]
sum(testglm$ClaimNb - testglm$fit_glm)
testxgb = test2[ind_xgb,]
sum(testxgb$ClaimNb - testxgb$fit_xgb)
paste("S/P GLM :", scales::percent(sum(testglm$ClaimNb)/sum(testglm$fit_glm)), ", S/P XGB :", scales::percent(sum(testxgb$ClaimNb)/sum(testxgb$fit_xgb)))
paste("Part de marché GLM :", scales::percent(length(ind_glm)/nrow(test)), ", Part de marché XGB :", scales::percent(length(ind_xgb)/nrow(test)))

library(scales)
# Utiliser la palette de couleur brewer
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
df_pie = data.frame(Model = c("GLM", "XGB"), Part_de_Marche = c(length(ind_glm),length(ind_xgb))/nrow(test),
                resultat = - c(sum(testglm$ClaimNb - testglm$fit_glm) ,sum(testxgb$ClaimNb - testxgb$fit_xgb) ),
                S_sur_P = c(percent(sum(testglm$ClaimNb)/sum(testglm$fit_glm)), percent(sum(testxgb$ClaimNb)/sum(testxgb$fit_xgb))))
pie = ggplot(df_pie, aes(x="", y=Part_de_Marche, fill=Model))+
  geom_bar(width = 1, stat = "identity")+coord_polar("y", start = 0)+
  scale_fill_brewer("Blues")

pie+geom_text(aes(y = Part_de_Marche/2 + c(0, cumsum(Part_de_Marche)[-length(Part_de_Marche)]), 
                  label = paste("S/P :",S_sur_P,"\n","Résultat :", round(resultat,2)), size=5))


# PDP

