###########################################################################################
################### RETRAITEMENT DES DONNEES ET ANALYSE PRELIMINAIRE ######################
###########################################################################################

############################################################################################
# Importation des packages ---------
Packages <- c("CASdatasets", "dplyr", "data.table", "ggplot2","gridExtra", "stringr","MASS","Metrics",
              "DescTools","plotrix", "reshape2", "xgboost","ranger","caret","mlr","Metrics","Matrix","forcats",
              "lime", "iml", "shapleyR", "ALEPlot","breakDown","condvis","DALEX","iBreakDown","ICEbox","plotly",
              "breakDown", "ICEbox", "ingredients", "localModel", "pdp","shapper", "vip", "xtable", "ggthemes")
a=lapply(Packages, library, character.only = TRUE)


#####################################################################################
# Parémètres à définir par l'utilisateur auparavant  !!! -------------
# (par exemple : différents pour la base freMTPL2)
# Ici : il s'agit des variables explicatives de la base freMTPL2freq (package CASdatasets)
# Variables explicatives pour la fréquence :
var_freq=c("Area", "BonusMalus","Density", "Region", "Power", "DriverAge", "Brand", "Gas", "CarAge") 
# Variables explicatives pour le coût (pas utiles ici): 
var_cout = var_freq
# Les variables du nombre de sinistres et du montant des sinistres doivent s'appeler :
# ClaimNb et ClaimAmount 
# La variable d'exposition doit s'appeler : Exposure !!! 
# (les renommer si différent dans la base étudiée)
#####################################################################################


################################################################################################
#################################### Fonctions utiles###########################################
################################################################################################


# Fusion base fréquence / sévérité ------
func_merge = function(freq, sev){
  # Variable : IDpol pour merge
  a = sev%>%group_by(IDpol)%>%summarise(ClaimAmount = sum(ClaimAmount))
  a = a%>%filter(IDpol%in%freq$IDpol)
  freMTPL2merge = merge(freq, a, by = "IDpol", all.x = T)
  freMTPL2merge = freMTPL2merge%>%mutate(PolicyID = IDpol, Power = VehPower, DriverAge = DrivAge, Brand = VehBrand,
                                         Gas = VehGas, CarAge = VehAge)
  freMTPL2merge$VehPower = NULL;freMTPL2merge$VehAge=NULL; freMTPL2merge$VehBrand = NULL; freMTPL2merge$VehGas = NULL; freMTPL2merge$DrivAge = NULL;freMTPL2merge$IDpol = NULL
  
  # On transforme NA en 0
  freMTPL2merge$ClaimAmount[is.na(freMTPL2merge$ClaimAmount)] = 0
  # On supprime les lignes ayant un ClaimAmount de 0 et un ClaimNb > 0
  freMTPL2merge = freMTPL2merge[-which(freMTPL2merge$ClaimAmount==0 & freMTPL2merge$ClaimNb>0),]
  # Opération bizarre pour mettre ClaimNb au bon format
  freMTPL2merge$ClaimNb = unlist(lapply(freMTPL2merge$ClaimNb,function(r){r}))
  freMTPL2merge
}

# freMTPL2merge = func_merge(freMTPL2freq, freMTPL2sev)

func_seperation_app_test_keep = function(seed = 2019, prop_app = 90/100){
  # Fonction pour permettre de gérer la séparation Ba et Bt avec les NA de la base de sévérité 
  fus = func_merge_bis(freMTPL2freq, freMTPL2sev)
  I0 = which(fus$ClaimNb>0 & fus$ClaimAmount==0)
  I1 = which(fus$ClaimNb>0 & fus$ClaimAmount>0)
  I_0_1 = which(fus$ClaimNb>0)
  I2 = which(fus$ClaimNb==0)
  # prop_app = 90/100
  n_I1 = (prop_app*length(I_0_1)-length(I0))
  n_I2 = prop_app*length(I2)
  set.seed(seed)
  I_app = c(I0, I1[sample(1:length(I1), n_I1)], I2[sample(1:length(I2), n_I2)])
  I_app
} ----

# Obtention de la base contenant uniquement les assurés sinistrés -------
data_sinistres = function(data = data){
  # Input :
  #       data : base de données de sinistralité (fréquence et coût) 
  #       [data.frame]
  # Ouput : base de données contenant uniquement les assurés ayant eu au moins un sinistre
  #         [data.frame]
  return(data[data$ClaimNb>0,])
}

# Séparation Base de test et base d'apprentissage ------
separation_base_test_app = function(data = data, prop_app = 85/100, seed = 2019){
  # Input :
  #        data : base de données de sinistralité (fréquence et coût) 
  #               [data.frame]
  #        prop_app : proportion de données conservées pour l'apprentissage (et la validation) 
  #                   [numeric] entre 0 et 1
  #        seed : graine pour la reproductibilité
  #              [integer]
  # Output : liste avec la base d'apprentissage et la base de test  
  #          [list] (de deux [data.frame] : $app et $test)
  n=nrow(data)
  n_app=floor(prop_app*n) 
  set.seed(seed)
  A=sample(1:n,n_app)
  app = data[A,]
  test = data[-A,]
  return(list(app = app, test = test))
}
# sep = separation_base_test_app(data, 0.85, 2019)
# app = sep$app 
# test = sep$test

# Retraitement de la base de données : à modifier suivant les retraitements à faire --------

retraitement_base = function(data = data){
  # Input:
  #       data : base de données initiale que l'on va retraiter
  #       [data.frame]
  #Output : base de données retraitées
  #         [data.frame]
  # Il s'agit du retraitement choisi pour la base freMTPL (pas retenue pour l'article)
  
  # Premier regroupement :
  data_2 = data
  # Power
  data_2$Power = as.character(data_2$Power)
  data_2$Power[data$Power=='e' | data$Power=='f'] = ("e-f")
  data_2$Power[data$Power=='g' | data$Power=='h'] = "g-h"
  data_2$Power[data$Power=='j' | data$Power=='k'] = "j-k"
  data_2$Power[data$Power=='m' | data$Power=='n'] = "m-n"
  data_2$Power = factor(data_2$Power,ordered = F)

  #DriverAge
  data_2$DriverAge[data$DriverAge<27] = "18-26"
  data_2$DriverAge[data$DriverAge>26 & data$DriverAge<43] = "27-42"
  data_2$DriverAge[data$DriverAge>42 & data$DriverAge<56] = "43-55"
  data_2$DriverAge[data$DriverAge>55 & data$DriverAge<70] = "56-69"
  data_2$DriverAge[data$DriverAge>69 & data$DriverAge<86] = "70-85"
  data_2$DriverAge[data$DriverAge>85] = "86+"
  data_2$DriverAge = factor(data_2$DriverAge, ordered = F)

  # CarAge
  data_2$CarAge[data$CarAge<1] = "0"
  data_2$CarAge[data$CarAge>0 & data$CarAge<4] = "1-3"
  data_2$CarAge[data$CarAge>3 & data$CarAge<9] = "4-8"
  data_2$CarAge[data$CarAge>8 & data$CarAge<13] = "9-12"
  data_2$CarAge[data$CarAge>12 & data$CarAge<21] = "13-20"
  data_2$CarAge[data$CarAge>20] = "20+"
  data_2$CarAge = factor(data_2$CarAge, levels = c("0", "1-3", "4-8", "9-12", "13-20", "20+"),ordered = F)

  # Density
  data_2$Density[data$Density<=100] = "0-100"
  data_2$Density[data$Density>100 & data$Density<501] = "101-500"
  data_2$Density[data$Density>500 & data$Density<2001] = "501-2000"
  data_2$Density[data$Density>2000 & data$Density<10001] = "2001-10000"
  data_2$Density[data$Density>10000 & data$Density<20000] = "10001-19999"
  data_2$Density[data$Density>=20000] = "20000+"
  data_2$Density=factor(data_2$Density,levels=c("0-100","101-500", "501-2000", "2001-10000", "10001-19999","20000+"),ordered="T")

  data3 =data_2
  # Deuxième regroupement :
  # Power
  data3$Power = as.character(data3$Power)
  data3$Power[data_2$Power%in%levels(data_2$Power)[-c(1:3)]] = "3"
  data3$Power[data_2$Power%in%levels(data_2$Power)[2:3]] = "2"
  data3$Power[data_2$Power=="d"]="1"
  data3$Power=factor(data3$Power,ordered = F)

  # CarAge
  data3$CarAge = as.character(data_2$CarAge)
  data3$CarAge[data_2$CarAge%in%levels(data_2$CarAge)[5:6]] = "13+"
  data3$CarAge=factor(data3$CarAge, levels=c("0","1-3", "4-8","9-12","13+"),ordered = F)

  # DriverAge
  data3$DriverAge = as.character(data_2$DriverAge)
  data3$DriverAge[data_2$Driver%in%levels(data_2$DriverAge)[4:5]] = "56-85"
  data3$DriverAge=factor(data3$DriverAge,ordered = F)

  # Brand
  data3$Brand = as.character(data_2$Brand)
  data3$Brand[data_2$Brand!="F"] = "!F"
  data3$Brand = factor(data3$Brand)

  # Region
  data3$Region = as.character(data_2$Region)
  data3$Region[data_2$Region%in%c('R23', 'R31', 'R72','R54')] = "R23_31_72_54"
  data3$Region = factor(data3$Region)

  # Density
  data3$Density = as.character(data_2$Density)
  data3$Density[data3$Density%in%levels(data_2$Density)[2:5]]="101-19999"
  data3$Density=factor(data3$Density,ordered = F,levels=c("0-100","101-19999","20000+"))
  data3$Density = as.character(data3$Density)
  data3$Density[data3$Density%in%levels(data3$Density)[2:3]]="101-19999"
  data3$Density=factor(data3$Density,ordered = F)
  return(data3)
}


retraitement_base_freq2 = function(dat){
  # Retraitements proposés dans l'article suivant :
  # Noll, Alexander and Salzmann, Robert and Wuthrich, Mario V., 
  # Case Study: French Motor Third-Party Liability Claims (November 8, 2018). 
  # Available at SSRN: https://ssrn.com/abstract=3164764 or http://dx.doi.org/10.2139/ssrn.3164764
  # Input:
  #       data : base de données initiale que l'on va retraiter
  #       [data.frame]
  #Output : base de données retraitées
  #         [data.frame]
  # Il s'agit du retraitement choisi pour la base freMTPL2 : retenue pour l'article
  
  # POWER :
  dat2 <- dat
  # ClaimNb : retraitement bizarre pour mettre ClaimNb au format numérique
  dat2$ClaimNb=unlist(lapply(dat$ClaimNb, function(r){r}))
  dat2$Area = as.factor(dat2$Area)
  # dat2$AreaGLM <- as.integer(dat2$Area)
  dat2$Power = dat2$Power -3
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
  dat2$AreaGLM <- dat2$Area
  dat2$Gas = as.factor(dat2$Gas)
  dat2$GasGLM = dat2$Gas
  dat2$BrandGLM = dat2$Brand
  dat2$RegionGLM = dat2$Region
  return(dat2)
}

# data_ = retraitement_base(data)

# Ecrêtement de la base d'apprentissage ----------
ecretement_base_app = function(base_app = data_app, var_amount = "ClaimAmount", ecret = 10000){
  # Input :
  #        data : base de données de sinistralité (fréquence et coût) 
  #               [data.frame]
  #        var_amount :  nom de la variable représentant le montant total des sinistres 
  #                   [character] 
  #        ecret : valeur pour laquelle on écrête les données
  #              [numeric] (> 0)
  # Output : base d'apprentissage écretée 
  #          [list] (de deux [data.frame])
  
  ind_amount = which(colnames(base_app)=="ClaimAmount") # indice de la colonne associée au montant des sinistres
  base_app_ecr = base_app
  # On "cappe" les sinistres supérieurs à "ecret" à cette valeur d'écretement
  base_app_ecr[,ind_amount] =ifelse(base_app[,ind_amount]<ecret,base_app[,ind_amount],ecret)
  return(base_app_ecr)
}
# data_app = ecretement_base_app(separation_base_test_app(data_)$app)
# data_test = separation_base_test_app(data_)$test


# Calcul de la matrice de corrélation et graphique associé --------
mat_correlation = function(data = data_, var = var_freq){
  # Input :
  #        data : base de données de sinistralité (fréquence et coût) 
  #               [data.frame]
  #        var_freq :  vecteur des noms de variables explicatives
  #                   [character]
  #        ecret : valeur pour laquelle on écrête les données
  #               (> 0)
  # Output : matrice de corrélation
  #          [matrix] 
  #Calcul des corrélations à l'aide de la fonction CramerV (package Desctools)
  bd=data
  bd_mat=as.matrix(bd)
  res=matrix(NA,nrow=length(var),ncol=length(var))
  for (i in 1:length(var)) {
    for (j in 1:length(var)) {
      ind_i=which(colnames(bd)==var[i])
      ind_j=which(colnames(bd)==var[j])
      res[i,j]=CramerV(bd_mat[,ind_i],bd_mat[,ind_j])
    }
  }
  colnames(res)=var
  rownames(res)=var
  #Matrice de corrélation
  res
  return(res)
  
}
# mat_corr = mat_correlation(data_,var_freq)

mat_correlation_plot = function(mat_corr){
  # Input :
  #        mat_corr : matrice de corrélation
  #        [matrix]
  # Output : graphique de la matrice de corrélation
  return(color2D.matplot(mat_corr, extremes = c("white", "red"), vcol = "black",nslices = 60,
                  show.legend=TRUE,axes=TRUE,show.values=TRUE,xlab="Columns",ylab="Rows"))
}

mat_correlation_plot_2 = function(mat_corr = mat_corr){
  # Input :
  #        mat_corr : matrice de corrélation
  #        [matrix]
  # Output : graphique de la matrice de corrélation avec nuance de couleurs
  # [plot]
  neworder=hclust(dist(mat_corr))$order
  res2=mat_corr[neworder,neworder]
  
  # Matrice de corrélation avec nuance de couleurs
  return(qplot(x=Var1, y=Var2, data=melt(res2), fill=value, geom="tile") +
    scale_fill_gradient2(low="white",high="blue",limits=c(0, 1)) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    xlab(NULL)+ylab(NULL)+labs(fill="V de Cramer"))
}

# Graphiques d'analyse préliminaire #####
# Fonction pour obtenir l'indice d'une variable dans le vecteur var_freq
get_indice_var = function(var = "Power"){
  # Input :
  #        var : nom de la variable
  #        [character]

  # Output :indice de la variable dans le vecteur var_freq (! bien définir var_freq avant!) :
  #         [integer]
  return(which(var_freq==var))
}
sinistres_par_modalite_variable = function(data = data_, ind_var = 1){
  # Input :
  #        data_ : base de données de sinistralité
  #        [matrix]
  #        ind_var : indice de la variable que l'on souhaite étudier dans le vecteur var_freq
  #        [integer]
  # Output : 3 graphiques :
  #         nombre de sinistres selon les modalités de la variable choisie
  #         proportion de sinistres selon les modalités de la variable choisie
  #         coût moyen des sinistres selon les modalités de la variable choisie
  # [plot]
  k=ind_var
  a1=data%>%group_by_(var_freq[k])%>%summarise(n=sum(Exposure))
  a2=data%>%group_by_(var_freq[k])%>%filter(ClaimNb>0)%>%summarise(n = sum(ClaimNb))
  bb = a2$n/a1$n
  db <- src_sqlite(tempfile(), create = TRUE)
  temp <- copy_to(db, a2[,1])
  vec <- pull(temp)
  names(bb) = vec
  aa = a1$n
  names(aa) = vec
  # mm = data%>%group_by_(var_freq[k])%>%filter(ClaimAmount>0)%>%summarise_if(is.numeric,.funs=sum)
  # cc = mm$ClaimAmount/mm$ClaimNb
  # names(cc)= vec
  barplot(aa*100,main=paste('Exposition totale par', var_freq[k], '(en %)'),col="black",names.arg=vec)
  barplot(bb*100,main=paste('Proportion de sinistres par', var_freq[k], '(en %)'),col="grey",names.arg=vec)
  # barplot(cc*100,main=paste('Cout Moyen sinistre par', var_freq[k], '(en %)'),col="red",names.arg=vec)
}

sinistres_par_modalite_variable2 = function(data = data_, ind_var = 1, zoom = FALSE){
  # Input :
  #        data_ : base de données de sinistralité
  #        [matrix]
  #        ind_var : indice de la variable que l'on souhaite étudier dans le vecteur var_freq
  #        [integer]
  #        zoom : booléen indiquant si on agrandit la taille de la police par défaut ou non
  #        [bool]
  # Output : 3 graphiques :
  #         nombre de sinistres selon les modalités de la variable choisie
  #         proportion de sinistres selon les modalités de la variable choisie
  #         coût moyen des sinistres selon les modalités de la variable choisie
  k=ind_var
  a1=data%>%group_by_(var_freq[k])%>%summarise(n=sum(Exposure))
  a2=data%>%group_by_(var_freq[k])%>%filter(ClaimNb>0)%>%count()
  
  bb = a2$n/a1$n
  db <- src_sqlite(tempfile(), create = TRUE)
  temp <- copy_to(db, a2[,1])
  vec <- pull(temp)
  names(bb) = vec
  aa = a1$n
  names(aa) = vec
  # mm = data%>%group_by_(var_freq[k])%>%filter(ClaimAmount>0)%>%summarise_if(is.numeric,.funs=sum)
  # cc = mm$ClaimAmount/mm$ClaimNb
  # names(cc)= vec
  zz = NULL
  if(zoom==T){
    zz = zoom_func()
  } 
  g1 = ggplot()+geom_col(data=data.frame(x=names(aa),y=aa),aes(x=x,y=y),fill='black')+
    theme_bw()+scale_fill_stata()+ggtitle("Exposition Totale")+xlab(var_freq[k])+zz
  g2 = ggplot()+geom_col(data=data.frame(x=names(bb),y=bb),aes(x=x,y=y),fill='grey')+
    theme_bw()+scale_fill_stata()+ggtitle("Fréquence sinistres")+xlab(var_freq[k])+zz
  # g3 = ggplot()+geom_col(data=data.frame(x=names(cc),y=cc),aes(x=x,y=y),fill='red')+
  #   theme_bw()+scale_fill_stata()+ggtitle("CM Sinistres")+xlab(var_freq[k])+zz
  grid.arrange(g1, g2, ncol = 2)
}


hist_func = function(dat = freq2, mod = "Power", xlim = NULL){
  # Fonction renvoyant deux graphiques :
  # - l'exposition totale par modalité de la variable choisie
  # - fréquence de sinistres par modalité de la variable choisie
  # On peut ajouter un paramètre de limite en abscisse pour plus de lisibilité des graphiques
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

zoom_func = function(){
  # fonction permettant d'agrandir la police des légendes des plot ggplot
  return(theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+theme(axis.text.x = element_text(size=14)))}
