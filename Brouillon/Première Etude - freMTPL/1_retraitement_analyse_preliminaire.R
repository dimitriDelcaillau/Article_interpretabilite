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
# Ici : il s'agit des variables explicatives de la base freMTPL (package CASdatasets)
# Variables explicatives pour la fréquence :
var_freq=c("Power","CarAge","DriverAge","Brand","Gas","Region","Density") 
# Variables explicatives pour le coût : 
var_cout=c("Power","CarAge","DriverAge","Brand","Gas","Region","Density") 

# Les variables du nombre de sinistres et du montant des sinistres doivent s'appeler :
# ClaimNb et ClaimAmount 
# La variable d'exposition doit s'appeler : Exposure !!! 
# (les renommer si différent dans la base étudiée)
#####################################################################################




############################################################################################
####### Importation des données : base de données freMTPL du package "CASdatasets" #########
############################################################################################
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

################################################################################################
#####################Description des données avant retraitement#################################
################################################################################################
# Dans cette partie, on réalise une analyse des données de sinistres de responsabailité que l'on a notre disposition.
# Dans les deux bases de données freMTPLfreq, freMTPLsev, les variables explicatives de risque sont collectées pour 413 169 polices responsabilité civile moteur (observées sur une année). De plus, nous avons les numéros de sinistres par police ainsi que les montants de sinistres correspondants. freMTPLfreq contient les variables explicatives (âge,puissance...) et le nombre de sinistres, tandis que freMTPLsev contient le montant des sinistres et l'ID de la police correspondante.
# Les variables à notre disposition, dans la base de données de fréquence sont:  
#   - PolicyID : le numéro de police (pas explicative, sert à relier les deux bases de données)  
# 
# - ClaimNB : le nombre de sinistres (ici entre 0 et 4)  
# 
# - Exposure : l'exposition (en fraction d'années)  
# 
# - Power : la puissance de la voiture (variable catégorielle ordonnée de "d" à "o")  
# 
# - CarAge : l'âge du véhicule (en années)  
# 
# - DriverAge : l'âge du conducteur (en années)
# 
# - Brand : la marque de la voiture, divisée en plusieurs groupes : 
#   
# A- Renaut Nissan and Citroen,     
# B- Volkswagen, Audi, Skoda and Seat  
# C- Opel, General Motors and Ford   
# D- Fiat  
# E- Mercedes Chrysler and BMW  
# F- Japanese (except Nissan) and Korean  
# G- other  
# 
# - Gas : Gaz de la voiture : "Diesel" ou "Regular"   
# 
# - Region : la région (en France) des polices, selon les standards français de classification 
# 
# - Density : nombre d'habitants par $km^2$ dans la ville où conduit le conducteur
# 
# Les variables dans la base de données de sévérité sont:
# 
# - PolicyID : le numéro de police  
# 
# - ClaimAmount : le montant du sinsitre, vue comme à une date récente  


################################################################################################
#################################### Fonctions utiles###########################################
################################################################################################


# Fusion base fréquence / sévérité ------
merge_freq_sev = function(freq = freq, sev = sev, var_ID = "PolicyID"){
  # Input : 
  #       freq : base de données de fréquence [data.frame]
  #       sev  : base de données de sévérité [data.frame]
  #       var_ID : variable qui permet de regrouper les bases de fréquence et sévérité [character]
  # Output : base de données "fusionnée" de fréquence et sévérité
  data=merge(freq,sev,by=var_ID,all=T)  # base "mergée"
  data$ClaimAmount[is.na(data$ClaimAmount)]=0 # ClaimAmount = NA => pas sinistre
  data$Power=factor(data$Power) #
  return(data)
}
# data = merge_freq_sev(freq, sev, "PolicyID")

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
#(utile pour le GLM: rend les variables catégorielles) 
retraitement_base = function(data = data){
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

data_ = retraitement_base(data)

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

mat_corelation_plot_2 = function(mat_corr = mat_corr){
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
  a2=data%>%group_by_(var_freq[k])%>%filter(ClaimAmount>0)%>%count()
  bb = a2$n/a1$n
  db <- src_sqlite(tempfile(), create = TRUE)
  temp <- copy_to(db, a2[,1])
  vec <- pull(temp)
  names(bb) = vec
  aa = a1$n
  names(aa) = vec
  mm = data%>%group_by_(var_freq[k])%>%filter(ClaimAmount>0)%>%summarise_if(is.numeric,.funs=sum)
  cc = mm$ClaimAmount/mm$ClaimNb
  names(cc)= vec
  barplot(aa*100,main=paste('Exposition totale par', var_freq[k], '(en %)'),col="black",names.arg=vec)
  barplot(bb*100,main=paste('Proportion de sinistres par', var_freq[k], '(en %)'),col="grey",names.arg=vec)
  barplot(cc*100,main=paste('Cout Moyen sinistre par', var_freq[k], '(en %)'),col="red",names.arg=vec)
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
  a2=data%>%group_by_(var_freq[k])%>%filter(ClaimAmount>0)%>%count()
  
  bb = a2$n/a1$n
  db <- src_sqlite(tempfile(), create = TRUE)
  temp <- copy_to(db, a2[,1])
  vec <- pull(temp)
  names(bb) = vec
  aa = a1$n
  names(aa) = vec
  mm = data%>%group_by_(var_freq[k])%>%filter(ClaimAmount>0)%>%summarise_if(is.numeric,.funs=sum)
  cc = mm$ClaimAmount/mm$ClaimNb
  names(cc)= vec
  zz = NULL
  if(zoom==T){
    zz = zoom_func()
  } 
  g1 = ggplot()+geom_col(data=data.frame(x=names(aa),y=aa),aes(x=x,y=y),fill='black')+
    theme_bw()+scale_fill_stata()+ggtitle("Exposition Totale")+xlab(var_freq[k])+zz
  g2 = ggplot()+geom_col(data=data.frame(x=names(bb),y=bb),aes(x=x,y=y),fill='grey')+
    theme_bw()+scale_fill_stata()+ggtitle("Fréquence sinistres")+xlab(var_freq[k])+zz
  g3 = ggplot()+geom_col(data=data.frame(x=names(cc),y=cc),aes(x=x,y=y),fill='red')+
    theme_bw()+scale_fill_stata()+ggtitle("CM Sinistres")+xlab(var_freq[k])+zz
  grid.arrange(g1,g2,g3,ncol=3)
}
change_expo_assure = function(data = data_, expo_min = 0.25){
    # Input :
    #        data : base de données de sinistralité (coût et fréquence)
    #        [data.frame]
    #        expo_min : si expo<expo_min : l'expo est "cappé" à expo_min (expo = max(expo_min,expo))
    #         [numeric] (entre 0 et 1)
    # Output :base de données contenant uniquement les assurés avec l'exposition modifiée
    #         [data.frame]
  res = data
  res$Exposure[res$Exposure<expo_min] = expo_min
  return(res)
}

zoom_func = function(){
  # fonction permettant d'agrandir la police des légendes des plot ggplot
  return(theme(axis.text.y = element_text(size=18))+theme(axis.title.x = element_text(size=14), title = element_text(size=16))+theme(axis.text.x = element_text(size=14)))}
