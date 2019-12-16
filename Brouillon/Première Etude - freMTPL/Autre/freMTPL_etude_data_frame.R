##### Application  ######

data("freMTPLfreq")  # fréquence
data("freMTPLsev")   # sévérité
freq=freMTPLfreq
sev=freMTPLsev
# pour la base de sévérité, on conserve uniquement la somme des sinistres par PolicyID
temp=sev%>%group_by(PolicyID)%>%mutate(ClaimAmount=sum(ClaimAmount))
sev = unique(temp)

MTPL = merge(freq, sev, by = "PolicyID", all = T)
MTPL$ClaimAmount[is.na(MTPL$ClaimAmount)] = 0
letters=c("D","F","E","C","G","A","B") 
levels(MTPL$Brand)=letters
data_r = retraitement_base(MTPL)
expo = 0.25
MTPL$Exposure2 = pmax(MTPL$Exposure,expo)
MTPL = MTPL%>%mutate(Power2 = data_r$Power, CarAge2 = data_r$CarAge, DriverAge2 = data_r$DriverAge,
              Brand2 = data_r$Brand, Gas2 = data_r$Gas, Region2 = data_r$Region,
              Density2 = data_r$Density )
set.seed(2015)
A = sample(1:nrow(MTPL), round(0.9*nrow(MTPL)))
MTPL_app = MTPL[A, ]
MTPL_test = MTPL[-A,]
MTPL_app$ClaimAmountEcret = pmin(MTPL_app$ClaimAmount, 10000)
MTPL_app_sin = MTPL_app%>%filter(ClaimNb>0)
ecret = mean(MTPL_app_sin$ClaimAmount/MTPL_app_sin$ClaimNb) - mean(MTPL_app_sin$ClaimAmountEcret/MTPL_app_sin$ClaimNb) 

## 3) Ajustement du GLM -----
# a) GLM fréquence 
# i) GLM fréquence "moyen"
glm_MTPL_freq_trivial = glm(ClaimNb~1,data = MTPL_app,offset=log(Exposure), family=poisson(link = 'log'))
MTPL_app$pred_GLM_freq_trivial = fitted(glm_MTPL_freq_trivial)  
mse(MTPL_app$pred_GLM_freq_trivial  , MTPL_app$ClaimNb)
MTPL_test$pred_GLM_freq_trivial = predict(glm_MTPL_freq_trivial,MTPL_test, type="response")  
m1=mse(MTPL_test$pred_GLM_freq_trivial  , MTPL_test$ClaimNb)

glm_MTPL_freq_complet = glm(ClaimNb~Power2+CarAge2+DriverAge2+Gas2+Brand2+Region2+Density2,
                       data = MTPL_app,offset=log(Exposure),family=poisson(link="log"))
MTPL_app$pred_GLM_freq_complet = fitted(glm_MTPL_freq_complet  )
mse(MTPL_app$pred_GLM_freq_complet  , MTPL_app$ClaimNb)
MTPL_test$pred_GLM_freq_complet = predict(glm_MTPL_freq_complet, MTPL_test,type='response'  )
m2=mse(MTPL_test$pred_GLM_freq_complet  , MTPL_test$ClaimNb)
Poisson.Deviance = function(pred, obs){
  2*(sum(pred)-sum(obs)+sum(log((obs/pred)^(obs))))/length(pred)
}
Gamma.Deviance = function(pred,obs){
  2*sum((obs-pred)/pred-log(obs/pred))/length(pred)
}
glm_MTPL_freq_complet_caret = caret::train(ClaimNb~Power2+CarAge2+DriverAge2+Gas2+Brand2+Region2+Density2,
                                           data = MTPL_app, offset = log(MTPL_app$Exposure), method="glm", family=poisson, 
                                           trControl=caret::trainControl(method="none"))


# b) GLM coût
# i) GLM coût moyen
# Ajustement
glm_MTPL_cout_trivial = glm(ClaimAmount/ClaimNb~1,data = MTPL_app_sin, weights = MTPL_app_sin$ClaimNb, family=Gamma(link = 'log'))
glm_MTPL_cout_complet = glm(ClaimAmount/ClaimNb~Power2+CarAge2+DriverAge2+Gas2+Brand2+Region2+Density2
                            ,data = MTPL_app_sin, weights = MTPL_app_sin$ClaimNb, family=Gamma(link = 'log'))
glm_MTPL_cout_best = glm(formula = ClaimAmount/ClaimNb ~ Power2 + DriverAge2 + Gas2 + 
                           Brand2 + Region2 + Density2, family = Gamma(link = "log"), 
                         data = MTPL_app_sin, weights = MTPL_app_sin$ClaimNb) #trouvé avec stepAIC
MTPL_app$pred_GLM_cout_trivial = predict(glm_MTPL_cout_trivial, MTPL_app, type="response")
MTPL_test$pred_GLM_cout_trivial = predict(glm_MTPL_cout_trivial, MTPL_test, type="response")
MTPL_app$pred_GLM_cout_complet = predict(glm_MTPL_cout_trivial, MTPL_app, type="response")
MTPL_test$pred_GLM_cout_complet = predict(glm_MTPL_cout_complet, MTPL_test, type="response")

MTPL_app$pred_GLM_cout_best = predict(glm_MTPL_cout_best, MTPL_app, type="response")
MTPL_test$pred_GLM_cout_best = predict(glm_MTPL_cout_best, MTPL_test, type="response")

caret_xgb_param_freq_1 <- expand.grid(nrounds = 120, eta = 0.1, max_depth = 4,
                                      gamma = 0.5, colsample_bytree = 1, min_child_weight = 1, subsample = 0.5)
xgb_MTPL_freq1 = caret::train(ClaimNb/Exposure~Power2 + DriverAge2 + Gas2 + 
                               Brand2 + Region2 + Density2, data = MTPL_app, method = "xgbTree",
                              weight = MTPL_app$Exposure, tuneGrid = caret_xgb_param_freq_1,
                              verbose=F, trControl = caret::trainControl(method="none") ,objective = "count:poisson")
Poisson.Deviance(fitted(xgb_MTPL_freq1)*MTPL_app$Exposure, MTPL_app$ClaimNb)

# save(MTPL_app, file="MTPL_app.RData")
# save(MTPL_test, file="MTPL_test.RData")
# save(MTPL, file="MTPL.RData")
