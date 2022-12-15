#### DM de NOM1 et NOM2 #Ã  remplacer par vos noms
rm(list=objects())
graphics.off()

############################
#### A COMPLETER:
grp_id = 'I' #Ã  remplacer par le nom de votre groupe (voir sur Edunao)
setwd("D:/Centrale supelec/OneDrive - CentraleSupelec/2 ème année CS/Statistiques avancées/DM/")

############################
#### A NE PAS TOUCHER:
data_poll<-read.table("pollution.txt",sep=" ",header=T,dec=".")
dict = c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 'AA', 'AB', 'AC', 'AD', 'AE', 'AF', 'AG', 'AH', 'AI', 'AJ', 'AK', 'AL', 'AM' )
set.seed(which(dict==grp_id))
idx = sample(1:length(data_poll$pollution))[1:80]
data_poll = data_poll[idx, ]

############################
#### SUITE DU FICHIER A COMPLETER:
############################
#### 1. Representation graphique unidimensionnelle
# dimensions des graphiques
options(repr.plot.width=4, repr.plot.height=4)
boxplot(data_poll[,2:4]) # boîtes par groupe
boxplot(data_poll[,5:7])
boxplot(data_poll[,8:10])
boxplot(data_poll[,c(1,11)])


barplot(table(data_poll$pluie))
barplot(table(data_poll$vent))
pie(table(data_poll$vent))

options(repr.plot.width=8, repr.plot.height=8)
pairs(data_poll[,1:11])
############################
#### 2.


 
library(corrplot)
corrplot(cor(data_poll[,1:11]))

############################
#### 3.


library(lattice)
xyplot(pollution~Vx12|factor(vent),data=data_poll) 

############################
#### 5.
lm1=lm(pollution ~ (T9+T12+T15+Ne9+Ne12+Ne15+Vx9+Vx12+Vx15+vpollution)*vent*pluie,data=data_poll) ;summary(lm1)
graphics.off()
corrplot(cor(data_poll[data_poll$vent=="Sud",1:11]))

############################
#### 6.
summary(lm1)

############################
#### 7.
summary(lm1)

############################
#### 8.

## significavité de la régression 
anova(lm(pollution~1,data=data_poll),lm1)

############################
#### 9. ## 
library(olsrr)
p_model = ols_step_forward_p(lm1,penter=.05)
p_model
############################
#### 11.
## sélection du modèle
library(MASS)
model_aic = stepAIC(lm1,direction='both',trace=1) ## critère AIC
summary(model_aic)

final_model = stepAIC(lm1,k=log(80),trace=0) ## critère BIC
summary(final_model)


############################
#### 12.

## validation du modèle
par(mfrow=c(2,2), oma=rep(0,4))
plot(final_model)


