#########################################################
#  Transformación                                       #
#########################################################

# ------------------------------------------------------------------------#
# Ejemplo: Créditos de  Banco Alemán                                      #
#-------------------------------------------------------------------------#
library(Fahrmeir)
data(credit)
help(credit)

head(credit)
hist(credit$DM)

#Z-score
library(reshape)
credit$zDM<-rescaler(x=credit$DM,type="sd")
#credit$DM_Zscore<-scale(x=credit$DM)

#Min-Max
library(DMwR)
credit$mmDM<-ReScaling(x = credit$DM, t.mn=0, t.mx=1)


#Escalamiento decimal
maxvect = max(abs(credit$DM))
kvector = ceiling(log10(maxvect))
scalefactor = 10^kvector
credit$dsDM= credit$DM/scalefactor

#Sigmoidal
credit$sigDM = (1 - exp(-rescaler(x=credit$DM)))/(1 + exp(-rescaler(x=credit$DM)))

#Haciendo plots para ver el efecto de la normalizacion
par(mfrow=c(1,2))
plot(sort(credit$DM))
plot(sort(credit$sigDM))
par(mfrow=c(1,1))

#Softmax
#credit$softDM<-DMwR::SoftMax(credit$DM,lambda=2*pi)
credit$softDM2 = 1/(1 + exp(-rescaler(x=credit$DM)))

summary(credit)

#Gráfico de comparación
boxplot(credit[,c(9,10,11,12,13)])



