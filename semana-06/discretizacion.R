#########################################################
#  Discretización                                       #
#########################################################

# ------------------------------------------------------------------------#
# Ejemplo: Créditos de  Banco Alemán                                      #
#-------------------------------------------------------------------------#
library(Fahrmeir)
data(credit)
help(credit)

head(credit)
hist(credit$DM)

#Discretizacion con intervalos de igual amplitud
library(arules)
nbins<-nclass.scott(credit$DM) # Usando Scott
# nbins<-nclass.Sturges(credit$DM) #Usando Sturges
# nbins<-nclass.FD(credit$DM) # Usando Friedman-Diaconis
nbins

credit$DM_dia<-discretize(credit$DM,method="interval", breaks= nbins)
table(credit$DM_dia)

#Discretización con intervalos de igual frecuencia
credit$DM_dif<-discretize(credit$DM, method="frequency", breaks= 10)
table(credit$DM_dif)

#Discretización por clusters (K-Medias)
credit$DM_cl<-discretize(credit$DM, method = "cluster", breaks = 5)
table(credit$DM_cl)

#Discretizacion por entropía
library(discretization)
DM_entropia = mdlp(data = credit[,c("DM","Y")])

DM_entropia$cutp
head(DM_entropia$Disc.data, n = 15)
table(DM_entropia$Disc.data)

credit$DM_dentr <- DM_entropia$Disc.data[,1]
table(credit$DM_dentr)

#Discretizacion con chiMerge
DM_chim = chiM(data = credit[,c("DM","Y")],alpha = 0.005)

DM_chim$cutp
head(DM_chim$Disc.data, n = 15)
table(DM_chim$Disc.data)

credit$DM_chim <- DM_chim$Disc.data[,1]
table(credit$DM_chim)


head(credit, n = 15)