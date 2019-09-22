# Instalaci?n de Paquetes
install.packages(c("VIM","DEoptimR","minqa","nloptr","DMwR", "simputation"),
                 dependencies = c("Depends"))

#########################################################
#  Diagn?stico de datos perdidos                        #
#########################################################

library(VIM)
data(tao)
head(tao)
help(tao) #https://en.wikipedia.org/wiki/Tropical_Atmosphere_Ocean_project

summary(tao)

#Para ver que columnas tienen valores perdidos
which(colSums(is.na(tao))!=0)

#Para ver el porcentaje de valores perdidos en las columnas
colmiss=c(4,5,6)
per.miss.col=100*colSums(is.na(tao[,colmiss]))/dim(tao)[1]
per.miss.col

# Aggregation plot
a=aggr(tao,numbers=T)
a
summary(a)
aggr(tao,numbers=T, sortComb=TRUE, sortVar=TRUE, only.miss=TRUE)

#########################################################
#  Mecanismo: ?MCAR o MAR?                              #
#########################################################

# Matrix plot
matrixplot(tao)

#Parallel boxplots
VIM::pbox(tao[4:6], pos=1) 

# Prueba t de medias
t.test(Sea.Surface.Temp ~ is.na(Humidity), data=tao)

# Gr?ficos de dispersi?n
marginplot(tao[,c("Air.Temp", "Humidity")])

#########################################################
#  Eliminaci?n de casos (solo si es trivial)            #
#########################################################
tao.cl=na.omit(tao)

#########################################################
#  Imputaci?n                                           #
#########################################################

#----------------------------------------#
# Usando una medida de Tendencia Central #
#----------------------------------------#
library(DMwR)
tao.c<-centralImputation(tao)
tao.d<-initialise(tao,method="median")

#----------------------------------------#
# Usando Modelos de Regresi?n            #
#----------------------------------------#

library(simputation)
## Reemplazando por la media
tao.i <- impute_lm(tao, Air.Temp + Humidity ~ 1)
tao[c(108:110, 463,551:552),]
mean(tao$Air.Temp, na.rm = TRUE)
mean(tao$Humidity, na.rm = TRUE)
tao.i[c(108:110, 463,551:552),]

## Reemplazando por la media de cada a?o
tao.i <- impute_lm(tao, Air.Temp + Humidity ~ 1 | Year)
tao[c(108:110, 463,551:552),]
tao.i[c(108:110, 463,551:552),]

## Considerando otras variables como predictoras
tao.i <- impute_lm(tao, Air.Temp + Humidity ~ Sea.Surface.Temp + UWind + VWind)
tao[c(108:110, 463,551:552),]
tao.i[c(108:110, 463,551:552),]

## Adicionando un residuo aleatorio
tao.i <- impute_lm(tao, Air.Temp + Humidity ~ Sea.Surface.Temp + UWind + VWind, add_residual = "normal")
tao[c(108:110, 463,551:552),]
tao.i[c(108:110, 463,551:552),]

#----------------------------------------#
# K-Vecinos m?s cercanos                 #
#----------------------------------------#

# Usando la libreria VIM
tao_vars <- c("Air.Temp","Humidity")
tao_i_knn <- VIM::kNN(data=tao, variable=tao_vars) # EL k valor por defecto es 5

tao[c(108:110, 463,551:552),]
tao_i_knn[c(108:110, 463,551:552),]

aggr(tao_i_knn, delimiter="_imp", numbers=TRUE, prop=c(TRUE,FALSE))
aggr(tao_i_knn,delimiter="_imp",numbers=TRUE, prop=c(TRUE,FALSE), combined = TRUE)

barMiss(tao_i_knn, delimiter="imp", selection="any")

# Usando la libreria DMwR
tao_i_knn2<-DMwR::knnImputation(tao)
tao_i_knn2[c(108:110, 463,551:552),]

#-------------------------------------------------#
# Iterative robust model-based imputation (IRMI)  #
#-------------------------------------------------#
#Usa regresiones interactivas
tao.i.irmi <- irmi(tao)
summary(tao.i.irmi)
tao.i.irmi[c(108:110, 463,551:552),]
