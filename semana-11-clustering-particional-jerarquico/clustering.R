# Instalación de Paquetes
install.packages(c("cluster","fpc","mclust","flexmix","prabclus","diptest",
			"trimcluster","plyr","modeltools","mvtnorm","robustbase","kernlab"),
                 dependencies = c("Depends"))
#                 dependencies = c("Depends", "Suggests"))

#########################################################
#  Distancias                                           #
#########################################################
set.seed(666)
x =matrix(rnorm(20), nrow=5)
dist(x)
dist(x, method= "manhattan",diag = TRUE)
dist(x, method= "maximum",upper = TRUE)

library(cluster)
#x =matrix(rnorm(20), nrow=5)
daisy(x)
x = cbind(rnorm(10),sample(1:3,10,replace=T))
x<-as.data.frame(x)
x[,2]<-as.factor(x[,2])
daisy(x)

#=======================================================#
#  Métodos Particionales                                #
#=======================================================#

#########################################################
#  K-Medias                                             #
#########################################################

#-------------------------------------------------------#
# Simulacion                                            #
#-------------------------------------------------------#

#Semilla para replicar resultados
set.seed(007)
x=cbind(rnorm(100,1000,100),
        c(rnorm(50),rnorm(50,10,1)))
plot(x,pch=16)
#k-means datos originales
res=kmeans(x,2)
plot(x,col=c("green","red")[res$cluster],pch=16)
# Estandarización
xs=scale(x)
plot(xs,pch=16)
#k-means datos estandarizados
res=kmeans(xs,2)
plot(x,col=c("green","red")[res$cluster],pch=16)


#Semilla para replicar resultados
set.seed(2)

#Simulación de las observaciones
x=matrix(rnorm(50*2), ncol=2)

#Creando 2 clusters en los datos.  Las 25 primeras observaciones tienen un cambio
#en la media
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

#Se realiza el análisis cluster considerando K=2
km.out2=kmeans(x,2,nstart=20)
km.out2

#Visualiza el cluster de pertenencia de las observaciones
km.out2$cluster

#Gráfica de los resultados
plot(x, col=(km.out2$cluster+1), 
     main="Resultados del Clustering  K-Medias con K=2", xlab="", ylab="", pch=20, cex=2)

#Se realiza el análisis considerando K=3
set.seed(4)
km.out3=kmeans(x,3,nstart=20)
km.out3
plot(x, col=(km.out3$cluster+1), main="Resultados del Clustering  K-Medias con K=3", xlab="", ylab="", pch=20, cex=2)

#Uso del parámetro nstart para múltiples asignaciones
set.seed(3)
km.out=kmeans(x,3,nstart=1)
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss


#-------------------------------------------------------#
# Ejemplo distritos                                     #
#-------------------------------------------------------#
library(foreign)
#distritos=read.spss("distritos.sav",
distritos=read.spss(file.choose(),
                    use.value.labels=TRUE, max.value.labels=Inf, to.data.frame=TRUE)
colnames(distritos) <- tolower(colnames(distritos))
nombres=distritos[,1]
distritos=distritos[,-1]
rownames(distritos)=nombres
head(distritos)

res<-kmeans(scale(distritos),2)
res

#------------------------------------#
# Determinar numero de conglomerados #
#------------------------------------#

# Suma de cuadrados dentro de clusters
wss<-numeric()
for(h in 2:10){
  b<-kmeans(scale(distritos),h)
  wss[h-1]<-b$tot.withinss
}
plot(2:10,wss,type="b")

# Silueta
diss.distritos=daisy(scale(distritos))
par(mfrow=c(1,3))
for(h in 2:4){
  res=kmeans(scale(distritos),h)
  plot(silhouette(res$cluster,diss.distritos))
}
par(mfrow=c(1,1))

# Criterio de Calinski-Harabasz
library(fpc)
ch<-numeric()
for(h in 2:10){
  res<-kmeans(scale(distritos),h,nstart = 100)
  ch[h-1]<-calinhara(scale(distritos),res$cluster)
}
plot(2:10,ch,type="b",xlab="k",
     ylab="Criterio de Calinski-Harabasz")

kmeansruns(scale(distritos),criterion="ch")
kmeansruns(scale(distritos),criterion="asw")

res=kmeans(scale(distritos),2)
plotcluster(distritos,res$cluster)

clusplot(distritos,res$cluster, color = TRUE,
         shade = TRUE, labels =2,lines=0,
         main ="Gráfico de Conglomerados")

#-----------------------------------------#
# Perfilado y caracterización de clusters #
#-----------------------------------------#

# Adicionar los cluster a la base de datos
distritos.new<-cbind(distritos,res$cluster)
colnames(distritos.new)<-c(colnames(distritos.new[,-length(distritos.new)]), "cluster.km")
head(distritos.new)

# Tabla de medias
med<-aggregate(x = distritos.new[,1:7],by = list(distritos.new$cluster.km),FUN = mean)
med

# Describir variables
par(mfrow=c(2,4))
for (i in 1:length(distritos.new[,1:7])) {
boxplot(distritos.new[,i]~distritos.new$cluster.km, main=names(distritos.new[i]), type="l")
}
par(mfrow=c(1,1))


#########################################################
#  PAM                                                  #
#########################################################

# library(cluster)
# data(iris)
# a=pam(iris[,1:4],3,diss=F)
# a$clustering
# table(a$clustering)
# table(a$clustering,iris[,5])
# 
# library(arules)
# data(AdultUCI)
# censusn<-AdultUCI
# census<-na.omit(censusn)
# census<-as.data.frame(census[1:1000,])
# b<-daisy(census[,-15])
# summary(b)
# a=pam(b,2,diss=T)
# a$clustering
# table(a$clustering)
# table(a$clustering,census[1:1000,15])

res=pam(scale(distritos),2)
res

plot(res)

asw<-numeric()
for(h in 2:10){
res<-pam(scale(distritos),h)
asw[h-1]<-res$silinfo$avg.width
}
plot(2:10,asw,type="b",xlab="k",ylab="ASW")

par(mfrow=c(1,3))
for(h in 2:4){
res=pam(scale(distritos),h)
plot(res,which.plots=2)
}

par(mfrow=c(1,1))
ch<-numeric()
for(h in 2:10){
res<-pam(scale(distritos),h)
ch[h-1]<-calinhara(scale(distritos),res$clustering)
}
plot(2:10,ch,type="b",xlab="k",
ylab="Criterio de Calinski-Harabasz")

pamk(scale(distritos),criterion="asw")
pamk(scale(distritos),criterion="ch")

res=pam(scale(distritos),2)
plotcluster(distritos,res$clustering)

#########################################################
#  CLARA                                                #
#########################################################

res=clara(scale(distritos),2)
res
plot(res)

asw<-numeric()
for(h in 2:10){
res<-clara(scale(distritos),h)
asw[h-1]<-res$silinfo$avg.width
}
plot(2:10,asw,type="b",xlab="k",ylab="ASW")

par(mfrow=c(1,3))
for(h in 2:4){
res=clara(scale(distritos),h)
plot(res,which.plots=2)
}

ch<-numeric()
for(h in 2:10){
res<-clara(scale(distritos),h)
ch[h-1]<-calinhara(scale(distritos),res$clustering)
}
plot(2:10,ch,type="b",xlab="k",
ylab="Criterio de Calinski-Harabasz")

pamk(scale(distritos),criterion="asw",usepam=FALSE)
pamk(scale(distritos),criterion="ch",usepam=FALSE)

res=clara(scale(distritos),2)
plotcluster(distritos,res$clustering)

#########################################################
#  Fuzzy C-Means (FCM): FANNY                           #
#########################################################

# Considerando 2 conglomerados
res.fanny =fanny(scale(distritos),2)
res.fanny

head(res.fanny$membership, 3) # Coeficientes de pertenencia
res.fanny$coeff # Coeficiente de Dunn
head(res.fanny$clustering) # Grupos de pertenencia

# Visualizar los conglomerados y la bondad de ajuste de los resulgados
plot(res.fanny)

library(factoextra)
fviz_cluster(res.fanny, ellipse.type = "norm", repel = TRUE,
             palette = "jco", ggtheme = theme_minimal(),
             legend = "right")

fviz_silhouette(res.fanny, palette = "jco",
                ggtheme = theme_minimal())

# Seleccionar el número de conglomerados
# Indice de Silueta
asw<-numeric()
for(h in 2:10){
res<-fanny(scale(distritos),h,maxit=5000)
asw[h-1]<-res$silinfo$avg.width
}
plot(2:10,asw,type="b",xlab="k",ylab="ASW")

par(mfrow=c(1,3))
for(h in 2:4){
res=fanny(scale(distritos),h)
plot(res,which.plots=2)
}
par(mfrow=c(1,1))

# Indice de Calinski-Harabasz
ch<-numeric()
for(h in 2:10){
res<-fanny(scale(distritos),h,maxit=5000)
ch[h-1]<-calinhara(scale(distritos),res$clustering)
}
plot(2:10,ch,type="b",xlab="k",
ylab="Criterio de Calinski-Harabasz")
  

#########################################################
#  Métodos Jerárquicos                                  #
#########################################################

#-------------------------------------------------------#
#  Cluster Jerárquico Aglomerativo                      #
#-------------------------------------------------------#
# Usando el enlace de Ward
res.hc=hclust(dist(scale(distritos)),method="ward.D")
plot(res.hc)

# Cortando el dendrograma considerando cuatro conglomerados
(res.hc4=cutree(res.hc, k=4))
fviz_dend(res.hc, cex = 0.5, k = 4, palette = "jco")


# Cortando el dendrograma considerando una altura de 20
(res.hc4=cutree(res.hc, h=20))

#Clustering jerarquico aglomerativo usando Agnes
library(cluster)

# Usando el enlace simple
res.hc.s=hclust(dist(scale(distritos)),method="single")
plot(res.hc.s)

## Comparación de enlaces
# Enlace de Ward
res.coph <- cophenetic(res.hc) # Distancia cofenética
cor(dist(scale(distritos)), res.coph) # Correlación entre la distancia cofenética y la original
# Enlace Simple
res.coph <- cophenetic(res.hc.s) # Distancia cofenética
cor(dist(scale(distritos)), res.coph) # Correlación entre la distancia cofenética y la original

#-------------------------------------------------------#
#  AGNES                                                #
#-------------------------------------------------------#

res.agnes.single = agnes(scale(distritos), method="single")
res.agnes.single
plot(res.agnes.single)

res.agnes.ward=agnes(scale(distritos),method="ward")
res.agnes.ward
plot(res.agnes.ward)

# Usando matriz de disimilaridad
diss.distritos=daisy(scale(distritos))
res.agnes.ward2 =agnes(diss.distritos,method="ward")

# Determinando el número óptimo de conglomerados
# Indice de Silueta
par(mfrow=c(1,3))
for(h in 2:4){
conglomerados=cutree(res.agnes.ward2,k=h)
plot(silhouette(conglomerados,diss.distritos))
}
par(mfrow=c(1,1))

fviz_dend(res.agnes.ward2, cex = 0.5,
          k = 2, 
          palette = "jco" 
)


#-------------------------------------------------------#
#  Algoritmo Divisivo (DIANA)                           #
#-------------------------------------------------------#

res.diana=diana(scale(distritos))
res.diana
plot(res.diana)

# Usando matriz de disimilaridad
diss.distritos=daisy(scale(distritos))
res=diana(diss.distritos)

# Determinando el número óptimo de conglomerados
# Indice de Silueta
par(mfrow=c(1,3))
for(h in 2:4){
conglomerados=cutree(res,h)
plot(silhouette(conglomerados,diss.distritos))
}

fviz_dend(res.diana, cex = 0.5,
          k = 2, 
          palette = "jco" 
)

#########################################################
#  DBSCAN                                               #
#########################################################

data("multishapes")
df <- multishapes[, 1:2]
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df,  geom = "point", 
             ellipse= FALSE, show.clust.cent = FALSE,
             palette = "jco", ggtheme = theme_classic())

library("fpc")
library(dbscan)
set.seed(123)
db <- dbscan::dbscan(df, 
                  eps = 0.15, # radio de la vecindad
                  MinPts = 5) # mínimo número de puntos con radio eps
print(db)

# Graficar los resultados de DBSCAN
library("factoextra")
fviz_cluster(db, data = df, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point",palette = "jco", ggtheme = theme_classic())


# Valor óptimo de eps

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

# Ejemplo de Distritos
dbscan::kNNdistplot(scale(distritos), k =  2)
abline(h = 1.45, lty = 3)

set.seed(123)
res.db <- dbscan::dbscan(scale(distritos), 
                  eps = 1.45, # radio de la vecindad
                  MinPts = 2) # mínimo número de puntos con radio eps
print(res.db)

fviz_cluster(res.db, data = distritos, stand = FALSE,
             ellipse = TRUE, show.clust.cent = FALSE,
             geom = c("point","text"),palette = "jco", 
             ggtheme = theme_classic(),
             labelsize = 8,
             ellipse.type = "convex")


#########################################################
#  Evaluación de Conglomerados                          #
#########################################################

#---------------------------------------------#
# Problema de disección: Evaluar la tendencia #
#---------------------------------------------#

# Inspección visual: PCA
fviz_pca_ind(prcomp(scale(distritos)), title = "PCA - Distritos", 
             palette = "jco",
             geom = "point", ggtheme = theme_classic(),
             legend = "bottom")

# Con los resultados de Agnes
set.seed(123)
fviz_cluster(list(data = scale(distritos), cluster = cutree(res.agnes.ward2, k = 2)),
             ellipse.type = "norm", geom = "point", stand = FALSE,
             palette = "jco", ggtheme = theme_classic())

# Estadístico de Hopkins 
res <- get_clust_tendency(scale(distritos), n = nrow(distritos)-1, graph = FALSE)
res$hopkins_stat #Debe ser mayor a 0.5

#---------------------------------------------#
# Número óptimo de conglomerados              #
#---------------------------------------------#
library(factoextra)

# Para K-Medias
# Elbow method
fviz_nbclust(scale(distritos), kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(scale(distritos), kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(scale(distritos), kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

library(NbClust)
nb <- NbClust(scale(distritos), min.nc = 2,
              max.nc = 10, method = "kmeans", index ="all")

#---------------------------------------------#
# Comparación de Algoritmos de Conglomerados  #
#---------------------------------------------#
library(clValid)
clmethods <- c("hierarchical","kmeans","pam","agnes","diana")

# Medidas de validación interna 
intern <- clValid(scale(distritos), nClust = 2:10,
                  clMethods = clmethods, validation = "internal")
summary(intern) 
plot(intern)

# Medidas de estabilidad
stab <- clValid(scale(distritos), nClust = 2:10, clMethods = clmethods,
                validation = "stability")
summary(stab)

# Mostrar solo scores óptimos
optimalScores(intern)
optimalScores(stab)

# Otras medidas de validacion de Clusters
# Ver: https://www.jstatsoft.org/article/view/v025i04

