# Instalaci?n de Paquetes
#install.packages(c("cluster","fpc","mclust","flexmix","prabclus","diptest",
#                   "trimcluster","plyr","modeltools","mvtnorm","robustbase","kernlab"), dependencies = c("Depends"))


library(cluster)

library(foreign)

tiendas = read.csv("MassBaratos.csv",header = T,sep = ",")

### ------- Paso 1 --------- ####

tiendas1= tiendas[,c(-1,-2)]
tiendas1

### -------  M?todo K Means ---- ####

diss.tiendas=daisy(scale(tiendas1)) #disimilaridad 
par(mfrow=c(2,5))
for(h in 2:10){
  res=kmeans(scale(tiendas1),h,nstart = 100)
  plot(silhouette(res$cluster,diss.tiendas))
}
par(mfrow=c(1,1))

### ------ M?todo PAM ----- ######

par(mfrow=c(1,3))

res=pam(scale(tiendas1),10)
res

plot(res)

asw1<-numeric()
for(h in 2:10){
  res<-pam(scale(tiendas1),h)
  asw1[h-1]<-res$silinfo$avg.width
}
plot(2:10,asw1,type="b",xlab="k",ylab="ASW")

par(mfrow=c(1,1))

asw1


### ------ M?todo Funny ----- ######

asw2<-numeric()
for(h in 2:10){
  res<-fanny(scale(tiendas1),h,maxit=5000)
  asw2[h-1]<-res$silinfo$avg.width
}
plot(2:10,asw2,type="b",xlab="k",ylab="ASW")

asw2


### ------ M?todo Agnes ----- ######

res.agnes.single = agnes(scale(tiendas1), method="single")
res.agnes.single
plot(res.agnes.single)

res.agnes.ward=agnes(scale(tiendas1),method="ward")
res.agnes.ward
plot(res.agnes.ward)

# Usando matriz de disimilaridad
diss.tiendas=daisy(scale(tiendas1))
res.agnes.ward2 =agnes(diss.tiendas,method="ward")

par(mfrow=c(2,5))
for(h in 2:10){
  conglomerados=cutree(res.agnes.ward2,k=h)
  plot(silhouette(conglomerados,diss.tiendas))
}

Metodo  |  Criterio | Valor ( k =1) | ....... | (k=10)
--------------------------------------   
K-medias |Silueta
         |Kalisti-Kalaban
         |Dann
PAM      |Silueta
         |Kalisti-Kalaban
         |Dann

