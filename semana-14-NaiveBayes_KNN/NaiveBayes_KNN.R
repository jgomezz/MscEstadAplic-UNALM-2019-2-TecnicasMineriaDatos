#------------------------------------------------------------------#
#   Naive Bayes                                                    #
#------------------------------------------------------------------#

####################################################################
# # Ejemplo: Diabetes                                            # #
####################################################################
diabetes=read.csv("DiabetesTrain.csv")
#diabetes=read.csv(file.choose())
head(diabetes)

# Sin discretizar
library(e1071)
a<-naiveBayes(class ~ .,data = diabetes)
a
pred=predict(a,diabetes[,-4],type="raw")
pred1=factor(max.col(pred), labels = levels(diabetes$class))
library(caret)

confusionMatrix(pred1,diabetes[,4])

library(naivebayes)
a<-naive_bayes(class ~ .,data = diabetes)
a
pred=predict(a,diabetes[,-4])
confusionMatrix(pred,diabetes[,4])
predict(a,diabetes[,-4],type = "prob")

a<-naive_bayes(class ~ .,data = diabetes,usekernel = TRUE)
a
plot(a)
pred=predict(a,diabetes[,-4])
confusionMatrix(pred,diabetes[,4])

# Discretizando por el m?todo Chi-Merge
library(discretization)
d_diab=chiM(diabetes,0.01)$Disc.data
for (i in 1:3){
  d_diab[,i]=as.factor(d_diab[,i])}
b<-naive_bayes(class ~ .,data = d_diab)
b
pred=predict(b,d_diab[,-4])
confusionMatrix(pred,d_diab[,4])



####################################################################
# # Otros ejemplos                                               # #
####################################################################

## S?lo datos categ?ricos:
library(e1071)
data(HouseVotes84, package = "mlbench")
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,])
predict(model, HouseVotes84[1:10,], type = "raw")

pred <- predict(model, HouseVotes84)
confusionMatrix(pred, HouseVotes84$Class)

## Usando suavizamiento de Laplace:
model <- naiveBayes(Class ~ ., data = HouseVotes84, laplace = 3)
pred <- predict(model, HouseVotes84[,-1])
confusionMatrix(pred, HouseVotes84$Class)


## Ejemplo con una tabla de contingencia:
data(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
predict(m, as.data.frame(Titanic))

## Ejemplo con predictores m?tricos:
data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## De manera alternativa:
m <- naiveBayes(iris[,-5], iris[,5])
m
confusionMatrix(predict(m, iris), iris[,5])

#------------------------------------------------------------------#
#  K-NN                                                            #
#------------------------------------------------------------------#

####################################################################
# # Ejemplo: Diabetes                                            # #
####################################################################
diabetes=read.csv("DiabetesTrain.csv")
#diabetes=read.csv(file.choose())
head(diabetes)

library(class)
b<-knn(train = diabetes[,1:3],test = diabetes[,1:3],cl = diabetes[,4])
#Estimacion del error por resubstituci?n
confusionMatrix(b,diabetes[,4])

# con K=3
k_3 <- knn(train = diabetes[,1:3],test = diabetes[,1:3],cl = diabetes[,4], k = 3)
confusionMatrix(k_3,diabetes[,4])
mean(k_3 == diabetes[,4])

# con K=7
k_7 <- knn(train = diabetes[,1:3],test = diabetes[,1:3],cl = diabetes[,4], k = 7)
confusionMatrix(k_7,diabetes[,4])
mean(k_7 == diabetes[,4])

# con K=15
k_15 <- knn(train = diabetes[,1:3],test = diabetes[,1:3],cl = diabetes[,4], k = 15)
confusionMatrix(k_15,diabetes[,4])
mean(k_15 == diabetes[,4])

## Usando validaci?n cruzada
set.seed(007)
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=1)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=3)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=5)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=7)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=9)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=11)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=13)==diabetes[,4])
mean(knn.cv(train = diabetes[,1:3],cl = diabetes[,4],k=15)==diabetes[,4])

K_3 <- knn(train = diabetes[,1:3],test = diabetes[,1:3],cl = diabetes[,4], k = 3, prob = TRUE)
confusionMatrix(K_3,diabetes[,4])
mean(K_3 == diabetes[,4])

# Obtener la pro. de votos
K_3_prob <- attr(K_3, "prob")

# Valores predichos
head(K_3)

# Prop. de "votos"
head(K_3_prob)

#===============================================================================
# Usando Caret
#===============================================================================
# N?mero de repeticiones y semilla
k <- 10
repeticiones <- 5
hiperparametros <- data.frame(k = c(1, 3, 5, 7, 9,  11, 13, 15, 17, 21, 31, 51))

set.seed(666)
seeds <- vector(mode = "list", length = (k * repeticiones) + 1)
for (i in 1:(k * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(k * repeticiones) + 1]] <- sample.int(1000, 1)


# AJUSTE DEL MODELO
# ==============================================================================
cvmod.knn <-train(class ~.,
                    data = diabetes,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = trainControl(method = "repeatedcv",
                                             number = k, seeds = seeds,
                                             repeats = repeticiones, returnResamp = "final") 
)
cvmod.knn
cvmod.knn$bestTune

library(ggplot2)
ggplot(cvmod.knn , highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evoluci?n del accuracy del modelo KNN", x = "K") +
  theme_bw()


####################################################################
# # Ejemplo: Admision                                            # #
####################################################################
admision <- read.csv("binary.csv")
admision$rank <- factor(admision$rank)
admision$admit <- factor(admision$admit)
head(admision)

#
k_11_a <- knn(train = admision[,2:3],test = admision[,2:3],cl = admision[,1], k = 11)
confusionMatrix(k_11_a,admision[,1])
# Se obtiene el accuracy es 

# Se mete a las ordinales
k_11_b <- knn(train = admision[,2:4],test = admision[,2:4],cl = admision[,1], k = 11)
confusionMatrix(k_11_b,admision[,1])
# El accuracy mejora

# la otra opcion es convertir las variabels ordinales a dummy
library(dummies)
rank_d <-dummy(admision$rank, sep = "_")  # se van a crear 4 variables binarias
#rank_e <-model.matrix( ~ rank-1, data=admision)
# Se crea una nueva base de datos 
admision_d<-cbind(admision[,-4], rank_d)

# se construye nuevamente el modelo
k_11_c <- knn(train = admision_d[,-1],test = admision_d[,-1],cl = admision[,1], k = 11)
confusionMatrix(k_11_c,admision[,1])
# El accuracy es menor , porque la diferencia de las escalas es diferente

# Se debe normalizar

# Usando normalizaci?n Min-Max
library(DMwR)
admision_mm<-admision[,2:3]
admision_mm<-sapply(admision[,2:3],FUN=ReScaling, t.mn=0, t.mx=1)
admision_mm<-cbind(admision_mm,rank_d)
summary(admision_mm)

k_11_mm <- knn(train = admision_mm,test = admision_mm,cl = admision[,1], k = 11)
confusionMatrix(k_11_mm,admision[,1])

# Usando normalizaci?n SoftMax  # Estan ormalizacion comprime las colas
admision_sm<-admision[,2:3]
admision_sm<-SoftMax(admision[,2:3],lambda=2*pi)
admision_sm<-cbind(admision_sm,rank_d)
summary(admision_sm)

# summary es 74.75% , que es mejor.

k_11_sm <- knn(train = admision_sm,test = admision_sm,cl = admision[,1], k = 11)
confusionMatrix(k_11_sm,admision[,1])

