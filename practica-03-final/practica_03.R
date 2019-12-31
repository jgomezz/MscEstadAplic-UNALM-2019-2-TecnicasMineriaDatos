#-------------------------------------------------------------------------#
#                              PRACTICA 03                                #
#-------------------------------------------------------------------------#
# EVALUCION PREDICTIVA DE MODELOS DE CLASIFICACI?N                        #
# ------------------------------------------------------------------------#

library(caret)
library(haven)
library(tidyverse)
library(Fahrmeir)
library(ggplot2)
library(ggpubr)
library(discretization)
library(randomForest)
library(recipes)
library(C50)
library(doParallel)
library(ranger)
library(e1071)
library(gbm)
library(party)
library(partykit)
library("readxl")

#############################################
##  1.- Lectura de datos
#############################################
credit <- read_excel("practica-03-final/base_final.xlsx", sheet = "Hoja2")

#retira tilde a SEGMENTO_CONSTRUCCION
colnames(credit)[6] <- "SEGMENTO_CONSTRUCCION"

str(credit)

# Transformamos a factor las variables no continuas
credit$FLG_MIGRA <- as.factor(credit$FLG_MIGRA)
credit$FAD_1 <- as.factor(credit$FLG_MIGRA)
credit$DELTA_DE_ENTIDADES_1	<- as.factor(credit$DELTA_DE_ENTIDADES_1)
credit$SEGMENTO_CONSTRUCCION	<- as.factor(credit$SEGMENTO_CONSTRUCCION)
credit$MONTO_APROBADO	<- as.factor(credit$MONTO_APROBADO)
credit$RANGO_DE_PD	<- as.factor(credit$RANGO_DE_PD)

str(credit)
head(credit)
names(credit)
summary(credit)

#############################################
##  2.-  Exploracion de datos
#############################################

library(agricolae)

# FLG_MIGRA : Indica si una empresa ha sido declarado en Default

ni    <- table(credit$FLG_MIGRA)
pi    <- round(prop.table(table(credit$FLG_MIGRA))*100,1)
tabla <- rbind(ni,pi)
tabla
barplot(pi, main="Distribución de Empresas en Default", 
        col="blue",xlab="Default",ylab="Porcentaje")

# VENTAS : Nivel de Ventas mensuales realizadas por la 
#          empresa al momento de la evaluación crediticia. 
#          (Unidad: Soles Peruanos)

tabla <- (table.freq(hist(credit$VENTAS,plot=FALSE)))  # "Sturges"
tabla
h1<-hist(credit$VENTAS, xlab="Ventas",
         ylab="Número de empresas", 
         main = "Distribución de Ventas")

# PASIVOS : Nivel de pasivos que posee la empresa al momento de la 
#           evaluación crediticia. (Unidad: Soles Peruanos).

tabla <- (table.freq(hist(credit$PASIVOS,plot=FALSE)))  # "Sturges"
tabla
h1<-hist(credit$PASIVOS, xlab="Pasivos",
         ylab="Número de empresas", 
         main = "Distribución de Pasivos")

# DEUDA TOTAL : Deuda que posee la empresa al momento de la 
#               evaluación crediticia. (Unidad: Soles Peruanos).

tabla <- (table.freq(hist(credit$DEUDA_TOTAL,plot=FALSE)))  # "Sturges"
tabla
h1<-hist(credit$DEUDA_TOTAL, xlab="Deudas Total",
         ylab="Número de empresas", 
         main = "Distribución de Deudas Total")

# MONTO APROBADO : Monto de desembolso de la empresa 
#                  (Unidad: Soles Peruanos).
#
#    1 --> <= 12k
#    2 --> <= 30k
#    3 --> <= 90k
#    4 --> <= 300k
#    5 --> <= 750k
#    6 --> <= 1.2M
#    7 --> >= 1.2M

tabla <- (table.freq(hist(credit$MONTO_APROBADO,plot=FALSE)))  # "Sturges"
tabla
h1<-hist(credit$MONTO_APROBADO, xlab="Deudas Total",
         ylab="Número de empresas", 
         main = "Distribución de Monto aprobado")

# SEGMENTO_CONSTRUCCION : Nivel de información que se posee del cliente 
#                         al momento de la evaluación.
#
#    1 --> Sin Información
#    2 --> Con Poca Información
#    3 --> Información Regular 1
#    4 --> Información Regular 2
#    5 --> Cliente con información Completa 1
#    6 --> Cliente con información Completa 2

ni    <- table(credit$SEGMENTO_CONSTRUCCION)
pi    <- round(prop.table(table(credit$SEGMENTO_CONSTRUCCION))*100,1)
tabla <- rbind(ni,pi)
tabla
barplot(pi, main="Nivel de información del Cliente", 
        col="blue",xlab="Información",ylab="Porcentaje")


# FAD_1 : Flujo antes de Deuda que posee la empresa al momento 
#         de la evaluación crediticia.
#
#  999 -->   Missing
#    1 --> [ 0k-20k]
#    2 --> [20k-30k]
#    3 --> [30k-40k]
#    4 --> [40k-60k]
#    5 --> [60k-Más]

ni    <- table(credit$FAD_1)
pi    <- round(prop.table(table(credit$FAD_1))*100,1)
tabla <- rbind(ni,pi)
tabla
barplot(pi, main="Flujo antes de la Deuda", 
        col="blue",xlab="Flujo",ylab="Porcentaje")


# DELTA_DE_ENTIDADES_1 : Variación de entidades en los 6 últimos meses 
#                        de la empresa al momento de la evaluación crediticia.
#
#   -2 --> <= -2
#   -1 --> -1
#    0 --> 0
#    1 --> 1
#    2 --> >=2

ni    <- table(credit$DELTA_DE_ENTIDADES_1)
pi    <- round(prop.table(table(credit$DELTA_DE_ENTIDADES_1))*100,1)
tabla <- rbind(ni,pi)
tabla
barplot(pi, main="Variación de entidades en los 6 últimos meses", 
        col="blue",xlab="Variación",ylab="Porcentaje")

# RANGO_DE_PD : Rangos de nivel de probabilidad de hacer default 
#               al momento de la admisión.
#
#    1 --> <= 2.0%
#    2 --> <2.0%-4.0%]
#    3 --> <4.0%-6.0%]
#    4 --> <6.0%-8.0%]
#    5 --> <8.0%-12.0%]
#    6 --> <12.0%-Más]

ni    <- table(credit$RANGO_DE_PD)
pi    <- round(prop.table(table(credit$RANGO_DE_PD))*100,1)
tabla <- rbind(ni,pi)
tabla
barplot(pi, main="Rangos de nivel de probabilidad de hacer default", 
        col="blue",xlab="Probabilidad",ylab="Porcentaje")

######################################################
# Exploratorio Bivariado (con respecto a variable dicotomica)

# FLG_MIGRA vs VENTAS
boxplot(credit$VENTAS ~ credit$FLG_MIGRA,
        xlab="Default",ylab="Ventas",
        main="Ventas por Cliente")
aggregate(credit$VENTAS ~credit$FLG_MIGRA, FUN=mean)


# FLG_MIGRA vs PASIVOS
boxplot(credit$PASIVOS ~ credit$FLG_MIGRA,
        xlab="Default",ylab="Ventas",
        main="Pasivos  por Cliente")
aggregate(credit$PASIVOS ~credit$FLG_MIGRA, FUN=mean)

# FLG_MIGRA vs DEUDA_TOTAL
boxplot(credit$DEUDA_TOTAL ~ credit$FLG_MIGRA,
        xlab="Default",ylab="Ventas",
        main="Deuda total por Cliente")
aggregate(credit$DEUDA_TOTAL ~credit$FLG_MIGRA, FUN=mean)

# FLG_MIGRA vs MONTO_APROBADO
table(credit$MONTO_APROBADO,credit$FLG_MIGRA)
prop.table(table(credit$MONTO_APROBADO,credit$FLG_MIGRA),margin=1)
counts <- table(credit$MONTO_APROBADO,credit$FLG_MIGRA)
counts
barplot(counts, main="Distribución de empresa según monto aprobado",
        ylab ="Nro. de Empresas",
        xlab="Default", col=c("blue","red", "green","yellow","orange"),
        legend = rownames(counts), beside=TRUE)

# FLG_MIGRA vs SEGMENTO_CONSTRUCCION
table(credit$SEGMENTO_CONSTRUCCION,credit$FLG_MIGRA)
prop.table(table(credit$SEGMENTO_CONSTRUCCION,credit$FLG_MIGRA),margin=1)
counts <- table(credit$SEGMENTO_CONSTRUCCION,credit$FLG_MIGRA)
counts
barplot(counts, main="Distribución de empresa según segmento de construcción",
        ylab ="Nro. de Empresas",
        xlab="Deafult", col=c("blue","red", "green","yellow","orange","purple"),
        legend = rownames(counts), beside=TRUE)

# FLG_MIGRA vs FAD_1
table(credit$FAD_1,credit$FLG_MIGRA)
prop.table(table(credit$FAD_1,credit$FLG_MIGRA),margin=1)
counts <- table(credit$FAD_1,credit$FLG_MIGRA)
counts
barplot(counts, main="Distribución de flujos antes de deuda por empresa",
        ylab ="Nro. de Empresas",
        xlab="Default", col=c("blue","red", "green","yellow","orange"),
        legend = rownames(counts), beside=TRUE)

# FLG_MIGRA vs DELTA_DE_ENTIDADES_1
table(credit$DELTA_DE_ENTIDADES_1,credit$FLG_MIGRA)
prop.table(table(credit$DELTA_DE_ENTIDADES_1,credit$FLG_MIGRA),margin=1)
counts <- table(credit$DELTA_DE_ENTIDADES_1,credit$FLG_MIGRA)
counts
barplot(counts, main="Variación de Entidad en los últimos 6 meses",
        ylab ="Nro. de Empresas",
        xlab="Default", col=c("blue","red", "green","yellow","orange"),
        legend = rownames(counts), beside=TRUE)

# FLG_MIGRA vs RANGO_DE_PD
table(credit$RANGO_DE_PD,credit$FLG_MIGRA)
prop.table(table(credit$RANGO_DE_PD,credit$FLG_MIGRA),margin=1)
counts <- table(credit$RANGO_DE_PD,credit$FLG_MIGRA)
counts
barplot(counts, main="Nivel de probabilidad de hacer default",
        ylab ="Nro. de Empresas",
        xlab="Default", col=c("blue","red", "green","yellow"),
        legend = rownames(counts), beside=TRUE)

#############################################
##  3.-  Exploracion de datos
#############################################

library(fBasics)

names(credit)

basicStats(credit$MONTO_APROBADO)
basicStats(credit$DELTA_DE_ENTIDADES_1)
basicStats(credit$RANGO_DE_PD)
basicStats(credit$FAD_1)
basicStats(credit$SEGMENTO_CONSTRUCCION)

basicStats(credit$VENTAS)
basicStats(credit$DEUDA_TOTAL)
basicStats(credit$PASIVOS)


#############################################
##  N.- FIN DE PRACTICA 03
#############################################








# Base de datos con variables principales
#data(credit)
#help(credit)





# Formato de datos
glimpse(credit)

# credit2$y<- if_else(credit2$y == 1, "Solvente", "No solvente")
# credit2$y <- as.factor(credit2$y)


#################################################
# Divisi?n de los datos en entrenamiento y test #
#################################################
set.seed(123)

# Se crean los ?ndices de las observaciones de entrenamiento
train <- createDataPartition(y = credit$FLG_MIGRA, p = 0.80, list = FALSE, times = 1)
datos_train <- credit[train, ]
datos_test  <- credit[-train, ]

# Se verifica la distribuci?n de la variable respuesta
prop.table(table(credit$FLG_MIGRA))
prop.table(table(datos_train$FLG_MIGRA))
prop.table(table(datos_test$FLG_MIGRA))

#################################################
# Preprocesamiento                              #
#################################################
# Crear un objeto del tipo recipe
# Ver: http://rstudio-pubs-static.s3.amazonaws.com/349127_caf711562db44e10a65c1fe0ec74e00c.html
objeto_recipe <- recipe(formula = FLG_MIGRA  ~ VENTAS + PASIVOS + FAD_1 + DEUDA_TOTAL + DELTA_DE_ENTIDADES_1
                        + SEGMENTO_CONSTRUCCIÓN + MONTO_APROBADO + RANGO_DE_PD
                         , data =  datos_train)

objeto_recipe

# Eliminar predictores con varianza cercana a cero
# (muchos valores similares)
datos_train %>% select(VENTAS , PASIVOS , FAD_1 , DEUDA_TOTAL , DELTA_DE_ENTIDADES_1
                       , SEGMENTO_CONSTRUCCIÓN , MONTO_APROBADO , RANGO_DE_PD) %>%
  nearZeroVar(saveMetrics = TRUE)

objeto_recipe <- objeto_recipe %>% step_nzv(all_predictors())

# Transformaci?n de variables cuantitativas: normalizaci?n
objeto_recipe <- objeto_recipe %>% step_center(all_numeric())
objeto_recipe <- objeto_recipe %>% step_scale(all_numeric())

# Binarizaci?n de variables cualitativas
objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), -all_outcomes())

# Se entrena el objeto recipe
trained_recipe <- prep(objeto_recipe, training = datos_train)
trained_recipe

# Se aplican las transformaciones al conjunto de entrenamiento y de test
datos_train_prep <- bake(trained_recipe, new_data = datos_train)
datos_test_prep  <- bake(trained_recipe, new_data = datos_test)

glimpse(datos_train_prep)

#################################################
# Construcci?n de Modelos                       #
#################################################
# Ver: http://topepo.github.io/caret/available-models.html

# PARALELIZACI?N DE PROCESO
#===============================================================================
#cl <- makePSOCKcluster(4)
#registerDoParallel(cl)
cl <- makeCluster(detectCores() - 1) # convention to leave 1 core for OS
registerDoParallel(cl)


# M?trica 
#===============================================================================
# Puede ser "Accuracy",   "logLoss", "ROC",   "Kappa"
metrica <- "Accuracy"

# HIPERPAR?METROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI?N
#===============================================================================
particiones  <- 10
repeticiones <- 5

#################################################
# K-Vecinos m?s Cercanos                        #
#################################################

# Hiperpar?metros
hiperparametros <- data.frame(k = seq(from = 1,to = 71,by = 2))

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICI?N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_knn <- train(FLG_MIGRA ~ ., data = datos_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = metrica,
                    trControl = control_train)
modelo_knn

library(ggplot2)
ggplot(modelo_knn , highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evoluci?n del accuracy del modelo KNN", x = "K") +
  theme_bw()

#################################################
# Naive Bayes                                   #
#################################################

# HIPERPAR?METROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI?N
#===============================================================================

# Hiperpar?metros
#hiperparametros <- data.frame(usekernel = FALSE, fL = 0 , adjust = 0)
hiperparametros <- data.frame(usekernel = FALSE, laplace = 0 , adjust = 0)

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICI?N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_nb <- train(FLG_MIGRA ~ ., data = datos_train_prep,
                   method = "naive_bayes", 
                   tuneGrid = hiperparametros,
                   metric = metrica,
                   trControl = control_train)
modelo_nb


#################################################
# ?rbol de Clasificaci?n: C5.0                  #
# Ver Quinlan (1993) y Kuhn and Johnson (2013)  #
#################################################

# HIPERPAR?METROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI?N
#===============================================================================
# Hiperpar?metros
hiperparametros <- data.frame(parameter = "none")

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICI?N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_C50Tree <- train(FLG_MIGRA ~ ., data = datos_train_prep,
                        method = "C5.0Tree",
                        tuneGrid = hiperparametros,
                        metric = metrica,
                        trControl = control_train)
modelo_C50Tree

summary(modelo_C50Tree$finalModel)

#################################################
# ?rbol de Clasificaci?n: RPART                #
#################################################

# HIPERPAR?METROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI?N
#===============================================================================
# Hiperpar?metros
hiperparametros <- data.frame(cp = seq(0.001,0.07,by = 0.0005))


set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICI?N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_rpart <- train(FLG_MIGRA ~ ., data = datos_train_prep,
                        method = "rpart",
                        tuneGrid = hiperparametros,
                        metric = metrica,
                        trControl = control_train)
modelo_rpart

library(ggplot2)
ggplot(modelo_rpart , highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$cp) +
  labs(title = "Evoluci?n del accuracy del modelo CART", x = "cp") +
  theme_bw()

modelo_rpart$finalModel

#################################################
# ?rbol de Clasificaci?n: Party                 #
#################################################

# HIPERPAR?METROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI?N
#===============================================================================
# Hiperpar?metro
#hiperparametros <- data.frame(mincriterion = seq(0.001,0.7,by = 0.01))
hiperparametros <- data.frame(mincriterion = seq(0.001,0.17,by = 0.001))


set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICI?N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_party <- train(FLG_MIGRA ~ ., data = datos_train_prep,
                      method = "ctree",
                      tuneGrid = hiperparametros,
                      metric = metrica,
                      trControl = control_train)
modelo_party

ggplot(modelo_party , highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$mincriterion) +
  labs(title = "Evoluci?n del accuracy del modelo Party", x = "mincriterion") +
  theme_bw()

modelo_party$finalModel

#######################################################
# M?quinas de Soporte Vectorial (SVM) - Kernel Radial #
#######################################################

# HIPERPAR?METROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI?N
#===============================================================================

# Hiperpar?metros
#hiperparametros <- expand.grid(sigma = c(0.001, 0.01, 0.1, 0.5, 1),
#                               C = c(1 , 20, 50, 100, 200, 500, 700))

hiperparametros <- expand.grid(sigma = seq(0.1,0.6,0.1),
                               C = seq(10,70,5))

set.seed(666)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)

# DEFINICI?N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
# ==============================================================================
set.seed(342)
modelo_svmrad <- train(FLG_MIGRA ~ ., data = datos_train_prep,
                       method = "svmRadial",
                       tuneGrid = hiperparametros,
                       metric = metrica,
                       trControl = control_train)
modelo_svmrad
modelo_svmrad$finalModel

# REPRESENTACI?N GR?FICA
# ==============================================================================
ggplot(modelo_svmrad, highlight = TRUE) +
  labs(title = "Evoluci?n del accuracy del modelo SVM Radial") +
  theme_bw()



#################################################
# Comparaci?n de Modelos                        #
#################################################
modelos <- list(KNN = modelo_knn, NB = modelo_nb, 
                Arbol_C5.0 = modelo_C50Tree, Arbol_CART = modelo_rpart,
                Arbol_Party = modelo_party,  
                SVMradial = modelo_svmrad
                )

resultados_resamples <- resamples(modelos)
resultados_resamples$values %>% head(10)

# Se trasforma el dataframe devuelto por resamples() para separar el nombre del
# modelo y las m?tricas en columnas distintas.
metricas_resamples <- resultados_resamples$values %>%
  gather(key = "modelo", value = "valor", -Resample) %>%
  separate(col = "modelo", into = c("modelo", "metrica"),
           sep = "~", remove = TRUE)
metricas_resamples %>% head()

# Accuracy y Kappa promedio de cada modelo
metricas_resamples %>% 
  group_by(modelo, metrica) %>% 
  summarise(media = mean(valor)) %>%
  spread(key = metrica, value = media) %>%
  arrange(desc(Accuracy))

metricas_resamples %>%
  filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  summarise(media = mean(valor)) %>%
  ggplot(aes(x = reorder(modelo, media), y = media, label = round(media, 2))) +
  geom_segment(aes(x = reorder(modelo, media), y = 0,
                   xend = modelo, yend = media),
               color = "grey50") +
  geom_point(size = 7, color = "firebrick") +
  geom_text(color = "white", size = 2.5) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.70, linetype = "dashed") +
  annotate(geom = "text", y = 0.70, x = 5.5, label = "Ratio No Informativo") +
  labs(title = "Validaci?n: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media",
       x = "modelo") +
  coord_flip() +
  theme_bw()

metricas_resamples %>% filter(metrica == "Accuracy") %>%
  group_by(modelo) %>% 
  mutate(media = mean(valor)) %>%
  ungroup() %>%
  ggplot(aes(x = reorder(modelo, media), y = valor, color = modelo)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
  geom_jitter(width = 0.1, alpha = 0.6) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.70, linetype = "dashed") +
  annotate(geom = "text", y = 0.70, x = 6.5, label = "Ratio No Informativo") +
  theme_bw() +
  labs(title = "Validaci?n: Accuracy medio repeated-CV",
       subtitle = "Modelos ordenados por media") +
  coord_flip() +
  theme(legend.position = "none")

#Test de Friedman para comparar el accuracy de los modelos
matriz_metricas <- metricas_resamples %>% filter(metrica == "Accuracy") %>%
  spread(key = modelo, value = valor) %>%
  select(-Resample, -metrica) %>% as.matrix()
friedman.test(y = matriz_metricas)

# Comparaciones m?ltiples con un test suma de rangos de Wilcoxon
# ==============================================================================

metricas_accuracy <- metricas_resamples %>% filter(metrica == "Accuracy")
comparaciones  <- pairwise.wilcox.test(x = metricas_accuracy$valor, 
                                       g = metricas_accuracy$modelo,
                                       paired = TRUE,
                                       p.adjust.method = "holm")

# Se almacenan los p_values en forma de dataframe
comparaciones <- comparaciones$p.value %>%
  as.data.frame() %>%
  rownames_to_column(var = "modeloA") %>%
  gather(key = "modeloB", value = "p_value", -modeloA) %>%
  na.omit() %>%
  arrange(modeloA) 

comparaciones

# Error en el Test
predicciones <- extractPrediction(
  models = modelos,
  testX = datos_test_prep[, -1],
  testY = datos_test_prep$Y
)
predicciones %>% head()

metricas_predicciones <- predicciones %>%
  mutate(acierto = ifelse(obs == pred, TRUE, FALSE)) %>%
  group_by(object, dataType) %>%
  summarise(accuracy = mean(acierto))

metricas_predicciones %>%
  spread(key = dataType, value = accuracy) %>%
  arrange(desc(Test))

ggplot(data = metricas_predicciones,
       aes(x = reorder(object, accuracy), y = accuracy,
           color = dataType, label = round(accuracy, 2))) +
  geom_point(size = 8) +
  scale_color_manual(values = c("orangered2", "gray50")) +
  geom_text(color = "white", size = 3) +
  scale_y_continuous(limits = c(0, 1)) +
  # Accuracy basal
  geom_hline(yintercept = 0.62, linetype = "dashed") +
  annotate(geom = "text", y = 0.70, x = 6.5, label = "Ratio No Informativo") +
  coord_flip() +
  labs(title = "Accuracy de entrenamiento y test", 
       x = "modelo") +
  theme_bw() + 
  theme(legend.position = "bottom")


# Detener clusters
stopCluster(cl)
remove(cl)
registerDoSEQ()

