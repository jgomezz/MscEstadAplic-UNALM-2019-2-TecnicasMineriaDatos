#-------------------------------------------------------------------------#
# EVALUCI?N PREDICTIVA DE MODELOS DE CLASIFICACI?N                        #
# ------------------------------------------------------------------------#
# Ejemplo: Cr?ditos de  Banco Alem?n                                      #
#-------------------------------------------------------------------------#

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

# FLG_MIGRA : Indica si una empresa ha sido declarado en Default
ni    <- table(credit$FLG_MIGRA)
pi    <- round(prop.table(table(credit$FLG_MIGRA))*100,1)
tabla <- rbind(ni,pi)
tabla
barplot(pi, main="Distribución de Empresas en Default", 
        col="blue",xlab="Default",ylab="Porcentaje")

# SEGMENTO_CONSTRUCCION
library(agricolae)
ni    <- table(credit$SEGMENTO_CONSTRUCCION)
pi    <- round(prop.table(table(credit$SEGMENTO_CONSTRUCCION))*100,1)
tabla <- rbind(ni,pi)
tabla
barplot(pi, main="Distribución de Empresas por segmento", 
        col="blue",xlab="Default",ylab="Porcentaje")





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

