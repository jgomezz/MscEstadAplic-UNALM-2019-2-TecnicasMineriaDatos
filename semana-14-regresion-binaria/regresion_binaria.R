####################################################################
# # Ejemplo: Cr?ditos de  Banco Alem?n                           # #
####################################################################
#Lectura de Datos
library(Fahrmeir)
data(credit)
help(credit)

head(credit)

# Estimacion
modelo_logistic <- glm(Y ~ ., family=binomial,data=credit)
summary(modelo_logistic)


#################################
# Evaluaci?n de la Predicci?n   #
#################################
# Ver: https://www.machinelearningplus.com/machine-learning/evaluation-metrics-classification-models-r/

## Matriz de confusi?n (error Clasificaci?n)

# Valores predichos de Y
yprob<-predict(modelo_logistic,type="response")
# Otra forma:
#yprob <-  predict(modelo_logistic, newdata = credit[,-1], type = "response")
ypred <- as.numeric(yprob >= 0.5 ); ypred <- factor(ypred, labels = levels(credit$Y))
#Otra forma
#ypred <- as.numeric(predict(modelo_logistic)>0)

# Matriz de Confusi?n
(mc<-table(ypred,credit$Y))
testerr <- mean(ypred!=credit$Y)
testerr

library(caret)
confusionMatrix(data = ypred,reference = credit$Y, positive = "mal")
confusionMatrix(data = ypred,reference = credit$Y, positive = "mal", mode = "prec_recall")
confusionMatrix(data = ypred,reference = credit$Y, positive = "mal", mode = "everything")

#LogLoss (http://wiki.fast.ai/index.php/Log_Loss)
Metrics::logLoss(as.numeric(credit$Y)-1, yprob)
MLmetrics::LogLoss(y_pred = yprob,y_true =as.numeric(credit$Y)-1)

## Curvas ROC
library(pROC)
# Area debajo de la curva ROC
analysis <- roc(response=credit$Y, predictor=yprob)
analysis
MLmetrics::AUC(y_pred = yprob,y_true =as.numeric(credit$Y)-1 )

# Coeficiente Gini
2*analysis$auc-1
MLmetrics::Gini(y_pred = yprob,y_true =as.numeric(credit$Y)-1 )


# Grafica de la Curva ROC
plot(1-analysis$specificities,analysis$sensitivities,type="l",
     ylab="Sensitividad",xlab="1-Especificidad",col="blue",lwd=2,
     main = "Curva ROC para el modelo logistico")
abline(a=0,b=1, col = "red")

# Hallar punto de corte 
# Usando el criterio del ?ndice J de Youden
# J = Sensitivity + Specificity - 1
e <- cbind(analysis$thresholds,analysis$sensitivities+analysis$specificities-1)
head(e)
opt_t <- subset(e,e[,2]==max(e[,2]))[,1]
opt_t

ypred2 <- factor(as.numeric(yprob >= opt_t ), labels = levels(credit$Y))
confusionMatrix(ypred2,credit$Y, positive = "mal")

# Otra forma
coords(analysis , "b", ret="t", best.method="youden") 

InformationValue::plotROC(actuals = as.numeric(credit$Y)-1, predictedScores = yprob)
InformationValue::AUROC(actuals = as.numeric(credit$Y)-1, predictedScores = yprob)
InformationValue::optimalCutoff(actuals = as.numeric(credit$Y)-1, predictedScores = yprob, optimiseFor = "Both")
InformationValue::optimalCutoff(actuals = as.numeric(credit$Y)-1, predictedScores = yprob, optimiseFor = "Ones")
InformationValue::optimalCutoff(actuals = as.numeric(credit$Y)-1, predictedScores = yprob, optimiseFor = "Zeros")

# Estad?stica KS (Kolmogorov-Smirnov)
InformationValue::ks_plot(actuals = as.numeric(credit$Y)-1, predictedScores = yprob)
InformationValue::ks_stat(actuals = as.numeric(credit$Y)-1, predictedScores = yprob)
InformationValue::ks_stat(actuals = as.numeric(credit$Y)-1, predictedScores = yprob, returnKSTable = TRUE)
#credit[order(-credit$prob_log),][0:100,c("Y","prob_log")]
#summary(credit[order(-credit$prob_log),][0:100,c("Y","prob_log")])


#Base de datos con las probabilidades y categorias predichas
credit$prob_log<- yprob ; credit$ypred_log<- ypred

##################
## Otros enlaces##
##################
fmla <- Y ~ Cuenta + Mes + Ppag + Uso + Estc

# probit
modelo_probit<- glm(fmla, family=binomial(link=probit),data=credit)
summary(modelo_probit)
yprob<-predict(modelo_probit,type="response")
ypred <- factor(as.numeric(yprob >= 0.5 ), labels = levels(credit$Y))

caret::confusionMatrix(ypred,credit$Y, positive = "mal")
MLmetrics::LogLoss(yprob,as.numeric(credit$Y)-1)
MLmetrics::AUC(yprob,as.numeric(credit$Y)-1 )
MLmetrics::Gini(yprob,as.numeric(credit$Y)-1 )
MLmetrics::KS_Stat(yprob,as.numeric(credit$Y)-1 )

confusionMatrix(as.factor(ypred),admision$admit, positive = "1")
logLoss(as.numeric(admision$admit), predict(admision.fit2, type="response"))


#cloglog
modelo_cloglog<- glm(fmla, family=binomial(link=cloglog),data=credit)
summary(modelo_cloglog)

yprob<-predict(modelo_cloglog,type="response")
ypred <- factor(as.numeric(yprob >= 0.5 ), labels = levels(credit$Y))

caret::confusionMatrix(ypred,credit$Y, positive = "mal")
MLmetrics::LogLoss(yprob,as.numeric(credit$Y)-1)
MLmetrics::AUC(yprob,as.numeric(credit$Y)-1 )
MLmetrics::Gini(yprob,as.numeric(credit$Y)-1 )
MLmetrics::KS_Stat(yprob,as.numeric(credit$Y)-1 )


#cauchit
modelo_cauchit<- glm(fmla, family=binomial(link=cauchit),data=credit)
summary(modelo_cauchit)

yprob<-predict(modelo_cauchit,type="response")
ypred <- factor(as.numeric(yprob >= 0.5 ), labels = levels(credit$Y))

caret::confusionMatrix(ypred,credit$Y, positive = "mal")

MLmetrics::LogLoss(yprob,as.numeric(credit$Y)-1)
MLmetrics::AUC(yprob,as.numeric(credit$Y)-1 )
MLmetrics::Gini(yprob,as.numeric(credit$Y)-1 )
MLmetrics::KS_Stat(yprob,as.numeric(credit$Y)-1 )

# Aranda-Ordaz
source('http://www.poleto.com/funcoes/arandafR.txt')
source('http://www.poleto.com/funcoes/aranda.bino.txt')

aranda.bino(modelo_logistic,seq(-0.,3.0,0.01))

modelo_aranda<- glm(fmla, family=aranda(1.64),data=credit)
summary(modelo_aranda)

yprob<-predict(modelo_aranda,type="response")
ypred <- factor(as.numeric(yprob >= 0.5 ), labels = levels(credit$Y))

caret::confusionMatrix(ypred,credit$Y, positive = "mal")
MLmetrics::LogLoss(yprob,as.numeric(credit$Y)-1)
MLmetrics::AUC(yprob,as.numeric(credit$Y)-1 )
MLmetrics::Gini(yprob,as.numeric(credit$Y)-1 )
MLmetrics::KS_Stat(yprob,as.numeric(credit$Y)-1 )


## Entrenamiento y Validaci?n con Caret

# M?trica 
#===============================================================================
# Puede ser "Accuracy",   "logLoss", "ROC",   "Kappa"
metrica <- "ROC"

# HIPERPAR?METROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI?N
#===============================================================================
particiones  <- 10
repeticiones <- 5

# HIPERPAR?METROS, N?MERO DE REPETICIONES Y SEMILLAS PARA CADA REPETICI?N
#===============================================================================
hiperparametros <- data.frame(parameter = "none")

# DEFINICI?N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones,
                              #seeds = seeds,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

set.seed(666)
modelo_logistic <- train(fmla, data = credit,
                         method = "glm",
                         tuneGrid = hiperparametros,
                         metric = metrica,
                         trControl = control_train,
                         family = "binomial")
modelo_logistic

# Usando LogLoss
metrica <- "logLoss"
# DEFINICI?N DEL ENTRENAMIENTO
#===============================================================================
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones,
                              #seeds = seeds,
                              classProbs = TRUE,
                              summaryFunction = mnLogLoss,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

set.seed(666)
modelo_logistic <- train(fmla, data = credit,
                         method = "glm",
                         tuneGrid = hiperparametros,
                         metric = metrica,
                         trControl = control_train,
                         family = "binomial")
modelo_logistic

