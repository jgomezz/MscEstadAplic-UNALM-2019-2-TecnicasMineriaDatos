#########################################################
#  Análisis Univariado de Valores Extremos              #
#########################################################

#-------------------------------------------------------#
#  Simulación                                           #
# ------------------------------------------------------#
set.seed(123)
dat <- matrix(rnorm(5*100),100,5)
summary(dat)
boxplot(dat)
pairs(dat) 
# (introducimos un outlier)
dat[23,4] <- dat[23,4] * 10
summary(dat)
boxplot(dat)
pairs(dat)

which.min(dat[,4])

#-------------------------------------------------------#
#  Ejemplo: Empleados de Midwestern bank (1965)         #
# ------------------------------------------------------#
# ID = Código del empleado                              #
# SALBEG = Salario inicial                              #
# GENDER = Género                                       #
# JOBTIME = Meses en el  trabajo                        #
# AGE = Edad (años)                                     #
# SALNOW = Salario actual                               #
# EDLEVEL = Experiencia académica (años)                #
# WORKEXP = Experiencia laboral previa (años)           #
# JOBCAT = Categoría laboral                            #
# MINORITY = Pertenece a minoría racial                 #
#-------------------------------------------------------#

Bank <- read.delim("BwBankTab.dat" )
head(Bank)

# Asignar etiquetas y valores perdidos
Bank$GENDER <- as.factor(Bank$GENDER); levels(Bank$GENDER)<- c("Hombre","Mujer",NA)
Bank$MINORITY <- as.factor(Bank$MINORITY); levels(Bank$MINORITY)<- c("Blanco","No blanco",NA)
Bank$JOBCAT <- as.factor(Bank$JOBCAT)
levels(Bank$JOBCAT)<- c(NA, "Oficinista", "Asistente", "Seguridad", 
                        "Académico", "Empleado", "Ejecutivo", "Técnico" )
Bank$SALBEG[Bank$SALBEG == 0] <- NA
Bank$JOBTIME[Bank$JOBTIME == 0] <- NA
Bank$AGE[Bank$AGE == 0] <- NA
Bank$SALNOW[Bank$SALNOW == 0] <- NA
Bank$EDLEVEL[Bank$EDLEVEL == 0] <- NA
Bank$WORKEXP[Bank$WORKEXP == 0] <- NA

summary(Bank)
Bank <- na.omit(Bank)

# Box Plots
#------------

library(dplyr)
library(ggplot2)

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

p1 <- Bank %>%
  mutate(outlier = ifelse(is_outlier(SALBEG), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = SALBEG)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p2 <- Bank %>%
  mutate(outlier = ifelse(is_outlier(SALNOW), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = SALNOW)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p3 <- Bank %>%
  mutate(outlier = ifelse(is_outlier(JOBTIME), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = JOBTIME)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p4 <- Bank %>%
  mutate(outlier = ifelse(is_outlier(AGE), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = AGE)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p5 <- Bank %>%
  mutate(outlier = ifelse(is_outlier(EDLEVEL), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = EDLEVEL)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p6 <- Bank %>%
  mutate(outlier = ifelse(is_outlier(EDLEVEL), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = 1, y = EDLEVEL)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

library(ggpubr)
final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Análisis Univariado de Valores Extremos", size = 15))
final_plot

# Ver outliers para una variable (JOBTIME)
Bank[is_outlier(Bank$JOBTIME),]

# Detección de valores extremos por grupos
p1 <- Bank %>%
  group_by(GENDER) %>%
  mutate(outlier = ifelse(is_outlier(SALBEG), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENDER, y = SALBEG)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p2 <- Bank %>%
  group_by(GENDER) %>%
  mutate(outlier = ifelse(is_outlier(SALNOW), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENDER, y = SALNOW)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p3 <- Bank %>%
  group_by(GENDER) %>%
  mutate(outlier = ifelse(is_outlier(JOBTIME), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENDER, y = JOBTIME)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p4 <- Bank %>%
  group_by(GENDER) %>%
  mutate(outlier = ifelse(is_outlier(AGE), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENDER, y = AGE)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p5 <- Bank %>%
  group_by(GENDER) %>%
  mutate(outlier = ifelse(is_outlier(EDLEVEL), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENDER, y = EDLEVEL)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

p6 <- Bank %>%
  group_by(GENDER) %>%
  mutate(outlier = ifelse(is_outlier(EDLEVEL), ID, as.numeric(NA))) %>%
  ggplot(., aes(x = GENDER, y = EDLEVEL)) +
  geom_boxplot(fill="lightblue") +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -0.3) +
  theme_bw()

library(ggpubr)
final_plot <- ggarrange(p1, p2, p3, p4, p5, p6, ncol = 2, nrow = 3)
final_plot <- annotate_figure(
  final_plot,
  top = text_grob("Análisis Univariado de Valores Extremos por Género", size = 15))
final_plot

# Puntuación Z
#--------------

is_outlier2 <- function(x,k = 2) {
  return(abs(scale(x)) > k)
}

Bank[is_outlier2(Bank$SALBEG,3),]

#########################################################
#  Análisis Multivariado de Valores Extremos            #
#########################################################

#-------------------------------------------------------#
# Ejemplo: Outliers en dos dimensiones                  #
#-------------------------------------------------------#

load(file = "simpleExample.rda")
#load(file.choose())
head(dat)
tail(dat)
summary(dat)

par(mfrow=c(1,3))
plot(dat, xlim = c(-5,5), ylim = c(-5,5))
barplot(dat[,1], main="Valores de X")
barplot(dat[,2], main="Valores de Y")
par(mfrow=c(1,1))

par(mfrow=c(1,3))
plot(dat, xlim = c(-5,5), ylim = c(-5,5))
boxplot(dat[,1],main="Valores de X")
boxplot(dat[,2],main="Valores de y")
par(mfrow=c(1,1))

# Distancia de Mahalanobis
cm <- colMeans(dat)
S <- cov(dat)
dm <- sqrt(apply(dat, 1, function(x) t(x-cm) %*% solve(S) %*% (x-cm)))

# Distancia de Mahalanobis cuadrada
d <- dm^2
d <- mahalanobis(dat, cm, S)
barplot(d, main="Mahalanobis")
which.max(d)

plot(dat, xlim = c(-5,5), ylim = c(-5,5))
identify(dat[,1], dat[,2],round(d,1))

#  QQ-plot:
qqplot(d, qchisq(ppoints(301), df = 2),
       main = expression("Q-Q plot para" ~~ {chi^2}[nu == 2]))

plot(sort(d), ppoints(301), xlab ="DM al cuadrado ordenada", ylab = "Probabilidad Acumulada")
Q <- qchisq(p = 0.975, df = 2)
abline(v=Q, col = "red")

par(mfrow = c(1,1))
plot(dat, xlim=c(-5,5), ylim=c(-5,5))
points(dat[301,1],dat[301,2],col="red")

#-------------------------------------------------------#
# Ejemplo: Más dimensiones                              #
#-------------------------------------------------------#
load(file = "3dExample.rda")
#load(file = file.choose())
pairs(dat)

# Introduciendo outliers
outFactor <- 1.5
dat <- rbind(dat, outFactor*c(-1,-1.2,0.7))
pairs(dat)
pairs(dat, col = c(rep(1,300), 2), pch = c(rep(1,300), 3), cex = c(rep(1,300), 2))

d <- mahalanobis(dat, colMeans(dat), cov(dat))
barplot(d, main="Mahalanobis")
which.max(d)

# QQ-plot:
par(mfrow=c(1,1))
qqplot(d, qchisq(ppoints(301), df = 3),
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 3]))
plot(sort(d), ppoints(301), xlab ="DM al cuadrado ordenada", ylab = "Probabilidad Acumulada")
Q <- qchisq(p = 0.975, df = 3)
abline(v=Q, col = "red")

library(rgl)
plot3d(dat, col = c(rep(1,300), 2))

#-------------------------------------------------------#
#  Ejemplo: Empleados de Midwestern bank (1965)         #
# ------------------------------------------------------#

dat <- Bank[c(2,4,5,6,7,8)]
d <- mahalanobis(dat, colMeans(dat), cov(dat))
barplot(d, main="Mahalanobis")
which.max(d)

# QQ-plot:
par(mfrow=c(1,1))
qqplot(d, qchisq(ppoints(433), df = 5),
       main = expression("Q-Q plot for" ~~ {chi^2}[nu == 5]))
plot(sort(d), ppoints(433), xlab ="DM al cuadrado ordenada", ylab = "Probabilidad Acumulada")
Q <- qchisq(p = 0.975, df = 5)
abline(v=Q, col = "red")

#--------------------------------------------------------------------------------------- #
#  Ejemplo: Estimadores robustos                                                       #
# Ver Filzmoser et al. (2005) Multivariate outlier detection in exploration geochemistry #
#----------------------------------------------------------------------------------------#

library(mvoutlier)
aq.plot(dat)
chisq.plot(dat)
uni.plot(dat,symb = TRUE)


#########################################################
#  Outliers Multivariados (Otros métodos)               #
#########################################################

# PCOut Method (robust principal components - más adecuado para grandes dimensiones)
# Ver Filzmoser et al. (2008) Outlier identification in high dimensions
outlier=pcout(dat, makeplot = TRUE)
outlier
Bank[order(outlier$wfinal,decreasing=F)[1:10],]


# Sign Method
outlier1=sign2(dat)
outlier1
Bank[order(outlier1$x.dist,decreasing=T)[1:10],]

# Densidad Local
library(DMwR)
lof=lofactor(dat,10)
lof
Bank[order(lof,decreasing=T)[1:10],]


# Clusters (PAM)
library(cluster)
pamBank=pam(dat,25,stand=T)
pamBank$clusinfo
Bank[pamBank$clustering==19,]
