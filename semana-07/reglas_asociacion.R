# Instalaci?n de Paquetes

install.packages(c("arules","arulesViz","TSP"),
                 dependencies = c("Depends"))

#########################################################
#  Ejemplo: Groceries                                   #
#########################################################

#_______________________________________________________
# Paso 1: Obtener y procesar la base de datos
#_______________________________________________________

# Usando librer?a arules
library(arules)
data("Groceries") 
head(Groceries)
summary(Groceries)
labels(Groceries)

# Importando de un archivo .csv y convirtiendo a transacciones
#groceries <- read.transactions("groceries.csv", sep = ",")
groceries <- read.transactions(file = file.choose(),
                               sep = ",",
                               format = "basket")
groceries
summary(groceries)
#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# El valor de densidad de 0.02609146 (2.6 %) se refieere a la proporci?n de celdas
# en la matriz que son distintas de cero.  Dado que hay 9835 * 169 = 1662115 celdas
# en la matriz, es posible calcular el n?mero total 1662115 * 0.02609146 = 43367
# de ?tems comprados en la tienda durante los 30 d?as de funcionamiento
#-----------------------------------------------------------------------------------
# En el siguiente bloque de la salidad de summary() se muestran los items m?s frecuentes
# encontrados en la base de datos de transacciones. Dado que 2513/9835 = 0.2555,
# podemos determinar que whole milk aparece en un 25.6% de todas las transacciones, 
# del mismo modo se interpetan el resto de los items frecuentes.
#-----------------------------------------------------------------------------------
# Finalmente, se presentan un conjunto de estad?sticas sobre el tama?o de las transacciones,
# Un total de  2159 transacciones contienen tan s?lo un ?tem, mientras hubo una transacci?n
# con 32 ?tems. 
#-----------------------------------------------------------------------------------

dim(groceries)
basketSizes <- size(groceries)
summary(basketSizes)
quantile(basketSizes, probs = seq(0,1,0.1))
library(ggplot2)
ggplot(data.frame(count = basketSizes)) +
  geom_density(aes(x=count))

# Mostrar las transacciones
labels(groceries)
colnames(groceries)[1:5]

# Mostrar un subconjunto de transacciones (p. ej. las cinco primeras)
inspect(groceries[1:5])

# Mostrar el soporte (proporci?n de transacciones) de un item (p. ej. de los tres primeros)
itemFrequency(groceries[, 1:3])
# Visualizar el soporte de los items (p. ej. de aquellos items con una proporci?n mayor a 0.10)
itemFrequencyPlot(groceries, support = 0.1)

# Visualizar el soporte de los items (p. ej.de los 20 ?tems con mayor soporte)
itemFrequencyPlot(groceries, topN = 20)

# Obtener los 20 items mas frecuents
groceriesFreq <- itemFrequency(groceries)
summary(groceriesFreq)
sum(groceriesFreq)

groceriesCount <- (groceriesFreq/sum(groceriesFreq))*sum(basketSizes)
summary(groceriesCount)
orderedgroceries <- sort(groceriesCount, decreasing = T)
orderedgroceries[1:20]

# Visualizar la matriz de transacciones (p. ej. para las 5 primeras transacciones)
image(groceries[1:5])

# Visualizar la matriz de transacciones (p. ej. seleccionar al azar 100 transacciones)
image(sample(groceries, 100))
#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# Los puntos deber?an verse dispersos con un patr?n aleatorio.
# Buscar patrones no aleatorios:
# - Si un ?tem aparece en todas las transacciones podr?a tratarse de informaci?n que no 
#   corresponde a un item comprado.
# - Si se ordenan los items por alg?n criterio, por ejemplo fecha de compra, podr?an
#   detectarse alg?n comportamiento estacional (Halloween, Navidad, etc.)
#-----------------------------------------------------------------------------------

#_______________________________________________________
# Paso 2: Entrenar el modelo con los datos
#_______________________________________________________

help(apriori)
apriori(groceries)
#----------------------------------------------------------------------------------
# Comentario :
# ---------------------------------------------------------------------------------
# Recordar que por defecto un soporte = 0.1 es usado para generar una regla, es decir
# que al menos un item debe aparecer en 0.1 * 9385 = 938.5 transacciones. Dado que solo
# ocho item tienen esta frecuencia, es bastante predecible que no se encuentre ninguna 
# regla de asociacion.
# ---------------------------------------------------------------------------------
# Recomendaciones para fijar un soporte y confianza m?nimo :
# --------------------------------------------------------------------------------- 
# - Pensar en el menor n?mero de transacciones que necesites para considerar que un patr?n 
#   es interesante. Por ejemplo, si se argumenta que si se compra un art?culo dos veces al d?a
#  (alrededor de 60 veces en un mes), esto puede ser un patr?n interesante. A partir de ah?,
#   es posible calcular el nivel de apoyo necesario para encontrar s?lo las reglas que coincidan
#   con al menos ese n?mero de transacciones. Como 60 de 9835 es aprox. 0.006, se puede establecer
#   el soporte a partir de este valor.
# - Determinar la confianza m?nima involucra realizar un balance muy delicado.  Por un lado, si la
#   confianza es es demasiado baja, es posible obtener un n?mero abrumadoramente alto de reglas con
#   poca fiabilidad (p. ej. pa?ales de bebe son comprados junto con muchos productos). Por otro lado,
#   si se fija una confianza muy alta, se limitaran a las reglas que son obvias o inevitable, (p. ej.
#   pa?ales de bebe son comprados junto a biberones o leche para recien nacidos).
# - El nivel de confianza m?nimo adecuado depende en gran medida de los objetivos del an?lisis.
#   Si se parte de un valor conservador, siempre se puede reducir para ampliar la b?squeda.
# - Para este ejemplo se iniciar? con un umbral para la confianza de 0.25, esto indica que para que una
#   regla de asociaci?n se considere relevante deber?a ocurrir en al menos un 25% de las veces. Esto
#   ayudar?a a eliminar la mayor?a de reglas poco fiables, al mismo tiempo que permite un cierto margen
#   para incentivar el comportamiento del cliente con promociones espec?ficas.
# - Adicionalmente al soporte y la confianza, ayuda fijar minlen = 2  para eliminar reglas que contengan
#   menos de dos ?tems.  Esto previene obtener reglas poco interesantes que se generan porque un item 
#   es comprado muy frecuentemente, por ejemplo {} -> whole milk. Esta regla cumple con el m?nimo de
#   soporte y confianza porque whole milk es comprada en m?s del 25% de las transacciones, pero no brinda
#   un insight accionable. 
#-----------------------------------------------------------------------------------



groceryrules <- apriori(groceries, parameter = list(support =0.006,
			 confidence = 0.25, minlen = 2))

groceryrules

#_______________________________________________________
# Paso 3: Evaluar el modelo
#_______________________________________________________

summary(groceryrules)

#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# - La distribuci?n para la longitud de las reglas (rule length distribution ) muestra
#   el n?mero de reglas existentes para cierta cantidad de items. Por ejemplo, en 
#   la salida se observa que 150 reglas tienen solo dos items, mientras que 297 tienen
#   tres, y 16 tienen 4.  Adem?s se muestra un resumen estad?stico.
# - El resumen de las medidas de calidad para las reglas (rule quality measures) es 
#   importante para evaluar si los par?metros fijados son adecuados.  Por ejemplo, si
#   la mayor?a de las reglas tuvieran un soporte y confianza muy cercana al m?nimo de
#   los umbrales fijados eso implicar?a que quiz? se fij? un l?mite demasiado alto.
#-----------------------------------------------------------------------------------

# Mostrar las tres primeras reglas de asociacion
inspect(groceryrules[1:3])

#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# - La primera regla puede ser leida de la siguiente forma: "Si un cliente compra
#   plantas en macetas (potted plants), tambi?n comprar? leche entera (whole milk).
#   Esto se da con un soporte de 0.007 y una confianza de 0.400, lo cual implica
#   que esta regla cubre el  0.7% de las transacciones y es cierta para el 40% de las
#   compras que involucren plantas en maceta.
# - El valor del lift nos dice que tanto m?s probable es que un cliente compre leche
#   entera en relaci?n al cliente t?pico, sabiendo que compr? plantas en macetas. Dado que se
#   sabe que cerca del 25.6% de los clientes compran leche entera (soporte), mientras
#   que un 40% de los clientes compran plantas en maceta (confianza), es posible calcular
#   el valor del lift 0.40/0.256 = 1.56.
# - ?Es razonable la regla anterior? Clasificar las reglas en: accionables/triviales/inexplicables
#-----------------------------------------------------------------------------------

#_______________________________________________________
# Paso 4: Mejorar la performance del modelo
#_______________________________________________________

# Mostrar las 5 reglas con mayor lift
inspect(sort(groceryrules, by = "lift")[1:5])

#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# - La primera regla, con un lift de aprox. 3.96, implica que las personas que compran hierbas son 
#   casi cuatro veces m?s propensos a comprar hortalizas que el cliente t?pico
#   (?alg?n tipo de guiso?)
# - La regla n?mero dos es tambi?n interesante. Crema batida es m?s de tres veces m?s probable
#   de ser encontrada en una canasta de compras con bayas en comparaci?n con otras canastas.
#   (?alg?n tipo de postre?)
#-----------------------------------------------------------------------------------
inspect(sort(groceryrules, by = "confidence")[1:5])

# Subconjuntos de reglas
berryrules <- subset(groceryrules, items %in% "berries")
inspect(berryrules)

#----------------------------------------------------------------------------------
# Uso de subset() :
# ---------------------------------------------------------------------------------
# - La palabra clave items empareja un item que aparezca en alguna regla. Es posible delimitar
#   que esta ocurra solo a la izquierda o derecha usando lhs y rhs.
# - El operador %in% significa que al menos uno de los items debe ser encontrado, de la lista de
#   items definidos.  Si se desea encontrar reglas con berries y yogurt,deber?a escribirse
#   %in%c("berries", "yogurt").
# - Existen otros operadores disponibles para emparejamiento parcial (%pin%) y emparejamiento
#   completo (%ain%). Emparejamiento parcial permite encontrar ambos: citrus fruit y tropical fruit
#   en una sola busqueda: items %pin% "fruit". Emparejamiento completo requiere que todos los items
#   listados est?n presentes. Por ejemplo, items %ain% c("berries", "yogurt") encuentra solo las
#   reglas con yogurt y berries al mismo tiempo.
# - Los subconjuntos tambien pueden ser imitados por soporte, confianza o lift. Por ejemplo,
#   confidence > 0.50.
# - Los criterios de emparejamiento pueden ser combinados con operadores de R estandar y logicos como 
#   y (&), o (|), y negacion (!).
#-----------------------------------------------------------------------------------

# Otras medidas
# Revisar: https://www.jstatsoft.org/article/view/v014i15/v14i15.pdf
measures <- interestMeasure(x = groceryrules, measure = c("coverage", "fishersExactTest"), 
                            transactions = groceries)
summary(measures)
  
  
# Exportar las reglas obtenidas
write(groceryrules, file = "groceryrules.csv",
	sep = ",", quote = TRUE, row.names = FALSE)

# Convertir reglas en dataframe
groceryrules_df <- as(groceryrules, "data.frame")
head(groceryrules_df)

# Realizaqr el analisis solo con clientes que hayan comprado mas de un producto
groceries_use <- groceries[basketSizes > 1]
groceryrules2 <- apriori(groceries_use, parameter = list(support =0.006,
                                                    confidence = 0.25, minlen = 2))

groceryrules2

# Visualizaci?n
rules <- apriori(groceries, parameter =
                   list(supp = 0.01, conf = 0.5, target = "rules")) 
library(arulesViz)
plot(rules)

subrules <- head(sort(rules, by="lift"), 10)

plot(subrules,method="graph",control=list(alpha=1))

plot(rules,method="matrix",measure="support")

plot(rules,method="matrix3D",measure="confidence")

#########################################################
#  Ejemplo: BUPA                                        #
#########################################################

library(discretization)
load("bupa.rda")
bupa <- read.csv(file.choose())
disc.bupa=chiM(bupa)
dbupa=disc.bupa$Disc.data

for (i in 1:7){dbupa[,i]=as.factor(dbupa[,i])}
dbupa<-as.data.frame(dbupa)
dbupa.ar<-as(dbupa, "transactions")

rules <- apriori(dbupa.ar,parameter =
                   list(supp = 0.20, conf = 0.9,target = "rules"))
summary(rules)
inspect(rules)
library(arulesViz)
plot(rules, measure=c("support", "lift"),
     shading="confidence", interactive=TRUE)
plot(rules,method="graph",control=list(alpha=1))
plot(rules, method="graph",
     control=list(alpha=1,type="items"))