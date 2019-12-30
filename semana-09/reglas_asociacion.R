# Instalación de Paquetes

install.packages(c("arules","arulesViz","TSP"),
                 dependencies = c("Depends"))

#########################################################
#  Ejemplo: Groceries                                   #
#########################################################

#_______________________________________________________
# Paso 1: Obtener y procesar la base de datos
#_______________________________________________________

# Usando librería arules
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
# El valor de densidad de 0.02609146 (2.6 %) se refieere a la proporción de celdas
# en la matriz que son distintas de cero.  Dado que hay 9835 * 169 = 1662115 celdas
# en la matriz, es posible calcular el número total 1662115 * 0.02609146 = 43367
# de ítems comprados en la tienda durante los 30 días de funcionamiento
#-----------------------------------------------------------------------------------
# En el siguiente bloque de la salidad de summary() se muestran los items más frecuentes
# encontrados en la base de datos de transacciones. Dado que 2513/9835 = 0.2555,
# podemos determinar que whole milk aparece en un 25.6% de todas las transacciones, 
# del mismo modo se interpetan el resto de los items frecuentes.
#-----------------------------------------------------------------------------------
# Finalmente, se presentan un conjunto de estadísticas sobre el tamaño de las transacciones,
# Un total de  2159 transacciones contienen tan sólo un ítem, mientras hubo una transacción
# con 32 ítems. 
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

# Mostrar el soporte (proporción de transacciones) de un item (p. ej. de los tres primeros)
itemFrequency(groceries[, 1:3])
# Visualizar el soporte de los items (p. ej. de aquellos items con una proporción mayor a 0.10)
itemFrequencyPlot(groceries, support = 0.1)

# Visualizar el soporte de los items (p. ej.de los 20 ítems con mayor soporte)
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
# Los puntos deberían verse dispersos con un patrón aleatorio.
# Buscar patrones no aleatorios:
# - Si un ítem aparece en todas las transacciones podría tratarse de información que no 
#   corresponde a un item comprado.
# - Si se ordenan los items por algún criterio, por ejemplo fecha de compra, podrían
#   detectarse algún comportamiento estacional (Halloween, Navidad, etc.)
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
# Recomendaciones para fijar un soporte y confianza mínimo :
# --------------------------------------------------------------------------------- 
# - Pensar en el menor número de transacciones que necesites para considerar que un patrón 
#   es interesante. Por ejemplo, si se argumenta que si se compra un artículo dos veces al día
#  (alrededor de 60 veces en un mes), esto puede ser un patrón interesante. A partir de ahí,
#   es posible calcular el nivel de apoyo necesario para encontrar sólo las reglas que coincidan
#   con al menos ese número de transacciones. Como 60 de 9835 es aprox. 0.006, se puede establecer
#   el soporte a partir de este valor.
# - Determinar la confianza mínima involucra realizar un balance muy delicado.  Por un lado, si la
#   confianza es es demasiado baja, es posible obtener un número abrumadoramente alto de reglas con
#   poca fiabilidad (p. ej. pañales de bebe son comprados junto con muchos productos). Por otro lado,
#   si se fija una confianza muy alta, se limitaran a las reglas que son obvias o inevitable, (p. ej.
#   pañales de bebe son comprados junto a biberones o leche para recien nacidos).
# - El nivel de confianza mínimo adecuado depende en gran medida de los objetivos del análisis.
#   Si se parte de un valor conservador, siempre se puede reducir para ampliar la búsqueda.
# - Para este ejemplo se iniciará con un umbral para la confianza de 0.25, esto indica que para que una
#   regla de asociación se considere relevante debería ocurrir en al menos un 25% de las veces. Esto
#   ayudaría a eliminar la mayoría de reglas poco fiables, al mismo tiempo que permite un cierto margen
#   para incentivar el comportamiento del cliente con promociones específicas.
# - Adicionalmente al soporte y la confianza, ayuda fijar minlen = 2  para eliminar reglas que contengan
#   menos de dos ítems.  Esto previene obtener reglas poco interesantes que se generan porque un item 
#   es comprado muy frecuentemente, por ejemplo {} -> whole milk. Esta regla cumple con el mínimo de
#   soporte y confianza porque whole milk es comprada en más del 25% de las transacciones, pero no brinda
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
# - La distribución para la longitud de las reglas (rule length distribution ) muestra
#   el número de reglas existentes para cierta cantidad de items. Por ejemplo, en 
#   la salida se observa que 150 reglas tienen solo dos items, mientras que 297 tienen
#   tres, y 16 tienen 4.  Además se muestra un resumen estadístico.
# - El resumen de las medidas de calidad para las reglas (rule quality measures) es 
#   importante para evaluar si los parámetros fijados son adecuados.  Por ejemplo, si
#   la mayoría de las reglas tuvieran un soporte y confianza muy cercana al mínimo de
#   los umbrales fijados eso implicaría que quizá se fijó un límite demasiado alto.
#-----------------------------------------------------------------------------------

# Mostrar las tres primeras reglas de asociacion
inspect(groceryrules[1:3])

#----------------------------------------------------------------------------------
# Interpretaciones :
# ---------------------------------------------------------------------------------
# - La primera regla puede ser leida de la siguiente forma: "Si un cliente compra
#   plantas en macetas (potted plants), también comprará leche entera (whole milk).
#   Esto se da con un soporte de 0.007 y una confianza de 0.400, lo cual implica
#   que esta regla cubre el  0.7% de las transacciones y es cierta para el 40% de las
#   compras que involucren plantas en maceta.
# - El valor del lift nos dice que tanto más probable es que un cliente compre leche
#   entera en relación al cliente típico, sabiendo que compró plantas en macetas. Dado que se
#   sabe que cerca del 25.6% de los clientes compran leche entera (soporte), mientras
#   que un 40% de los clientes compran plantas en maceta (confianza), es posible calcular
#   el valor del lift 0.40/0.256 = 1.56.
# - ¿Es razonable la regla anterior? Clasificar las reglas en: accionables/triviales/inexplicables
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
#   casi cuatro veces más propensos a comprar hortalizas que el cliente típico
#   (¿algún tipo de guiso?)
# - La regla número dos es también interesante. Crema batida es más de tres veces más probable
#   de ser encontrada en una canasta de compras con bayas en comparación con otras canastas.
#   (¿algún tipo de postre?)
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
#   items definidos.  Si se desea encontrar reglas con berries y yogurt,debería escribirse
#   %in%c("berries", "yogurt").
# - Existen otros operadores disponibles para emparejamiento parcial (%pin%) y emparejamiento
#   completo (%ain%). Emparejamiento parcial permite encontrar ambos: citrus fruit y tropical fruit
#   en una sola busqueda: items %pin% "fruit". Emparejamiento completo requiere que todos los items
#   listados estén presentes. Por ejemplo, items %ain% c("berries", "yogurt") encuentra solo las
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

# Visualización
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