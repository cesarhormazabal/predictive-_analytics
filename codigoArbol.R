data_banco <- read.csv(
  "https://raw.githubusercontent.com/cesarhormazabal/predictive-_analytics/master/Marketing%20Laboral/bank-full.csv", 
  sep=";")


#Cargar liberías
library(caret) #Métodos de ML
library(tidyverse) #Librería hecha, facilitadores de la vida
library(rpart) # Árboles de decisión
library(e1071) # Modelos de ML
library(ranger) # Bosques de decisión 

#Revisar si está todo bien cargado
str(data_banco)
head(data_banco)
summary(data_banco)

#¿Qué data varía poc?
data_banco%>%
  nearZeroVar()->c.casi.cero.var

names(data_banco)[c.casi.cero.var]

#Me quedo con la data que no varía poco, y le quito las columnas
#que no me sirven
data_banco%>%
  select(-default,-pdays,-day,-month,-contact)->data_banco

#Genero mi partición en data de entrenamiento y validación
train_index <- createDataPartition(data_banco$y,
                                   p = 0.8,
                                   list = FALSE,
                                   times = 1)

banco_entrenamiento<-data_banco[train_index,]
banco_validacion<-data_banco[-train_index,]

#Reviso si funcionó bien
str(banco_entrenamiento)


#Entrenamiento de árbol
rf_fit <- train(y ~ ., 
                data = banco_entrenamiento, 
                method = "rpart")
#Predicción de árbol
prediccion.arbol1<-predict(rf_fit,banco_validacion,type="prob")

#Generar la data necesaria para la curva ROC
rocdata.arbol <- data.frame(
  D = banco_validacion$y
  ,M = prediccion.arbol1$yes
  , Z = rep("Arbol",dim(banco_validacion)[1])
)

#Dibujar la curva roc
roc<-ggplot(rocdata.arbol, aes(m = M, d = D,color=Z)) + geom_roc()
roc


#Entrenamiento de Bosque
#Importance determina que guardaré la importancia de las variables
#probability=TRUE significa que guardaré la probabilidad de pertenecer a alguna clase
rf_fit.ranger <- ranger(y ~ ., 
                data = banco_entrenamiento,
                importance="impurity",
                probability = TRUE
                )

#Importancia de las variables según el nivel de impureza
ranger::importance(rf_fit.ranger)

#Guardar Modelo
saveRDS(rf_fit.ranger,"modelo_bosque.rds")

#predecir el modelo
prediccion.bosque<-predict(rf_fit.ranger,banco_validacion)


#Librería
install.packages("plotROC")
library(plotROC)

#Data necesaria para generar la curva ROC
rocdata.bosque <- data.frame(
  D = banco_validacion$y
  ,M = prediccion.bosque$predictions[,"yes"]
  , Z = rep("Bosque",dim(banco_validacion)[1])
)


#Entrenamiento de data (árbol con riesgo de sobreajuste)
fit3 <- rpart(y ~ .,
              data = banco_entrenamiento, 
              control = rpart.control(cp = 0.00000000001))

#Generar predicción
prediccion.arbol.2<-predict(fit3,banco_validacion,type="prob")

#Data necesaria para generar la curva ROC
rocdata.arbol.2 <- data.frame(
  D = banco_validacion$y
  ,M = prediccion.arbol.2[,"yes"]
  , Z = rep("Arbol Sobre Ajustado",dim(banco_validacion)[1])
)

#Unir con los otros modelos para comparar
rocdata<-bind_rows(rocdata.arbol,rocdata.bosque,rocdata.arbol.2)

#Gráfico
roc<-ggplot(rocdata, aes(m = M, d = D,color=Z)) + geom_roc()

x11()
roc
