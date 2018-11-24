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


x11()
hist(data_banco$age)

quantile(data_banco$age,c(.1, .2,.3,.4,.5,.6,.7,.8,.9))

g <- ggplot(data_banco, aes(job))
g+geom_bar()

g <- ggplot(data_banco, aes(marital))
g+geom_bar()

g <- ggplot(data_banco, aes(default))
g+geom_bar()

x11()
hist(data_banco$balance)

quantile(data_banco$balance,c(.1, .2,.3,.4,.5,.6,.7,.8,.9))

g <- ggplot(data_banco, aes(housing))
g+geom_bar()

x11()
g <- ggplot(data_banco, aes(loan))
g+geom_bar()

hist(data_banco$duration)

g <- ggplot(data_banco, aes(poutcome))
g+geom_bar()


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
names(banco_entrenamiento)


#Entrenamiento de árbol
rf_fit <- train(y ~ ., 
                data = banco_entrenamiento, 
                method = "rpart")

#Predicción de árbol
prediccion.arbol1<-predict(rf_fit,
                           banco_validacion,
                           type="prob")

library(plotROC)

#Generar la data necesaria para la curva ROC
rocdata.arbol <- data.frame(
  D = banco_validacion$y #Validación
  ,M = prediccion.arbol1$yes #Probabilidad que entregó el modelo
  , Z = rep("Arbol",dim(banco_validacion)[1]) #El nombre del modelo
)

head(rocdata.arbol)

#Dibujar la curva roc
roc<-ggplot(rocdata.arbol, aes(m = M, d = D,color=Z))+ geom_roc()
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


#Data necesaria para generar la curva ROC
rocdata.bosque <- data.frame(
  D = banco_validacion$y
  ,M = prediccion.bosque$predictions[,"yes"]
  , Z = rep("Bosque",dim(banco_validacion)[1])
)
roc<-ggplot(rocdata.bosque, aes(m = M, d = D,color=Z))+ geom_roc()
roc

# formula
# respuesta~variable1+variable2+variable3...
# respuesta~ . #las variables son TODAS las otras columnas

objeto.modelo.entrenado<-train(formula,data)

predict(objeto.modelo.entrenado,nuevadata)


#Entrenamiento de data (árbol con riesgo de sobreajuste)
fit3 <- rpart(y ~ .,
              data = banco_entrenamiento, 
              control = rpart.control(cp = 0.00000000001))
visNetwork::visTree(fit3)

visNetwork::visTree(rf_fit$finalModel)

#Generar predicción
prediccion.arbol.2<-predict(fit3,banco_validacion,type="prob")

#Data necesaria para generar la curva ROC
rocdata.arbol.2 <- data.frame(
  D = banco_validacion$y
  ,M = prediccion.arbol.2[,"yes"]
  , Z = rep("Arbol Sobre Ajustado",dim(banco_validacion)[1])
)

#Unir con los otros modelos para comparar
rocdata<-bind_rows(rocdata.arbol,
                   rocdata.bosque,
                   rocdata.arbol.2)
summary(rocdata)
#Gráfico
roc<-ggplot(rocdata, aes(m = M, d = D,color=Z)) + geom_roc()

x11()
roc

#Agregamos un modo de control

train_control <- trainControl(method="repeatedcv", 
                              number=10, repeats=5)

rf_fit.busqueda.parametros <- train(y ~ ., 
                                  data = banco_entrenamiento, 
                                  method = "rpart",
                                  trControl=train_control)

#Agregamos un naive bayes

nb.busqueda.parametros <- train(y ~ ., 
                                    data = banco_entrenamiento, 
                                    method = "nb",
                                    trControl=train_control)




