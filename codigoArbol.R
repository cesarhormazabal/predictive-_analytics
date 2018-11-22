data_banco <- read.csv(
  "https://raw.githubusercontent.com/cesarhormazabal/predictive-_analytics/master/Marketing%20Laboral/bank-full.csv", 
  sep=";")

head(data_banco)

library(caret) #Métodos de ML
library(tidyverse) #Librería hecha, facilitadores de la vida
library(rpart) # Árboles de decisión
library(e1071) # Modelos de ML
library(ranger) # Bosques de decisión 

str(data_banco)

data_banco%>%
  nearZeroVar()->c.casi.cero.var

names(data_banco)[c.casi.cero.var]

data_banco%>%
  select(-default,-pdays,-day,-month,-contact)->data_banco

train_index <- createDataPartition(data_banco$y,
                                   p = 0.8,
                                   list = FALSE,
                                   times = 1)

banco_entrenamiento<-data_banco[train_index,]
banco_validacion<-data_banco[-train_index,]

str(banco_entrenamiento)

#Entrenamiento de árbol
rf_fit <- train(y ~ ., 
                data = banco_entrenamiento, 
                method = "rpart")

prediccion.arbol1<-predict(rf_fit,banco_validacion,type="prob")


rocdata.arbol <- data.frame(
  D = banco_validacion$y
  ,M = prediccion.arbol1$yes
  , Z = rep("Arbol",dim(banco_validacion)[1])
)
roc<-ggplot(rocdata.arbol, aes(m = M, d = D,color=Z)) + geom_roc()

x11()
roc


#Entrenamiento de Bosque
rf_fit.ranger <- ranger(y ~ ., 
                data = banco_entrenamiento,
                importance="impurity",
                probability = TRUE
                )

ranger::importance(rf_fit.ranger)

saveRDS(rf_fit.ranger,"modelo_bosque.rds")

prediccion.bosque<-predict(rf_fit.ranger,banco_validacion)

prediccion.bosque%>%
  head

library(plotROC)

rocdata.bosque <- data.frame(
  D = banco_validacion$y
  ,M = prediccion.bosque$predictions[,"yes"]
  , Z = rep("Bosque",dim(banco_validacion)[1])
)



fit3 <- rpart(y ~ .,
              data = banco_entrenamiento, 
              control = rpart.control(cp = 0.00000000001))

prediccion.arbol.2<-predict(fit3,banco_validacion,type="prob")


rocdata.arbol.2 <- data.frame(
  D = banco_validacion$y
  ,M = prediccion.arbol.2[,"yes"]
  , Z = rep("Arbol Sobre Ajustado",dim(banco_validacion)[1])
)

rocdata<-bind_rows(rocdata.arbol,rocdata.bosque,rocdata.arbol.2)

roc<-ggplot(rocdata, aes(m = M, d = D,color=Z)) + geom_roc()

x11()
roc
