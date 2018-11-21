data_banco <- read.csv(
  "https://raw.githubusercontent.com/cesarhormazabal/predictive-_analytics/master/Marketing%20Laboral/bank-full.csv", 
  sep=";")

head(data_banco)

library(caret)
library(tidyverse)
library(rpart)
library(e1071)
library(ranger)

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

rf_fit <- train(y ~ ., 
                data = banco_entrenamiento, 
                method = "rpart")


modelo.arbol<-rf_fit$finalModel

library(visNetwork)
visNetwork::visTree(modelo.arbol)

