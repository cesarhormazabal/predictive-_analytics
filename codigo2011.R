calidad_aire <- read.csv(
  "https://raw.githubusercontent.com/cesarhormazabal/predictive-_analytics/master/Calidad%20del%20Aire/AirQualityUCI.csv", 
  sep=";",dec = ",")

head(calidad_aire)
summary(calidad_aire)

library(caret)
library(tidyverse)

names(calidad_aire)

calidad_aire%>%
  nearZeroVar()->c.casi.cero.var

calidad_aire<-calidad_aire[,-c.casi.cero.var]

calidad_aire%>%
  select(-Date,-Time)%>%
  drop_na()->calidad_aire
  
  
train_index <- createDataPartition(calidad_aire$AH,
                                   p = 0.8,
                                   list = FALSE,
                                   times = 1)
head(calidad_aire[train_index,])



calidad_aire_train<-calidad_aire[train_index,]
calidad_aire_test<-calidad_aire[-train_index,]

mod.lineal <- train(AH ~ ., 
                data = calidad_aire_train, 
                method = "lm")

mod.lineal

mod.lineal$finalModel
