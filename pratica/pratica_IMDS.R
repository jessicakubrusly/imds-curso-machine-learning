library(tidyverse)
library(dplyr)

base = read_csv(file="PRSA_data_2010.1.1-2014.12.31.csv")
glimpse(base)

base$No = as.character(base$No)
base$year = as.factor(base$year)
base$month = as.factor(base$month)
base$day = as.factor(base$day)
base$hour = as.factor(base$hour)
base$cbwd = as.factor(base$cbwd)


glimpse(base)

#buscar por NA
library(naniar)

base |> gg_miss_var()
base |> vis_miss()

dim(base)

#indices onde estaos as NA
indices = is.na(base$pm2.5)
sum(indices)
base  = base |> filter(!indices)

dim(base)

base |> gg_miss_var()
base |> vis_miss()


cor(base |> select(where(is.numeric)))

base = base |> select(-TEMP)

base = base |> mutate(air_q = ifelse(pm2.5>75,"ruim",
                                     ifelse(pm2.5 < 35,"boa","moderada")))
base$air_q = factor(base$air_q)

#Treino x teste

library(caret)
set.seed(123456789)

N = nrow(base) #numero de linhas da base
indices_treino = createDataPartition(1:N,p=0.75)[[1]]
base_treino = base |> slice(indices_treino)
base_teste  = base |> slice(-indices_treino)


#Floresta Aleatoria
library(randomForest)

X = base_treino |> select(-c(No,pm2.5,air_q))
Y = base_treino$air_q

RF = randomForest(x = X,y = Y,ntree = 100)

importancia = RF$importance
sort(importancia[,1],decreasing = TRUE)


#XGBoost
library(xgboost)
M_X = model.matrix(~. , data = X)[,-1]

Y_ = ifelse(
  base_treino$air_q == "boa",0,
  ifelse(base_treino$air_q =="moderada",1,2))

XGB <- xgboost(data = M_X,
               label = Y_,
               num_class = 3,
               objective = "multi:softprob",
               nrounds = 100,
               subsample = 0.5,
               eta = 0.1)

xgb.importance(model = XGB)



### Previsoes para a prob

#treino
y_rf_treino = predict(RF,newdata = X)
head(y_rf_treino)

y_xgb_treino = predict(XGB,newdata = M_X) 
y_xgb_treino = matrix(y_xgb_treino,byrow = TRUE,
                      ncol=3,
                      dimnames =  list(NULL,c("boa","moderada","ruim")))
head(y_xgb_treino)
y_= y_xgb_treino
y_xgb_classe = ifelse(y_[,"boa"]>y_[,"moderada"] & y_[,"boa"]>y_[,"ruim"],"boa", 
                  ifelse(y_[,"moderada"]>y_[,"ruim"],"moderada","ruim"))
y_xgb_classe = factor(y_xgb_classe)

#teste
X_teste = base_teste |> select(-c(No,pm2.5,air_q))
y_rf_teste = predict(RF,newdata = X_teste,type = "prob")
head(y_rf_teste)

M_X_teste = model.matrix(~. , data = X_teste)[,-1]
y_xgb_teste = predict(XGB,newdata = M_X_teste) 
y_xgb_teste = matrix(y_xgb_teste,
                      byrow = TRUE,ncol=3,
                      dimnames =  list(NULL,c("boa","moderada","ruim")))
head(y_xgb_teste)
y_= y_xgb_teste
y_xgb_classe_teste = ifelse(y_[,"boa"]>y_[,"moderada"] & y_[,"boa"]>y_[,"ruim"],"boa", 
                      ifelse(y_[,"moderada"]>y_[,"ruim"],"moderada","ruim"))
y_xgb_classe_teste = factor(y_xgb_classe_teste)




