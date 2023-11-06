

# Carregar pacotes e base de treino
library(tidyverse)
library(rpart)
base_treino = readRDS(file="salvos/base_treino_final.rds")

#install.packages("randomForest")
library(randomForest)

set.seed(123456789)


X = base_treino |> select(SG_UF_ESCOLA,
                          TP_DEPENDENCIA_ADM_ESCOLA,
                          TP_LOCALIZACAO_ESCOLA,
                          NU_MATRICULAS,
                          INSE,
                          PC_FORMACAO_DOCENTE,
                          NU_TAXA_PERMANENCIA,
                          NU_TAXA_REPROVACAO,
                          NU_TAXA_ABANDONO,
                          PORTE_ESCOLA)
Y = base_treino$NU_MEDIA_MT

RF_REG <- randomForest(x =  X, y = Y, ntree=500)


importancia = RF_REG$importance
sort(importancia[,1],decreasing = TRUE)

y_ = predict(RF_REG)
y = base_treino$NU_MEDIA_MT
(RSS = sum((y - y_)^2))
(EMQ = mean((y-y_)^2))
(R2 = 1 - sum((y-y_)^2)/sum((y-mean(y))^2))



#Classificação Binária


TAXA_PART_BAIXA = ifelse(base_treino$TAXA_PART_CAT == "baixa",1,0)
Y = factor(TAXA_PART_BAIXA)
RF_CLASS_2 <- randomForest(x =  X, y = Y, ntree=500)

importancia = RF_CLASS_2$importance
sort(importancia[,1],decreasing = TRUE)

y_ = predict(RF_CLASS_2,type="prob")
class(y_)
dim(y_)
head(y_)

library(pROC)
ROC = roc(response = Y,predictor = y_[,"1"])
plot.roc(ROC,
         print.auc = TRUE,
         print.thres = TRUE)
y_classe = ifelse(y_[,"1"]>0.324,1,0)
library(caret)
confusionMatrix(Y,as.factor(y_classe))

#Classificação Multiclasses

Y = base_treino$TAXA_PART_CAT

RF_CLASS_3 <- randomForest(x =  X, y = Y, ntree=500)


importancia = RF_CLASS_3$importance
sort(importancia[,1],decreasing = TRUE)

y_ = predict(RF_CLASS_3, type = "prob")
head(y_)


y_classe = ifelse(y_[,"alta"]>y_[,"baixa"] & y_[,"alta"]>y_[,"media"],"alta", ifelse(y_[,"baixa"]>y_[,"media"],"baixa","media"))
y_classe = factor(y_classe)
levels(y_classe)
confusionMatrix(data = y_classe, reference = Y)


saveRDS(RF_REG,file="RF_REG.rds")
saveRDS(RF_CLASS_2,file="RF_CLASS_2.rds")
saveRDS(RF_CLASS_3,file="RF_CLASS_3.rds")

