
 ## XGBoost
  


# Carregar pacotes e base de treino
library(tidyverse)
library(rpart)
base_treino = readRDS(file="salvos//base_treino_final.rds")

#install.packages("xgboost")
library(xgboost)

set.seed(123456789)

### Regressão

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
M_X = model.matrix(~. , data = X)[,-1]

  

XGB_REG <- xgboost(data = M_X,
                   label = base_treino$NU_MEDIA_MT,
                   objective = "reg:squarederror",
                   nrounds = 100,
                   subsample = 0.5,
                   eta = 0.1)

xgb.importance(model = XGB_REG)

y_ = predict(XGB_REG,newdata = M_X)

y = base_treino$NU_MEDIA_MT
(RSS = sum((y - y_)^2))
(EMQ = mean((y-y_)^2))
(R2 = 1 - sum((y-y_)^2)/sum((y-mean(y))^2))

### Classificação Binária


TAXA_PART_BAIXA = ifelse(base_treino$TAXA_PART_CAT == "baixa",1,0)

XGB_CLASS_2 <- xgboost(data = M_X,
                       label = TAXA_PART_BAIXA,
                       objective = "binary:logistic",
                       nrounds = 100,
                       subsample = 0.5,
                       eta = 0.1)


xgb.importance(model = XGB_CLASS_2)

y_= predict(XGB_CLASS_2, newdata = M_X)
head(y_)

library(pROC)
ROC = roc(response = TAXA_PART_BAIXA,predictor = y_)
plot.roc(ROC,
         print.auc = TRUE,
         print.thres = TRUE)
y_classe = ifelse(y_>0.402,1,0)
library(caret)
confusionMatrix(as.factor(TAXA_PART_BAIXA),as.factor(y_classe))

### Classificação Multiclasses {.unnumbered}

Y = ifelse(base_treino$TAXA_PART_CAT == "baixa",0,ifelse(base_treino$TAXA_PART_CAT =="media",1,2))

XGB_CLASS_3 <- xgboost(data = M_X,
                       label = Y,
                       num_class = 3,
                       objective = "multi:softprob",
                       nrounds = 100,
                       subsample = 0.5,
                       eta = 0.1)
  


xgb.importance(model = XGB_CLASS_3)

predict(XGB_CLASS_3,newdata=M_X)

pred = predict(XGB_CLASS_3,newdata=M_X)
y_ = matrix(pred,byrow = TRUE,ncol=3,dimnames =  list(NULL,c("baixa","media","alta")))
y_

y_classe = ifelse(y_[,"alta"]>y_[,"baixa"] & y_[,"alta"]>y_[,"media"],"alta", ifelse(y_[,"baixa"]>y_[,"media"],"baixa","media"))
y_classe = factor(y_classe)
levels(y_classe)
confusionMatrix(data = y_classe, reference = base_treino$TAXA_PART_CAT)

saveRDS(XGB_REG,file="XGB_REG.rds")
saveRDS(XGB_CLASS_2,file="XGB_CLASS_2.rds")
saveRDS(XGB_CLASS_3,file="XGB_CLASS_3.rds")