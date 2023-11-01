
library(tidyverse)
library(rpart)
base_treino = readRDS(file="salvos//base_treino_final.rds")

TREE_REG = rpart(formula = NU_MEDIA_MT ~ 
                   SG_UF_ESCOLA + 
                   TP_DEPENDENCIA_ADM_ESCOLA + 
                   TP_LOCALIZACAO_ESCOLA + 
                   NU_MATRICULAS + 
                   INSE + 
                   PC_FORMACAO_DOCENTE + 
                   NU_TAXA_PERMANENCIA + 
                   NU_TAXA_REPROVACAO + 
                   NU_TAXA_ABANDONO + 
                   PORTE_ESCOLA, data = base_treino)


library("rpart.plot")
rpart.plot(TREE_REG,type=5,cex=1)

imp <- caret::varImp(TREE_REG, scale = FALSE)
importancia = tibble(overall = imp$Overall,
                     var = rownames(imp))
importancia |> arrange(desc(overall))

y_ = predict(TREE_REG)
y = base_treino$NU_MEDIA_MT
(RSS = sum((y - y_)^2))
(EMQ = mean((y-y_)^2))
(R2 = 1 - sum((y-y_)^2)/sum((y-mean(y))^2))

saveRDS(TREE_REG,file = "salvos/TREE_REG.rds")






## Árvores de Classificação

library(tidyverse)
library(rpart)
base_treino = readRDS(file="salvos//base_treino_final.rds")

### Classificação Binária


base_treino = base_treino |> 
  mutate(TAXA_PART_BAIXA = 
           ifelse(base_treino$TAXA_PART_CAT == "baixa",1,0))

TREE_CLASS_2 <- rpart(formula = TAXA_PART_BAIXA ~ 
                        SG_UF_ESCOLA + 
                        TP_DEPENDENCIA_ADM_ESCOLA + 
                        TP_LOCALIZACAO_ESCOLA + 
                        NU_MATRICULAS + 
                        INSE + 
                        PC_FORMACAO_DOCENTE + 
                        NU_TAXA_PERMANENCIA + 
                        NU_TAXA_REPROVACAO + 
                        NU_TAXA_ABANDONO + 
                        PORTE_ESCOLA, 
                      data = base_treino,
                      method = "class"
)


rpart.plot(TREE_CLASS_2,type=5,cex=1)



imp <- caret::varImp(TREE_CLASS_2, scale = FALSE)
importancia = tibble(overall = imp$Overall,
                     var = rownames(imp))
importancia |> arrange(desc(overall))



y_ = predict(TREE_CLASS_2)
class(y_)
dim(y_)
head(y_)

y = base_treino$TAXA_PART_BAIXA
library(pROC)
ROC = roc(response = y,predictor = y_[,"1"])
plot.roc(ROC,
         print.auc = TRUE,
         print.thres = TRUE)
y_classe = ifelse(y_[,"1"]>0.329,1,0)
library(caret)
confusionMatrix(as.factor(y),as.factor(y_classe))

saveRDS(TREE_CLASS_2,file = "salvos//TREE_CLASS_2.rds")





### Classificação multi-classes {.unnumbered}


TREE_CLASS_3 <- rpart(formula = TAXA_PART_CAT ~ 
                        SG_UF_ESCOLA + 
                        TP_DEPENDENCIA_ADM_ESCOLA + 
                        TP_LOCALIZACAO_ESCOLA + 
                        NU_MATRICULAS + 
                        INSE + 
                        PC_FORMACAO_DOCENTE + 
                        NU_TAXA_PERMANENCIA + 
                        NU_TAXA_REPROVACAO + 
                        NU_TAXA_ABANDONO + 
                        PORTE_ESCOLA, 
                      data = base_treino,
                      method = "class")
)

TREE_CLASS_3 = readRDS("salvos//TREE_CLASS_3.rds")



rpart.plot(TREE_CLASS_3,type=5,cex=1)

imp <- caret::varImp(TREE_CLASS_3, scale = FALSE)
importancia = tibble(overall = imp$Overall,
                     var = rownames(imp))
importancia |> arrange(desc(overall))

y_ = predict(TREE_CLASS_3)
head(y_)
y_classe = ifelse(y_[,"alta"]>y_[,"baixa"] & y_[,"alta"]>y_[,"media"],"alta", ifelse(y_[,"baixa"]>y_[,"media"],"baixa","media"))
y_classe = factor(y_classe)
levels(y_classe)
head(y_classe)
y = base_treino$TAXA_PART_CAT
head(y)
confusionMatrix(data = y_classe, reference = y)

saveRDS(TREE_CLASS_3,file = "salvos/TREE_CLASS_3.rds")

