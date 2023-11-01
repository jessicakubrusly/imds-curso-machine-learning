# Modelos Lineares

## Modelo Linear Múltiplo

library(tidyverse)
base_treino = readRDS(file = "salvos//base_treino_final.rds")
glimpse(base_treino)
ML = lm(formula = NU_MEDIA_MT ~ 
          SG_UF_ESCOLA + 
          TP_DEPENDENCIA_ADM_ESCOLA + 
          TP_LOCALIZACAO_ESCOLA + 
          NU_MATRICULAS + 
          INSE + 
          PC_FORMACAO_DOCENTE + 
          NU_TAXA_PERMANENCIA + 
          NU_TAXA_REPROVACAO + 
          NU_TAXA_ABANDONO + 
          PORTE_ESCOLA, data = base_treino
)

summary(ML)

y_ = predict(ML)

y = base_treino$NU_MEDIA_MT
(RSS = sum((y - y_)^2))
(EMQ = mean((y-y_)^2))
(R2 = 1 - sum((y-y_)^2)/sum((y-mean(y))^2))

        

### Seleção das variáveis

ML_final = step(ML)

summary(ML_final)

y_final = predict(ML_final)
(RSS = sum((y - y_final)^2))
(EMQ = mean((y-y_final)^2))
(R2 = 1 - sum((y-y_final)^2)/sum((y-mean(y))^2))





## Modelo Logístico

base_treino = base_treino |> 
mutate(TAXA_PART_BAIXA = 
       ifelse(base_treino$TAXA_PART_CAT == "baixa",1,0))
table(base_treino$TAXA_PART_BAIXA)


MLOG = glm(formula = TAXA_PART_BAIXA ~ 
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
       family = gaussian,
       data = base_treino
)
        
summary(MLOG)
        


y_ = predict(MLOG)
y = base_treino$TAXA_PART_BAIXA
library(pROC)
ROC = roc(response = y,predictor = y_)
plot.roc(ROC,
         print.auc = TRUE,
         print.thres = TRUE)
y_classe = ifelse(y_>0.444,1,0)
library(caret)
confusionMatrix(as.factor(y),as.factor(y_classe))

        
### Seleção das variáveis
        
        
MLOG_final = step(MLOG)
        
        
        
summary(MLOG_final)


y_final = predict(MLOG_final)
ROC_final = roc(response = y,predictor = y_final)
plot.roc(ROC_final,
         print.auc = TRUE,
         print.thres = TRUE)
y_classe_final = ifelse(y_final>0.443,1,0)
confusionMatrix(as.factor(y),as.factor(y_classe_final))



write_rds(ML_final,file = "salvos//ML_final.rds")
write_rds(MLOG_final,file = "salvos//MLOG_final.rds")

