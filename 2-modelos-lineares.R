library(tidyverse)

####  leitura da base salva

base_treino = readRDS(file = "salvos//base_treino_final.rds")
glimpse(base_treino)

####  ajuste do modelo linear
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
    
####  previsoes
y_ = predict(ML)
y = base_treino$NU_MEDIA_MT
R2 = 1 - sum((y-y_)^2)/sum((y-mean(y))^2)
EMQ = mean((y-y_)^2)
    

####  criação da nova variável alvo binária
base_treino = base_treino |> 
  mutate(TAXA_PART_BAIXA = 
           ifelse(base_treino$TAXA_PART_CAT == "baixa",1,0))

####  ajuste do modelo logístico
LOG = glm(formula = TAXA_PART_BAIXA ~ 
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
    
summary(LOG)

####  previsoes
y_ = predict(LOG)
y_classe = ifelse(y_>0.5,1,0)
y = base_treino$TAXA_PART_BAIXA

####  matriz de confusao
library(caret)
confusionMatrix(as.factor(y),as.factor(y_classe))