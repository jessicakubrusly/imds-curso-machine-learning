#Leitura, limpeza e organização dos dados

#install.packages("tidyverse")
library(tidyverse)
library(dplyr)



####### leitura da base de dados
base = read_csv2(file="MICRODADOS_ENEM_ESCOLA_2015.csv")

glimpse(base)

base$NO_MUNICIPIO_ESCOLA[1:50]

base = readr::read_csv2(
  file="MICRODADOS_ENEM_ESCOLA_2015.csv",
  locale = locale(encoding = "latin1"))

base$NO_MUNICIPIO_ESCOLA[1:50]


####### Corrigir os tipos das variáveis
base$CO_ESCOLA_EDUCACENSO = as.character(base$CO_ESCOLA_EDUCACENSO)
base$NU_ANO = factor(base$NU_ANO)
base$CO_UF_ESCOLA = factor(base$CO_UF_ESCOLA)
base$SG_UF_ESCOLA = factor(base$SG_UF_ESCOLA)
base$CO_MUNICIPIO_ESCOLA = factor(base$CO_MUNICIPIO_ESCOLA)
base$NO_MUNICIPIO_ESCOLA = factor(base$NO_MUNICIPIO_ESCOLA)
base$TP_DEPENDENCIA_ADM_ESCOLA = factor(base$TP_DEPENDENCIA_ADM_ESCOLA) 
base$TP_LOCALIZACAO_ESCOLA = factor(base$TP_LOCALIZACAO_ESCOLA) 
base$INSE = factor(base$INSE)
base$PORTE_ESCOLA = factor(base$PORTE_ESCOLA)


glimpse(base)


####### Separação da base em treino e teste
library(caret)
set.seed(123456789)
N = nrow(base) #numero de linhas da base
indices_treino = createDataPartition(1:N,p=0.75)[[1]]
base_treino = base |> slice(indices_treino)
base_teste  = base |> slice(-indices_treino)
n_treino = dim(base_treino)[1]
n_teste = dim(base_teste)[1]

#verificando a proporcao entre as bases 
n_treino/(n_treino+n_teste)
n_teste/(n_treino+n_teste)

summary(base_treino)

####### Excluir variáveis sem valores informados
base_treino = base_treino  |> select(-c(NU_MEDIA_OBJ, NU_MEDIA_TOT))

####### Análise dos dados faltantes
library(naniar)
gg_miss_var(x = base_treino)
vis_miss(x = base_treino)
miss_var_summary(data = base_treino)

base_treino |> gg_miss_var()
base_treino |> vis_miss()
base_treino |> miss_var_summary()
  

base_treino = base_treino  |>
  mutate(NU_TAXA_APROVACAO = replace_na(NU_TAXA_APROVACAO, mean(NU_TAXA_APROVACAO, na.rm = TRUE)),
         NU_TAXA_REPROVACAO = replace_na(NU_TAXA_REPROVACAO, mean(NU_TAXA_REPROVACAO, na.rm = TRUE)),
         NU_TAXA_ABANDONO = replace_na(NU_TAXA_ABANDONO, mean(NU_TAXA_ABANDONO, na.rm = TRUE)),
         PC_FORMACAO_DOCENTE = replace_na(PC_FORMACAO_DOCENTE, mean(PC_FORMACAO_DOCENTE, na.rm = TRUE))
         )


moda = names(table(base_treino$INSE))[which.max(table(base_treino$INSE))]
base_treino = base_treino  |> mutate(INSE = replace_na(INSE,moda))
base_treino  |> miss_var_summary()


####### Busca por variáveis com variância (quase) zero

################ entre variáveis quantitativas
diag(var(base_treino  |>  select(where(is.numeric)) ))

################ entre variáveis qualitativas
summary(base_treino  |>  select(where(is.factor)))
base_treino = base_treino  |>  select(-NU_ANO)


####### Análise de Multicolinearidade

######## Entre pares de covariáveis quantitativas
mat_cor = base_treino  |> 
  select(where(is.numeric))  |> 
  select(-c(NU_TAXA_PARTICIPACAO, 
            NU_PARTICIPANTES,
            NU_MEDIA_CN, 
            NU_MEDIA_LP, 
            NU_MEDIA_MT, 
            NU_MEDIA_RED, 
            NU_MEDIA_CH))   |> cor()
mat_cor


library(GGally)
base_treino  |>
  select(where(is.numeric))  |>
  select(-c(NU_TAXA_PARTICIPACAO,
            NU_PARTICIPANTES,
            NU_MEDIA_CN,
            NU_MEDIA_LP,
            NU_MEDIA_MT,
            NU_MEDIA_RED,
            NU_MEDIA_CH))  |> ggpairs()

base_treino = base_treino  |> select(-NU_TAXA_APROVACAO)




######## Entre pares de covariáveis qualitativas
base_treino = base_treino  |> 
  select(-c(CO_UF_ESCOLA,
            CO_MUNICIPIO_ESCOLA,
            NO_MUNICIPIO_ESCOLA))



cores = c("gold","orange","red3","pink","purple","blue","skyblue","springgreen","darkgreen","gray")
x = base_treino$TP_DEPENDENCIA_ADM_ESCOLA
y = base_treino$TP_LOCALIZACAO_ESCOLA
tabela = table(x,y)
tabela1 = prop.table(tabela,margin = 2)
barplot(tabela1,col=cores,legend.text = row.names(tabela1))
tabela2 = t(prop.table(tabela,margin = 1))
barplot(tabela2,col=cores,legend.text = row.names(tabela2))



###### Análise Descritiva
base_treino = base_treino |> 
  mutate(TAXA_PART_CAT = ifelse(NU_TAXA_PARTICIPACAO > 90, "alta", ifelse(NU_TAXA_PARTICIPACAO < 60, "baixa","media")))
base_treino$TAXA_PART_CAT = as.factor(base_treino$TAXA_PART_CAT)


plot(base_treino$NU_MATRICULAS,base_treino$NU_MEDIA_MT)
plot(base_treino$NU_PARTICIPANTES_NEC_ESP,base_treino$NU_MEDIA_MT)
plot(base_treino$NU_PARTICIPANTES,base_treino$NU_MEDIA_MT)
plot(base_treino$NU_TAXA_PERMANENCIA,base_treino$NU_MEDIA_MT)
plot(base_treino$NU_TAXA_REPROVACAO,base_treino$NU_MEDIA_MT)
plot(base_treino$NU_TAXA_ABANDONO,base_treino$NU_MEDIA_MT)
plot(base_treino$PC_FORMACAO_DOCENTE,base_treino$NU_MEDIA_MT)

boxplot(base_treino$NU_MEDIA_MT ~ base_treino$TP_DEPENDENCIA_ADM_ESCOLA)
boxplot(base_treino$NU_MEDIA_MT ~ base_treino$TP_LOCALIZACAO_ESCOLA)
boxplot(base_treino$TP_LOCALIZACAO_ESCOLA ~ base_treino$INSE)
boxplot(base_treino$TP_LOCALIZACAO_ESCOLA ~ base_treino$PORTE_ESCOLA)


boxplot(base_treino$NU_MATRICULAS ~ base_treino$TAXA_PART_CAT,horizontal = T)
boxplot(base_treino$NU_PARTICIPANTES_NEC_ESP ~ base_treino$TAXA_PART_CAT,horizontal = T)
boxplot(base_treino$NU_PARTICIPANTES ~ base_treino$TAXA_PART_CAT,horizontal = T)
boxplot(base_treino$NU_TAXA_PERMANENCIA ~ base_treino$TAXA_PART_CAT,horizontal = T)
boxplot(base_treino$NU_TAXA_REPROVACAO ~ base_treino$TAXA_PART_CAT,horizontal = T)
boxplot(base_treino$NU_TAXA_ABANDONO ~ base_treino$TAXA_PART_CAT,horizontal = T)
boxplot(base_treino$PC_FORMACAO_DOCENTE ~ base_treino$TAXA_PART_CAT,horizontal = T)


tabela = prop.table(table(base_treino$TAXA_PART_CAT,base_treino$TP_DEPENDENCIA_ADM_ESCOLA),2)
barplot(tabela,legend.text = row.names(tabela))

tabela = prop.table(table(base_treino$TAXA_PART_CAT,base_treino$TP_LOCALIZACAO_ESCOLA),2)
barplot(tabela,legend.text = row.names(tabela))

tabela = prop.table(table(base_treino$TAXA_PART_CAT,base_treino$INSE),2)
barplot(tabela,legend.text = row.names(tabela))

tabela = prop.table(table(base_treino$TAXA_PART_CAT,base_treino$PORTE_ESCOLA),2)
barplot(tabela,legend.text = row.names(tabela))


###### Salvar a base final


saveRDS(base_treino,file="salvos/base_treino_final.rds")
write_csv2(base_treino,file="salvos/base_treino_final.csv")


saveRDS(base_teste,file="salvos/base_teste.rds")
write_csv2(base_teste,file="salvos/base_teste.csv")
