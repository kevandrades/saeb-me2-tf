###### Carregando os pacotes ######
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, readr, dplyr, forcats)
library(reshape2)
library(purrr)

#=================================================#
# 1. | Carregando dados 
# 2. | Medidas basicas
# 
# 
# 
# 
# 
# 
# 
# 
#=================================================#

####  1. ##### Carregando os dados ######

# Definindo banco de dados
dados <- read_csv('data/saeb.csv') # 5271 observações 


# Selecionando as colunas
dados <- select(amostra, LOCALIZACAO, RACA_COR, SEXO, ESC_MAE, NOTA_LP, NOTA_MT, AFAZERES_DOM)


# Fatoramento das colunas com string
dados$LOCALIZACAO <- factor(dados$LOCALIZACAO)
dados$RACA_COR <- factor(dados$RACA_COR)
dados$SEXO <- factor(dados$SEXO)
dados$ESC_MAE <- factor(dados$ESC_MAE)
dados$AFAZERES_DOM <- factor(dados$AFAZERES_DOM)  


# Ordena RACA_COR, AFAZERES_DOM e ESC_MAE
dados$RACA_COR <- dados$RACA_COR %>% 
  fct_relevel(c("Não quero declarar",
                "Amarela",
                "Branca",
                "Indígena",
                "Parda",
                "Preta")) 

dados$AFAZERES_DOM <- dados$AFAZERES_DOM %>% 
  fct_relevel(c("Não faço trabalhos domésticos", 
                "Menos de 1 hora",
                "Entre 1 e 2 horas",
                "Mais de 2 horas, até 3 horas",
                "Mais de 3 horas"))
  
dados$ESC_MAE <- dados$ESC_MAE %>% 
  fct_relevel(c("Não sei", 
                "Nunca estudou",
                "Não completou a 4.ª série/5.º ano do Ensino Fundamental",
                "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
                "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
                "Completou o Ensino Médio, mas não completou a Faculdade",
                "Completou a Faculdade"))


#=================================================#

#### 2. ##### Medidas basicas ######

# Total de alunos de cada fator
freq <- map(select(dados, -c(NOTA_LP,NOTA_MT)), ~select(data.frame(table(.x)), Freq, .x ))






#=================================================#


