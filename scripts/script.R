###### Carregando os pacotes ######
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, readr, dplyr, forcats, reshape, purrr, data.table)

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
dados <- fread('data/saeb.csv', # definindo os dados
  select = c( # escolhendo colunas
    'LOCALIZACAO',
    'RACA_COR',
    'SEXO',
    'ESC_MAE',
    'NOTA_LP',
    'NOTA_MT',
    'AFAZERES_DOM')) %>%
  as_tibble() # transformando em tibble

# Fatoramento das colunas com string
dados <- dados %>%
  mutate(
    LOCALIZACAO = factor(LOCALIZACAO),
    RACA_COR = factor(RACA_COR) %>% fct_relevel( # reordenando fatores
      c("Não quero declarar",
        "Amarela",
        "Branca",
        "Indígena",
        "Parda",
        "Preta")),
    SEXO = factor(SEXO),
    ESC_MAE = factor(ESC_MAE) %>% fct_relevel( # reordenando fatores
      c("Não sei", 
        "Nunca estudou",
        "Não completou a 4.ª série/5.º ano do Ensino Fundamental",
        "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
        "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
        "Completou o Ensino Médio, mas não completou a Faculdade",
        "Completou a Faculdade")),
    AFAZERES_DOM = factor(AFAZERES_DOM) %>% fct_relevel(
      c("Não faço trabalhos domésticos", # reordenando fatores
        "Menos de 1 hora", # 
        "Entre 1 e 2 horas", 
        "Mais de 2 horas, até 3 horas",
        "Mais de 3 horas"))
)

#=================================================#

#### 2. ##### Medidas basicas ######

# Total de alunos de cada fator
freq <- map(select(dados, -c(NOTA_LP,NOTA_MT)), ~select(data.frame(table(.x)), Freq, .x ))






#=================================================#


