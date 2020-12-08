if (!require(pacman)) install.packages(pacman)
pacman::p_load(data.table, tidyverse)

saeb <- fread('data/saeb.csv', encoding = 'UTF-8', # definindo os dados
  select = c( # escolhendo colunas
    'LOCALIZACAO',
    'RACA_COR',
    'SEXO',
    'ESC_MAE',
    'NOTA_LP',
    'NOTA_MT',
    'AFAZERES_DOM')) %>%
  as_tibble() %>% # transformando em tibble
  mutate( # Fatoramento das colunas com string
    LOCALIZACAO = factor(LOCALIZACAO),
    RACA_COR = factor(RACA_COR) %>% fct_relevel( # reordenando fatores
      c("Não quero declarar",
        "Amarela",
        "Branca",
        "Indígena",
        "Parda",
        "Preta")),
    SEXO = factor(SEXO),
    ESC_MAE = factor(ESC_MAE) %>% fct_relevel( # Reordenando fatores
      c("Não sei", 
        "Nunca estudou",
        "Não completou o 5.º ano do Ensino Fundamental",
        "Completou o 5.º ano, mas não completou o 9.º ano do Ensino Fundamental",
        "Completou o 9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
        "Completou o Ensino Médio, mas não completou a Faculdade",
        "Completou a Faculdade")),
    AFAZERES_DOM = factor(AFAZERES_DOM) %>% fct_relevel(
      c("Não faço trabalhos domésticos", # Reordenando fatores
        "Menos de 1 hora", 
        "Entre 1 e 2 horas", 
        "Mais de 2 horas, até 3 horas",
        "Mais de 3 horas")) %>% fct_collapse("Não faz ou faz menos de 1 hora" = c("Não faço trabalhos domésticos",
                                                                                  "Menos de 1 hora")),
    NOTAS = NOTA_MT + NOTA_LP)