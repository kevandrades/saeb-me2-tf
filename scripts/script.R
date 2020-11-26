
###### Carregando os pacotes ######
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, readr, dplyr, forcats)

#=================================================#

###### Carregando os dados ######
# Definindo banco de dados
amostra <- read_csv('data/saeb.csv')

# Selecionando as colunas
amostra <- select(amostra, LOCALIZACAO, RACA_COR, SEXO, ESC_MAE, NOTA_LP, NOTA_MT, AFAZERES_DOM)

#=================================================#

mae <- unique(amostra$ESC_MAE) # Escolaridade da mãe

afzr <- unique(amostra$AFAZERES_DOM) # Tempos de afazeres domesticos

amostra <- amostra %>%
  mutate(
    ESC_MAE = factor(ESC_MAE, # ordena a escolaridade da mãe
      labels = c(mae[4], mae[7], mae[5], mae[6], mae[2], mae[1], mae[3])),
    AFAZERES_DOM = factor(AFAZERES_DOM, # ordena afazeres domésticos
      labels = c(afzr[5], afzr[2], afzr[3], afzr[4], afzr[1]))
)

ggplot(amostra, aes())