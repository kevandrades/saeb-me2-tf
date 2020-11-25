library(ggplot2)
library(readr)
library(dplyr)
library(forcats)

amostra <- read_csv(file.choose())


amostra <- select(amostra, LOCALIZACAO,RACA_COR,SEXO,ESC_MAE,NOTA_LP,NOTA_MT, AFAZERES_DOM)

mae <-unique(amostra$ESC_MAE) # Escolaridade da mãe

amostra$ESC_MAE <-
  factor(amostra$ESC_MAE,
         labels = c(mae[4], mae[7], mae[5], mae[6], mae[2], mae[1], mae[3])) # Ordena ESC_MAE

afzr <- unique(amostra$AFAZERES_DOM) # Tempos de afazeres domesticos

amostra$AFAZERES_DOM <-
  factor(amostra$AFAZERES_DOM,  
         labels = c(afzr[5], afzr[2], afzr[3], afzr[4], afzr[1])) # Ordena a variavel nominal AFAZERES_DOM



ggplot(amostra, aes())



