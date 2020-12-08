if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

saeb <- fread('data/saeb.csv', encoding = 'UTF-8', # definindo os saeb
               select = c( # escolhendo colunas
                 'LOCALIZACAO',
                 'RACA_COR',
                 'SEXO',
                 'ESC_MAE',
                 'NOTA_LP',
                 'NOTA_MT',
                 'AFAZERES_DOM')) %>%
  as_tibble() %>%
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
    NOTAS = NOTA_LP + NOTA_MT
  )


# Raça/Cor por notas
ggplot(saeb, aes(x = RACA_COR, y = NOTAS, color = RACA_COR)) + 
    geom_boxplot() +
    theme_minimal() +
    labs(x = 'Raça/Cor', y = 'Nota',
    caption = 'Fonte: SAEB 2017') +
    ggsave('img/raca_cor_notas.png')

# Escolaridade da mãe por notas
ggplot(saeb, aes(color = ESC_MAE, x = NOTAS))+ 
    geom_boxplot()  +
    theme_minimal() +
    labs(y = 'Escolaridade da Mãe', x = 'Nota',
    caption = 'Fonte: SAEB 2017') +
    theme(legend.box = "vertical",
          legend.position="bottom",
          axis.text.y = element_blank()) +
    guides(color = guide_legend(label.position =  "right",
        title="Escolaridade da Mãe",
        ncol=1,
        reverse = T)) 
    ggsave('img/esc_mae_notas.pdf',
        width = 7.6,
        height = 7,
        dpi = 500)

    
    
    
    