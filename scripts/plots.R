if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)

saeb = read_csv('data/saeb.csv')

# Raça/Cor por notas
ggplot(saeb) +
    aes(x = RACA_COR, y = (NOTA_LP + NOTA_MT)/2) + 
    geom_boxplot() +
    theme_minimal() +
    labs(x = 'Raça/Cor', y = 'Nota',
    caption = 'Fonte: SAEB 2017') +
    ggsave('img/raca_cor_notas.png')

# Escolaridade da mãe por notas
ggplot(saeb) +
    aes(color = ESC_MAE, x = (NOTA_LP + NOTA_MT)/2) + 
    geom_boxplot()  +
    theme_minimal() +
    labs(y = 'Escolaridade da Mãe', x = 'Nota',
    caption = 'Fonte: SAEB 2017') +
    theme(legend.position="bottom") +
    guides(color=guide_legend(
        title="Escolaridade da Mãe",
        ncol=1)) +
    ggsave('img/esc_mae_notas.png')
