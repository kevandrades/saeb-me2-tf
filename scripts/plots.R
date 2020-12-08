source('scripts/data.R')

# Raça/Cor por notas
ggplot(saeb, aes(x = RACA_COR, y = NOTAS, color = RACA_COR, fill = RACA_COR)) + 
    geom_boxplot(alpha = 0.5) +
    theme_minimal() +
    labs(x = 'Raça/Cor', y = 'Nota',
    caption = 'Fonte: SAEB 2017') +
  theme(legend.box = "vertical",
        legend.position="right") +
  guides(color = guide_legend(label.position =  "right",
                              title="Raça/cor",
                              ncol=1,
                              reverse = T),
         fill = guide_legend(label.position =  "right",
                              title="Raça/cor",
                              ncol=1,
                              reverse = T)) +
    ggsave('img/raca_cor_notas.pdf',
           width = 7.6,
           height = 7,
           dpi = 500)

# Escolaridade da mãe por notas
ggplot(saeb, aes(color = ESC_MAE, x = NOTAS, fill = ESC_MAE))+ 
    geom_boxplot(alpha = 0.5)  +
    theme_minimal() +
    labs(y = 'Escolaridade da Mãe', x = 'Nota',
    caption = 'Fonte: SAEB 2017') +
    theme(legend.box = "vertical",
          legend.position="bottom",
          axis.text.y = element_blank()) +
    guides(color = guide_legend(label.position =  "right",
        title="Escolaridade da Mãe",
        ncol=1,
        reverse = T),
        fill = guide_legend(label.position =  "right",
                              title="Escolaridade da Mãe",
                              ncol=1,
                              reverse = T)) +
    ggsave('img/esc_mae_notas.pdf',
        width = 7.6,
        height = 7,
        dpi = 500)

    
    
    
    