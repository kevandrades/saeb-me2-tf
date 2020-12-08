if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  gridExtra, tidyverse, reshape2, patchwork,
  data.table, EnvStats, PMCMR, gridExtra
)


####  1. ##### Carregando os dados ######
source('scripts/data.R')

#=============== Gráficos ======================#

#_________________________Temazin
theme_hjw <- function(){
  theme(panel.background = element_rect(fill = "#404040", colour = "#404040",size = 2,
                                        linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',colour = "#808080"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "#808080"),
        plot.background = element_rect(fill = "#404040"),
        axis.title.x=element_text(colour="white", size=7),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(colour = "white"),
        plot.title = element_blank(),
        plot.subtitle = element_blank(),
        plot.caption =  element_blank(),
        legend.position = element_blank())
}

#___________________________________Função para gerar o histograma

histMtLp <- function(dados, xname){

dados <- dados  
 
# separando o banco em 2 pra juntar nota em mat e lp em uma coluna só
ntmt <- dados
ntmt$NOTA_LP <- NULL
ntmt$area <- 'Matemática'
names(ntmt)[names(ntmt) == "NOTA_MT"] <- "nota"


ntpt <- dados
ntpt$NOTA_MT <- NULL
ntpt$area <- 'Língua Portuguesa'
names(ntpt)[names(ntpt) == "NOTA_LP"] <- "nota"

dados <- rbind(ntpt,ntmt)

#grafico
dados %>%
    ggplot(aes(x = nota, color = area, fill = area)) +
    geom_histogram(binwidth = 18,alpha = 0.5, position = 'identity')+
    scale_color_manual(values=c("#638bd6","white"))+
    scale_fill_manual(values=c("#638bd6","white"))+
    labs(x=xname, y = '')+
    theme_hjw()+ theme(legend.position="none")
  
}

#fazendo cada um dos histogramas individualmente pra gerar o grid

#escolaridade da mae                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
escm1 <- saeb %>% filter(ESC_MAE=='Não sei') %>% histMtLp('Não Sabe')
escm2 <- saeb %>% filter(ESC_MAE=='Nunca estudou') %>%histMtLp('Nunca estudou')
escm3 <- saeb %>% filter(ESC_MAE=='Não completou a 4.ª série/5.º ano do Ensino Fundamental')%>%histMtLp('Fundamental 1 Incompleto')
escm4 <- saeb %>% filter(ESC_MAE=='Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental')%>%histMtLp('Fundamental 2 Incompleto')
escm5 <- saeb %>% filter(ESC_MAE=='Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio')%>%histMtLp('Ensino Médio Incompleto')
escm6 <- saeb %>% filter(ESC_MAE=='Completou o Ensino Médio, mas não completou a Faculdade')%>%histMtLp('Ensino Médio Completo')
escm7 <- saeb %>% filter(ESC_MAE=='Completou a Faculdade')%>%histMtLp('Completou Faculdade')

#raca/cor
rc1 <- saeb %>% filter(RACA_COR=='Não quero declarar')%>%histMtLp('Não quer declarar')
rc2 <- saeb %>% filter(RACA_COR=='Amarela')%>%histMtLp('Amarela')
rc3 <- saeb %>% filter(RACA_COR=='Branca')%>%histMtLp('Branca')
rc4 <- saeb %>% filter(RACA_COR=='Indígena')%>%histMtLp('Indígena')
rc5 <- saeb %>% filter(RACA_COR=='Parda')%>%histMtLp('Parda')
rc6 <- saeb %>% filter(RACA_COR=='Preta')%>%histMtLp('Preta')

#localizacao
lc1 <- saeb %>% filter(LOCALIZACAO=='Urbana')%>%histMtLp('Urbana')
lc2 <- saeb %>% filter(LOCALIZACAO=='Rural')%>%histMtLp('Rural')

#sexo
sx1 <- saeb %>% filter(SEXO=='Feminino')%>%histMtLp('Feminino')
sx2 <- saeb %>% filter(SEXO=='Masculino')%>%histMtLp('Masculino')

#fundo escuro pro grid
vf <- ggplot(data = saeb)+
    theme(panel.background = element_rect(fill = "#404040",colour = "#404040",size = 2,linetype = "solid"),
    plot.background = element_rect(fill = "#404040"))
                            

grid.arrange(escm1,rc1,lc1,sx1,
             escm2,rc2,lc2,sx2,
             escm3,rc3,vf,vf,
             escm4,rc4,vf,vf,
             escm5,rc5,vf,vf,
             escm6,rc6,vf,vf,
             escm7,vf,vf,vf,
             nrow=7, top='Histogramas para o desempenho dos Estudantes')
x11()



# Densidade (LOC --- NOTAS )

# Função para criar linhas da densidade
StatNormalDensity <- ggproto(
  "StatNormalDensity", Stat,
  required_aes = "x",
  default_aes = aes(y = stat(y)),
  
  compute_group = function(data, scales, xlim = NULL, n = 101) {
    mean_value <- mean(data$x)
    sd_value <- sd(data$x)
    fun <- function(x) dnorm(x, mean = mean_value, sd = sd_value)
    StatFunction$compute_group(data, scales, fun = fun, xlim = xlim, n = n)
  }
)

ggplot(saeb, aes(x = NOTAS, color = LOCALIZACAO, fill = LOCALIZACAO )) +
  geom_density(alpha = 0.7, size = 0, linetype = 1)+
# geom_line(aes(fill = LOCALIZACAO),stat = StatNormalDensity, size = 1.5, linetype = 1)+
#  geom_vline(data = saeb %>% group_by(LOCALIZACAO) %>% summarise(m = mean(NOTAS), sd = sd(NOTAS)),
#             aes(xintercept = m, color = LOCALIZACAO), size = 1.5, show.legend = F,linetype = 2)+

  theme_minimal()+
  guides(color = NULL) +
  labs(x = "Nota",
       y = "Densidade",
       fill = "Localização",
       color = "Localização") +
  scale_fill_manual(values = c("#00B81F","#E08B00") )+
  scale_color_manual(values = c("#00B81F","#E08B00") )+
  ggsave('report/img/loc_notas.pdf',
         width = 7.6,
         height = 3,
         dpi = 500)


# Barras (Sexo --- afazeres)

# Dados da porcetagem do sexo com base nos afazeres
pct_sexo  <- saeb %>% 
  group_by(AFAZERES_DOM,SEXO) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

ggplot(pct_sexo, aes(x = AFAZERES_DOM,y = perc,fill = SEXO)) +
  geom_col(position = "dodge", size = 2, alpha = 0.75) +
  labs(fill = "Sexo",
       x = "Tempo de afazeres domésticos",
       y = NULL)+
  scale_y_continuous(breaks = seq(0,1,.25), labels = scales::percent(seq(0,1,.25)),limits = c(0,1))+
  theme_minimal()+
  scale_fill_manual(values = c("#F8766D","#00BBDB"))+
  ggsave('report/img/sexo_afazeres.pdf',
         width = 9,
         height = 4,
         dpi = 500)
  


# Raça/Cor por notas
ggplot(saeb, aes(x = RACA_COR, y = NOTAS, color = RACA_COR, fill = RACA_COR)) + 
    geom_boxplot(alpha = 0.5) +
    theme_minimal() +
    labs(x = 'Raça/Cor', y = 'Nota') +
  theme(legend.box = "vertical",
        legend.position="right") +
  guides(color = guide_legend(label.position =  "right",
                              title="Raça/Cor",
                              ncol=1,
                              reverse = T),
         fill = guide_legend(label.position =  "right",
                              title="Raça/Cor",
                              ncol=1,
                              reverse = T)) +
    ggsave('report/img/raca_cor_notas.pdf',
           width = 7.6,
           height = 4,
           dpi = 500)

# Escolaridade da mãe por notas
ggplot(saeb, aes(color = ESC_MAE, x = NOTAS, fill = ESC_MAE))+ 
    geom_boxplot(alpha = 0.5)  +
    theme_minimal() +
    labs(y = 'Escolaridade da Mãe', x = 'Nota') +
    theme(legend.box = "vertical",
          legend.position="bottom",
          axis.text.y = element_blank()) +
    guides(color = guide_legend(label.position =  "right",
        title="Escolaridade \n        da \n      Mãe",
        ncol=1,
        reverse = T),
        fill = guide_legend(label.position =  "right",
                              title="Escolaridade \n        da \n      Mãe",
                              ncol=1,
                              reverse = T)) +
    ggsave('report/img/esc_mae_notas.pdf',
        width = 7.6,
        height = 5,
        dpi = 500)
