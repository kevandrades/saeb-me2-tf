if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  gridExtra, ggplot2, readr, dplyr,
  forcats, reshape2, purrr, patchwork,
  data.table, EnvStats, PMCMR, gridExtra
)


####  1. ##### Carregando os saeb ######
saeb <- fread('data/saeb.csv', encoding = 'UTF-8', # definindo os saeb
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
saeb <- saeb %>%
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
        "Não completou a 4.ª série/5.º ano do Ensino Fundamental",
        "Completou a 4.ª série/5.º ano, mas não completou a 8.ª série/9.º ano do Ensino Fundamental",
        "Completou a 8.ª série/9.º ano do Ensino Fundamental, mas não completou o Ensino Médio",
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



ggplot(saeb, aes(x = NOTAS, color = LOCALIZACAO)) +
#  geom_density(alpha = 0.7, size = 1, linetype = 1)+
  geom_line(stat = StatNormalDensity, size = 1, linetype = 1)+
  geom_vline(data = saeb %>% group_by(LOCALIZACAO) %>% summarise(m = mean(NOTAS), sd = sd(NOTAS)),
             aes(xintercept = m, color = LOCALIZACAO), size = 1, show.legend = F)+
  scale_color_manual(values = c("black","red")) +
  theme_minimal()+
  guides(color = guide_legend(title = "Localização")) +
  labs(title = "Densidade normal da soma das notas dos alunos com base \nna localização da escola",
       subtitle = "Rural ~N(479, 84.3²), Urbana ~N(512, 86²)",
       x = "Nota",
       y = "Densidade")


# Barras (Sexo --- afazeres)

# Dados da porcetagem do sexo com base nos afazeres
pct_sexo  <- saeb %>% 
  group_by(AFAZERES_DOM,SEXO) %>% 
  summarise(count = n()) %>% 
  mutate(perc = count/sum(count))

ggplot(pct_sexo, aes(x = AFAZERES_DOM,y = perc, fill = SEXO)) +
  geom_col(position = "dodge") +
  labs(title = "Tempo de afazeres domésticos com base na porcetagens dos sexos dos alunos",
       fill = "Sexo",
       x = "Tempo de afazeres domésticos",
       y = NULL)+
  scale_y_continuous(breaks = seq(0,1,.25), labels = scales::percent(seq(0,1,.25)),limits = c(0,1))+
  scale_x_discrete()+
  theme_minimal()



