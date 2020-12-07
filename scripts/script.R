###### Carregando os pacotes ######
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  ggplot2, readr, dplyr,
  forcats, reshape2, purrr,
  data.table, EnvStats, PMCMR
  )


#================== Relações =====================#
# 
# -------------- Descrições ----------------------#
# 
# - Quantitativa:
# Continua: NOTAS_(MT/LP)
# Discreta: AFAZERES_DOM
# 
# - Qualitativa:
# Nominal: LOCALIZACAO, SEXO, RACA_COR
# Ordinal: AFAZERES_DOM, ESC_MAE
# ------------------------------------------------#
# 
# -- Teste + Fator --- Interesse -----------------# 
#
# RD || RI + Loc --- Notas 
#  RD & RI + Raça_cor --- Notas e Afazeres
#       RD + Esc_mae --- Notas e Afazeres
#       RI + sexo --- Afazeres
# 
#=================================================#
# 1. | Carregando dados 
# 2. | Medidas basicas
# 3. | Testes
# - Aderência
# - Comparação
# 
# 
# 
# 
# 
#=================================================#
#
# 3. Testes:
# 
# ----- Aderência (A): ----
# .1) Qui²
# .2) Komogorov
# .3) Shapiro-Wilk
# .4) Anderson-Darling
# .5) Lilliefor
# 
# ----- Variância (V) ----
# >> Dois grupos (2): << 
# .1) F-snedecor
# 
# >> Vários grupos (3): <<
# .1) Bartlett
# .2) Levene
# 
# ----- Relações independentes (RI) ----
# >> Dois grupos (2): <<
# .1) T-student (N)
# .2) Wilcox
# .3) Komogorov-Sminorf
# .4) Cramer-von mise
# 
# >> Vários grupos (3): <<
# .1) Fisher (ANOVA) (N)
# .2) Kruskal- Wallis
# 
# ----- Relações Dependentes (RD) ----
# >> Dois grupos (2): <<
# .1) T-student pareado (N)
# .2) Cox-stuart/John-Arbuthnot(Sinais)
# .3) Wilcox sinalizados
# 
# >> Vários grupos (3):
# .1) Friedman
# .2) Quade
# 
# 
#=================================================#


####  1. ##### Carregando os dados ######
dados <- fread('data/saeb.csv', encoding = 'UTF-8', # definindo os dados
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
                                                                                  "Menos de 1 hora"))
)

#=================================================#

#### 2. ##### Medidas basicas ######

# Total de alunos para cada fator (Frequencia)
freq <- map(select(dados, -c(NOTA_LP,NOTA_MT)), # Dados
            ~select(data.frame(table(.x)), Freq, variavel= .x ))  # Função


#------------ Resumo das relações ----------------#


# (Medias-Resumo) para as relações "LOCALIZACAO","RACA_COR","ESC_MAE" com as NOTAS_(MT/LP)
{basico_notas <- list()
  for (fator in c("LOCALIZACAO","RACA_COR","ESC_MAE")) { # Fator
  basico_notas[[fator]] <- list()
    for (nota in c("NOTA_MT", "NOTA_LP")){ # Interesse
      valor <- aggregate(as.formula(paste(nota ,"~" , fator)), dados, summary) # Est. Basicas
      basico_notas[[fator]][[nota]] <- data.frame(Grupo = valor[,1],  valor[,2]) # data. frame
    }
  }
}

# ?? Proporção ??? para as relações "LOCALIZACAO", "RACA_COR", "SEXO" com AFAZERES_DOM

{basico_afazer <- list()
  for (fator in c("LOCALIZACAO", "RACA_COR", "SEXO")) {
    basico_afazer[[fator]] <- round(prop.table(table(select(dados,fator,AFAZERES_DOM)),2),2)
  } # Proporção por coluna (2)
}


#=================================================#

#### 2. ##### Testes ######

#---------------- Aderência (A) ------------------#
# Para as NOTAS_(LP/MT) testes de normalidade

set.seed(7)
notas <- select(dados, NOTA_MT, NOTA_LP)

# P-valores dos testes de Shapiro-Wilk (sw), Shapio-Francia (sf) e Aderson-Darling (ad)
{testes_adere <- list()
  for (n in c(30, 50, 100)) { # Tamanhos das amostras
    adere_amostra <- sample_n(notas, n)
    testes_adere[[as.character(n)]] <- list()
    for (teste in c("sw", "sf", "ad")) { # Tipos de testes
      testes_adere[[as.character(n)]][[teste]] <-
        map_dbl(notas, ~ gofTest(.x, test = teste)$p.value) # P-valor do teste
    }
  }
}

#================== Relações =====================#
# 
# -------------- Descrições ----------------------#
# 
# - Quantitativa:
# Continua: NOTAS_(MT/LP)
# Discreta: AFAZERES_DOM
# 
# - Qualitativa:
# Nominal: LOCALIZACAO, SEXO, RACA_COR
# Ordinal: AFAZERES_DOM, ESC_MAE
# ------------------------------------------------#
# 
# -- Teste + Fator --- Interesse -----------------# 
#
# RD || RI + Loc --- Notas 
#  RD & RI + Raça_cor --- Notas e Afazeres
#       RD + Esc_mae --- Notas e Afazeres
#       RI + sexo --- Afazeres
# 
# ------------------------------------------------#



#-------------- Comparação (NOTAS) ---------------#
# Testes para as relações LOCALIZACAO, RACA_COR, ESC_MAE, com as NOTAS_(LP/MT)
comp_notas <- select(dados, LOCALIZACAO, RACA_COR, ESC_MAE, NOTA_MT, NOTA_LP)




# Testes ANOVA e Kruskal-Wallis #

# P-valores para as realções da LP + MT, (Todas tiveram relação!!! [RA 95%])
{testes_notas_mais <- list()
  for (fator in c("LOCALIZACAO","RACA_COR","ESC_MAE")) { # Fator
    testes_notas_mais[[fator]][["Bartlett"]] <- bartlett.test(as.formula(paste("(NOTA_LP + NOTA_MT) ~", fator)), comp_notas)$p.value
    testes_notas_mais[[fator]][["ANOVA"]] <-  summary(aov(as.formula(paste("(NOTA_LP + NOTA_MT) ~", fator)), comp_notas))[[1]][["Pr(>F)"]][1]
  }
}


# Comparação (dois a dois) para os testes das realções da LP + MT


# LOCALIZACAO, em média maiores notas na Ubana
comp_notas %>% group_by(LOCALIZACAO) %>% summarise(media = mean(NOTA_LP + NOTA_MT))


# RACA_COR
pairwise.t.test(comp_notas$NOTA_LP+comp_notas$NOTA_MT, comp_notas$RACA_COR, p.adjust.method = "b") # Testes dois a dois
posthoc.kruskal.conover.test(comp_notas$NOTA_LP+comp_notas$NOTA_MT, comp_notas$RACA_COR) # Testes dois a dois


# ESC_MAE
pairwise.t.test(comp_notas$NOTA_LP+comp_notas$NOTA_MT, comp_notas$ESC_MAE, p.adjust.method = "b") # Testes dois a dois
posthoc.kruskal.conover.test(comp_notas$NOTA_LP+comp_notas$NOTA_MT, comp_notas$ESC_MAE) # Teste



#----------- Comparação (AFAZERES_DOM) ----------------#
# Testes para as relações ESC_MAE, RACA_COR, SEXO, com os AFAZERES_DOM

comp_afr <- select(dados, ESC_MAE, RACA_COR, SEXO, AFAZERES_DOM) %>% # Comparações 
  mutate(AFAZERES_DOM = AFAZERES_DOM %>% fct_recode("1" = "Não faz ou faz menos de 1 hora",
                                                    "2" = "Entre 1 e 2 horas",
                                                    "3" = "Mais de 2 horas, até 3 horas",
                                                    "4" = "Mais de 3 horas") %>% as.numeric()) # Transforma em ordinal ("Discreta")



## RACA_COR --- AFAZERES_DOM ##
# Não teve relação!
kruskal.test(AFAZERES_DOM ~ RACA_COR, comp_afr) # Mesmo p-valor para 2 categorias


## ESC_MAE --- AFAZERES_DOM ##
# Teve relação!
kruskal.test(AFAZERES_DOM ~ ESC_MAE, comp_afr) 

# Comparação 2 a 2
pairwise.wilcox.test(comp_afr$AFAZERES_DOM, comp_notas$ESC_MAE, p.adjust.method = "b")


## SEXO --- AFAZERES_DOM ##
# Teve relação!
kruskal.test(AFAZERES_DOM ~ SEXO, comp_afr) # Possui o mesmo p-valor
wilcox.test(AFAZERES_DOM ~ SEXO, comp_afr) # Tanto faz utilizar


# Em media o sexo feminino faz por mais tempo afazeres domesticos que o sexo masculino
comp_afr %>% group_by(SEXO) %>% summarise(media = mean(AFAZERES_DOM))



