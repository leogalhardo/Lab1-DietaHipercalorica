

setwd("C:/Users/vhlm-/OneDrive/Área de Trabalho/Lab 1/Projeto 1")

library(tidyverse)
library(readxl)
library(GGally)
library(ggpubr)

dados_feto <- read_excel("C:/Users/vhlm-/OneDrive/Área de Trabalho/Lab 1/Projeto 1/RESULTADOS Corrigido 24-03.xlsx", 
                         sheet = "FETO GERAL", col_types = c("numeric", 
                                                             "text", "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric"))
View(dados_feto)


## Boxplot
boxplot(dados_feto$`Placenta-PESO`)# Existem outiliers
boxplot(dados_feto$PESO)# Existem outiliers
boxplot(dados_feto$`Eficiência Placentária`)# Existem outiliers
boxplot(dados_feto$COMPRIMENTO)# Existem outiliers

# QQ Normal Plot do ggpubr
ggqqplot(dados_feto$`Placenta-PESO`)
ggqqplot(dados_feto$PESO)
ggqqplot(dados_feto$`Eficiência Placentária`)
ggqqplot(dados_feto$COMPRIMENTO)


# Teste de Normalidade Shapiro
shapiro.test(dados_feto$`Placenta-PESO`) #Não Apresenta Normalidade
shapiro.test(dados_feto$PESO)#Não Apresenta Normalidade
shapiro.test(dados_feto$`Eficiência Placentária`)#Não Apresenta Normalidade
shapiro.test(dados_feto$COMPRIMENTO)#Não Apresenta Normalidade


teste <- dados_feto %>% select(`Placenta-PESO`,PESO,`Eficiência Placentária`,
                               COMPRIMENTO)

ggpairs(teste)




