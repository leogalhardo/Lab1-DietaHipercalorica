

setwd("C:/Users/vhlm-/OneDrive/Área de Trabalho/Lab 1/Projeto 1")

library(tidyverse)
library(readxl)
library(GGally)
library(ggpubr)
library(agricolae)
library(rstatix)
library(writexl)

dados_feto <- read_excel("C:/Users/vhlm-/OneDrive/Área de Trabalho/Lab 1/Projeto 1/RESULTADOS Corrigido 24-03.xlsx", 
                         sheet = "FETO GERAL", col_types = c("numeric", 
                                                             "text", "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric"))
View(dados_feto)

FETO_GERAL <- dados_feto


##-------- Boxplots para as 4 variáveis --------##

## Boxplot
boxplot(dados_feto$`Placenta-PESO`)# Existem outiliers
boxplot(dados_feto$PESO)# Existem outiliers
boxplot(dados_feto$`Eficiência Placentária`)# Existem outiliers
boxplot(dados_feto$COMPRIMENTO)# Existem outiliers


##-------- QQplot para as 4 variáveis --------##

# QQ Normal Plot do ggpubr
ggqqplot(dados_feto$`Placenta-PESO`)
ggqqplot(dados_feto$PESO)
ggqqplot(dados_feto$`Eficiência Placentária`)
ggqqplot(dados_feto$COMPRIMENTO)


##-------- Teste de Normalidade para as 4 variáveis --------##


# Teste de Normalidade Shapiro
shapiro.test(dados_feto$`Placenta-PESO`) #Não Apresenta Normalidade
shapiro.test(dados_feto$PESO)#Não Apresenta Normalidade
shapiro.test(dados_feto$`Eficiência Placentária`)#Não Apresenta Normalidade
shapiro.test(dados_feto$COMPRIMENTO)#Não Apresenta Normalidade


## --------- Gráfico de correlação entre as variáveis --------##

cor <- dados_feto %>% select(`Placenta-PESO`,PESO,`Eficiência Placentária`,
                               COMPRIMENTO)

grafico_cor <- ggpairs(cor)
grafico_cor


## --------- Analisando mais profundamente os gráficos ------##

# Boxplot Placenta-Peso por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = `TRATAMENTO`, y = `Placenta-PESO`)) +
  geom_boxplot()

# Boxplot PESO por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = `TRATAMENTO`, y = PESO)) +
  geom_boxplot()

# Boxplot Eficiência Placentária por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = `TRATAMENTO`, y = `Eficiência Placentária`)) +
  geom_boxplot()

# Boxplot COMPRIMENTO por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = `TRATAMENTO`, y = COMPRIMENTO)) +
  geom_boxplot()


## ------------ Gráficos de Barras de Erros ------------##


# Gráfico de barras de erro para Placenta-PESO por tipo de tratamento

ggplot(data = dados_feto) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = `Placenta-PESO`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = `Placenta-PESO`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para PESO por tipo de tratamento

ggplot(data = dados_feto) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = PESO), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = PESO),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para Eficiência Placentária por tipo de tratamento

ggplot(data = dados_feto) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = `Eficiência Placentária`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = `Eficiência Placentária`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para COMPRIMENTO por tipo de tratamento

ggplot(data = dados_feto) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = COMPRIMENTO), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = COMPRIMENTO),
           stat = "summary",
           fun = "mean")

summary(dados_feto$COMPRIMENTO)

#### Tabela Descritiva por tratamento #########


dados_coco <- dados_feto %>% filter(TRATAMENTO == "COCONUT")

descritiva <- as.data.frame(dados_coco %>% get_summary_stats(`Placenta-PESO`,PESO,`Eficiência Placentária`,COMPRIMENTO))


write_xlsx(descritiva,"DescritivaCOCO.xlsx")


dados_cont <- dados_feto %>% filter(TRATAMENTO == "CONTROLE")

descritiva <- as.data.frame(dados_cont %>% get_summary_stats(`Placenta-PESO`,PESO,`Eficiência Placentária`,COMPRIMENTO))


write_xlsx(descritiva,"DescritivaCONT.xlsx")



dados_lard <- dados_feto %>% filter(TRATAMENTO == "LARD")

descritiva <- as.data.frame(dados_lard %>% get_summary_stats(`Placenta-PESO`,PESO,`Eficiência Placentária`,COMPRIMENTO))


write_xlsx(descritiva,"DescritivaLARD.xlsx")



dados_soy <- dados_feto %>% filter(TRATAMENTO == "SOYBEAN")

descritiva <- as.data.frame(dados_soy %>% get_summary_stats(`Placenta-PESO`,PESO,`Eficiência Placentária`,COMPRIMENTO))


write_xlsx(descritiva,"DescritivaSOY.xlsx")


#`(%) CEREBRO`,`FÍGADO`,`PC/PF`

descritiva <- as.data.frame(dados_coco %>% get_summary_stats(`(%) CEREBRO`,`FÍGADO`,`PC/PF`))


write_xlsx(descritiva,"DescritivaCOCO.xlsx")



descritiva <- as.data.frame(dados_cont %>% get_summary_stats(`(%) CEREBRO`,`FÍGADO`,`PC/PF`))


write_xlsx(descritiva,"DescritivaCONT.xlsx")



descritiva <- as.data.frame(dados_lard %>% get_summary_stats(`(%) CEREBRO`,`FÍGADO`,`PC/PF`))


write_xlsx(descritiva,"DescritivaLARD.xlsx")


descritiva <- as.data.frame(dados_soy %>% get_summary_stats(`(%) CEREBRO`,`FÍGADO`,`PC/PF`))


write_xlsx(descritiva,"DescritivaSOY.xlsx")

