

setwd("C:/Users/vhlm-/OneDrive/Área de Trabalho/Lab 1/Projeto 1")

library(tidyverse)
library(readxl)
library(GGally)
library(ggpubr)
library(agricolae)
library(rstatix)
library(writexl)

dados_feto <- read_excel("C:/Users/vhlm-/OneDrive/Área de Trabalho/Lab 1/Projeto 1/RESULTADOS alterados 03-04.xlsx", 
                         sheet = "FETO GERAL", col_types = c("numeric", 
                                                             "text", "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric"))
View(dados_feto)


dados_mae <- read_excel("RESULTADOS alterados 03-04.xlsx", 
                                         sheet = "MÃE GERAL", col_types = c("numeric", 
                                                                            "text", "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric", 
                                                                            "numeric", "numeric", "numeric"))




## --------- Gráfico de correlação entre as variáveis --------##

cor <- dados_feto %>% select(`Placenta-PESO`,PESO,`Eficiência Placentária`,
                               COMPRIMENTO,`(%) CEREBRO`,CEREBRO,
                             `(%) FÍGADO`,FÍGADO,`PC/PF`,`DIAMETRO MEDIO PLACENTA`)

ggpairs(cor)

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

# Boxplot '(%) CEREBRO' por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = `TRATAMENTO`, y = `(%) CEREBRO`)) +
  geom_boxplot() +
  ggtitle("Boxplot da variável '(%) CEREBRO'")


# Boxplot CEREBRO por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = `TRATAMENTO`, y = CEREBRO)) +
  geom_boxplot() +
  ggtitle("Boxplot da variável CEREBRO")

# Boxplot `(%) FÍGADO` por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = TRATAMENTO, y = `(%) FÍGADO`)) +
  geom_boxplot() +
  ggtitle("Boxplot da variável `(%) FÍGADO`")


# Boxplot FÍGADO` por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = TRATAMENTO, y = FÍGADO)) +
  geom_boxplot() +
  ggtitle("Boxplot da variável FÍGADO")


# Boxplot `PC/PF` por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = TRATAMENTO, y = `PC/PF`)) +
  geom_boxplot() +
  ggtitle("Boxplot da variável `PC/PF`")


# Boxplot `DIAMETRO MEDIO PLACENTA` por tipo de tratamento
ggplot(data = dados_feto, mapping = aes(x = TRATAMENTO, y = `DIAMETRO MEDIO PLACENTA`)) +
  geom_boxplot() +
  ggtitle("Boxplot da variável `DIAMETRO MEDIO PLACENTA`")


`(%) FÍGADO`,FÍGADO,`PC/PF`,`DIAMETRO MEDIO PLACENTA`

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


# Gráfico de barras de erro para `(%) FÍGADO` por tipo de tratamento

ggplot(data = dados_feto) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = `(%) FÍGADO`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = `(%) FÍGADO`),
           stat = "summary",
           fun = "mean")



# Gráfico de barras de erro para FÍGADO por tipo de tratamento

ggplot(data = dados_feto) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = FÍGADO), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = FÍGADO),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `PC/PF` por tipo de tratamento

ggplot(data = dados_feto) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = `PC/PF`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = `PC/PF`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `DIAMETRO MEDIO PLACENTA` por tipo de tratamento

ggplot(data = dados_feto) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = `DIAMETRO MEDIO PLACENTA``), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = `DIAMETRO MEDIO PLACENTA``),
           stat = "summary",
           fun = "mean")


#### Tabela Descritiva por tratamento #########


dados_coco <- dados_feto %>% filter(TRATAMENTO == "COCONUT")

descritiva_coco <- dados_coco %>% get_summary_stats(`Placenta-PESO`,PESO,
                                               `Eficiência Placentária`,
                                               COMPRIMENTO,`(%) FÍGADO`,FÍGADO,
                                               `PC/PF`,`DIAMETRO MEDIO PLACENTA`)
descritiva_coco


dados_cont <- dados_feto %>% filter(TRATAMENTO == "CONTROLE")

descritiva_cont <- dados_cont %>% get_summary_stats(`Placenta-PESO`,PESO,
                                                    `Eficiência Placentária`,
                                                    COMPRIMENTO,`(%) FÍGADO`,FÍGADO,
                                                    `PC/PF`,`DIAMETRO MEDIO PLACENTA`)


dados_lard <- dados_feto %>% filter(TRATAMENTO == "LARD")

descritiva_lard <- dados_lard %>% get_summary_stats(`Placenta-PESO`,PESO,
                                                    `Eficiência Placentária`,
                                                    COMPRIMENTO,`(%) FÍGADO`,FÍGADO,
                                                    `PC/PF`,`DIAMETRO MEDIO PLACENTA`)


dados_soy <- dados_feto %>% filter(TRATAMENTO == "SOYBEAN")

descritiva_soy <- dados_soy %>% get_summary_stats(`Placenta-PESO`,PESO,
                                                    `Eficiência Placentária`,
                                                    COMPRIMENTO,`(%) FÍGADO`,FÍGADO,
                                                    `PC/PF`,`DIAMETRO MEDIO PLACENTA`)


##-------- QQplot para as variáveis --------##

# QQ Normal Plot do ggpubr
ggqqplot(dados_feto$`Placenta-PESO`)
ggqqplot(dados_feto$PESO)
ggqqplot(dados_feto$`Eficiência Placentária`)
ggqqplot(dados_feto$COMPRIMENTO)
ggqqplot(dados_feto$`(%) CEREBRO`)
ggqqplot(dados_feto$CEREBRO)
ggqqplot(dados_feto$`(%) FÍGADO`)
ggqqplot(dados_feto$FÍGADO)
ggqqplot(dados_feto$`PC/PF`)
ggqqplot(dados_feto$`DIAMETRO MEDIO PLACENTA`)

##-------- Teste de Normalidade para as variáveis --------##


# Teste de Normalidade Shapiro
shapiro.test(dados_feto$`Placenta-PESO`) #Apresenta Normalidade
shapiro.test(dados_feto$PESO)#Não Apresenta Normalidade
shapiro.test(dados_feto$`Eficiência Placentária`)# Apresenta Normalidade
shapiro.test(dados_feto$COMPRIMENTO)#Não Apresenta Normalidade
shapiro.test(dados_feto$`(%) CEREBRO`)#Não Apresenta Normalidade
shapiro.test(dados_feto$CEREBRO)#Não Apresenta Normalidade
shapiro.test(dados_feto$`(%) FÍGADO`)# Apresenta Normalidade
shapiro.test(dados_feto$FÍGADO)#Não Apresenta Normalidade
shapiro.test(dados_feto$`PC/PF`)#Não Apresenta Normalidade
shapiro.test(dados_feto$`DIAMETRO MEDIO PLACENTA`)# Apresenta Normalidade


###------------ Testes de Hipóteses -------------###

# Testando `Placenta-PESO` por TRATAMENTO (Paramétrico)
mod <- aov(`Placenta-PESO` ~ `TRATAMENTO`, data = dados_feto)
anova(mod)

### Não apresenta diferença significativa entre os grupos


#Testando PESO por TRATAMENTO (Não- paramétrico)

kruskal.test(PESO ~ factor(TRATAMENTO),dados_feto) # os grupos apresentam diferença significativa

pairwise.wilcox.test(dados_feto$PESO, factor(dados_feto$TRATAMENTO),
                     p.adjust.method = "none")




# Testando `Eficiência Placentária` por TRATAMENTO (Paramétrico)
mod <- aov(`Eficiência Placentária` ~ `TRATAMENTO`, data = dados_feto)
anova(mod)# os grupos apresentam diferença significativa

bonferroni <- LSD.test(mod, "TRATAMENTO", p.adj="bonferroni", group=TRUE)
print(bonferroni)



#Testando COMPRIMENTO por TRATAMENTO (Não- paramétrico)

kruskal.test(COMPRIMENTO ~ factor(TRATAMENTO),dados_feto) # os grupos apresentam diferença significativa

pairwise.wilcox.test(dados_feto$COMPRIMENTO, factor(dados_feto$TRATAMENTO),
                     p.adjust.method = "none")



#Testando `(%) CEREBRO` por TRATAMENTO (Não- paramétrico)

kruskal.test(`(%) CEREBRO` ~ factor(TRATAMENTO),dados_feto) # os grupos apresentam diferença significativa

pairwise.wilcox.test(dados_feto$`(%) CEREBRO`, factor(dados_feto$TRATAMENTO),
                     p.adjust.method = "none")



