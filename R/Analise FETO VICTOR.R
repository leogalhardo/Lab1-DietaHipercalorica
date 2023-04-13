

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




#### PLANILHA FETO GERAL ###########


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


#Testando `CEREBRO` por TRATAMENTO (Não- paramétrico)

kruskal.test(`CEREBRO` ~ factor(TRATAMENTO),dados_feto) # os grupos não apresentam diferença significativa



# Testando `(%) FÍGADO` por TRATAMENTO (Paramétrico)
mod <- aov(`(%) FÍGADO` ~ `TRATAMENTO`, data = dados_feto)
anova(mod)

bonferroni <- LSD.test(mod, "TRATAMENTO", p.adj="bonferroni", group=TRUE)
print(bonferroni)


#Testando FÍGADO por TRATAMENTO (Não- paramétrico)

kruskal.test(FÍGADO ~ factor(TRATAMENTO),dados_feto) # os grupos apresentam diferença significativa

pairwise.wilcox.test(dados_feto$FÍGADO, factor(dados_feto$TRATAMENTO),
                     p.adjust.method = "none")


#Testando `PC/PF` por TRATAMENTO (Não- paramétrico)

kruskal.test(`PC/PF` ~ factor(TRATAMENTO),dados_feto) # os grupos apresentam diferença significativa


`DIAMETRO MEDIO PLACENTA`

# Testando `DIAMETRO MEDIO PLACENTA` por TRATAMENTO (Paramétrico)
mod <- aov(`DIAMETRO MEDIO PLACENTA` ~ `TRATAMENTO`, data = dados_feto)
anova(mod)

bonferroni <- LSD.test(mod, "TRATAMENTO", p.adj="bonferroni", group=TRUE)
print(bonferroni)




####### PLANILHA MÃE GERAL #########

## --------- Analisando mais profundamente os gráficos ------##

# Boxplot PESO 0 por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `PESO 0`)) +
  geom_boxplot()

# Boxplot PESO FINAL por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `PESO FINAL`)) +
  geom_boxplot()

# Boxplot `GANHO DE PESO` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `GANHO DE PESO`)) +
  geom_boxplot()

# Boxplot `N. FETOS` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `N. FETOS`)) +
  geom_boxplot()


# Boxplot `figado (%)` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `figado (%)`)) +
  geom_boxplot()

# Boxplot FÍGADO por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = FÍGADO)) +
  geom_boxplot()


# Boxplot `baço (%)` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `baço (%)`)) +
  geom_boxplot()

# Boxplot BAÇO por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = BAÇO)) +
  geom_boxplot()


# Boxplot `G ABD (%)` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `G ABD (%)`)) +
  geom_boxplot()


# Boxplot `G ABDOMINAL` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `G ABDOMINAL`)) +
  geom_boxplot()

# Boxplot `GGD (%)` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `GGD (%)`)) +
  geom_boxplot()


# Boxplot `GG D.` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `GG D.`)) +
  geom_boxplot()


# Boxplot `GGE (%)` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `GGE (%)`)) +
  geom_boxplot()


# Boxplot `GG ES.` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `GG ES.`)) +
  geom_boxplot()

# Boxplot `OVA D (%)` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `OVA D (%)`)) +
  geom_boxplot()

# Boxplot `OVÁRIO D.` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `OVÁRIO D.`)) +
  geom_boxplot()

# Boxplot `OVA E (%)` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `OVA E (%)`)) +
  geom_boxplot()

# Boxplot `OVÁRIO E.` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `OVÁRIO E.`)) +
  geom_boxplot()


# Boxplot `N. TENTATIVAS COPULA` por grupo
ggplot(data = dados_mae, mapping = aes(x = `GRUPO`, y = `N. TENTATIVAS COPULA`)) +
  geom_boxplot()


## ------------ Gráficos de Barras de Erros ------------##


# Gráfico de barras de erro para "PESO 0" por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `PESO 0`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `PESO 0`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `PESO FINAL` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `PESO FINAL`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `PESO FINAL`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `GANHO DE PESO` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `GANHO DE PESO`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `GANHO DE PESO`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `GANHO/FETO` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `GANHO/FETO`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `GANHO/FETO`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `N. FETOS` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `N. FETOS`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `N. FETOS`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `figado (%)` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `figado (%)`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `figado (%)`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `FÍGADO` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `FÍGADO`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `FÍGADO`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `baço (%)` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `baço (%)`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `baço (%)`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `BAÇO` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `BAÇO`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `BAÇO`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `G ABD (%)` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `G ABD (%)`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `G ABD (%)`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `G ABDOMINAL` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `G ABDOMINAL`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `G ABDOMINAL`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `GGD (%)` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `GGD (%)`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `GGD (%)`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `GG D.` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `GG D.`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `GG D.`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `GGE (%)` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `GGE (%)`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `GGE (%)`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `GG ES.` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `GG ES.`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `GG ES.`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `OVA D (%)` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `OVA D (%)`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `OVA D (%)`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `OVÁRIO D.` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `OVÁRIO D.`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `OVÁRIO D.`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `OVA E (%)` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `OVA E (%)`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `OVA E (%)`),
           stat = "summary",
           fun = "mean")


# Gráfico de barras de erro para `OVÁRIO E.` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `OVÁRIO E.`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `OVÁRIO E.`),
           stat = "summary",
           fun = "mean")

# Gráfico de barras de erro para `N. TENTATIVAS COPULA` por Grupo

ggplot(data = dados_mae) +
  geom_errorbar(mapping = aes(x = `GRUPO`, y = `N. TENTATIVAS COPULA`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `GRUPO`, y = `N. TENTATIVAS COPULA`),
           stat = "summary",
           fun = "mean")


##### Filtrando os grupos na planilha mãe geral #####

dados_coco <- dados_mae %>% filter(GRUPO == "COCONUT")

descritiva_coco <- dados_coco %>% get_summary_stats(`PESO 0`,`PESO FINAL`,`GANHO DE PESO`,`GANHO/FETO`,
                                                    `N. FETOS`,`figado (%)`,`FÍGADO`,`baço (%)`,
                                                    `BAÇO`,`G ABD (%)`,`G ABDOMINAL`,`GGD (%)`,
                                                    `GG D.`,`GGE (%)`,`GG ES.`,`OVA D (%)`,`OVÁRIO D.`,
                                                    `OVA E (%)`,`OVÁRIO E.`,`N. TENTATIVAS COPULA`)
descritiva_coco


dados_cont <- dados_mae %>% filter(GRUPO == "CONTROLE")

descritiva_cont <- dados_cont %>% get_summary_stats(`PESO 0`,`PESO FINAL`,`GANHO DE PESO`,`GANHO/FETO`,
                                                    `N. FETOS`,`figado (%)`,`FÍGADO`,`baço (%)`,
                                                    `BAÇO`,`G ABD (%)`,`G ABDOMINAL`,`GGD (%)`,
                                                    `GG D.`,`GGE (%)`,`GG ES.`,`OVA D (%)`,`OVÁRIO D.`,
                                                    `OVA E (%)`,`OVÁRIO E.`,`N. TENTATIVAS COPULA`)

descritiva_cont



dados_lard <- dados_mae %>% filter(GRUPO == "LARD")

descritiva_lard <- dados_lard %>% get_summary_stats(`PESO 0`,`PESO FINAL`,`GANHO DE PESO`,`GANHO/FETO`,
                                                    `N. FETOS`,`figado (%)`,`FÍGADO`,`baço (%)`,
                                                    `BAÇO`,`G ABD (%)`,`G ABDOMINAL`,`GGD (%)`,
                                                    `GG D.`,`GGE (%)`,`GG ES.`,`OVA D (%)`,`OVÁRIO D.`,
                                                    `OVA E (%)`,`OVÁRIO E.`,`N. TENTATIVAS COPULA`)


descritiva_lard

dados_soy <- dados_mae %>% filter(GRUPO == "SOYBEAN")

descritiva_soy <- dados_soy %>% get_summary_stats(`PESO 0`,`PESO FINAL`,`GANHO DE PESO`,`GANHO/FETO`,
                                                  `N. FETOS`,`figado (%)`,`FÍGADO`,`baço (%)`,
                                                  `BAÇO`,`G ABD (%)`,`G ABDOMINAL`,`GGD (%)`,
                                                  `GG D.`,`GGE (%)`,`GG ES.`,`OVA D (%)`,`OVÁRIO D.`,
                                                  `OVA E (%)`,`OVÁRIO E.`,`N. TENTATIVAS COPULA`)


descritiva_soy

##-------- QQplot para as variáveis --------##

# QQ Normal Plot do ggpubr
ggqqplot(dados_mae$`PESO 0`)
ggqqplot(dados_mae$`PESO FINAL`)
ggqqplot(dados_mae$`GANHO DE PESO`)
ggqqplot(dados_mae$`GANHO/FETO`)
ggqqplot(dados_mae$`N. FETOS`)
ggqqplot(dados_mae$`figado (%)`)
ggqqplot(dados_mae$`FÍGADO`)
ggqqplot(dados_mae$`baço (%)`)
ggqqplot(dados_mae$`BAÇO`)
ggqqplot(dados_mae$`G ABD (%)`)
ggqqplot(dados_mae$`G ABDOMINAL`)
ggqqplot(dados_mae$`GGD (%)`)
ggqqplot(dados_mae$`GG D.`)
ggqqplot(dados_mae$`GGE (%)`)
ggqqplot(dados_mae$`GG ES.`)
ggqqplot(dados_mae$`OVA D (%)`)
ggqqplot(dados_mae$`OVÁRIO D.`)
ggqqplot(dados_mae$`OVA E (%)`)
ggqqplot(dados_mae$`OVÁRIO E.`)
ggqqplot(dados_mae$`N. TENTATIVAS COPULA`)


##-------- Teste de Normalidade para as variáveis --------##


# Teste de Normalidade Shapiro
shapiro.test(dados_mae$`PESO 0`) #Apresenta Normalidade
shapiro.test(dados_mae$`PESO FINAL`)#Apresenta Normalidade
shapiro.test(dados_mae$`GANHO DE PESO`)#Apresenta Normalidade
shapiro.test(dados_mae$`GANHO/FETO`)#Apresenta Normalidade
shapiro.test(dados_mae$`N. FETOS`)#Apresenta Normalidade
shapiro.test(dados_mae$`figado (%)`)#Apresenta Normalidade
shapiro.test(dados_mae$`FÍGADO`)#Apresenta Normalidade
shapiro.test(dados_mae$`baço (%)`)#Apresenta Normalidade
shapiro.test(dados_mae$`BAÇO`)#Apresenta Normalidade
shapiro.test(dados_mae$`G ABD (%)`)#Apresenta Normalidade
shapiro.test(dados_mae$`G ABDOMINAL`)#Apresenta Normalidade
shapiro.test(dados_mae$`GGD (%)`)#Apresenta Normalidade
shapiro.test(dados_mae$`GG D.`)#Apresenta Normalidade
shapiro.test(dados_mae$`GGE (%)`)#Apresenta Normalidade
shapiro.test(dados_mae$`GG ES.`)#Apresenta Normalidade
shapiro.test(dados_mae$`OVA D (%)`)#Apresenta Normalidade
shapiro.test(dados_mae$`OVÁRIO D.`)#Apresenta Normalidade
shapiro.test(dados_mae$`OVA E (%)`)# Não apresenta Normalidade
shapiro.test(dados_mae$`OVÁRIO E.`)# Não apresenta Normalidade
shapiro.test(dados_mae$`N. TENTATIVAS COPULA`)# Não apresenta Normalidade




