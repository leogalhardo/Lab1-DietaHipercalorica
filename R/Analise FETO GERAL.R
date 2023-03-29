install.packages("ggplot2")
install.packages("ggpubr")
install.packages("pacman")
library(remotes)
install_github("fndemarqui/planex", force = TRUE)
library(planex)
library(pacman)
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(agricolae)

#################### ANÁLISE TABELA FETO GERAL ###############################

##### VARIÁVEL `(%) CEREBRO`

## GRÁFICO DE FREQUÊNCIAS POR DENSIDADE, SEPARANDO TRATAMENTOS DIFERENTES POR COR

ggplot(data = FETO_GERAL, mapping = aes(x = `(%) CEREBRO`, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = `TRATAMENTO`))

# ELIMINANDO OUTLIER

sem_out <- filter(FETO_GERAL, `(%) CEREBRO` < 0.4)
  
ggplot(data = sem_out, mapping = aes(x = `(%) CEREBRO`, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = `TRATAMENTO`), size = 1.5)

## BOXPLOT

ggplot(data = FETO_GERAL, mapping = aes(x = `TRATAMENTO`, y = `(%) CEREBRO`)) +
  geom_boxplot()

# ELIMINANDO OUTLIER

ggplot(data = sem_out, mapping = aes(x = `TRATAMENTO`, y = `(%) CEREBRO`)) +
  geom_boxplot()

## GRÁFICO DE BARRAS DE MÉDIAS E ERROS - sem outlier

p_load(tidyverse, ggplot2, ggpubr)

ggplot(data = sem_out) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = `(%) CEREBRO`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = `(%) CEREBRO`),
           stat = "summary",
           fun = "mean")

## ANOVA - sem outlier

mod <- aov(`(%) CEREBRO` ~ `TRATAMENTO`, data = sem_out)
summary(mod)

## TESTE DE BONFERRONI - COMPARAÇOES MULTIPLAS

bonferroni <- LSD.test(mod, "TRATAMENTO", p.adj="bonferroni", group=TRUE)
print(bonferroni)

