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

## TESTE DE NORMALIDADE

shapiro.test(sem_out$`(%) CEREBRO`)

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

## TESTE RESIDUOS ANOVA

plotResiduals(mod)
testResiduals(mod)

## TESTE DE BONFERRONI - COMPARAÇOES MULTIPLAS

bonferroni <- LSD.test(mod, "TRATAMENTO", p.adj="bonferroni", group=TRUE)
print(bonferroni)

## TESTE LSD - COMPARAÇOES MULTIPLAS

lsd <- LSD.test(mod, "TRATAMENTO", group=TRUE)
print(lsd)

################################################################################

##### VARIÁVEL `FÍGADO`

## GRÁFICO DE FREQUÊNCIAS POR DENSIDADE, SEPARANDO TRATAMENTOS DIFERENTES POR COR

ggplot(data = FETO_GERAL, mapping = aes(x = `FÍGADO`, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = `TRATAMENTO`))

# ELIMINANDO OUTLIER

sem_out_figado <- filter(FETO_GERAL, `FÍGADO` < 0.75)

ggplot(data = sem_out_figado, mapping = aes(x = `FÍGADO`, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = `TRATAMENTO`), size = 1.5)

## BOXPLOT - sem outlier

ggplot(data = sem_out_figado, mapping = aes(x = `TRATAMENTO`, y = `FÍGADO`)) +
  geom_boxplot()

## TESTE DE NORMALIDADE

shapiro.test(sem_out_figado$FÍGADO)

## GRÁFICO DE BARRAS DE MÉDIAS E ERROS - sem outlier

p_load(tidyverse, ggplot2, ggpubr)

ggplot(data = sem_out_figado) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = `FÍGADO`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = `FÍGADO`),
           stat = "summary",
           fun = "mean")

## ANOVA - sem outlier

mod_figado <- aov(`FÍGADO` ~ `TRATAMENTO`, data = sem_out_figado)
summary(mod_figado)

## TESTE RESIDUOS ANOVA

plotResiduals(mod_figado)
testResiduals(mod_figado)

## TESTE DE BONFERRONI - COMPARAÇOES MULTIPLAS

bonferroni_figado <- LSD.test(mod_figado, "TRATAMENTO", p.adj="bonferroni", group=TRUE)
print(bonferroni_figado) # teste LSD apresentou mesmos resultados

################################################################################

##### VARIÁVEL `PC/PF`

## GRÁFICO DE FREQUÊNCIAS POR DENSIDADE, SEPARANDO TRATAMENTOS DIFERENTES POR COR

ggplot(data = FETO_GERAL, mapping = aes(x = `PC/PF`, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = `TRATAMENTO`))

# ELIMINANDO OUTLIER

sem_out_pcpf <- filter(FETO_GERAL, `PC/PF` < 5)

ggplot(data = sem_out_pcpf, mapping = aes(x = `PC/PF`, y = ..density..)) +
  geom_freqpoly(mapping = aes(colour = `TRATAMENTO`), size = 1.5)

## BOXPLOT - sem outlier

ggplot(data = sem_out_pcpf, mapping = aes(x = `TRATAMENTO`, y = `PC/PF`)) +
  geom_boxplot()

## TESTE DE NORMALIDADE

shapiro.test(sem_out_pcpf$`PC/PF`)

## GRÁFICO DE BARRAS DE MÉDIAS E ERROS - sem outlier

p_load(tidyverse, ggplot2, ggpubr)

ggplot(data = sem_out_pcpf) +
  geom_errorbar(mapping = aes(x = `TRATAMENTO`, y = `PC/PF`), 
                stat = "summary",
                fun.data = "mean_se") +
  geom_bar(mapping = aes(x = `TRATAMENTO`, y = `PC/PF`),
           stat = "summary",
           fun = "mean")

## ANOVA - sem outlier

mod_pcpf <- aov(`PC/PF` ~ `TRATAMENTO`, data = sem_out_pcpf)
summary(mod_pcpf)

## TESTE RESIDUOS ANOVA

plotResiduals(mod_pcpf)
testResiduals(mod_pcpf)

## TESTE DE LSD - COMPARAÇOES MULTIPLAS

lsd_pcpf <- LSD.test(mod_pcpf, "TRATAMENTO", group=TRUE)
print(lsd_pcpf)
