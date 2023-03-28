library(readxl)

CICLO_ESTRAL <- read_excel("D:/UFMG 2023.1/Lab 1/Rodada 1/Dados/CICLO ESTRAL.xlsx")

CONSUMO_DIARIO <- read_excel("D:/UFMG 2023.1/Lab 1/Rodada 1/Dados/CONSUMO DIÁRIO.xlsx")

FETO_GERAL <- read_excel("D:/UFMG 2023.1/Lab 1/Rodada 1/Dados/FETO GERAL.xlsx", 
                         col_types = c("text", "text", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric"))

GANHO_DE_PESO_GESTACAO <- read_excel("D:/UFMG 2023.1/Lab 1/Rodada 1/Dados/GANHO DE PESO GESTAÇÃO.xlsx")

GANHO_DE_PESO <- read_excel("D:/UFMG 2023.1/Lab 1/Rodada 1/Dados/GANHO DE PESO.xlsx")

MAE_GERAL <- read_excel("D:/UFMG 2023.1/Lab 1/Rodada 1/Dados/MÃE GERAL.xlsx")

NUMERO_DE_CICLOS<- read_excel("D:/UFMG 2023.1/Lab 1/Rodada 1/Dados/NUMERO DE CICLOS.xlsx")
