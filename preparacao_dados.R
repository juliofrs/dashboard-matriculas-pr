# 1. Instalar os pacotes necessários (se você ainda não tiver)
# Tire o '#' da linha abaixo e rode uma vez, caso precise instalar:
install.packages(c("readxl", "dplyr", "janitor"))

# 2. Carregar as bibliotecas
library(readxl)
library(dplyr)
library(janitor) # Ótimo para limpar nomes de colunas

# 3. Importar a base de dados
# Estamos puxando a aba "Base", que contém os dados gerais
caminho_arquivo <- "Matriculas_Municipio_AI_AF_EM_populacao.xlsx"

dados_matriculas <- read_excel(caminho_arquivo, sheet = "Base") |> 
  clean_names() # Deixa os nomes das colunas padronizados (sem espaços e minúsculos)

# 4. Visualizar as primeiras linhas e a estrutura
head(dados_matriculas)
glimpse(dados_matriculas)
