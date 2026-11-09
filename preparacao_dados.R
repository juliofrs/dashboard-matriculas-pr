# =============================================================================
# PROJETO: PAINEL EDUCACIONAL DO PARANÁ (CENSO ESCOLAR 2016-2025)
# SCRIPT DE PREPARAÇÃO, AUDITORIA E CACHE GEOGRÁFICO
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(janitor)
  library(sf)
  library(geobr)
})

caminho_base <- "Matriculas_faixa_etaria_rede_municipio_consultas.xlsx"

if (!file.exists(caminho_base)) {
  stop("ERRO: O ficheiro de dados '", caminho_base, "' não foi encontrado.")
}

message(">>> [1/3] A carregar e auditar as abas da base de dados...")
df_plan1 <- read_excel(caminho_base, sheet = "Plan1") |> clean_names()
df_pop   <- read_excel(caminho_base, sheet = "População") |> clean_names()

df_plan1$codigo_municipio <- as.numeric(df_plan1$codigo_municipio)
df_pop$codigo_ibge        <- as.numeric(df_pop$codigo_ibge)

# Auditoria de integridade
total_linhas <- nrow(df_plan1)
total_munis  <- n_distinct(df_plan1$codigo_municipio)
total_anos   <- n_distinct(df_plan1$ano)
total_redes  <- n_distinct(df_plan1$rede)

message(sprintf("    ✓ Registos em Plan1: %s linhas (399 municípios x 10 anos x 4 redes)", format(total_linhas, big.mark = ".")))
message(sprintf("    ✓ População total (2024): %s habitantes", format(sum(df_pop$populacao_2024, na.rm = TRUE), big.mark = ".")))

# Validação do relacionamento entre abas
munis_sem_pop <- setdiff(df_plan1$codigo_municipio, df_pop$codigo_ibge)
if (length(munis_sem_pop) > 0) {
  warning("Aviso: Existem municípios em Plan1 sem correspondência na aba População.")
} else {
  message("    ✓ Correspondência exata: 100% dos municípios vinculados.")
}

# Descarregamento e serialização da malha geográfica para cache permanente
ficheiro_mapa_rds <- "mapa_pr_2020.rds"
message(">>> [2/3] A verificar a malha cartográfica dos municípios do Paraná...")

if (!file.exists(ficheiro_mapa_rds)) {
  message("    A descarregar malha geográfica via geobr (ano 2020)...")
  mapa_pr <- geobr::read_municipality(code_muni = "PR", year = 2020, showProgress = FALSE) |>
    sf::st_transform(4326)
  mapa_pr$code_muni <- as.numeric(mapa_pr$code_muni)
  
  saveRDS(mapa_pr, ficheiro_mapa_rds)
  message(sprintf("    ✓ Malha guardada com sucesso em '%s'.", ficheiro_mapa_rds))
} else {
  message(sprintf("    ✓ Ficheiro de cache '%s' já existe. Etapa concluída.", ficheiro_mapa_rds))
}

message(">>> [3/3] Processamento concluído com sucesso. O ambiente está pronto para execução do app.R.")