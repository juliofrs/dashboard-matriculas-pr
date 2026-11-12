# =============================================================================
# PROJETO: PLATAFORMA DE INTELIGÊNCIA EDUCACIONAL DO PARANÁ (MATRÍCULAS & FUNDEB)
# SCRIPT DE PREPARAÇÃO, AUDITORIA E CACHE GEOGRÁFICO
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(janitor)
  library(sf)
  library(geobr)
})

caminho_matriculas <- "Matriculas_faixa_etaria_rede_municipio_consultas.xlsx"
caminho_fundeb     <- "Fundeb.xlsx"

if (!file.exists(caminho_matriculas)) {
  stop("ERRO: A base de matrículas '", caminho_matriculas, "' não foi encontrada.")
}
if (!file.exists(caminho_fundeb)) {
  stop("ERRO: A base financeira '", caminho_fundeb, "' não foi encontrada.")
}

message(">>> [1/4] A auditar a base de matrículas e cadastro territorial...")
df_plan1 <- read_excel(caminho_matriculas, sheet = "Plan1") |> clean_names()
df_pop   <- read_excel(caminho_matriculas, sheet = "População") |> clean_names()

df_plan1$codigo_municipio <- as.numeric(df_plan1$codigo_municipio)
df_pop$codigo_ibge        <- as.numeric(df_pop$codigo_ibge)

message(sprintf("    ✓ Registos em Plan1: %s linhas (%d municípios x %d anos x %d redes)",
                format(nrow(df_plan1), big.mark = "."),
                n_distinct(df_plan1$codigo_municipio),
                n_distinct(df_plan1$ano),
                n_distinct(df_plan1$rede)))

munis_sem_pop <- setdiff(df_plan1$codigo_municipio, df_pop$codigo_ibge)
if (length(munis_sem_pop) > 0) {
  warning("Aviso: Existem municípios em Plan1 sem correspondência na aba População.")
} else {
  message("    ✓ Correspondência exata: 100% dos 399 municípios vinculados à população.")
}

message(">>> [2/4] A auditar a base financeira FUNDEB (2024 a 2026)...")
source("R/funcoes_fundeb.R")
res_fundeb <- carregar_dados_fundeb(caminho_fundeb, df_plan1)
relatorio  <- res_fundeb$relatorio_validacao

message(sprintf("    ✓ Registos municipais consolidados: %d linhas (%d municípios x 3 exercícios)",
                nrow(res_fundeb$municipios), n_distinct(res_fundeb$municipios$codigo_ibge)))
message(sprintf("    ✓ Registos estaduais isolados (GOVERNO DO ESTADO - IBGE 41): %d linhas",
                nrow(res_fundeb$estado)))
message(sprintf("    ✓ Códigos sem correspondência: %d | Duplicidades: %d | Sem NRE/PPA: %d",
                relatorio$sem_correspondencia, relatorio$duplicidades, relatorio$sem_nre_ppa))

ficheiro_mapa_rds <- "mapa_pr_2020.rds"
message(">>> [3/4] A verificar a malha cartográfica dos municípios do Paraná...")

if (!file.exists(ficheiro_mapa_rds)) {
  message("    A descarregar malha geográfica via geobr (ano 2020)...")
  mapa_pr <- geobr::read_municipality(code_muni = "PR", year = 2020, showProgress = FALSE) |>
    sf::st_transform(4326)
  mapa_pr$code_muni <- as.numeric(mapa_pr$code_muni)
  saveRDS(mapa_pr, ficheiro_mapa_rds)
  message(sprintf("    ✓ Malha guardada com sucesso em '%s'.", ficheiro_mapa_rds))
} else {
  message(sprintf("    ✓ Ficheiro de cache '%s' já existe.", ficheiro_mapa_rds))
}

message(">>> [4/4] Auditoria concluída. Ambiente validado para execução do app.R.")