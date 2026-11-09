# =============================================================================
# R/modulo_ia.R: INTEGRAÇÃO SEGURA COM GOOGLE GEMINI VIA ELLMER
# =============================================================================

suppressPackageStartupMessages({
  library(ellmer)
  library(dplyr)
})

inicializar_assistente_ia <- function(dados_matriculas) {
  if (file.exists(".Renviron")) {
    readRenviron(".Renviron")
  }
  
  chave_gemini <- Sys.getenv("GEMINI_API_KEY")
  chave_gemini <- trimws(gsub('^["\']|["\']$', '', chave_gemini))
  
  if (!nzchar(chave_gemini)) {
    return(list(
      bot = NULL,
      erro = "A variável GEMINI_API_KEY não foi configurada no ambiente (.Renviron ou variáveis do servidor)."
    ))
  }
  
  res <- tryCatch({
    bot <- ellmer::chat_google_gemini(
      model = "gemini-3.7-flash",
      api_key = chave_gemini,
      system_prompt = "Você é um especialista analítico oficial em dados educacionais do Estado do Paraná.
      Responda dúvidas sobre o Censo Escolar da Educação Básica (2016 a 2025).
      Regras inegociáveis:
      1. Nunca invente valores. Sempre chame as ferramentas fornecidas para consultar os dados exatos.
      2. Formate números no padrão brasileiro: pontos nos milhares (ex: 15.420) e vírgula nos decimais (ex: 24,5%).
      3. Seja conciso, executivo e estruturado."
    )
    
    # Ferramenta 1: Ranking com suporte a etapas e faixas etárias
    bot$register_tool(
      tool(
        fun = function(ano = 2025, rede = "Privada", tipo = "percentual", filtro_nre = "todos", filtro_ppa = "todas", top_n = 5) {
          ano <- as.numeric(ano)
          top_n <- as.numeric(top_n)
          
          df <- dados_matriculas |> filter(ano == !!ano)
          if (tolower(filtro_nre) != "todos") df <- df |> filter(tolower(nome_nre) == tolower(filtro_nre))
          if (tolower(filtro_ppa) != "todas") df <- df |> filter(tolower(regiao_ppa) == tolower(filtro_ppa))
          
          tabela <- df |>
            group_by(municipio) |>
            summarise(
              total = sum(total_educacao_basica, na.rm = TRUE),
              estadual = sum(total_educacao_basica[rede == "Estadual"], na.rm = TRUE),
              municipal = sum(total_educacao_basica[rede == "Municipal"], na.rm = TRUE),
              federal = sum(total_educacao_basica[rede == "Federal"], na.rm = TRUE),
              privada = sum(total_educacao_basica[rede == "Privada"], na.rm = TRUE),
              publico = estadual + municipal + federal,
              .groups = "drop"
            )
          
          r_lower <- tolower(rede)
          coluna_valor <- if (grepl("priv", r_lower)) {
            tabela$privada
          } else if (grepl("est", r_lower)) {
            tabela$estadual
          } else if (grepl("mun", r_lower)) {
            tabela$municipal
          } else if (grepl("fed", r_lower)) {
            tabela$federal
          } else if (grepl("p[uú]b", r_lower)) {
            tabela$publico
          } else {
            tabela$total
          }
          
          if (grepl("perc|taxa|propor", tolower(tipo))) {
            tabela$metrica <- ifelse(tabela$total == 0, 0, round((coluna_valor / tabela$total) * 100, 2))
          } else {
            tabela$metrica <- coluna_valor
          }
          
          tabela |>
            select(municipio, total_matriculas = total, valor_alvo = metrica) |>
            arrange(desc(valor_alvo)) |>
            head(top_n)
        },
        description = "Consulta o ranking dos municípios com maiores matrículas ou proporções por rede no Paraná.",
        arguments = list(
          ano = ellmer::type_integer("Ano de análise (2016 a 2025)"),
          rede = ellmer::type_string("Rede de ensino: 'Privada', 'Estadual', 'Municipal', 'Federal' ou 'Total'"),
          tipo = ellmer::type_string("Formato: 'percentual' para taxas ou 'absoluto' para quantidade"),
          filtro_nre = ellmer::type_string("Nome do NRE ou 'todos'"),
          filtro_ppa = ellmer::type_string("Nome da região PPA ou 'todas'"),
          top_n = ellmer::type_integer("Quantidade de municípios a exibir")
        )
      )
    )
    
    # Ferramenta 2: Totais Agregados por Recorte
    bot$register_tool(
      tool(
        fun = function(ano = 2025, nivel = "estado", nome_localidade = "") {
          ano <- as.numeric(ano)
          df <- dados_matriculas |> filter(ano == !!ano)
          
          if (tolower(nivel) == "ppa" && nzchar(nome_localidade)) {
            df <- df |> filter(tolower(regiao_ppa) == tolower(nome_localidade))
          } else if (tolower(nivel) == "nre" && nzchar(nome_localidade)) {
            df <- df |> filter(tolower(nome_nre) == tolower(nome_localidade))
          } else if (tolower(nivel) == "municipio" && nzchar(nome_localidade)) {
            df <- df |> filter(tolower(municipio) == tolower(nome_localidade))
          }
          
          resumo <- df |>
            group_by(rede) |>
            summarise(total_matriculas = sum(total_educacao_basica, na.rm = TRUE), .groups = "drop")
          
          tot <- sum(resumo$total_matriculas)
          resumo$participacao_pct <- round((resumo$total_matriculas / tot) * 100, 2)
          resumo
        },
        description = "Retorna o total e a participação percentual das redes para o Paraná, Região PPA, NRE ou Município.",
        arguments = list(
          ano = ellmer::type_integer("Ano de análise (2016 a 2025)"),
          nivel = ellmer::type_string("Nível territorial: 'estado', 'ppa', 'nre' ou 'municipio'"),
          nome_localidade = ellmer::type_string("Nome da localidade pesquisada ou vazio para estado")
        )
      )
    )
    
    list(bot = bot, erro = NULL)
  }, error = function(e) {
    list(bot = NULL, erro = e$message)
  })
  
  res
}

extrair_texto_usuario <- function(msg) {
  if (is.character(msg)) return(msg)
  if (is.list(msg)) {
    if (!is.null(msg$text) && is.character(msg$text)) return(msg$text)
    if (!is.null(msg$value) && is.character(msg$value)) return(msg$value)
    if (length(msg) > 0 && is.character(msg[[1]])) return(msg[[1]])
  }
  as.character(msg)
}