# =============================================================================
# R/modulo_ia.R: INTEGRAÇÃO SEGURA COM GOOGLE GEMINI (MATRÍCULAS + FUNDEB)
# =============================================================================

suppressPackageStartupMessages({
  library(ellmer)
  library(dplyr)
})

inicializar_assistente_ia <- function(dados_matriculas, dados_fundeb_mun = NULL, dados_fundeb_estado = NULL) {
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
      system_prompt = "Você é um especialista analítico oficial em dados educacionais e financiamento da educação (FUNDEB) do Estado do Paraná.
      Você possui acesso a duas bases independentes:
      1. Censo Escolar da Educação Básica (2016 a 2025) para os 399 municípios do Paraná.
      2. Previsões de Receitas e Complementações da União do FUNDEB (2024, 2025 e 2026) para os 399 municípios e para o Governo do Estado.
      
      Regras inegociáveis:
      1. Nunca invente valores. Sempre utilize as ferramentas disponíveis para consultar os números exatos.
      2. Formate números inteiros no padrão brasileiro (ex: 15.420), percentuais com vírgula (ex: 24,5%) e valores monetários em Reais (ex: R$ 12.450.320,00).
      3. Diferencie rigorosamente R$ 0,00 (zero efetivo informado) de 'Não informado' (dado ausente na planilha oficial, comum na aba de 2024).
      4. Nunca some os valores do GOVERNO DO ESTADO (código IBGE 41) aos totais dos 399 municípios.
      5. Sempre informe o ano e o recorte territorial analisado na resposta."
    )
    
    # -------------------------------------------------------------------------
    # FERRAMENTAS DE MATRÍCULAS (2016-2025)
    # -------------------------------------------------------------------------
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
        description = "Consulta o ranking dos municípios com maiores matrículas ou proporções por rede no Paraná (2016 a 2025).",
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
        description = "Retorna o total e a participação percentual das redes de ensino em matrículas (2016 a 2025).",
        arguments = list(
          ano = ellmer::type_integer("Ano de análise (2016 a 2025)"),
          nivel = ellmer::type_string("Nível territorial: 'estado', 'ppa', 'nre' ou 'municipio'"),
          nome_localidade = ellmer::type_string("Nome da localidade pesquisada ou vazio para estado")
        )
      )
    )
    
    # -------------------------------------------------------------------------
    # FERRAMENTAS DO FUNDEB (2024-2026)
    # -------------------------------------------------------------------------
    if (!is.null(dados_fundeb_mun)) {
      
      # Ferramenta 3: Ranking Financeiro do FUNDEB
      bot$register_tool(
        tool(
          fun = function(ano = 2026, indicador = "complementacao_vaar", filtro_ppa = "todas", filtro_nre = "todos", top_n = 5) {
            ano <- as.numeric(ano)
            top_n <- as.numeric(top_n)
            
            col_valida <- switch(tolower(indicador),
                                 "vaaf" = "complementacao_vaaf",
                                 "complementacao_vaaf" = "complementacao_vaaf",
                                 "vaat" = "complementacao_vaat",
                                 "complementacao_vaat" = "complementacao_vaat",
                                 "vaar" = "complementacao_vaar",
                                 "complementacao_vaar" = "complementacao_vaar",
                                 "uniao" = "complementacao_uniao_total",
                                 "complementacao_uniao_total" = "complementacao_uniao_total",
                                 "contribuicao" = "receita_contribuicao_fundeb",
                                 "receita_contribuicao_fundeb" = "receita_contribuicao_fundeb",
                                 "receita_total_prevista"
            )
            
            df <- dados_fundeb_mun |> filter(ano == !!ano)
            if (tolower(filtro_ppa) != "todas") df <- df |> filter(tolower(regiao_ppa) == tolower(filtro_ppa))
            if (tolower(filtro_nre) != "todos") df <- df |> filter(tolower(nome_nre) == tolower(filtro_nre))
            
            n_tot <- nrow(df)
            df_validos <- df |> filter(!is.na(.data[[col_valida]]))
            n_val <- nrow(df_validos)
            
            top_df <- df_validos |>
              arrange(desc(.data[[col_valida]])) |>
              head(top_n) |>
              transmute(
                ano = ano,
                municipio = municipio,
                regiao_ppa = regiao_ppa,
                nome_nre = nome_nre,
                valor_reais = .data[[col_valida]],
                cobertura_recorte = sprintf("%d de %d municípios informados", n_val, n_tot)
              )
            
            top_df
          },
          description = "Consulta o ranking dos municípios do Paraná por indicador financeiro do FUNDEB (2024 a 2026), ignorando valores não informados.",
          arguments = list(
            ano = ellmer::type_integer("Ano do exercício FUNDEB: 2024, 2025 ou 2026"),
            indicador = ellmer::type_string("Indicador: 'complementacao_vaar', 'complementacao_vaat', 'complementacao_vaaf', 'complementacao_uniao_total', 'receita_contribuicao_fundeb' ou 'receita_total_prevista'"),
            filtro_ppa = ellmer::type_string("Região PPA ('Curitiba', 'Cascavel', 'Maringá', 'Londrina', 'PontaGrossa', 'Guarapuava') ou 'todas'"),
            filtro_nre = ellmer::type_string("Nome do NRE ou 'todos'"),
            top_n = ellmer::type_integer("Quantidade de municípios no ranking")
          )
        )
      )
      
      # Ferramenta 4: Totais e Comparação Interanual do FUNDEB
      bot$register_tool(
        tool(
          fun = function(nivel = "estado", nome_localidade = "") {
            df <- dados_fundeb_mun
            if (tolower(nivel) == "ppa" && nzchar(nome_localidade)) {
              df <- df |> filter(tolower(regiao_ppa) == tolower(nome_localidade))
            } else if (tolower(nivel) == "nre" && nzchar(nome_localidade)) {
              df <- df |> filter(tolower(nome_nre) == tolower(nome_localidade))
            } else if (tolower(nivel) == "municipio" && nzchar(nome_localidade)) {
              df <- df |> filter(tolower(municipio) == tolower(nome_localidade))
            }
            
            df |>
              group_by(ano) |>
              summarise(
                municipios_recorte = n(),
                contrib_fundeb_soma = sum(receita_contribuicao_fundeb, na.rm = TRUE),
                vaaf_validos = sum(!is.na(complementacao_vaaf)),
                vaaf_soma    = ifelse(vaaf_validos == 0, NA_real_, sum(complementacao_vaaf, na.rm = TRUE)),
                vaat_validos = sum(!is.na(complementacao_vaat)),
                vaat_soma    = ifelse(vaat_validos == 0, NA_real_, sum(complementacao_vaat, na.rm = TRUE)),
                vaar_validos = sum(!is.na(complementacao_vaar)),
                vaar_soma    = ifelse(vaar_validos == 0, NA_real_, sum(complementacao_vaar, na.rm = TRUE)),
                uniao_validos = sum(!is.na(complementacao_uniao_total)),
                uniao_soma    = ifelse(uniao_validos == 0, NA_real_, sum(complementacao_uniao_total, na.rm = TRUE)),
                receita_total_soma = sum(receita_total_prevista, na.rm = TRUE),
                .groups = "drop"
              ) |>
              arrange(ano)
          },
          description = "Retorna os totais e a cobertura de dados do FUNDEB para os exercícios 2024, 2025 e 2026 para os municípios do Paraná, Região PPA, NRE ou Município específico (excluindo Governo do Estado).",
          arguments = list(
            nivel = ellmer::type_string("Nível territorial: 'estado', 'ppa', 'nre' ou 'municipio'"),
            nome_localidade = ellmer::type_string("Nome da localidade (PPA, NRE ou Município) ou vazio se nível for 'estado'")
          )
        )
      )
      
      # Ferramenta 5: Diagnóstico de Municípios sem Informação (NA) ou com Zero Efetivo
      bot$register_tool(
        tool(
          fun = function(ano = 2024, indicador = "complementacao_vaat") {
            ano <- as.numeric(ano)
            col_valida <- switch(tolower(indicador),
                                 "vaaf" = "complementacao_vaaf",
                                 "complementacao_vaaf" = "complementacao_vaaf",
                                 "vaat" = "complementacao_vaat",
                                 "complementacao_vaat" = "complementacao_vaat",
                                 "vaar" = "complementacao_vaar",
                                 "complementacao_vaar" = "complementacao_vaar",
                                 "uniao" = "complementacao_uniao_total",
                                 "complementacao_uniao_total" = "complementacao_uniao_total",
                                 "complementacao_vaar"
            )
            
            df <- dados_fundeb_mun |> filter(ano == !!ano)
            n_na   <- sum(is.na(df[[col_valida]]))
            n_zero <- sum(!is.na(df[[col_valida]]) & df[[col_valida]] == 0)
            n_pos  <- sum(!is.na(df[[col_valida]]) & df[[col_valida]] > 0)
            
            exemplos_na <- df |>
              filter(is.na(.data[[col_valida]])) |>
              pull(municipio) |>
              head(10)
            
            list(
              ano = ano,
              indicador = col_valida,
              total_municipios = nrow(df),
              nao_informados_na = n_na,
              zero_efetivo = n_zero,
              com_valor_positivo = n_pos,
              exemplos_municipios_nao_informados = exemplos_na
            )
          },
          description = "Verifica quantos municípios possuem valor 'Não informado' (NA), zero efetivo (R$ 0,00) ou valor positivo em determinado indicador do FUNDEB.",
          arguments = list(
            ano = ellmer::type_integer("Ano de referência: 2024, 2025 ou 2026"),
            indicador = ellmer::type_string("Indicador: 'complementacao_vaaf', 'complementacao_vaat', 'complementacao_vaar' ou 'complementacao_uniao_total'")
          )
        )
      )
    }
    
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