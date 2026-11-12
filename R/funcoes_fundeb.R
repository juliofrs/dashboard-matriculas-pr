# =============================================================================
# R/funcoes_fundeb.R: ENGENHARIA E CÁLCULOS FINANCEIROS DO FUNDEB (2024-2026)
# =============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(janitor)
})

# Formatação monetária brasileira estrita
formatar_moeda_br <- function(valor, texto_na = "Não informado") {
  vapply(valor, function(v) {
    if (is.na(v) || is.null(v)) return(texto_na)
    paste0("R$ ", formatC(v, format = "f", digits = 2, big.mark = ".", decimal.mark = ","))
  }, character(1))
}

# Conversão segura preservando NA para hífens ('-') e mantendo zeros reais
converter_valor_fundeb <- function(vetor) {
  if (is.numeric(vetor)) return(as.numeric(vetor))
  txt <- trimws(as.character(vetor))
  txt[txt %in% c("", "-", "–", "—", "NA", "N/A", "null", "NULL")] <- NA_character_
  suppressWarnings(as.numeric(txt))
}

# Leitura, consolidação e integração territorial via código IBGE
carregar_dados_fundeb <- function(caminho_fundeb, dados_matriculas) {
  abas_esperadas <- list(
    "2024" = "Fundeb 2024",
    "2025" = "Fundeb 2025",
    "2026" = "Fundeb 2026"
  )
  
  # Cadastro territorial canónico extraído da base de matrículas
  cadastro_territorial <- dados_matriculas |>
    mutate(codigo_ibge = str_trim(as.character(codigo_municipio))) |>
    select(codigo_ibge, municipio, cod_nre, nome_nre, regiao_ppa) |>
    distinct()
  
  lista_abas <- lapply(names(abas_esperadas), function(ano_str) {
    aba <- abas_esperadas[[ano_str]]
    df_raw <- read_excel(caminho_fundeb, sheet = aba)
    
    # Padronização de nomes das colunas para snake_case
    names(df_raw) <- c(
      "uf",
      "codigo_ibge",
      "entidade_origem",
      "receita_contribuicao_fundeb",
      "complementacao_vaaf",
      "complementacao_vaat",
      "complementacao_vaar",
      "complementacao_uniao_total",
      "receita_total_prevista"
    )
    
    df_raw |>
      mutate(
        uf              = str_trim(as.character(uf)),
        codigo_ibge     = str_trim(as.character(codigo_ibge)),
        entidade_origem = str_trim(as.character(entidade_origem)),
        ano             = as.integer(ano_str),
        receita_contribuicao_fundeb = converter_valor_fundeb(receita_contribuicao_fundeb),
        complementacao_vaaf         = converter_valor_fundeb(complementacao_vaaf),
        complementacao_vaat         = converter_valor_fundeb(complementacao_vaat),
        complementacao_vaar         = converter_valor_fundeb(complementacao_vaar),
        complementacao_uniao_total  = converter_valor_fundeb(complementacao_uniao_total),
        receita_total_prevista      = converter_valor_fundeb(receita_total_prevista)
      )
  })
  
  consolidado <- bind_rows(lista_abas)
  
  # Separação estrita entre GOVERNO DO ESTADO (codigo_ibge == "41") e os 399 municípios
  df_estado <- consolidado |>
    filter(codigo_ibge == "41") |>
    mutate(municipio = "GOVERNO DO ESTADO") |>
    arrange(desc(ano))
  
  df_mun_bruto <- consolidado |>
    filter(codigo_ibge != "41")
  
  # Diagnóstico de relacionamento antes do join
  duplicidades <- sum(duplicated(paste(df_mun_bruto$ano, df_mun_bruto$codigo_ibge)))
  sem_corresp  <- length(setdiff(unique(df_mun_bruto$codigo_ibge), cadastro_territorial$codigo_ibge))
  
  # Relacionamento exclusivamente via codigo_ibge
  df_municipios <- df_mun_bruto |>
    left_join(cadastro_territorial, by = "codigo_ibge") |>
    mutate(
      municipio = ifelse(is.na(municipio), entidade_origem, municipio),
      # Indicadores derivados (calculados apenas quando os componentes são válidos e denominador > 0)
      pct_vaaf_uniao = ifelse(!is.na(complementacao_vaaf) & !is.na(complementacao_uniao_total) & complementacao_uniao_total > 0,
                              (complementacao_vaaf / complementacao_uniao_total) * 100, NA_real_),
      pct_vaat_uniao = ifelse(!is.na(complementacao_vaat) & !is.na(complementacao_uniao_total) & complementacao_uniao_total > 0,
                              (complementacao_vaat / complementacao_uniao_total) * 100, NA_real_),
      pct_vaar_uniao = ifelse(!is.na(complementacao_vaar) & !is.na(complementacao_uniao_total) & complementacao_uniao_total > 0,
                              (complementacao_vaar / complementacao_uniao_total) * 100, NA_real_),
      pct_uniao_receita = ifelse(!is.na(complementacao_uniao_total) & !is.na(receita_total_prevista) & receita_total_prevista > 0,
                                 (complementacao_uniao_total / receita_total_prevista) * 100, NA_real_)
    ) |>
    arrange(desc(ano), municipio)
  
  sem_nre_ppa <- sum(is.na(df_municipios$nome_nre) | is.na(df_municipios$regiao_ppa))
  
  # Verificação de consistência interna (soma dos componentes vs total informado)
  consistencia <- df_municipios |>
    mutate(
      soma_comp = rowSums(cbind(complementacao_vaaf, complementacao_vaat, complementacao_vaar), na.rm = TRUE),
      tem_algum_comp = !is.na(complementacao_vaaf) | !is.na(complementacao_vaat) | !is.na(complementacao_vaar),
      diff_uniao = ifelse(tem_algum_comp & !is.na(complementacao_uniao_total),
                          abs(soma_comp - complementacao_uniao_total), 0),
      diff_total = ifelse(!is.na(receita_contribuicao_fundeb) & !is.na(receita_total_prevista),
                          abs((receita_contribuicao_fundeb + ifelse(is.na(complementacao_uniao_total), 0, complementacao_uniao_total)) - receita_total_prevista), 0)
    )
  
  relatorio_validacao <- list(
    total_municipios_unicos = n_distinct(df_municipios$codigo_ibge),
    sem_correspondencia     = sem_corresp,
    duplicidades            = duplicidades,
    sem_nre_ppa             = sem_nre_ppa,
    divergencias_uniao      = sum(consistencia$diff_uniao > 0.01, na.rm = TRUE),
    divergencias_total      = sum(consistencia$diff_total > 0.01, na.rm = TRUE)
  )
  
  list(
    municipios          = df_municipios,
    estado              = df_estado,
    relatorio_validacao = relatorio_validacao
  )
}

# Resumo agregado de uma variável respeitando cobertura (n_validos vs n_total)
resumir_variavel_fundeb <- function(vetor) {
  n_total    <- length(vetor)
  n_validos  <- sum(!is.na(vetor))
  n_ausentes <- n_total - n_validos
  
  if (n_validos == 0) {
    return(list(
      soma          = NA_real_,
      n_validos     = 0L,
      n_total       = n_total,
      n_ausentes    = n_ausentes,
      parcial       = TRUE,
      cobertura_txt = sprintf("0 de %d municípios informados (Não disponível)", n_total)
    ))
  }
  
  soma_val <- sum(vetor, na.rm = TRUE)
  parcial  <- n_validos < n_total
  cobertura_txt <- if (parcial) {
    sprintf("Parcial: %d de %d municípios informados (%d sem dado)", n_validos, n_total, n_ausentes)
  } else {
    sprintf("Cobertura integral: %d de %d municípios", n_validos, n_total)
  }
  
  list(
    soma          = soma_val,
    n_validos     = n_validos,
    n_total       = n_total,
    n_ausentes    = n_ausentes,
    parcial       = parcial,
    cobertura_txt = cobertura_txt
  )
}

# Cálculo dos KPIs do Fundeb para o recorte selecionado, com variação interanual segura
calcular_kpis_fundeb <- function(df_municipios, ano_sel, nivel = "parana", localidade = NULL) {
  filtrar_recorte <- function(a) {
    d <- df_municipios |> filter(ano == a)
    if (nivel == "ppa" && !is.null(localidade) && nzchar(localidade)) {
      d <- d |> filter(regiao_ppa == localidade)
    } else if (nivel == "nre" && !is.null(localidade) && nzchar(localidade)) {
      d <- d |> filter(nome_nre == localidade)
    } else if (nivel == "municipio" && !is.null(localidade) && nzchar(localidade)) {
      d <- d |> filter(codigo_ibge == localidade)
    }
    d
  }
  
  df_atual <- filtrar_recorte(ano_sel)
  df_ant   <- if (ano_sel > min(df_municipios$ano)) filtrar_recorte(ano_sel - 1) else NULL
  
  variaveis <- c(
    "complementacao_vaaf",
    "complementacao_vaat",
    "complementacao_vaar",
    "complementacao_uniao_total",
    "receita_contribuicao_fundeb",
    "receita_total_prevista"
  )
  
  res_atual <- lapply(variaveis, function(v) resumir_variavel_fundeb(df_atual[[v]]))
  names(res_atual) <- variaveis
  
  res_ant <- if (!is.null(df_ant) && nrow(df_ant) > 0) {
    tmp <- lapply(variaveis, function(v) resumir_variavel_fundeb(df_ant[[v]]))
    names(tmp) <- variaveis
    tmp
  } else {
    NULL
  }
  
  tot_previsto_atual <- res_atual$receita_total_prevista$soma
  
  out <- lapply(variaveis, function(v) {
    item <- res_atual[[v]]
    
    part_pct <- if (!is.na(item$soma) && !is.na(tot_previsto_atual) && tot_previsto_atual > 0 && v != "receita_total_prevista") {
      (item$soma / tot_previsto_atual) * 100
    } else {
      NA_real_
    }
    
    var_txt  <- "Primeiro exercício da série (2024)"
    var_tipo <- "neutral"
    
    if (!is.null(res_ant)) {
      ant_item <- res_ant[[v]]
      if (item$n_validos == 0 || ant_item$n_validos == 0) {
        var_txt <- sprintf("Comparação indisponível com %d (dados ausentes)", ano_sel - 1)
      } else if (item$parcial || ant_item$parcial) {
        var_txt <- sprintf("Comparação não calculada (cobertura parcial em %d)", ano_sel - 1)
      } else if (ant_item$soma == 0 && item$soma == 0) {
        var_txt <- sprintf("Sem variação (R$ 0,00 em %d e %d)", ano_sel - 1, ano_sel)
      } else if (ant_item$soma == 0 && item$soma > 0) {
        var_txt <- sprintf("+%s vs R$ 0,00 em %d", formatar_moeda_br(item$soma), ano_sel - 1)
        var_tipo <- "positive"
      } else {
        dif_abs <- item$soma - ant_item$soma
        dif_pct <- (dif_abs / ant_item$soma) * 100
        sinal   <- ifelse(dif_abs > 0, "+", "")
        var_txt <- sprintf("%s%.1f%% vs %d (%s%s)", sinal, dif_pct, ano_sel - 1, sinal, formatar_moeda_br(dif_abs))
        var_tipo <- ifelse(dif_abs > 0, "positive", ifelse(dif_abs < 0, "negative", "neutral"))
      }
    }
    
    item$participacao_pct <- part_pct
    item$var_txt          <- var_txt
    item$var_tipo         <- var_tipo
    item
  })
  names(out) <- variaveis
  out
}

# Indicadores complementares por matrícula da Rede Municipal (apenas 2024 e 2025)
calcular_indicadores_por_matricula_municipal <- function(df_fundeb_mun, dados_matriculas, ano_sel) {
  if (!ano_sel %in% unique(dados_matriculas$ano)) {
    return(NULL)
  }
  
  mat_mun <- dados_matriculas |>
    filter(ano == ano_sel, rede == "Municipal") |>
    mutate(codigo_ibge = str_trim(as.character(codigo_municipio))) |>
    group_by(codigo_ibge) |>
    summarise(matriculas_rede_municipal = sum(total_educacao_basica, na.rm = TRUE), .groups = "drop")
  
  df_fundeb_mun |>
    filter(ano == ano_sel) |>
    left_join(mat_mun, by = "codigo_ibge") |>
    mutate(
      receita_prevista_por_mat_mun = ifelse(!is.na(receita_total_prevista) & !is.na(matriculas_rede_municipal) & matriculas_rede_municipal > 0,
                                            receita_total_prevista / matriculas_rede_municipal, NA_real_),
      uniao_por_mat_mun            = ifelse(!is.na(complementacao_uniao_total) & !is.na(matriculas_rede_municipal) & matriculas_rede_municipal > 0,
                                            complementacao_uniao_total / matriculas_rede_municipal, NA_real_),
      vaar_por_mat_mun             = ifelse(!is.na(complementacao_vaar) & !is.na(matriculas_rede_municipal) & matriculas_rede_municipal > 0,
                                            complementacao_vaar / matriculas_rede_municipal, NA_real_)
    )
}
