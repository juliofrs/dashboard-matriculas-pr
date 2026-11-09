# =============================================================================
# R/funcoes_calculo.R: REGRAS ESTATÍSTICAS E CONSOLIDAÇÃO DE DADOS
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(scales)
  library(janitor)
})

# Formatação estrita no padrão brasileiro
formatar_numero_br <- function(valor, decimais = 0) {
  if (is.na(valor) || is.null(valor)) return("—")
  format(round(valor, decimais), big.mark = ".", decimal.mark = ",", scientific = FALSE)
}

formatar_percentual_br <- function(valor, decimais = 1) {
  if (is.na(valor) || is.null(valor)) return("—")
  paste0(format(round(valor * 100, decimais), big.mark = ".", decimal.mark = ",", nsmall = decimais), "%")
}

# Consolidação da tabela por município e rede de ensino
consolidar_matriculas_rede <- function(dados, ano_filtro, nre_filtro = "todos", ppa_filtro = "todas") {
  df <- dados |> filter(ano == ano_filtro)
  
  if (ppa_filtro != "todas") {
    df <- df |> filter(regiao_ppa == ppa_filtro)
  }
  if (nre_filtro != "todos") {
    df <- df |> filter(nome_nre == nre_filtro)
  }
  
  tabela <- df |>
    select(codigo_municipio, municipio, regiao_ppa, nome_nre, rede, valor_etapa, populacao_2024) |>
    pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |>
    clean_names()
  
  if (!"federal" %in% names(tabela))   tabela$federal   <- 0
  if (!"municipal" %in% names(tabela)) tabela$municipal <- 0
  if (!"estadual" %in% names(tabela))  tabela$estadual  <- 0
  if (!"privada" %in% names(tabela))   tabela$privada   <- 0
  
  tabela |>
    mutate(
      publico = estadual + municipal + federal,
      total   = publico + privada
    )
}

# Cálculo do valor final (absoluto ou percentual) respeitando as regras estatísticas
calcular_metrica_final <- function(tabela_larga, rede_selecionada, tipo_valor) {
  tabela <- tabela_larga
  
  tabela$valor_base <- case_when(
    rede_selecionada == "publico"   ~ tabela$publico,
    rede_selecionada == "estadual"  ~ tabela$estadual,
    rede_selecionada == "municipal" ~ tabela$municipal,
    rede_selecionada == "federal"   ~ tabela$federal,
    rede_selecionada == "privada"   ~ tabela$privada,
    TRUE                            ~ tabela$total
  )
  
  total_recorte_soma <- sum(tabela$total, na.rm = TRUE)
  
  if (tipo_valor == "percentual") {
    if (rede_selecionada == "total") {
      # Participação do município no total do recorte
      tabela$valor_final <- ifelse(total_recorte_soma == 0, 0, tabela$total / total_recorte_soma)
    } else {
      # Proporção da rede selecionada face ao total de matrículas do município
      tabela$valor_final <- ifelse(tabela$total == 0, 0, tabela$valor_base / tabela$total)
    }
  } else {
    tabela$valor_final <- as.numeric(tabela$valor_base)
  }
  
  tabela
}

# Resumo executivo de KPIs e variação face ao ano anterior
calcular_kpis_executivos <- function(dados, ano_filtro, nre_filtro = "todos", ppa_filtro = "todas") {
  filtrar_base <- function(a) {
    df <- dados |> filter(ano == a)
    if (ppa_filtro != "todas") df <- df |> filter(regiao_ppa == ppa_filtro)
    if (nre_filtro != "todos") df <- df |> filter(nome_nre == nre_filtro)
    df
  }
  
  df_atual <- filtrar_base(ano_filtro)
  df_ant   <- if (ano_filtro > min(dados$ano)) filtrar_base(ano_filtro - 1) else NULL
  
  extrair_totais <- function(df) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    tot_est <- sum(df$valor_etapa[df$rede == "Estadual"], na.rm = TRUE)
    tot_mun <- sum(df$valor_etapa[df$rede == "Municipal"], na.rm = TRUE)
    tot_fed <- sum(df$valor_etapa[df$rede == "Federal"], na.rm = TRUE)
    tot_priv <- sum(df$valor_etapa[df$rede == "Privada"], na.rm = TRUE)
    tot_pub  <- tot_est + tot_mun + tot_fed
    tot_geral <- tot_pub + tot_priv
    
    list(
      total = tot_geral,
      publico = tot_pub,
      privada = tot_priv,
      estadual = tot_est,
      municipal = tot_mun,
      federal = tot_fed
    )
  }
  
  atuais <- extrair_totais(df_atual)
  anteriores <- extrair_totais(df_ant)
  
  gerar_indicador <- function(chave) {
    val_atual <- atuais[[chave]]
    pct_participacao <- ifelse(atuais$total == 0, 0, val_atual / atuais$total)
    
    if (is.null(anteriores)) {
      return(list(
        valor = val_atual,
        participacao = pct_participacao,
        dif_abs = NA,
        dif_pct = NA,
        label_var = "Série inicial (2016)"
      ))
    }
    
    val_ant <- anteriores[[chave]]
    dif_abs <- val_atual - val_ant
    dif_pct <- ifelse(val_ant == 0, NA, (dif_abs / val_ant) * 100)
    
    sinal <- ifelse(dif_abs > 0, "+", "")
    label_var <- if (is.na(dif_pct)) {
      "Ano base sem dados anteriores"
    } else {
      sprintf("%s%s matrículas (%s%.1f%% vs %d)", 
              sinal, format(dif_abs, big.mark = "."), 
              sinal, dif_pct, ano_filtro - 1)
    }
    
    list(
      valor = val_atual,
      participacao = pct_participacao,
      dif_abs = dif_abs,
      dif_pct = dif_pct,
      label_var = label_var
    )
  }
  
  list(
    total     = gerar_indicador("total"),
    publico   = gerar_indicador("publico"),
    privada   = gerar_indicador("privada"),
    estadual  = gerar_indicador("estadual"),
    municipal = gerar_indicador("municipal"),
    federal   = gerar_indicador("federal")
  )
}