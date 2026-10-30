library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(tidyr)
library(ggplot2)
library(scales)
library(leaflet)
library(sf)
library(geobr)
library(shinychat)
library(ellmer)

# =============================================================================
# 1. CARREGAMENTO E TRATAMENTO DA BASE DE DADOS
# =============================================================================
caminho_arquivo <- "Matriculas_faixa_etaria_rede_municipio_consultas.xlsx"

dados_matriculas <- read_excel(caminho_arquivo, sheet = "Plan1") |> clean_names()
dados_matriculas$codigo_municipio <- as.numeric(dados_matriculas$codigo_municipio)

dados_populacao <- read_excel(caminho_arquivo, sheet = "População") |> clean_names()
dados_populacao$codigo_ibge <- as.numeric(dados_populacao$codigo_ibge)

dados_matriculas <- dados_matriculas |>
  left_join(dados_populacao |> select(codigo_ibge, populacao_2024), 
            by = c("codigo_municipio" = "codigo_ibge"))

lista_anos <- sort(unique(dados_matriculas$ano), decreasing = TRUE) 
lista_municipios <- sort(unique(dados_matriculas$municipio))
lista_nre <- sort(unique(na.omit(dados_matriculas$nome_nre)))
lista_ppa <- sort(unique(na.omit(dados_matriculas$regiao_ppa)))

# Carregamento da malha geográfica dos municípios do Paraná
mapa_pr <- read_municipality(code_muni = "PR", year = 2020, showProgress = FALSE) |>
  st_transform(4326)
mapa_pr$code_muni <- as.numeric(mapa_pr$code_muni)

# =============================================================================
# 2. INTERFACE DO USUÁRIO (UI)
# =============================================================================
ui <- page_navbar(
  title = "Dashboard Educacional - Paraná",
  theme = bs_theme(preset = "flatly"),
  
  sidebar = sidebar(
    title = "Filtros Globais",
    
    selectInput(
      inputId = "ano_selecionado", 
      label = "Escolha o Ano (exceto Histórico):", 
      choices = lista_anos,
      selected = max(lista_anos)
    ),
    
    selectInput(
      inputId = "regiao_ppa",
      label = "Região do PPA:",
      choices = c("Todas as Regiões" = "todas", lista_ppa),
      selected = "todas"
    ),
    
    selectInput(
      inputId = "nre_selecionado",
      label = "Filtrar por NRE:",
      choices = c("Todos os NREs" = "todos", lista_nre),
      selected = "todos"
    ),
    
    selectizeInput(
      inputId = "municipios_historico",
      label = "Comparar Municípios (no Histórico):",
      choices = lista_municipios,
      multiple = TRUE,
      selected = NULL,
      options = list(placeholder = 'Escolha um ou mais municípios...', plugins = list('remove_button'))
    ),
    
    hr(),
    
    selectInput(
      inputId = "etapa_ensino",
      label = "Por Etapa de Ensino:",
      choices = c(
        "Geral (Todas as Etapas)" = "geral", 
        "Creche" = "creche",
        "Pré-escola" = "pre_escola",
        "Anos Iniciais" = "ai", 
        "Anos Finais" = "af", 
        "Ensino Médio" = "em"
      ),
      selected = "geral"
    ),
    
    selectInput(
      inputId = "faixa_etaria",
      label = "Por Faixa Etária:",
      choices = c(
        "Geral (Todas as Idades)" = "geral",
        "0 a 3 anos" = "0_3",
        "4 a 5 anos" = "4_5",
        "6 a 10 anos" = "6_10",
        "11 a 14 anos" = "11_14",
        "15 a 17 anos" = "15_17",
        "18 anos ou mais" = "18_mais"
      ),
      selected = "geral"
    ),
    
    hr(),
    
    selectInput(
      inputId = "rede_selecionada",
      label = "Rede de Ensino:",
      choices = c(
        "Total Geral" = "total",
        "Rede Pública (Soma)" = "publico",
        "Estadual" = "estadual",
        "Municipal" = "municipal",
        "Federal" = "federal",
        "Privada" = "privada"
      ),
      selected = "total"
    ),
    
    radioButtons(
      inputId = "tipo_valor",
      label = "Formato de Exibição:",
      choices = c("Números Absolutos" = "absoluto", "Percentuais (%)" = "percentual"),
      selected = "absoluto"
    )
  ),
  
  # --- ABA 1: RANKING ---
  nav_panel(
    title = "Ranking de Municípios",
    card(
      full_screen = TRUE,
      card_header("Ranking de Municípios com Maiores Matrículas"),
      plotOutput("grafico_ranking", height = "550px")
    )
  ),
  
  # --- ABA 2: HISTÓRICO ---
  nav_panel(
    title = "Evolução Histórica",
    card(
      full_screen = TRUE,
      card_header("Comparativo Histórico de 2016 a 2025 (Estado x PPA x Regional x Municípios)"),
      plotOutput("grafico_historico", height = "550px")
    )
  ),
  
  # --- ABA 3: MAPA INTERATIVO ---
  nav_panel(
    title = "Mapa Interativo",
    card(
      full_screen = TRUE,
      card_header("Distribuição Geográfica no Estado (Passe o mouse)"),
      leafletOutput("mapa_interativo", height = "600px")
    )
  ),
  
  # --- ABA 4: TABELA DETALHADA ---
  nav_panel(
    title = "Tabela Detalhada",
    card(
      card_header("Detalhamento por Município e Rede de Ensino no Ano Selecionado"),
      DTOutput("tabela_dinamica")
    )
  ),
  
  # --- ABA 5: ASSISTENTE IA ---
  nav_panel(
    title = "Assistente IA",
    card(
      full_screen = TRUE,
      card_header("Converse com a Base de Dados (Perguntas, Rankings e Comparações)"),
      chat_ui("chat_educacao")
    )
  )
)

# =============================================================================
# 3. LÓGICA DO SERVIDOR (SERVER)
# =============================================================================
server <- function(input, output, session) {
  
  # Cascata de Seletores: PPA -> NRE
  observeEvent(input$regiao_ppa, {
    if (input$regiao_ppa == "todas") {
      nres_disponiveis <- lista_nre
    } else {
      nres_disponiveis <- sort(unique(na.omit(dados_matriculas$nome_nre[dados_matriculas$regiao_ppa == input$regiao_ppa])))
    }
    sel_atual <- if (input$nre_selecionado %in% nres_disponiveis) input$nre_selecionado else "todos"
    updateSelectInput(session, "nre_selecionado", choices = c("Todos os NREs" = "todos", nres_disponiveis), selected = sel_atual)
  }, ignoreInit = TRUE)
  
  # Cascata de Seletores: NRE/PPA -> Municípios do Histórico
  observeEvent(list(input$regiao_ppa, input$nre_selecionado), {
    df_muni_filt <- dados_matriculas
    if (input$regiao_ppa != "todas") df_muni_filt <- df_muni_filt |> filter(regiao_ppa == input$regiao_ppa)
    if (input$nre_selecionado != "todos") df_muni_filt <- df_muni_filt |> filter(nome_nre == input$nre_selecionado)
    
    munis_disp <- sort(unique(df_muni_filt$municipio))
    sel_existente <- intersect(input$municipios_historico, munis_disp)
    updateSelectizeInput(session, "municipios_historico", choices = munis_disp, selected = sel_existente, server = TRUE)
  }, ignoreInit = TRUE)
  
  # Alternância entre Etapa de Ensino e Faixa Etária
  observeEvent(input$etapa_ensino, {
    if (input$etapa_ensino != "geral") updateSelectInput(session, "faixa_etaria", selected = "geral")
  }, ignoreInit = TRUE)
  
  observeEvent(input$faixa_etaria, {
    if (input$faixa_etaria != "geral") updateSelectInput(session, "etapa_ensino", selected = "geral")
  }, ignoreInit = TRUE)
  
  # Base Reativa filtrada por Etapa ou Faixa Etária
  dados_reativos <- reactive({
    df <- dados_matriculas
    coluna_alvo <- "total_educacao_basica"
    
    if (input$faixa_etaria != "geral") {
      if (input$faixa_etaria == "0_3") coluna_alvo <- "matriculas_0_a_3_anos"
      else if (input$faixa_etaria == "4_5") coluna_alvo <- "matriculas_4_a_5_anos"
      else if (input$faixa_etaria == "6_10") coluna_alvo <- "matriculas_6_a_10_anos"
      else if (input$faixa_etaria == "11_14") coluna_alvo <- "matriculas_11_a_14_anos"
      else if (input$faixa_etaria == "15_17") coluna_alvo <- "matriculas_15_a_17_anos"
      else if (input$faixa_etaria == "18_mais") coluna_alvo <- "matriculas_18_anos_ou_mais"
    } else if (input$etapa_ensino != "geral") {
      if (input$etapa_ensino == "creche") coluna_alvo <- "matriculas_creche"
      else if (input$etapa_ensino == "pre_escola") coluna_alvo <- "matriculas_pre_escola"
      else if (input$etapa_ensino == "ai") coluna_alvo <- "matriculas_anos_iniciais"
      else if (input$etapa_ensino == "af") coluna_alvo <- "matriculas_anos_finais"
      else if (input$etapa_ensino == "em") coluna_alvo <- "matriculas_ensino_medio"
    }
    
    df$valor_etapa <- df[[coluna_alvo]]
    return(df)
  })
  
  # --- 1. RANKING DE MUNICÍPIOS ---
  output$grafico_ranking <- renderPlot({
    dados_ano <- dados_reativos() |> filter(ano == input$ano_selecionado)
    if (input$regiao_ppa != "todas") dados_ano <- dados_ano |> filter(regiao_ppa == input$regiao_ppa)
    if (input$nre_selecionado != "todos") dados_ano <- dados_ano |> filter(nome_nre == input$nre_selecionado)
    
    tabela_larga <- dados_ano |>
      select(codigo_municipio, municipio, rede, valor_etapa) |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |>
      clean_names()
    
    if(!"federal" %in% names(tabela_larga)) tabela_larga$federal <- 0
    if(!"municipal" %in% names(tabela_larga)) tabela_larga$municipal <- 0
    if(!"estadual" %in% names(tabela_larga)) tabela_larga$estadual <- 0
    if(!"privada" %in% names(tabela_larga)) tabela_larga$privada <- 0
    
    tabela_larga <- tabela_larga |> mutate(publico = estadual + municipal + federal, total = publico + privada)
    
    tabela_larga$valor_base <- case_when(
      input$rede_selecionada == "publico" ~ tabela_larga$publico,
      input$rede_selecionada == "estadual" ~ tabela_larga$estadual,
      input$rede_selecionada == "municipal" ~ tabela_larga$municipal,
      input$rede_selecionada == "federal" ~ tabela_larga$federal,
      input$rede_selecionada == "privada" ~ tabela_larga$privada,
      TRUE ~ tabela_larga$total
    )
    
    tabela_larga$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
      ifelse(tabela_larga$total == 0, 0, tabela_larga$valor_base / tabela_larga$total)
    } else {
      as.numeric(tabela_larga$valor_base)
    }
    
    qtd_linhas <- ifelse(input$regiao_ppa == "todas" && input$nre_selecionado == "todos", 15, 20)
    metricas <- tabela_larga |> select(municipio, valor_final) |> arrange(desc(valor_final)) |> head(qtd_linhas)
    metricas$municipio <- factor(metricas$municipio, levels = rev(metricas$municipio))
    
    titulo_eixo <- ifelse(input$tipo_valor == "percentual",
                          paste("Proporção de", tools::toTitleCase(input$rede_selecionada)),
                          paste("Total de Matrículas (", tools::toTitleCase(input$rede_selecionada), ")"))
    
    if (input$tipo_valor == "percentual") {
      ggplot(metricas, aes(x = municipio, y = valor_final, fill = valor_final)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
        scale_fill_gradient(low = "#fdba74", high = "#c2410c") +
        theme_minimal(base_size = 14) +
        labs(x = "", y = titulo_eixo) +
        theme(panel.grid.major.y = element_blank())
    } else {
      ggplot(metricas, aes(x = municipio, y = valor_final, fill = valor_final)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
        scale_fill_gradient(low = "#fed7aa", high = "#9a3412") +
        theme_minimal(base_size = 14) +
        labs(x = "", y = titulo_eixo) +
        theme(panel.grid.major.y = element_blank())
    }
  })
  
  # --- 2. EVOLUÇÃO HISTÓRICA ---
  output$grafico_historico <- renderPlot({
    df_todos <- dados_reativos()
    
    resumo_estado <- df_todos |>
      group_by(ano, rede) |>
      summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |>
      clean_names()
    
    if(!"federal" %in% names(resumo_estado)) resumo_estado$federal <- 0
    if(!"municipal" %in% names(resumo_estado)) resumo_estado$municipal <- 0
    if(!"estadual" %in% names(resumo_estado)) resumo_estado$estadual <- 0
    if(!"privada" %in% names(resumo_estado)) resumo_estado$privada <- 0
    
    resumo_estado <- resumo_estado |> mutate(publico = estadual + municipal + federal, total = publico + privada)
    resumo_estado$valor_base <- case_when(
      input$rede_selecionada == "publico" ~ resumo_estado$publico,
      input$rede_selecionada == "estadual" ~ resumo_estado$estadual,
      input$rede_selecionada == "municipal" ~ resumo_estado$municipal,
      input$rede_selecionada == "federal" ~ resumo_estado$federal,
      input$rede_selecionada == "privada" ~ resumo_estado$privada,
      TRUE ~ resumo_estado$total
    )
    resumo_estado$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
      ifelse(resumo_estado$total == 0, 0, resumo_estado$valor_base / resumo_estado$total)
    } else {
      as.numeric(resumo_estado$valor_base)
    }
    
    df_plot <- resumo_estado |> mutate(local = "PARANÁ (Total Estadual)") |> select(ano, local, valor_final)
    
    if (input$regiao_ppa != "todas") {
      resumo_ppa <- df_todos |> filter(regiao_ppa == input$regiao_ppa) |>
        group_by(ano, rede) |> summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names()
      if(!"federal" %in% names(resumo_ppa)) resumo_ppa$federal <- 0
      if(!"municipal" %in% names(resumo_ppa)) resumo_ppa$municipal <- 0
      if(!"estadual" %in% names(resumo_ppa)) resumo_ppa$estadual <- 0
      if(!"privada" %in% names(resumo_ppa)) resumo_ppa$privada <- 0
      resumo_ppa <- resumo_ppa |> mutate(publico = estadual + municipal + federal, total = publico + privada)
      resumo_ppa$valor_base <- case_when(
        input$rede_selecionada == "publico" ~ resumo_ppa$publico,
        input$rede_selecionada == "estadual" ~ resumo_ppa$estadual,
        input$rede_selecionada == "municipal" ~ resumo_ppa$municipal,
        input$rede_selecionada == "federal" ~ resumo_ppa$federal,
        input$rede_selecionada == "privada" ~ resumo_ppa$privada,
        TRUE ~ resumo_ppa$total
      )
      resumo_ppa$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
        ifelse(resumo_ppa$total == 0, 0, resumo_ppa$valor_base / resumo_ppa$total)
      } else as.numeric(resumo_ppa$valor_base)
      
      df_plot <- bind_rows(df_plot, resumo_ppa |> mutate(local = paste0("Região PPA ", input$regiao_ppa)) |> select(ano, local, valor_final))
    }
    
    if (input$nre_selecionado != "todos") {
      resumo_nre <- df_todos |> filter(nome_nre == input$nre_selecionado) |>
        group_by(ano, rede) |> summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names()
      if(!"federal" %in% names(resumo_nre)) resumo_nre$federal <- 0
      if(!"municipal" %in% names(resumo_nre)) resumo_nre$municipal <- 0
      if(!"estadual" %in% names(resumo_nre)) resumo_nre$estadual <- 0
      if(!"privada" %in% names(resumo_nre)) resumo_nre$privada <- 0
      resumo_nre <- resumo_nre |> mutate(publico = estadual + municipal + federal, total = publico + privada)
      resumo_nre$valor_base <- case_when(
        input$rede_selecionada == "publico" ~ resumo_nre$publico,
        input$rede_selecionada == "estadual" ~ resumo_nre$estadual,
        input$rede_selecionada == "municipal" ~ resumo_nre$municipal,
        input$rede_selecionada == "federal" ~ resumo_nre$federal,
        input$rede_selecionada == "privada" ~ resumo_nre$privada,
        TRUE ~ resumo_nre$total
      )
      resumo_nre$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
        ifelse(resumo_nre$total == 0, 0, resumo_nre$valor_base / resumo_nre$total)
      } else as.numeric(resumo_nre$valor_base)
      
      df_plot <- bind_rows(df_plot, resumo_nre |> mutate(local = paste0("NRE ", input$nre_selecionado)) |> select(ano, local, valor_final))
    }
    
    if (!is.null(input$municipios_historico) && length(input$municipios_historico) > 0) {
      df_muni <- df_todos |> filter(municipio %in% input$municipios_historico) |>
        group_by(ano, municipio, rede) |> summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names()
      if(!"federal" %in% names(df_muni)) df_muni$federal <- 0
      if(!"municipal" %in% names(df_muni)) df_muni$municipal <- 0
      if(!"estadual" %in% names(df_muni)) df_muni$estadual <- 0
      if(!"privada" %in% names(df_muni)) df_muni$privada <- 0
      df_muni <- df_muni |> mutate(publico = estadual + municipal + federal, total = publico + privada)
      df_muni$valor_base <- case_when(
        input$rede_selecionada == "publico" ~ df_muni$publico,
        input$rede_selecionada == "estadual" ~ df_muni$estadual,
        input$rede_selecionada == "municipal" ~ df_muni$municipal,
        input$rede_selecionada == "federal" ~ df_muni$federal,
        input$rede_selecionada == "privada" ~ df_muni$privada,
        TRUE ~ df_muni$total
      )
      df_muni$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
        ifelse(df_muni$total == 0, 0, df_muni$valor_base / df_muni$total)
      } else as.numeric(df_muni$valor_base)
      
      df_plot <- bind_rows(df_plot, df_muni |> rename(local = municipio) |> select(ano, local, valor_final))
    }
    
    locais_unicos <- unique(df_plot$local)
    ordem_referencias <- c("PARANÁ (Total Estadual)", grep("^Região PPA", locais_unicos, value = TRUE), grep("^NRE ", locais_unicos, value = TRUE))
    locais_ordenados <- c(ordem_referencias, setdiff(locais_unicos, ordem_referencias))
    df_plot$local <- factor(df_plot$local, levels = locais_ordenados)
    
    cores_base <- c("#0F172A", "#0D9488", "#2563EB", "#DC2626", "#059669", "#D97706", "#7C3AED", "#DB2777")
    cores_finais <- setNames(rep(cores_base, length.out = length(locais_ordenados)), locais_ordenados)
    
    max_y <- max(df_plot$valor_final, na.rm = TRUE)
    if (is.na(max_y) || max_y == 0) max_y <- 1
    
    if (input$tipo_valor == "percentual") {
      lim_y <- min(1.0, max_y * 1.15)
      ggplot(df_plot, aes(x = ano, y = valor_final, color = local, group = local)) +
        geom_line(linewidth = 1.3) +
        geom_point(size = 3.5) +
        geom_text(aes(label = percent(valor_final, accuracy = 0.1)), vjust = -1.2, size = 4, fontface = "bold", show.legend = FALSE) +
        scale_color_manual(values = cores_finais) +
        scale_y_continuous(labels = percent_format(), limits = c(0, lim_y)) +
        scale_x_continuous(breaks = unique(df_plot$ano)) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = 14) +
        labs(x = "Ano", y = paste("Proporção (", tools::toTitleCase(input$rede_selecionada), ")"), color = "Recorte") +
        theme(legend.position = "top", panel.grid.minor.x = element_blank(), legend.title = element_text(face = "bold"),
              legend.text = element_text(size = 11, face = "bold"), plot.margin = margin(t = 20, r = 20, b = 10, l = 10))
    } else {
      ggplot(df_plot, aes(x = ano, y = valor_final, color = local, group = local)) +
        geom_line(linewidth = 1.3) +
        geom_point(size = 3.5) +
        geom_text(aes(label = comma(valor_final, big.mark = ".", decimal.mark = ",")), vjust = -1.5, size = 4, fontface = "bold", show.legend = FALSE) +
        scale_color_manual(values = cores_finais) +
        scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","), limits = c(0, max_y * 1.25)) +
        scale_x_continuous(breaks = unique(df_plot$ano)) +
        coord_cartesian(clip = "off") +
        theme_minimal(base_size = 14) +
        labs(x = "Ano", y = paste("Total de Matrículas (", tools::toTitleCase(input$rede_selecionada), ")"), color = "Recorte") +
        theme(legend.position = "top", panel.grid.minor.x = element_blank(), legend.title = element_text(face = "bold"),
              legend.text = element_text(size = 11, face = "bold"), plot.margin = margin(t = 20, r = 20, b = 10, l = 10))
    }
  })
  
  # --- 3. MAPA INTERATIVO (ESRI GRAY CANVAS) ---
  output$mapa_interativo <- renderLeaflet({
    dados_ano <- dados_reativos() |> filter(ano == input$ano_selecionado)
    if (input$regiao_ppa != "todas") dados_ano <- dados_ano |> filter(regiao_ppa == input$regiao_ppa)
    if (input$nre_selecionado != "todos") dados_ano <- dados_ano |> filter(nome_nre == input$nre_selecionado)
    
    tabela_larga_mapa <- dados_ano |> 
      select(codigo_municipio, municipio, rede, valor_etapa) |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> 
      clean_names()
    
    if(!"federal" %in% names(tabela_larga_mapa)) tabela_larga_mapa$federal <- 0
    if(!"municipal" %in% names(tabela_larga_mapa)) tabela_larga_mapa$municipal <- 0
    if(!"estadual" %in% names(tabela_larga_mapa)) tabela_larga_mapa$estadual <- 0
    if(!"privada" %in% names(tabela_larga_mapa)) tabela_larga_mapa$privada <- 0
    
    tabela_larga_mapa <- tabela_larga_mapa |> mutate(publico = estadual + municipal + federal, total = publico + privada)
    tabela_larga_mapa$valor_base <- case_when(
      input$rede_selecionada == "publico" ~ tabela_larga_mapa$publico,
      input$rede_selecionada == "estadual" ~ tabela_larga_mapa$estadual,
      input$rede_selecionada == "municipal" ~ tabela_larga_mapa$municipal,
      input$rede_selecionada == "federal" ~ tabela_larga_mapa$federal,
      input$rede_selecionada == "privada" ~ tabela_larga_mapa$privada,
      TRUE ~ tabela_larga_mapa$total
    )
    tabela_larga_mapa$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
      ifelse(tabela_larga_mapa$total == 0, 0, tabela_larga_mapa$valor_base / tabela_larga_mapa$total)
    } else as.numeric(tabela_larga_mapa$valor_base)
    
    metricas <- tabela_larga_mapa |> select(codigo_municipio, municipio, valor_final)
    
    if (input$regiao_ppa != "todas" || input$nre_selecionado != "todos") {
      mapa_pr_dados <- mapa_pr |> inner_join(metricas, by = c("code_muni" = "codigo_municipio"))
    } else {
      mapa_pr_dados <- mapa_pr |> left_join(metricas, by = c("code_muni" = "codigo_municipio"))
      mapa_pr_dados$valor_final[is.na(mapa_pr_dados$valor_final)] <- 0
    }
    
    if (input$tipo_valor == "percentual") {
      pal <- colorNumeric("YlOrRd", domain = c(0, 1), na.color = "transparent")
      popup_texto <- sprintf("<strong>%s</strong><br/>%s: <strong>%.1f%%</strong>", mapa_pr_dados$name_muni, tools::toTitleCase(input$rede_selecionada), mapa_pr_dados$valor_final * 100)
      titulo_legenda <- paste("<strong>% de", tools::toTitleCase(input$rede_selecionada), "</strong>")
      
      leaflet(mapa_pr_dados) |>
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Cinza Minimalista (Padrão)") |>
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") |>
        addTiles(group = "OpenStreetMap (Ruas)") |>
        addPolygons(fillColor = ~pal(valor_final), weight = 1, opacity = 1, color = "#555555", fillOpacity = 0.85,
                    highlightOptions = highlightOptions(weight = 3, color = "#2563eb", fillOpacity = 1, bringToFront = TRUE),
                    label = lapply(popup_texto, htmltools::HTML),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) |>
        addLegend(pal = pal, values = c(0, 1), opacity = 0.85, title = titulo_legenda, position = "bottomright", labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)) |>
        addLayersControl(baseGroups = c("Cinza Minimalista (Padrão)", "Topográfico", "OpenStreetMap (Ruas)"), options = layersControlOptions(collapsed = TRUE))
    } else {
      d_min <- min(mapa_pr_dados$valor_final, na.rm = TRUE)
      d_max <- max(mapa_pr_dados$valor_final, na.rm = TRUE)
      if (d_min == d_max) d_max <- d_min + 1
      pal <- colorNumeric("Oranges", domain = c(d_min, d_max), na.color = "transparent")
      popup_texto <- sprintf("<strong>%s</strong><br/>%s: <strong>%s</strong> matrículas", mapa_pr_dados$name_muni, tools::toTitleCase(input$rede_selecionada), format(mapa_pr_dados$valor_final, big.mark = ".", scientific = FALSE))
      titulo_legenda <- paste("<strong>Total:", tools::toTitleCase(input$rede_selecionada), "</strong>")
      
      leaflet(mapa_pr_dados) |>
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Cinza Minimalista (Padrão)") |>
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") |>
        addTiles(group = "OpenStreetMap (Ruas)") |>
        addPolygons(fillColor = ~pal(valor_final), weight = 1, opacity = 1, color = "#555555", fillOpacity = 0.85,
                    highlightOptions = highlightOptions(weight = 3, color = "#2563eb", fillOpacity = 1, bringToFront = TRUE),
                    label = lapply(popup_texto, htmltools::HTML),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) |>
        addLegend(pal = pal, values = c(d_min, d_max), opacity = 0.85, title = titulo_legenda, position = "bottomright", labFormat = labelFormat(big.mark = ".")) |>
        addLayersControl(baseGroups = c("Cinza Minimalista (Padrão)", "Topográfico", "OpenStreetMap (Ruas)"), options = layersControlOptions(collapsed = TRUE))
    }
  })
  
  # --- 4. TABELA DETALHADA ---
  output$tabela_dinamica <- renderDT({
    dados_ano <- dados_reativos() |> filter(ano == input$ano_selecionado)
    if (input$regiao_ppa != "todas") dados_ano <- dados_ano |> filter(regiao_ppa == input$regiao_ppa)
    if (input$nre_selecionado != "todos") dados_ano <- dados_ano |> filter(nome_nre == input$nre_selecionado)
    
    tabela_larga <- dados_ano |> select(municipio, rede, valor_etapa, populacao_2024) |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names() 
    
    if(!"federal" %in% names(tabela_larga)) tabela_larga$federal <- 0
    if(!"municipal" %in% names(tabela_larga)) tabela_larga$municipal <- 0
    if(!"estadual" %in% names(tabela_larga)) tabela_larga$estadual <- 0
    if(!"privada" %in% names(tabela_larga)) tabela_larga$privada <- 0
    
    tabela_municipios <- tabela_larga |>
      mutate(publico = estadual + municipal + federal, total = publico + privada) |>
      select(municipio, estadual, municipal, federal, publico, privada, total, populacao_2024) |>
      arrange(desc(total))
    
    val_est <- sum(tabela_municipios$estadual, na.rm = TRUE)
    val_mun <- sum(tabela_municipios$municipal, na.rm = TRUE)
    val_fed <- sum(tabela_municipios$federal, na.rm = TRUE)
    val_pub <- sum(tabela_municipios$publico, na.rm = TRUE)
    val_priv <- sum(tabela_municipios$privada, na.rm = TRUE)
    val_tot <- sum(tabela_municipios$total, na.rm = TRUE)
    val_pop <- sum(tabela_municipios$populacao_2024, na.rm = TRUE)
    
    nome_linha_fixa <- if (input$nre_selecionado != "todos") {
      paste0("NRE ", input$nre_selecionado, " (Total Regional)")
    } else if (input$regiao_ppa != "todas") {
      paste0("Região PPA ", input$regiao_ppa, " (Total Macrorregional)")
    } else {
      "PARANÁ (Total Estadual)"
    }
    
    if (input$tipo_valor == "percentual") {
      p_est <- ifelse(val_tot == 0, "0,0%", percent(val_est / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_mun <- ifelse(val_tot == 0, "0,0%", percent(val_mun / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_fed <- ifelse(val_tot == 0, "0,0%", percent(val_fed / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_pub <- ifelse(val_tot == 0, "0,0%", percent(val_pub / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_priv <- ifelse(val_tot == 0, "0,0%", percent(val_priv / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_tot <- "100,0%"
      p_pop <- comma(val_pop, big.mark = ".", decimal.mark = ",")
      
      tabela_municipios <- tabela_municipios |> mutate(
        estadual = ifelse(total == 0, 0, estadual / total),
        municipal = ifelse(total == 0, 0, municipal / total),
        federal = ifelse(total == 0, 0, federal / total),
        publico = ifelse(total == 0, 0, publico / total),
        privada = ifelse(total == 0, 0, privada / total)
      )
    } else {
      p_est <- comma(val_est, big.mark = ".", decimal.mark = ",")
      p_mun <- comma(val_mun, big.mark = ".", decimal.mark = ",")
      p_fed <- comma(val_fed, big.mark = ".", decimal.mark = ",")
      p_pub <- comma(val_pub, big.mark = ".", decimal.mark = ",")
      p_priv <- comma(val_priv, big.mark = ".", decimal.mark = ",")
      p_tot <- comma(val_tot, big.mark = ".", decimal.mark = ",")
      p_pop <- comma(val_pop, big.mark = ".", decimal.mark = ",")
    }
    
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(th("Município"), th("Estadual"), th("Municipal"), th("Federal"), th("Público"), th("Privado"), th("Total Absoluto"), th("População (2024)")),
        tr(
          style = "background-color: #e2e8f0; font-weight: bold; color: #0f172a; font-size: 14px;",
          th(style = "text-align: left; padding: 8px 10px;", nome_linha_fixa),
          th(style = "text-align: right; padding: 8px 10px;", p_est),
          th(style = "text-align: right; padding: 8px 10px;", p_mun),
          th(style = "text-align: right; padding: 8px 10px;", p_fed),
          th(style = "text-align: right; padding: 8px 10px;", p_pub),
          th(style = "text-align: right; padding: 8px 10px;", p_priv),
          th(style = "text-align: right; padding: 8px 10px;", p_tot),
          th(style = "text-align: right; padding: 8px 10px;", p_pop)
        )
      )
    ))
    
    tabela_dt <- datatable(tabela_municipios, container = sketch, rownames = FALSE,
                           options = list(pageLength = 10, orderCellsTop = TRUE, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')))
    
    if (input$tipo_valor == "percentual") {
      tabela_dt <- tabela_dt |> formatPercentage(c("estadual", "municipal", "federal", "publico", "privada"), 1) 
    } else {
      tabela_dt <- tabela_dt |> formatCurrency(c("estadual", "municipal", "federal", "publico", "privada", "total", "populacao_2024"), currency = "", interval = 3, mark = ".", digits = 0)
    }
    tabela_dt 
  })
  
  # =========================================================================
  # --- 5. MOTOR DO CHATBOT COM GOOGLE GEMINI (ELLMER + SHINYCHAT) ---
  # =========================================================================
  if (file.exists(".Renviron")) {
    readRenviron(".Renviron")
  }
  
  chave_gemini <- Sys.getenv("GEMINI_API_KEY")
  chave_gemini <- trimws(gsub('^["\']|["\']$', '', chave_gemini))
  
  erro_detalhado_ia <- NULL
  
  # Função auxiliar para extrair a string pura vinda do shinychat
  extrair_texto_chat <- function(msg) {
    if (is.character(msg)) return(msg)
    if (is.list(msg)) {
      if (!is.null(msg$text) && is.character(msg$text)) return(msg$text)
      if (!is.null(msg$value) && is.character(msg$value)) return(msg$value)
      if (length(msg) > 0 && is.character(msg[[1]])) return(msg[[1]])
    }
    return(as.character(msg))
  }
  
  chat_bot <- tryCatch({
    if (!nzchar(chave_gemini)) {
      stop("A variável GEMINI_API_KEY está vazia no arquivo .Renviron.")
    }
    
    bot <- ellmer::chat_google_gemini(
      model = "gemini-3.7-flash",
      api_key = chave_gemini,
      system_prompt = "Você é um especialista analítico em dados educacionais do Estado do Paraná.
      Responda dúvidas sobre o Censo Escolar (2016 a 2025).
      Sempre utilize as ferramentas disponíveis para obter os números exatos.
      Formate os números no padrão brasileiro: pontos nos milhares (ex: 15.420) e vírgula nos decimais (ex: 24,5%).
      Seja conciso, direto e analítico."
    )
    
    # Ferramenta 1: Ranking de Municípios
    bot$register_tool(
      tool(
        fun = function(ano = 2025, rede = "Privada", tipo = "percentual", filtro_nre = "todos", filtro_ppa = "todas", top_n = 5) {
          ano <- as.numeric(ano)
          top_n <- as.numeric(top_n)
          
          df <- dados_matriculas |> filter(ano == !!ano)
          
          if (tolower(filtro_nre) != "todos") {
            df <- df |> filter(tolower(nome_nre) == tolower(filtro_nre))
          }
          if (tolower(filtro_ppa) != "todas") {
            df <- df |> filter(tolower(regiao_ppa) == tolower(filtro_ppa))
          }
          
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
          
          resultado <- tabela |>
            select(municipio, total_matriculas = total, valor_alvo = metrica) |>
            arrange(desc(valor_alvo)) |>
            head(top_n)
          
          return(resultado)
        },
        description = "Consulta o ranking dos municípios com maiores matrículas ou proporção percentual em determinada rede de ensino.",
        arguments = list(
          ano = ellmer::type_integer("Ano de análise (2016 a 2025)"),
          rede = ellmer::type_string("Rede de ensino: 'Privada', 'Estadual', 'Municipal', 'Federal' ou 'Total'"),
          tipo = ellmer::type_string("Formato: 'percentual' para taxas ou 'absoluto' para quantidade"),
          filtro_nre = ellmer::type_string("Nome do NRE ou 'todos'"),
          filtro_ppa = ellmer::type_string("Nome da região PPA ou 'todas'"),
          top_n = ellmer::type_integer("Quantidade de municípios no ranking")
        )
      )
    )
    
    # Ferramenta 2: Totais Agregados
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
          return(resumo)
        },
        description = "Retorna os totais e percentuais de matrículas por rede para o Estado, Região PPA, NRE ou Município.",
        arguments = list(
          ano = ellmer::type_integer("Ano de análise (2016 a 2025)"),
          nivel = ellmer::type_string("Nível territorial: 'estado', 'ppa', 'nre' ou 'municipio'"),
          nome_localidade = ellmer::type_string("Nome da localidade (NRE, PPA ou Município), ou vazio se nível for estado")
        )
      )
    )
    
    bot
  }, error = function(e) {
    erro_detalhado_ia <<- e$message
    message(">>> ERRO IA NA INICIALIZACAO: ", e$message)
    NULL
  })
  
  # Resposta reativa no chat com streaming e blindagem contra travamentos
  observeEvent(input$chat_educacao_user_input, {
    if (is.null(chat_bot)) {
      chat_append(
        "chat_educacao", 
        paste0("⚠️ **Falha ao inicializar o Assistente IA:**\n\n`", erro_detalhado_ia, "`\n\n*Verifique se a variável GEMINI_API_KEY está configurada no seu arquivo .Renviron.*")
      )
      return()
    }
    
    texto_usuario <- extrair_texto_chat(input$chat_educacao_user_input)
    
    if (!nzchar(trimws(texto_usuario))) return()
    
    tryCatch({
      stream <- chat_bot$stream_async(texto_usuario)
      chat_append("chat_educacao", stream)
    }, error = function(e) {
      tryCatch({
        resposta_direta <- chat_bot$chat(texto_usuario)
        chat_append("chat_educacao", resposta_direta)
      }, error = function(e2) {
        chat_append("chat_educacao", paste0("⚠️ **Erro ao consultar os dados:**\n\n`", e2$message, "`"))
      })
    })
  })
}

# =============================================================================
# 4. INICIALIZAÇÃO DA APLICAÇÃO
# =============================================================================
shinyApp(ui, server)