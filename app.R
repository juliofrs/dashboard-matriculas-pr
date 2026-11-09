# =============================================================================
# PROJETO: PLATAFORMA DE INTELIGÊNCIA EDUCACIONAL DO PARANÁ
# CÓDIGO PRINCIPAL: app.R
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(readxl)
  library(dplyr)
  library(janitor)
  library(DT)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  library(scales)
  library(leaflet)
  library(sf)
  library(shinychat)
  library(ellmer)
})

# Carregamento modular resiliente
if (dir.exists("R")) {
  source("R/funcoes_calculo.R")
  source("R/componentes_ui.R")
  source("R/modulo_ia.R")
}

# =============================================================================
# 1. CARREGAMENTO E PREPARAÇÃO DOS DADOS
# =============================================================================
caminho_base <- "Matriculas_faixa_etaria_rede_municipio_consultas.xlsx"
dados_matriculas <- read_excel(caminho_base, sheet = "Plan1") |> clean_names()
dados_matriculas$codigo_municipio <- as.numeric(dados_matriculas$codigo_municipio)

dados_populacao <- read_excel(caminho_base, sheet = "População") |> clean_names()
dados_populacao$codigo_ibge <- as.numeric(dados_populacao$codigo_ibge)

dados_matriculas <- dados_matriculas |>
  left_join(dados_populacao |> select(codigo_ibge, populacao_2024), 
            by = c("codigo_municipio" = "codigo_ibge"))

lista_anos       <- sort(unique(dados_matriculas$ano), decreasing = TRUE) 
lista_municipios <- sort(unique(dados_matriculas$municipio))
lista_nre        <- sort(unique(na.omit(dados_matriculas$nome_nre)))
lista_ppa        <- sort(unique(na.omit(dados_matriculas$regiao_ppa)))

# Carregamento do mapa a partir da cache rápida ou geobr como fallback
ficheiro_mapa_rds <- "mapa_pr_2020.rds"
if (file.exists(ficheiro_mapa_rds)) {
  mapa_pr <- readRDS(ficheiro_mapa_rds)
} else {
  mapa_pr <- geobr::read_municipality(code_muni = "PR", year = 2020, showProgress = FALSE) |>
    sf::st_transform(4326)
  mapa_pr$code_muni <- as.numeric(mapa_pr$code_muni)
}

# =============================================================================
# 2. INTERFACE DO UTILIZADOR (UI)
# =============================================================================
ui <- page_navbar(
  title = "Educação Básica em Dados | Paraná",
  theme = bs_theme(
    version = 5,
    preset = "flatly",
    primary = "#14532D",
    base_font = font_google("Inter")
  ),
  header = estilos_customizados(),
  
  sidebar = sidebar(
    title = span(style = "font-weight: 700; font-size: 0.95rem;", "Filtros Analíticos"),
    width = 310,
    
    div(
      actionButton("limpar_filtros", "↺ Restaurar Filtros Padrão", class = "btn btn-limpar-filtros mb-3")
    ),
    
    div(class = "sidebar-section-title", "Período & Território"),
    selectInput("ano_selecionado", "Ano de Referência:", choices = lista_anos, selected = max(lista_anos)),
    selectInput("regiao_ppa", "Macrorregião PPA:", choices = c("Todas as Regiões" = "todas", lista_ppa), selected = "todas"),
    selectInput("nre_selecionado", "Núcleo Regional (NRE):", choices = c("Todos os NREs" = "todos", lista_nre), selected = "todos"),
    selectizeInput(
      "municipios_historico", "Municípios (Histórico):",
      choices = lista_municipios, multiple = TRUE, selected = NULL,
      options = list(placeholder = 'Adicionar municípios ao histórico...', plugins = list('remove_button'))
    ),
    
    div(class = "sidebar-section-title", "Recorte Educacional"),
    selectInput("etapa_ensino", "Etapa de Ensino:", choices = c(
      "Geral (Todas as Etapas)" = "geral", 
      "Creche" = "creche",
      "Pré-escola" = "pre_escola",
      "Anos Iniciais" = "ai", 
      "Anos Finais" = "af", 
      "Ensino Médio" = "em"
    ), selected = "geral"),
    
    selectInput("faixa_etaria", "Faixa Etária:", choices = c(
      "Geral (Todas as Idades)" = "geral",
      "0 a 3 anos" = "0_3",
      "4 a 5 anos" = "4_5",
      "6 a 10 anos" = "6_10",
      "11 a 14 anos" = "11_14",
      "15 a 17 anos" = "15_17",
      "18 anos ou mais" = "18_mais"
    ), selected = "geral"),
    
    div(class = "sidebar-section-title", "Rede & Métrica"),
    selectInput("rede_selecionada", "Rede de Ensino:", choices = c(
      "Total Geral" = "total",
      "Rede Pública (Soma)" = "publico",
      "Estadual" = "estadual",
      "Municipal" = "municipal",
      "Federal" = "federal",
      "Privada" = "privada"
    ), selected = "total"),
    
    radioButtons("tipo_valor", "Formato de Exibição:", choices = c(
      "Números Absolutos" = "absoluto", 
      "Percentuais (%)" = "percentual"
    ), selected = "absoluto")
  ),
  
  # --- ABA 1: VISÃO GERAL (NOVO COCKPIT EXECUTIVO) ---
  nav_panel(
    title = "Visão Geral",
    div(
      class = "mb-3 p-3 bg-white border rounded",
      style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 10px;",
      div(
        h5(style = "margin: 0; font-weight: 700; color: var(--pr-green-dark);", "Painel Executivo de Indicadores Educacionais"),
        span(style = "color: var(--text-secondary); font-size: 0.85rem;", 
             "Acompanhamento da Educação Básica no Paraná com base nos microdados do Censo Escolar (2016 a 2025).")
      ),
      div(
        uiOutput("badge_contexto_ativo")
      )
    ),
    
    # Linha de 6 Cartões de KPI Dinâmicos
    layout_column_wrap(
      width = 1/6,
      kpi_card_ui("kpi_total", "Total de Matrículas", "#14532D", "Volume consolidado de matrículas no recorte selecionado."),
      kpi_card_ui("kpi_publico", "Rede Pública", "#228B52", "Soma das redes Estadual, Municipal e Federal."),
      kpi_card_ui("kpi_privada", "Rede Privada", "#D97706", "Total de matrículas da rede particular no recorte."),
      kpi_card_ui("kpi_estadual", "Rede Estadual", "#0284C7", "Matrículas geridas pelo Estado do Paraná."),
      kpi_card_ui("kpi_municipal", "Rede Municipal", "#0D9488", "Matrículas geridas pelas redes municipais."),
      kpi_card_ui("kpi_federal", "Rede Federal", "#6366F1", "Institutos e Colégios de aplicação federais.")
    ),
    
    # Grelha Analítica Executiva (2x2)
    layout_column_wrap(
      width = 1/2,
      card(
        card_header("Evolução Histórica do Recorte (2016-2025)"),
        plotlyOutput("grafico_vg_historico", height = "320px")
      ),
      card(
        card_header("Distribuição Proporcional por Rede de Ensino"),
        plotlyOutput("grafico_vg_redes", height = "320px")
      ),
      card(
        card_header("Top 10 Municípios no Recorte Selecionado"),
        plotlyOutput("grafico_vg_top", height = "330px")
      ),
      card(
        card_header("Síntese Cartográfica Estadual"),
        leafletOutput("mapa_vg_resumo", height = "330px")
      )
    )
  ),
  
  # --- ABA 2: RANKING DE MUNICÍPIOS ---
  nav_panel(
    title = "Ranking de Municípios",
    card(
      full_screen = TRUE,
      card_header("Ranking Municipal Interativo (Top 15 Estadual ou Top 20 Regional)"),
      plotlyOutput("grafico_ranking", height = "560px")
    )
  ),
  
  # --- ABA 3: EVOLUÇÃO HISTÓRICA ---
  nav_panel(
    title = "Evolução Histórica",
    card(
      full_screen = TRUE,
      card_header("Série Histórica Multirrecorte (Paraná x Região PPA x NRE x Municípios)"),
      plotlyOutput("grafico_historico", height = "560px")
    )
  ),
  
  # --- ABA 4: MAPA INTERATIVO ---
  nav_panel(
    title = "Mapa Interativo",
    card(
      full_screen = TRUE,
      card_header("Distribuição Espacial das Matrículas no Território Paranaense"),
      leafletOutput("mapa_interativo", height = "600px")
    )
  ),
  
  # --- ABA 5: TABELA DETALHADA ---
  nav_panel(
    title = "Tabela Detalhada",
    card(
      card_header("Detalhamento por Município, Redes de Ensino e População IBGE"),
      DTOutput("tabela_dinamica")
    )
  ),
  
  # --- ABA 6: ASSISTENTE IA ---
  nav_panel(
    title = "Assistente IA",
    layout_sidebar(
      sidebar = sidebar(
        title = "Sugestões de Perguntas",
        width = 300,
        p(style = "font-size: 0.85rem; color: var(--text-secondary);", 
          "Selecione uma pergunta frequente para carregar no assistente:"),
        actionLink("sug_1", "📊 5 maiores municípios em rede privada (2025)", class = "d-block mb-2 text-decoration-none"),
        actionLink("sug_2", "🏛️ Totais de matrículas do NRE Maringá", class = "d-block mb-2 text-decoration-none"),
        actionLink("sug_3", "📈 Comparativo da rede pública vs privada em Londrina", class = "d-block mb-2 text-decoration-none"),
        actionLink("sug_4", "📍 Top 5 municípios da Região PPA Cascavel", class = "d-block mb-2 text-decoration-none"),
        hr(),
        p(style = "font-size: 0.78rem; color: #94A3B8;", 
          "O assistente consulta diretamente a base de dados via ferramentas analíticas em R.")
      ),
      card(
        full_screen = TRUE,
        card_header("Consulta Analítica à Base de Dados com Google Gemini"),
        chat_ui("chat_educacao")
      )
    )
  )
)

# =============================================================================
# 3. SERVIDOR (SERVER)
# =============================================================================
server <- function(input, output, session) {
  
  # Botão Restaurar Filtros
  observeEvent(input$limpar_filtros, {
    updateSelectInput(session, "ano_selecionado", selected = max(lista_anos))
    updateSelectInput(session, "regiao_ppa", selected = "todas")
    updateSelectInput(session, "nre_selecionado", selected = "todos")
    updateSelectizeInput(session, "municipios_historico", selected = character(0))
    updateSelectInput(session, "etapa_ensino", selected = "geral")
    updateSelectInput(session, "faixa_etaria", selected = "geral")
    updateSelectInput(session, "rede_selecionada", selected = "total")
    updateRadioButtons(session, "tipo_valor", selected = "absoluto")
  })
  
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
  
  # Cascata de Seletores: NRE/PPA -> Municípios
  observeEvent(list(input$regiao_ppa, input$nre_selecionado), {
    df_muni_filt <- dados_matriculas
    if (input$regiao_ppa != "todas") df_muni_filt <- df_muni_filt |> filter(regiao_ppa == input$regiao_ppa)
    if (input$nre_selecionado != "todos") df_muni_filt <- df_muni_filt |> filter(nome_nre == input$nre_selecionado)
    
    munis_disp <- sort(unique(df_muni_filt$municipio))
    sel_existente <- intersect(input$municipios_historico, munis_disp)
    updateSelectizeInput(session, "municipios_historico", choices = munis_disp, selected = sel_existente, server = TRUE)
  }, ignoreInit = TRUE)
  
  # Exclusividade mútua entre Etapa de Ensino e Faixa Etária
  observeEvent(input$etapa_ensino, {
    if (input$etapa_ensino != "geral") updateSelectInput(session, "faixa_etaria", selected = "geral")
  }, ignoreInit = TRUE)
  
  observeEvent(input$faixa_etaria, {
    if (input$faixa_etaria != "geral") updateSelectInput(session, "etapa_ensino", selected = "geral")
  }, ignoreInit = TRUE)
  
  # Base Reativa Central
  dados_reativos <- reactive({
    df <- dados_matriculas
    coluna_alvo <- "total_educacao_basica"
    
    if (input$faixa_etaria != "geral") {
      if (input$faixa_etaria == "0_3")      coluna_alvo <- "matriculas_0_a_3_anos"
      else if (input$faixa_etaria == "4_5") coluna_alvo <- "matriculas_4_a_5_anos"
      else if (input$faixa_etaria == "6_10") coluna_alvo <- "matriculas_6_a_10_anos"
      else if (input$faixa_etaria == "11_14") coluna_alvo <- "matriculas_11_a_14_anos"
      else if (input$faixa_etaria == "15_17") coluna_alvo <- "matriculas_15_a_17_anos"
      else if (input$faixa_etaria == "18_mais") coluna_alvo <- "matriculas_18_anos_ou_mais"
    } else if (input$etapa_ensino != "geral") {
      if (input$etapa_ensino == "creche")      coluna_alvo <- "matriculas_creche"
      else if (input$etapa_ensino == "pre_escola") coluna_alvo <- "matriculas_pre_escola"
      else if (input$etapa_ensino == "ai")     coluna_alvo <- "matriculas_anos_iniciais"
      else if (input$etapa_ensino == "af")     coluna_alvo <- "matriculas_anos_finais"
      else if (input$etapa_ensino == "em")     coluna_alvo <- "matriculas_ensino_medio"
    }
    
    df$valor_etapa <- df[[coluna_alvo]]
    df
  })
  
  # Dados Consolidados do Ano Selecionado
  dados_ano_consolidados <- reactive({
    consolidar_matriculas_rede(
      dados_reativos(), 
      ano_filtro = as.numeric(input$ano_selecionado),
      nre_filtro = input$nre_selecionado,
      ppa_filtro = input$regiao_ppa
    )
  })
  
  # Métrica Final Reativa
  tabela_metrica_final <- reactive({
    calcular_metrica_final(
      dados_ano_consolidados(),
      rede_selecionada = input$rede_selecionada,
      tipo_valor = input$tipo_valor
    )
  })
  
  # Badge de Contexto Ativo na Visão Geral
  output$badge_contexto_ativo <- renderUI({
    txt_territorio <- if (input$nre_selecionado != "todos") {
      paste("NRE:", input$nre_selecionado)
    } else if (input$regiao_ppa != "todas") {
      paste("PPA:", input$regiao_ppa)
    } else {
      "Paraná (Todo o Estado)"
    }
    
    span(
      class = "badge",
      style = "background-color: var(--pr-green-light); color: var(--pr-green-dark); font-size: 0.85rem; padding: 0.4rem 0.75rem; border: 1px solid #C6E2D0;",
      paste0("Ano: ", input$ano_selecionado, " | ", txt_territorio)
    )
  })
  
  # KPIs da Visão Geral
  kpis_calculados <- reactive({
    calcular_kpis_executivos(
      dados_reativos(),
      ano_filtro = as.numeric(input$ano_selecionado),
      nre_filtro = input$nre_selecionado,
      ppa_filtro = input$regiao_ppa
    )
  })
  
  output$kpi_total     <- renderUI({ render_kpi_conteudo(kpis_calculados()$total, is_total = TRUE) })
  output$kpi_publico   <- renderUI({ render_kpi_conteudo(kpis_calculados()$publico) })
  output$kpi_privada   <- renderUI({ render_kpi_conteudo(kpis_calculados()$privada) })
  output$kpi_estadual  <- renderUI({ render_kpi_conteudo(kpis_calculados()$estadual) })
  output$kpi_municipal <- renderUI({ render_kpi_conteudo(kpis_calculados()$municipal) })
  output$kpi_federal   <- renderUI({ render_kpi_conteudo(kpis_calculados()$federal) })
  
  # Gráficos da Visão Geral
  output$grafico_vg_historico <- renderPlotly({
    df_todos <- dados_reativos()
    if (input$regiao_ppa != "todas") df_todos <- df_todos |> filter(regiao_ppa == input$regiao_ppa)
    if (input$nre_selecionado != "todos") df_todos <- df_todos |> filter(nome_nre == input$nre_selecionado)
    
    resumo_hist <- df_todos |>
      group_by(ano) |>
      summarise(total_matriculas = sum(valor_etapa, na.rm = TRUE), .groups = 'drop')
    
    plot_ly(resumo_hist, x = ~ano, y = ~total_matriculas, type = 'scatter', mode = 'lines+markers',
            line = list(color = "#14532D", width = 3), marker = list(size = 8, color = "#228B52"),
            hovertemplate = "Ano %{x}: <b>%{y:,.0f}</b> matrículas<extra></extra>") |>
      layout(
        margin = list(l = 60, r = 20, t = 20, b = 40),
        xaxis = list(title = "Ano", dtick = 1),
        yaxis = list(title = "Total de Matrículas"),
        paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
        font = list(family = "Inter, sans-serif")
      ) |> config(displayModeBar = FALSE)
  })
  
  output$grafico_vg_redes <- renderPlotly({
    k <- kpis_calculados()
    df_redes <- data.frame(
      Rede = c("Estadual", "Municipal", "Privada", "Federal"),
      Matriculas = c(k$estadual$valor, k$municipal$valor, k$privada$valor, k$federal$valor),
      Cor = c("#0284C7", "#0D9488", "#D97706", "#6366F1")
    )
    
    plot_ly(df_redes, labels = ~Rede, values = ~Matriculas, type = 'pie',
            marker = list(colors = df_redes$Cor),
            textinfo = 'percent', hoverinfo = 'label+value+percent') |>
      layout(
        margin = list(l = 20, r = 20, t = 10, b = 20),
        showlegend = TRUE,
        legend = list(orientation = "h", x = 0, y = -0.1),
        paper_bgcolor = "#FFFFFF", font = list(family = "Inter, sans-serif")
      ) |> config(displayModeBar = FALSE)
  })
  
  output$grafico_vg_top <- renderPlotly({
    df_top <- tabela_metrica_final() |>
      arrange(desc(valor_final)) |>
      head(10)
    
    render_plotly_ranking(df_top, input$tipo_valor, tools::toTitleCase(input$rede_selecionada))
  })
  
  output$mapa_vg_resumo <- renderLeaflet({
    metricas <- tabela_metrica_final() |> select(codigo_municipio, municipio, valor_final)
    mapa_dados <- mapa_pr |> inner_join(metricas, by = c("code_muni" = "codigo_municipio"))
    
    pal <- colorNumeric("Greens", domain = mapa_dados$valor_final, na.color = "transparent")
    
    leaflet(mapa_dados) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addPolygons(
        fillColor = ~pal(valor_final), weight = 1, opacity = 1, color = "#64748B", fillOpacity = 0.85,
        label = ~paste0(name_muni, ": ", format(valor_final, big.mark = "."))
      )
  })
  
  # --- 1. RANKING COMPLETO ---
  output$grafico_ranking <- renderPlotly({
    qtd_linhas <- ifelse(input$regiao_ppa == "todas" && input$nre_selecionado == "todos", 15, 20)
    metricas <- tabela_metrica_final() |>
      arrange(desc(valor_final)) |>
      head(qtd_linhas)
    
    render_plotly_ranking(metricas, input$tipo_valor, tools::toTitleCase(input$rede_selecionada))
  })
  
  # --- 2. EVOLUÇÃO HISTÓRICA COMPLETA ---
  output$grafico_historico <- renderPlotly({
    df_todos <- dados_reativos()
    
    # Linha Paraná
    resumo_estado <- df_todos |>
      group_by(ano, rede) |>
      summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names()
    
    if (!"federal" %in% names(resumo_estado))   resumo_estado$federal   <- 0
    if (!"municipal" %in% names(resumo_estado)) resumo_estado$municipal <- 0
    if (!"estadual" %in% names(resumo_estado))  resumo_estado$estadual  <- 0
    if (!"privada" %in% names(resumo_estado))   resumo_estado$privada   <- 0
    
    resumo_estado <- resumo_estado |> mutate(publico = estadual + municipal + federal, total = publico + privada)
    resumo_estado$valor_base <- case_when(
      input$rede_selecionada == "publico"   ~ resumo_estado$publico,
      input$rede_selecionada == "estadual"  ~ resumo_estado$estadual,
      input$rede_selecionada == "municipal" ~ resumo_estado$municipal,
      input$rede_selecionada == "federal"   ~ resumo_estado$federal,
      input$rede_selecionada == "privada"   ~ resumo_estado$privada,
      TRUE                                  ~ resumo_estado$total
    )
    resumo_estado$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
      ifelse(resumo_estado$total == 0, 0, resumo_estado$valor_base / resumo_estado$total)
    } else {
      as.numeric(resumo_estado$valor_base)
    }
    
    df_plot <- resumo_estado |> mutate(local = "PARANÁ (Total Estadual)") |> select(ano, local, valor_final)
    
    # Recorte PPA
    if (input$regiao_ppa != "todas") {
      resumo_ppa <- df_todos |> filter(regiao_ppa == input$regiao_ppa) |>
        group_by(ano, rede) |> summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names()
      if (!"federal" %in% names(resumo_ppa))   resumo_ppa$federal   <- 0
      if (!"municipal" %in% names(resumo_ppa)) resumo_ppa$municipal <- 0
      if (!"estadual" %in% names(resumo_ppa))  resumo_ppa$estadual  <- 0
      if (!"privada" %in% names(resumo_ppa))   resumo_ppa$privada   <- 0
      resumo_ppa <- resumo_ppa |> mutate(publico = estadual + municipal + federal, total = publico + privada)
      resumo_ppa$valor_base <- case_when(
        input$rede_selecionada == "publico"   ~ resumo_ppa$publico,
        input$rede_selecionada == "estadual"  ~ resumo_ppa$estadual,
        input$rede_selecionada == "municipal" ~ resumo_ppa$municipal,
        input$rede_selecionada == "federal"   ~ resumo_ppa$federal,
        input$rede_selecionada == "privada"   ~ resumo_ppa$privada,
        TRUE                                  ~ resumo_ppa$total
      )
      resumo_ppa$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
        ifelse(resumo_ppa$total == 0, 0, resumo_ppa$valor_base / resumo_ppa$total)
      } else as.numeric(resumo_ppa$valor_base)
      
      df_plot <- bind_rows(df_plot, resumo_ppa |> mutate(local = paste0("Região PPA ", input$regiao_ppa)) |> select(ano, local, valor_final))
    }
    
    # Recorte NRE
    if (input$nre_selecionado != "todos") {
      resumo_nre <- df_todos |> filter(nome_nre == input$nre_selecionado) |>
        group_by(ano, rede) |> summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names()
      if (!"federal" %in% names(resumo_nre))   resumo_nre$federal   <- 0
      if (!"municipal" %in% names(resumo_nre)) resumo_nre$municipal <- 0
      if (!"estadual" %in% names(resumo_nre))  resumo_nre$estadual  <- 0
      if (!"privada" %in% names(resumo_nre))   resumo_nre$privada   <- 0
      resumo_nre <- resumo_nre |> mutate(publico = estadual + municipal + federal, total = publico + privada)
      resumo_nre$valor_base <- case_when(
        input$rede_selecionada == "publico"   ~ resumo_nre$publico,
        input$rede_selecionada == "estadual"  ~ resumo_nre$estadual,
        input$rede_selecionada == "municipal" ~ resumo_nre$municipal,
        input$rede_selecionada == "federal"   ~ resumo_nre$federal,
        input$rede_selecionada == "privada"   ~ resumo_nre$privada,
        TRUE                                  ~ resumo_nre$total
      )
      resumo_nre$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
        ifelse(resumo_nre$total == 0, 0, resumo_nre$valor_base / resumo_nre$total)
      } else as.numeric(resumo_nre$valor_base)
      
      df_plot <- bind_rows(df_plot, resumo_nre |> mutate(local = paste0("NRE ", input$nre_selecionado)) |> select(ano, local, valor_final))
    }
    
    # Municípios Comparados
    if (!is.null(input$municipios_historico) && length(input$municipios_historico) > 0) {
      df_muni <- df_todos |> filter(municipio %in% input$municipios_historico) |>
        group_by(ano, municipio, rede) |> summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names()
      if (!"federal" %in% names(df_muni))   df_muni$federal   <- 0
      if (!"municipal" %in% names(df_muni)) df_muni$municipal <- 0
      if (!"estadual" %in% names(df_muni))  df_muni$estadual  <- 0
      if (!"privada" %in% names(df_muni))   df_muni$privada   <- 0
      df_muni <- df_muni |> mutate(publico = estadual + municipal + federal, total = publico + privada)
      df_muni$valor_base <- case_when(
        input$rede_selecionada == "publico"   ~ df_muni$publico,
        input$rede_selecionada == "estadual"  ~ df_muni$estadual,
        input$rede_selecionada == "municipal" ~ df_muni$municipal,
        input$rede_selecionada == "federal"   ~ df_muni$federal,
        input$rede_selecionada == "privada"   ~ df_muni$privada,
        TRUE                                  ~ df_muni$total
      )
      df_muni$valor_final <- if (input$tipo_valor == "percentual" && input$rede_selecionada != "total") {
        ifelse(df_muni$total == 0, 0, df_muni$valor_base / df_muni$total)
      } else as.numeric(df_muni$valor_base)
      
      df_plot <- bind_rows(df_plot, df_muni |> rename(local = municipio) |> select(ano, local, valor_final))
    }
    
    render_plotly_historico(df_plot, input$tipo_valor, tools::toTitleCase(input$rede_selecionada))
  })
  
  # --- 3. MAPA INTERATIVO COMPLETO ---
  output$mapa_interativo <- renderLeaflet({
    metricas <- tabela_metrica_final() |> select(codigo_municipio, municipio, valor_final)
    
    if (input$regiao_ppa != "todas" || input$nre_selecionado != "todos") {
      mapa_pr_dados <- mapa_pr |> inner_join(metricas, by = c("code_muni" = "codigo_municipio"))
    } else {
      mapa_pr_dados <- mapa_pr |> left_join(metricas, by = c("code_muni" = "codigo_municipio"))
      mapa_pr_dados$valor_final[is.na(mapa_pr_dados$valor_final)] <- 0
    }
    
    if (input$tipo_valor == "percentual") {
      pal <- colorNumeric("YlGn", domain = c(0, 1), na.color = "transparent")
      popup_texto <- sprintf("<strong>%s</strong><br/>%s: <strong>%.1f%%</strong>", 
                             mapa_pr_dados$name_muni, tools::toTitleCase(input$rede_selecionada), mapa_pr_dados$valor_final * 100)
      titulo_legenda <- paste("<strong>% de", tools::toTitleCase(input$rede_selecionada), "</strong>")
      
      leaflet(mapa_pr_dados) |>
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Cinza Minimalista (Padrão)") |>
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") |>
        addTiles(group = "OpenStreetMap (Ruas)") |>
        addPolygons(fillColor = ~pal(valor_final), weight = 1, opacity = 1, color = "#64748B", fillOpacity = 0.85,
                    highlightOptions = highlightOptions(weight = 2, color = "#14532D", fillOpacity = 1, bringToFront = TRUE),
                    label = lapply(popup_texto, htmltools::HTML)) |>
        addLegend(pal = pal, values = c(0, 1), opacity = 0.85, title = titulo_legenda, position = "bottomright", labFormat = labelFormat(suffix = "%", transform = function(x) x * 100)) |>
        addLayersControl(baseGroups = c("Cinza Minimalista (Padrão)", "Topográfico", "OpenStreetMap (Ruas)"), options = layersControlOptions(collapsed = TRUE))
    } else {
      d_min <- min(mapa_pr_dados$valor_final, na.rm = TRUE)
      d_max <- max(mapa_pr_dados$valor_final, na.rm = TRUE)
      if (d_min == d_max) d_max <- d_min + 1
      pal <- colorNumeric("Greens", domain = c(d_min, d_max), na.color = "transparent")
      popup_texto <- sprintf("<strong>%s</strong><br/>%s: <strong>%s</strong> matrículas", 
                             mapa_pr_dados$name_muni, tools::toTitleCase(input$rede_selecionada), format(mapa_pr_dados$valor_final, big.mark = "."))
      titulo_legenda <- paste("<strong>Total:", tools::toTitleCase(input$rede_selecionada), "</strong>")
      
      leaflet(mapa_pr_dados) |>
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Cinza Minimalista (Padrão)") |>
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") |>
        addTiles(group = "OpenStreetMap (Ruas)") |>
        addPolygons(fillColor = ~pal(valor_final), weight = 1, opacity = 1, color = "#64748B", fillOpacity = 0.85,
                    highlightOptions = highlightOptions(weight = 2, color = "#14532D", fillOpacity = 1, bringToFront = TRUE),
                    label = lapply(popup_texto, htmltools::HTML)) |>
        addLegend(pal = pal, values = c(d_min, d_max), opacity = 0.85, title = titulo_legenda, position = "bottomright", labFormat = labelFormat(big.mark = ".")) |>
        addLayersControl(baseGroups = c("Cinza Minimalista (Padrão)", "Topográfico", "OpenStreetMap (Ruas)"), options = layersControlOptions(collapsed = TRUE))
    }
  })
  
  # --- 4. TABELA DETALHADA ---
  output$tabela_dinamica <- renderDT({
    tabela_larga <- dados_ano_consolidados()
    
    tabela_municipios <- tabela_larga |>
      select(municipio, estadual, municipal, federal, publico, privada, total, populacao_2024) |>
      arrange(desc(total))
    
    val_est  <- sum(tabela_municipios$estadual, na.rm = TRUE)
    val_mun  <- sum(tabela_municipios$municipal, na.rm = TRUE)
    val_fed  <- sum(tabela_municipios$federal, na.rm = TRUE)
    val_pub  <- sum(tabela_municipios$publico, na.rm = TRUE)
    val_priv <- sum(tabela_municipios$privada, na.rm = TRUE)
    val_tot  <- sum(tabela_municipios$total, na.rm = TRUE)
    val_pop  <- sum(tabela_municipios$populacao_2024, na.rm = TRUE)
    
    nome_linha_fixa <- if (input$nre_selecionado != "todos") {
      paste0("NRE ", input$nre_selecionado, " (Total Regional)")
    } else if (input$regiao_ppa != "todas") {
      paste0("Região PPA ", input$regiao_ppa, " (Total Macrorregional)")
    } else {
      "PARANÁ (Total Estadual)"
    }
    
    if (input$tipo_valor == "percentual") {
      p_est  <- ifelse(val_tot == 0, "0,0%", percent(val_est / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_mun  <- ifelse(val_tot == 0, "0,0%", percent(val_mun / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_fed  <- ifelse(val_tot == 0, "0,0%", percent(val_fed / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_pub  <- ifelse(val_tot == 0, "0,0%", percent(val_pub / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_priv <- ifelse(val_tot == 0, "0,0%", percent(val_priv / val_tot, accuracy = 0.1, decimal.mark = ","))
      p_tot  <- "100,0%"
      p_pop  <- format(val_pop, big.mark = ".")
      
      tabela_municipios <- tabela_municipios |> mutate(
        estadual = ifelse(total == 0, 0, estadual / total),
        municipal = ifelse(total == 0, 0, municipal / total),
        federal = ifelse(total == 0, 0, federal / total),
        publico = ifelse(total == 0, 0, publico / total),
        privada = ifelse(total == 0, 0, privada / total)
      )
    } else {
      p_est  <- format(val_est, big.mark = ".")
      p_mun  <- format(val_mun, big.mark = ".")
      p_fed  <- format(val_fed, big.mark = ".")
      p_pub  <- format(val_pub, big.mark = ".")
      p_priv <- format(val_priv, big.mark = ".")
      p_tot  <- format(val_tot, big.mark = ".")
      p_pop  <- format(val_pop, big.mark = ".")
    }
    
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(th("Município"), th("Estadual"), th("Municipal"), th("Federal"), th("Público"), th("Privado"), th("Total Geral"), th("População (2024)")),
        tr(
          class = "linha-totalizador-dt",
          th(style = "text-align: left; padding: 10px;", nome_linha_fixa),
          th(style = "text-align: right; padding: 10px;", p_est),
          th(style = "text-align: right; padding: 10px;", p_mun),
          th(style = "text-align: right; padding: 10px;", p_fed),
          th(style = "text-align: right; padding: 10px;", p_pub),
          th(style = "text-align: right; padding: 10px;", p_priv),
          th(style = "text-align: right; padding: 10px;", p_tot),
          th(style = "text-align: right; padding: 10px;", p_pop)
        )
      )
    ))
    
    dt <- datatable(
      tabela_municipios, 
      container = sketch, 
      rownames = FALSE,
      options = list(
        pageLength = 10, 
        orderCellsTop = TRUE, 
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
      )
    )
    
    if (input$tipo_valor == "percentual") {
      dt <- dt |> formatPercentage(c("estadual", "municipal", "federal", "publico", "privada"), 1)
    } else {
      dt <- dt |> formatCurrency(c("estadual", "municipal", "federal", "publico", "privada", "total", "populacao_2024"), currency = "", interval = 3, mark = ".", digits = 0)
    }
    dt
  })
  
  # --- 5. ASSISTENTE IA (ELLMER + SHINYCHAT) ---
  ia_obj <- inicializar_assistente_ia(dados_matriculas)
  chat_bot <- ia_obj$bot
  erro_ia <- ia_obj$erro
  
  # Atalhos de Perguntas Sugeridas
  observeEvent(input$sug_1, {
    chat_append("chat_educacao", user_turn("Quais são os 5 municípios com maior taxa de rede privada em 2025?"))
    processar_pergunta("Quais são os 5 municípios com maior taxa de rede privada em 2025?")
  })
  observeEvent(input$sug_2, {
    chat_append("chat_educacao", user_turn("Qual é o total de matrículas por rede no NRE de Maringá em 2025?"))
    processar_pergunta("Qual é o total de matrículas por rede no NRE de Maringá em 2025?")
  })
  observeEvent(input$sug_3, {
    chat_append("chat_educacao", user_turn("Como se distribuem as matrículas públicas e privadas em Londrina no ano de 2025?"))
    processar_pergunta("Como se distribuem as matrículas públicas e privadas em Londrina no ano de 2025?")
  })
  observeEvent(input$sug_4, {
    chat_append("chat_educacao", user_turn("Quais os 5 maiores municípios em matrículas na Região PPA Cascavel em 2025?"))
    processar_pergunta("Quais os 5 maiores municípios em matrículas na Região PPA Cascavel em 2025?")
  })
  
  processar_pergunta <- function(texto) {
    if (is.null(chat_bot)) {
      chat_append("chat_educacao", paste0("⚠️ **Assistente indisponível:**\n\n`", erro_ia, "`"))
      return()
    }
    tryCatch({
      resp <- chat_bot$chat(texto)
      chat_append("chat_educacao", resp)
    }, error = function(e) {
      if (grepl("429", e$message)) {
        chat_append("chat_educacao", "⏳ **Limite de requisições por minuto atingido (HTTP 429).** Aguarde cerca de 60 segundos antes de efetuar nova consulta.")
      } else {
        chat_append("chat_educacao", paste0("⚠️ **Erro na consulta:** ", e$message))
      }
    })
  }
  
  observeEvent(input$chat_educacao_user_input, {
    texto <- extrair_texto_usuario(input$chat_educacao_user_input)
    if (!nzchar(trimws(texto))) return()
    processar_pergunta(texto)
  })
}

# =============================================================================
# 4. INICIALIZAÇÃO DA APLICAÇÃO
# =============================================================================
shinyApp(ui, server)