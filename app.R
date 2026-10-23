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

# 1. Carregando os Dados do Novo Excel
caminho_arquivo <- "Matriculas_faixa_etaria_rede_municipio_consultas.xlsx"

# Lê a base principal
dados_matriculas <- read_excel(caminho_arquivo, sheet = "Plan1") |> clean_names()
dados_matriculas$codigo_municipio <- as.numeric(dados_matriculas$codigo_municipio)

# Lê a população e junta aos dados
dados_populacao <- read_excel(caminho_arquivo, sheet = "População") |> clean_names()
dados_populacao$codigo_ibge <- as.numeric(dados_populacao$codigo_ibge)

dados_matriculas <- dados_matriculas |>
  left_join(dados_populacao |> select(codigo_ibge, populacao_2024), 
            by = c("codigo_municipio" = "codigo_ibge"))

lista_anos <- sort(unique(dados_matriculas$ano), decreasing = TRUE) 
lista_municipios <- sort(unique(dados_matriculas$municipio))

# 2. Carregando o Mapa Geográfico do Paraná e convertendo para EPSG:4326
mapa_pr <- read_municipality(code_muni = "PR", year = 2020, showProgress = FALSE) |>
  st_transform(4326)
mapa_pr$code_muni <- as.numeric(mapa_pr$code_muni)

# 3. Interface (UI) com 4 Abas
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
    
    selectizeInput(
      inputId = "municipios_historico",
      label = "Comparar Municípios (no Histórico):",
      choices = lista_municipios,
      multiple = TRUE,
      selected = NULL,
      options = list(placeholder = 'Escolha um ou mais municípios...', plugins = list('remove_button'))
    ),
    
    hr(),
    
    # Filtro 1: Etapa de Ensino
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
    
    # Filtro 2: Faixa Etária
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
    ),
    
    p(class = "text-muted", "Dica: Ao selecionar um recorte (Etapa ou Idade), o outro será resetado automaticamente para evitar conflitos.")
  ),
  
  # --- ABA 1: RANKING ---
  nav_panel(
    title = "Ranking de Municípios",
    card(
      full_screen = TRUE,
      card_header("Top 15 Municípios com Maiores Matrículas no Ano Selecionado"),
      plotOutput("grafico_ranking", height = "550px")
    )
  ),
  
  # --- ABA 2: EVOLUÇÃO HISTÓRICA ---
  nav_panel(
    title = "Evolução Histórica",
    card(
      full_screen = TRUE,
      card_header("Comparativo Histórico de 2016 a 2025 (Estado x Municípios)"),
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
  
  # --- ABA 4: TABELA ---
  nav_panel(
    title = "Tabela Detalhada",
    card(
      card_header("Detalhamento por Município e Rede de Ensino no Ano Selecionado"),
      DTOutput("tabela_dinamica")
    )
  )
)

# 4. Lógica do Servidor
server <- function(input, output, session) {
  
  # --- Lógica Inteligente para os Filtros ---
  observeEvent(input$etapa_ensino, {
    if (input$etapa_ensino != "geral") {
      updateSelectInput(session, "faixa_etaria", selected = "geral")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$faixa_etaria, {
    if (input$faixa_etaria != "geral") {
      updateSelectInput(session, "etapa_ensino", selected = "geral")
    }
  }, ignoreInit = TRUE)
  
  # --- Preparação Dinâmica dos Dados ---
  dados_reativos <- reactive({
    df <- dados_matriculas
    coluna_alvo <- "total_educacao_basica"
    
    if (input$faixa_etaria != "geral") {
      if (input$faixa_etaria == "0_3") { coluna_alvo <- "matriculas_0_a_3_anos" }
      else if (input$faixa_etaria == "4_5") { coluna_alvo <- "matriculas_4_a_5_anos" }
      else if (input$faixa_etaria == "6_10") { coluna_alvo <- "matriculas_6_a_10_anos" }
      else if (input$faixa_etaria == "11_14") { coluna_alvo <- "matriculas_11_a_14_anos" }
      else if (input$faixa_etaria == "15_17") { coluna_alvo <- "matriculas_15_a_17_anos" }
      else if (input$faixa_etaria == "18_mais") { coluna_alvo <- "matriculas_18_anos_ou_mais" }
    } else if (input$etapa_ensino != "geral") {
      if (input$etapa_ensino == "creche") { coluna_alvo <- "matriculas_creche" }
      else if (input$etapa_ensino == "pre_escola") { coluna_alvo <- "matriculas_pre_escola" }
      else if (input$etapa_ensino == "ai") { coluna_alvo <- "matriculas_anos_iniciais" }
      else if (input$etapa_ensino == "af") { coluna_alvo <- "matriculas_anos_finais" }
      else if (input$etapa_ensino == "em") { coluna_alvo <- "matriculas_ensino_medio" }
    }
    
    df$valor_etapa <- df[[coluna_alvo]]
    return(df)
  })
  
  # --- GRÁFICO 1: RANKING DE MUNICÍPIOS ---
  output$grafico_ranking <- renderPlot({
    dados_ano <- dados_reativos() |> 
      filter(ano == input$ano_selecionado) |>
      select(codigo_municipio, municipio, rede, valor_etapa)
    
    tabela_larga <- dados_ano |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |>
      clean_names()
    
    if(!"federal" %in% names(tabela_larga)) tabela_larga$federal <- 0
    if(!"municipal" %in% names(tabela_larga)) tabela_larga$municipal <- 0
    if(!"estadual" %in% names(tabela_larga)) tabela_larga$estadual <- 0
    if(!"privada" %in% names(tabela_larga)) tabela_larga$privada <- 0
    
    tabela_larga <- tabela_larga |>
      mutate(publico = estadual + municipal + federal, total = publico + privada)
    
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
    
    metricas <- tabela_larga |> select(municipio, valor_final) |> arrange(desc(valor_final)) |> head(15)
    metricas$municipio <- factor(metricas$municipio, levels = rev(metricas$municipio))
    
    if (input$tipo_valor == "percentual") {
      ggplot(metricas, aes(x = municipio, y = valor_final, fill = valor_final)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
        scale_fill_gradient(low = "#fdba74", high = "#c2410c") +
        theme_minimal(base_size = 14) +
        labs(x = "", y = paste("Proporção de", tools::toTitleCase(input$rede_selecionada))) +
        theme(panel.grid.major.y = element_blank())
    } else {
      ggplot(metricas, aes(x = municipio, y = valor_final, fill = valor_final)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
        scale_fill_gradient(low = "#fed7aa", high = "#9a3412") +
        theme_minimal(base_size = 14) +
        labs(x = "", y = paste("Total de Matrículas (", tools::toTitleCase(input$rede_selecionada), ")")) +
        theme(panel.grid.major.y = element_blank())
    }
  })
  
  # --- GRÁFICO 2: EVOLUÇÃO HISTÓRICA (COM TETO DO EIXO Y EM NO MÁXIMO 100%) ---
  output$grafico_historico <- renderPlot({
    df_todos <- dados_reativos()
    
    # 1. Agregação Estadual (PARANÁ)
    resumo_estado <- df_todos |>
      group_by(ano, rede) |>
      summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |>
      clean_names()
    
    if(!"federal" %in% names(resumo_estado)) resumo_estado$federal <- 0
    if(!"municipal" %in% names(resumo_estado)) resumo_estado$municipal <- 0
    if(!"estadual" %in% names(resumo_estado)) resumo_estado$estadual <- 0
    if(!"privada" %in% names(resumo_estado)) resumo_estado$privada <- 0
    
    resumo_estado <- resumo_estado |>
      mutate(publico = estadual + municipal + federal, total = publico + privada)
    
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
    
    df_plot <- resumo_estado |>
      mutate(local = "PARANÁ (Total Estadual)") |>
      select(ano, local, valor_final)
    
    # 2. Agregação dos Municípios Selecionados
    if (!is.null(input$municipios_historico) && length(input$municipios_historico) > 0) {
      df_muni <- df_todos |>
        filter(municipio %in% input$municipios_historico) |>
        group_by(ano, municipio, rede) |>
        summarise(valor_etapa = sum(valor_etapa, na.rm = TRUE), .groups = 'drop') |>
        pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |>
        clean_names()
      
      if(!"federal" %in% names(df_muni)) df_muni$federal <- 0
      if(!"municipal" %in% names(df_muni)) df_muni$municipal <- 0
      if(!"estadual" %in% names(df_muni)) df_muni$estadual <- 0
      if(!"privada" %in% names(df_muni)) df_muni$privada <- 0
      
      df_muni <- df_muni |>
        mutate(publico = estadual + municipal + federal, total = publico + privada)
      
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
      } else {
        as.numeric(df_muni$valor_base)
      }
      
      df_muni_plot <- df_muni |>
        rename(local = municipio) |>
        select(ano, local, valor_final)
      
      df_plot <- bind_rows(df_plot, df_muni_plot)
    }
    
    # Ordenação dos fatores
    locais_unicos <- unique(df_plot$local)
    locais_ordenados <- c("PARANÁ (Total Estadual)", setdiff(locais_unicos, "PARANÁ (Total Estadual)"))
    df_plot$local <- factor(df_plot$local, levels = locais_ordenados)
    
    cores_finais <- setNames(
      c("#0F172A", rep(c("#DC2626", "#2563EB", "#059669", "#D97706", "#7C3AED", "#0891B2", "#DB2777"), length.out = length(locais_ordenados) - 1)),
      locais_ordenados
    )
    
    max_y <- max(df_plot$valor_final, na.rm = TRUE)
    if (is.na(max_y) || max_y == 0) max_y <- 1
    
    # 3. Renderização com Teto Limitado Rigorosamente a 100% (1.0)
    if (input$tipo_valor == "percentual") {
      lim_y <- min(1.0, max_y * 1.15) # Teto máximo travado em 1.0 (100%)
      
      ggplot(df_plot, aes(x = ano, y = valor_final, color = local, group = local)) +
        geom_line(linewidth = 1.3) +
        geom_point(size = 3.5) +
        geom_text(aes(label = percent(valor_final, accuracy = 0.1)), vjust = -1.2, size = 4, fontface = "bold", show.legend = FALSE) +
        scale_color_manual(values = cores_finais) +
        scale_y_continuous(labels = percent_format(), limits = c(0, lim_y)) +
        scale_x_continuous(breaks = unique(df_plot$ano)) +
        coord_cartesian(clip = "off") + # Garante que rótulos não sejam cortados
        theme_minimal(base_size = 14) +
        labs(
          x = "Ano", 
          y = paste("Proporção (", tools::toTitleCase(input$rede_selecionada), ")"),
          color = "Localidade"
        ) +
        theme(
          legend.position = "top",
          panel.grid.minor.x = element_blank(),
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 11, face = "bold"),
          plot.margin = margin(t = 20, r = 20, b = 10, l = 10)
        )
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
        labs(
          x = "Ano", 
          y = paste("Total de Matrículas (", tools::toTitleCase(input$rede_selecionada), ")"),
          color = "Localidade"
        ) +
        theme(
          legend.position = "top",
          panel.grid.minor.x = element_blank(),
          legend.title = element_text(face = "bold"),
          legend.text = element_text(size = 11, face = "bold"),
          plot.margin = margin(t = 20, r = 20, b = 10, l = 10)
        )
    }
  })
  
  # --- MAPA INTERATIVO ---
  output$mapa_interativo <- renderLeaflet({
    dados_ano <- dados_reativos() |> filter(ano == input$ano_selecionado) |> select(codigo_municipio, municipio, rede, valor_etapa)
    tabela_larga_mapa <- dados_ano |> pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names()
    
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
    } else {
      as.numeric(tabela_larga_mapa$valor_base)
    }
    
    metricas <- tabela_larga_mapa |> select(codigo_municipio, municipio, valor_final)
    mapa_pr_dados <- mapa_pr |> left_join(metricas, by = c("code_muni" = "codigo_municipio"))
    mapa_pr_dados$valor_final[is.na(mapa_pr_dados$valor_final)] <- 0
    
    if (input$tipo_valor == "percentual") {
      pal <- colorNumeric("YlOrRd", domain = c(0, 1), na.color = "transparent")
      popup_texto <- sprintf("<strong>%s</strong><br/>%s: <strong>%.1f%%</strong>", mapa_pr_dados$name_muni, tools::toTitleCase(input$rede_selecionada), mapa_pr_dados$valor_final * 100)
      titulo_legenda <- paste("<strong>% de", tools::toTitleCase(input$rede_selecionada), "</strong>")
      
      leaflet(mapa_pr_dados) |>
        addProviderTiles(providers$CartoDB.Voyager) |>
        addPolygons(fillColor = ~pal(valor_final), weight = 2, opacity = 1, color = "#444444", fillOpacity = 0.9,
                    highlightOptions = highlightOptions(weight = 5, color = "blue", fillOpacity = 1, bringToFront = TRUE),
                    label = lapply(popup_texto, htmltools::HTML),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) |>
        addLegend(pal = pal, values = c(0, 1), opacity = 0.8, title = titulo_legenda, position = "bottomright", labFormat = labelFormat(suffix = "%", transform = function(x) x * 100))
    } else {
      d_min <- min(mapa_pr_dados$valor_final, na.rm = TRUE)
      d_max <- max(mapa_pr_dados$valor_final, na.rm = TRUE)
      if (d_min == d_max) d_max <- d_min + 1
      
      pal <- colorNumeric("Oranges", domain = c(d_min, d_max), na.color = "transparent")
      popup_texto <- sprintf("<strong>%s</strong><br/>%s: <strong>%s</strong> matrículas", mapa_pr_dados$name_muni, tools::toTitleCase(input$rede_selecionada), format(mapa_pr_dados$valor_final, big.mark = ".", scientific = FALSE))
      titulo_legenda <- paste("<strong>Total:", tools::toTitleCase(input$rede_selecionada), "</strong>")
      
      leaflet(mapa_pr_dados) |>
        addProviderTiles(providers$CartoDB.Voyager) |>
        addPolygons(fillColor = ~pal(valor_final), weight = 2, opacity = 1, color = "#444444", fillOpacity = 0.9,
                    highlightOptions = highlightOptions(weight = 5, color = "blue", fillOpacity = 1, bringToFront = TRUE),
                    label = lapply(popup_texto, htmltools::HTML),
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")) |>
        addLegend(pal = pal, values = c(d_min, d_max), opacity = 0.8, title = titulo_legenda, position = "bottomright", labFormat = labelFormat(big.mark = "."))
    }
  })
  
  # --- TABELA DETALHADA ---
  output$tabela_dinamica <- renderDT({
    dados_ano <- dados_reativos() |> filter(ano == input$ano_selecionado) |> select(municipio, rede, valor_etapa, populacao_2024)
    tabela_larga <- dados_ano |> pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |> clean_names() 
    
    if(!"federal" %in% names(tabela_larga)) tabela_larga$federal <- 0
    if(!"municipal" %in% names(tabela_larga)) tabela_larga$municipal <- 0
    if(!"estadual" %in% names(tabela_larga)) tabela_larga$estadual <- 0
    if(!"privada" %in% names(tabela_larga)) tabela_larga$privada <- 0
    
    tabela_municipios <- tabela_larga |>
      mutate(publico = estadual + municipal + federal, total = publico + privada) |>
      select(municipio, estadual, municipal, federal, publico, privada, total, populacao_2024) |>
      arrange(desc(total))
    
    # 1. Totais Consolidados do Estado
    val_est <- sum(tabela_municipios$estadual, na.rm = TRUE)
    val_mun <- sum(tabela_municipios$municipal, na.rm = TRUE)
    val_fed <- sum(tabela_municipios$federal, na.rm = TRUE)
    val_pub <- sum(tabela_municipios$publico, na.rm = TRUE)
    val_priv <- sum(tabela_municipios$privada, na.rm = TRUE)
    val_tot <- sum(tabela_municipios$total, na.rm = TRUE)
    val_pop <- sum(tabela_municipios$populacao_2024, na.rm = TRUE)
    
    # 2. Formatação explícita com alinhamento
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
    
    # 3. Cabeçalho fixo (sketch)
    sketch <- htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th("Município"),
          th("Estadual"),
          th("Municipal"),
          th("Federal"),
          th("Público"),
          th("Privado"),
          th("Total Absoluto"),
          th("População (2024)")
        ),
        tr(
          style = "background-color: #e2e8f0; font-weight: bold; color: #0f172a; font-size: 14px;",
          th(style = "text-align: left; padding: 8px 10px;", "PARANÁ (Total Estadual)"),
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
    
    tabela_dt <- datatable(
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
      tabela_dt <- tabela_dt |> formatPercentage(c("estadual", "municipal", "federal", "publico", "privada"), 1) 
    } else {
      tabela_dt <- tabela_dt |> formatCurrency(c("estadual", "municipal", "federal", "publico", "privada", "total", "populacao_2024"), currency = "", interval = 3, mark = ".", digits = 0)
    }
    
    tabela_dt 
  })
}

shinyApp(ui, server)