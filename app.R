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

# 2. Carregando o Mapa Geográfico do Paraná e convertendo para EPSG:4326
mapa_pr <- read_municipality(code_muni = "PR", year = 2020, showProgress = FALSE) |>
  st_transform(4326)

mapa_pr$code_muni <- as.numeric(mapa_pr$code_muni)

# 3. Interface (UI) com 3 Abas
ui <- page_navbar(
  title = "Dashboard Educacional - Paraná",
  theme = bs_theme(preset = "flatly"),
  
  sidebar = sidebar(
    title = "Filtros Globais",
    
    selectInput(
      inputId = "ano_selecionado", 
      label = "Escolha o Ano:", 
      choices = lista_anos,
      selected = max(lista_anos)
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
      label = "Rede de Ensino (Gráfico e Mapa):",
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
      label = "Formato (Visualizações e Tabela):",
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
      card_header("Top 15 Municípios com Maiores Matrículas"),
      plotOutput("grafico_ranking", height = "550px")
    )
  ),
  
  # --- ABA 2: MAPA INTERATIVO ---
  nav_panel(
    title = "Mapa Interativo",
    card(
      full_screen = TRUE,
      card_header("Distribuição Geográfica no Estado (Passe o mouse)"),
      leafletOutput("mapa_interativo", height = "600px")
    )
  ),
  
  # --- ABA 3: TABELA ---
  nav_panel(
    title = "Tabela Detalhada",
    card(
      card_header("Detalhamento por Município e Rede de Ensino"),
      DTOutput("tabela_dinamica")
    )
  )
)

# 4. Lógica do Servidor
server <- function(input, output, session) {
  
  # --- Lógica Inteligente para os Filtros ---
  # Se o usuário mudar a Etapa de Ensino, reseta a Faixa Etária
  observeEvent(input$etapa_ensino, {
    if (input$etapa_ensino != "geral") {
      updateSelectInput(session, "faixa_etaria", selected = "geral")
    }
  }, ignoreInit = TRUE)
  
  # Se o usuário mudar a Faixa Etária, reseta a Etapa de Ensino
  observeEvent(input$faixa_etaria, {
    if (input$faixa_etaria != "geral") {
      updateSelectInput(session, "etapa_ensino", selected = "geral")
    }
  }, ignoreInit = TRUE)
  
  # --- Preparação Dinâmica dos Dados ---
  dados_reativos <- reactive({
    df <- dados_matriculas
    coluna_alvo <- "total_educacao_basica"
    
    # Verifica qual filtro está ativo e direciona para a nova coluna correspondente
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
    
    # Repassa os valores da coluna correta para que os gráficos e tabelas leiam facilmente
    df$valor_etapa <- df[[coluna_alvo]]
    return(df)
  })
  
  # --- GRÁFICO DE RANKING ---
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
      mutate(
        publico = estadual + municipal + federal,
        total = publico + privada
      )
    
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
    
    metricas <- tabela_larga |>
      select(municipio, valor_final) |>
      arrange(desc(valor_final)) |>
      head(15)
    
    metricas$municipio <- factor(metricas$municipio, levels = rev(metricas$municipio))
    
    if (input$tipo_valor == "percentual") {
      ggplot(metricas, aes(x = municipio, y = valor_final, fill = valor_final)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
        scale_fill_gradient(low = "#ffcc80", high = "#e65100") +
        theme_minimal(base_size = 14) +
        labs(
          x = "", 
          y = paste("Proporção de", tools::toTitleCase(input$rede_selecionada)),
          title = paste("Top 15 Municípios - Proporção de", tools::toTitleCase(input$rede_selecionada), "(", input$ano_selecionado, ")")
        ) +
        theme(panel.grid.major.y = element_blank())
    } else {
      ggplot(metricas, aes(x = municipio, y = valor_final, fill = valor_final)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) +
        scale_fill_gradient(low = "#ffe0b2", high = "#bf360c") +
        theme_minimal(base_size = 14) +
        labs(
          x = "", 
          y = paste("Total de Matrículas (", tools::toTitleCase(input$rede_selecionada), ")"),
          title = paste("Top 15 Municípios - Volume de Matrículas (", input$ano_selecionado, ")")
        ) +
        theme(panel.grid.major.y = element_blank())
    }
  })
  
  # --- MAPA INTERATIVO SEGURO ---
  output$mapa_interativo <- renderLeaflet({
    dados_ano <- dados_reativos() |> 
      filter(ano == input$ano_selecionado) |>
      select(codigo_municipio, municipio, rede, valor_etapa)
    
    tabela_larga_mapa <- dados_ano |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |>
      clean_names()
    
    if(!"federal" %in% names(tabela_larga_mapa)) tabela_larga_mapa$federal <- 0
    if(!"municipal" %in% names(tabela_larga_mapa)) tabela_larga_mapa$municipal <- 0
    if(!"estadual" %in% names(tabela_larga_mapa)) tabela_larga_mapa$estadual <- 0
    if(!"privada" %in% names(tabela_larga_mapa)) tabela_larga_mapa$privada <- 0
    
    tabela_larga_mapa <- tabela_larga_mapa |>
      mutate(
        publico = estadual + municipal + federal,
        total = publico + privada
      )
    
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
    
    metricas <- tabela_larga_mapa |>
      select(codigo_municipio, municipio, valor_final)
    
    mapa_pr_dados <- mapa_pr |>
      left_join(metricas, by = c("code_muni" = "codigo_municipio"))
    
    mapa_pr_dados$valor_final[is.na(mapa_pr_dados$valor_final)] <- 0
    
    if (input$tipo_valor == "percentual") {
      pal <- colorNumeric("YlOrRd", domain = c(0, 1), na.color = "transparent")
      popup_texto <- sprintf(
        "<strong>%s</strong><br/>%s: <strong>%.1f%%</strong>",
        mapa_pr_dados$name_muni, 
        tools::toTitleCase(input$rede_selecionada),
        mapa_pr_dados$valor_final * 100
      )
      titulo_legenda <- paste("<strong>% de", tools::toTitleCase(input$rede_selecionada), "</strong>")
      
      leaflet(mapa_pr_dados) |>
        addProviderTiles(providers$CartoDB.Voyager) |>
        addPolygons(
          fillColor = ~pal(valor_final),
          weight = 2, opacity = 1, color = "#444444", fillOpacity = 0.9,
          highlightOptions = highlightOptions(weight = 5, color = "blue", fillOpacity = 1, bringToFront = TRUE),
          label = lapply(popup_texto, htmltools::HTML),
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
        ) |>
        addLegend(pal = pal, values = c(0, 1), opacity = 0.8, title = titulo_legenda, position = "bottomright",
                  labFormat = labelFormat(suffix = "%", transform = function(x) x * 100))
    } else {
      d_min <- min(mapa_pr_dados$valor_final, na.rm = TRUE)
      d_max <- max(mapa_pr_dados$valor_final, na.rm = TRUE)
      if (d_min == d_max) d_max <- d_min + 1
      
      pal <- colorNumeric("Oranges", domain = c(d_min, d_max), na.color = "transparent")
      popup_texto <- sprintf(
        "<strong>%s</strong><br/>%s: <strong>%s</strong> matrículas",
        mapa_pr_dados$name_muni, 
        tools::toTitleCase(input$rede_selecionada),
        format(mapa_pr_dados$valor_final, big.mark = ".", scientific = FALSE)
      )
      titulo_legenda <- paste("<strong>Total:", tools::toTitleCase(input$rede_selecionada), "</strong>")
      
      leaflet(mapa_pr_dados) |>
        addProviderTiles(providers$CartoDB.Voyager) |>
        addPolygons(
          fillColor = ~pal(valor_final),
          weight = 2, opacity = 1, color = "#444444", fillOpacity = 0.9,
          highlightOptions = highlightOptions(weight = 5, color = "blue", fillOpacity = 1, bringToFront = TRUE),
          label = lapply(popup_texto, htmltools::HTML),
          labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
        ) |>
        addLegend(pal = pal, values = c(d_min, d_max), opacity = 0.8, title = titulo_legenda, position = "bottomright",
                  labFormat = labelFormat(big.mark = "."))
    }
  })
  
  # --- TABELA DETALHADA ---
  output$tabela_dinamica <- renderDT({
    dados_ano <- dados_reativos() |> filter(ano == input$ano_selecionado) |>
      select(municipio, rede, valor_etapa, populacao_2024)
    
    tabela_larga <- dados_ano |>
      pivot_wider(names_from = rede, values_from = valor_etapa, values_fill = 0) |>
      clean_names() 
    
    if(!"federal" %in% names(tabela_larga)) tabela_larga$federal <- 0
    if(!"municipal" %in% names(tabela_larga)) tabela_larga$municipal <- 0
    if(!"estadual" %in% names(tabela_larga)) tabela_larga$estadual <- 0
    if(!"privada" %in% names(tabela_larga)) tabela_larga$privada <- 0
    
    tabela_final <- tabela_larga |>
      mutate(publico = estadual + municipal + federal, total = publico + privada) |>
      select(municipio, estadual, municipal, federal, publico, privada, total, populacao_2024) |>
      arrange(desc(total))
    
    if (input$tipo_valor == "percentual") {
      tabela_final <- tabela_final |>
        mutate(
          estadual = ifelse(total == 0, 0, estadual / total),
          municipal = ifelse(total == 0, 0, municipal / total),
          federal = ifelse(total == 0, 0, federal / total),
          publico = ifelse(total == 0, 0, publico / total),
          privada = ifelse(total == 0, 0, privada / total)
        )
    }
    
    tabela_dt <- datatable(
      tabela_final,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
      ),
      colnames = c("Município", "Estadual", "Municipal", "Federal", "Público", "Privado", "Total Absoluto", "População (2024)")
    )
    
    if (input$tipo_valor == "percentual") {
      tabela_dt <- tabela_dt |> 
        formatPercentage(c("estadual", "municipal", "federal", "publico", "privada"), 1) 
    }
    
    tabela_dt 
  })
}

shinyApp(ui, server)