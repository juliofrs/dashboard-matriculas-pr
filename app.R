library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(tidyr)
library(ggplot2)
library(scales)

# 1. Carregando os Dados do Excel
caminho_arquivo <- "Matriculas_Municipio_AI_AF_EM_populacao.xlsx"
dados_matriculas <- read_excel(caminho_arquivo, sheet = "Base") |> clean_names()

lista_anos <- sort(unique(dados_matriculas$ano), decreasing = TRUE) 

# 2. Interface (UI)
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
    
    selectInput(
      inputId = "etapa_ensino",
      label = "Etapa de Ensino:",
      choices = c(
        "Geral (Todas as Etapas)" = "geral", 
        "Anos Iniciais (6 a 10 anos)" = "ai", 
        "Anos Finais (11 a 14 anos)" = "af", 
        "Ensino Médio (15 a 17 anos)" = "em"
      ),
      selected = "geral"
    ),
    
    hr(),
    
    selectInput(
      inputId = "rede_grafico",
      label = "Rede de Ensino (para o Gráfico):",
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
      label = "Formato (Gráfico e Tabela):",
      choices = c("Números Absolutos" = "absoluto", "Percentuais (%)" = "percentual"),
      selected = "absoluto"
    ),
    
    p(class = "text-muted", "Nota: No modo percentual, o gráfico exibe a proporção da rede escolhida em relação ao total do município.")
  ),
  
  # --- ABA 1: GRÁFICO DE RANKING ---
  nav_panel(
    title = "Ranking de Municípios",
    card(
      full_screen = TRUE,
      card_header("Top 15 Municípios com Maiores Matrículas"),
      plotOutput("grafico_ranking", height = "550px")
    )
  ),
  
  # --- ABA 2: TABELA ---
  nav_panel(
    title = "Tabela Detalhada",
    card(
      card_header("Detalhamento por Município e Rede de Ensino"),
      DTOutput("tabela_dinamica")
    )
  )
)

# 3. Lógica do Servidor
server <- function(input, output, session) {
  
  dados_reativos <- reactive({
    df <- dados_matriculas
    if (input$etapa_ensino == "geral") {
      df$valor_etapa <- df$total_matriculas
    } else if (input$etapa_ensino == "ai") {
      df$valor_etapa <- df$matriculas_6_a_10_anos
    } else if (input$etapa_ensino == "af") {
      df$valor_etapa <- df$matriculas_11_a_14_anos
    } else if (input$etapa_ensino == "em") {
      df$valor_etapa <- df$matriculas_15_a_17_anos
    }
    return(df)
  })
  
  # --- CONSTRUÇÃO DO GRÁFICO DE RANKING (COM CÁLCULO BLINDADO) ---
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
    
    # Seleção da base de cálculo sem conflitos de vetorização
    tabela_larga$valor_base <- case_when(
      input$rede_grafico == "publico" ~ tabela_larga$publico,
      input$rede_grafico == "estadual" ~ tabela_larga$estadual,
      input$rede_grafico == "municipal" ~ tabela_larga$municipal,
      input$rede_grafico == "federal" ~ tabela_larga$federal,
      input$rede_grafico == "privada" ~ tabela_larga$privada,
      TRUE ~ tabela_larga$total
    )
    
    tabela_larga$valor_final <- if (input$tipo_valor == "percentual" && input$rede_grafico != "total") {
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
          y = paste("Proporção de", tools::toTitleCase(input$rede_grafico)),
          title = paste("Top 15 Municípios - Proporção de", tools::toTitleCase(input$rede_grafico), "(", input$ano_selecionado, ")")
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
          y = paste("Total de Matrículas (", tools::toTitleCase(input$rede_grafico), ")"),
          title = paste("Top 15 Municípios - Volume de Matrículas (", input$ano_selecionado, ")")
        ) +
        theme(panel.grid.major.y = element_blank())
    }
  })
  
  # --- CONSTRUÇÃO DA TABELA ---
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