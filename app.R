library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(DT)
library(tidyr)
library(scales) 

# 1. Carregando os Dados
caminho_arquivo <- "Matriculas_Municipio_AI_AF_EM_populacao.xlsx"
dados_matriculas <- read_excel(caminho_arquivo, sheet = "Base") |> clean_names()

lista_municipios <- sort(unique(dados_matriculas$municipio))
lista_anos <- sort(unique(dados_matriculas$ano), decreasing = TRUE) 

# 2. Interface (UI)
ui <- page_navbar(
  title = "Dashboard Educacional - Paraná",
  theme = bs_theme(preset = "flatly"),
  
  sidebar = sidebar(
    title = "Controles Globais",
    selectInput(
      inputId = "municipio_selecionado",
      label = "Escolha o Município:",
      choices = lista_municipios,
      selected = "Curitiba"
    ),
    
    # --- NOVO FILTRO: Etapa de Ensino ---
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
    radioButtons(
      inputId = "tipo_valor",
      label = "Mostrar valores em:",
      choices = c("Números Absolutos" = "absoluto", "Percentuais (%)" = "percentual"),
      selected = "absoluto"
    )
  ),
  
  nav_panel(
    title = "Evolução das Matrículas", 
    card(
      card_header("Histórico por Rede de Ensino"),
      plotOutput("grafico_matriculas")
    )
  ),
  
  nav_panel(
    title = "Tabela por Ano",
    card(
      card_header(
        class = "d-flex justify-content-between align-items-center",
        "Detalhamento de Matrículas por Município e Rede",
        selectInput(
          inputId = "ano_selecionado", 
          label = "Filtrar Ano da Tabela:", 
          choices = lista_anos, 
          width = "150px"
        )
      ),
      DTOutput("tabela_dinamica")
    )
  )
)

# 3. Lógica do Servidor
server <- function(input, output, session) {
  
  # --- BASE REATIVA DE ACORDO COM A ETAPA DE ENSINO ---
  # Aqui o R decide qual coluna da planilha vai ser usada nos cálculos
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
  
  # --- LÓGICA DO GRÁFICO ---
  output$grafico_matriculas <- renderPlot({
    # Substituímos "dados_matriculas" por "dados_reativos()"
    dados_grafico <- dados_reativos() |> filter(municipio == input$municipio_selecionado)
    
    if (input$tipo_valor == "percentual") {
      dados_grafico <- dados_grafico |>
        group_by(ano) |>
        mutate(
          total_do_ano = sum(valor_etapa),
          valor_exibir = ifelse(total_do_ano == 0, 0, valor_etapa / total_do_ano)
        ) |>
        ungroup()
      
      ggplot(dados_grafico, aes(x = as.factor(ano), y = valor_exibir, fill = rede)) +
        geom_col(position = "dodge") +
        scale_y_continuous(labels = percent_format()) + 
        theme_minimal() +
        labs(x = "Ano", y = "% de Matrículas", fill = "Rede de Ensino", 
             title = paste("Proporção de Matrículas em", input$municipio_selecionado)) +
        theme(text = element_text(size = 14))
      
    } else {
      # Substituímos "total_matriculas" por "valor_etapa"
      ggplot(dados_grafico, aes(x = as.factor(ano), y = valor_etapa, fill = rede)) +
        geom_col(position = "dodge") +
        scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
        theme_minimal() +
        labs(x = "Ano", y = "Matrículas", fill = "Rede de Ensino",
             title = paste("Matrículas em", input$municipio_selecionado)) +
        theme(text = element_text(size = 14))
    }
  })
  
  # --- LÓGICA DA TABELA ---
  output$tabela_dinamica <- renderDT({
    # Lendo dos dados_reativos() e selecionando a coluna dinâmica 'valor_etapa'
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