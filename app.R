library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(tidyr)

# 1. Carregando os Dados
caminho_arquivo <- "Matriculas_Municipio_AI_AF_EM_populacao.xlsx"
dados_matriculas <- read_excel(caminho_arquivo, sheet = "Base") |> clean_names()

lista_anos <- sort(unique(dados_matriculas$ano), decreasing = TRUE) 

# 2. Interface (UI) simplificada (sem abas)
ui <- page_sidebar(
  title = "Dashboard Educacional - Paraná",
  theme = bs_theme(preset = "flatly"),
  
  sidebar = sidebar(
    title = "Controles",
    
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
  
  # Apenas a tabela na área principal
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

# 3. Lógica do Servidor
server <- function(input, output, session) {
  
  # Define qual coluna de matrícula usar com base na etapa escolhida
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
  
  # Gera a Tabela
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