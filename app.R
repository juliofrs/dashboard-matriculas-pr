library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(DT)
library(tidyr) # Pacote novo para transformar linhas em colunas

# 1. Carregar os dados apenas da "Base"
caminho_arquivo <- "Matriculas_Municipio_AI_AF_EM_populacao.xlsx"
dados_matriculas <- read_excel(caminho_arquivo, sheet = "Base") |> clean_names()

lista_municipios <- sort(unique(dados_matriculas$municipio))
# Criar uma lista com os anos disponíveis na base (ordenada do mais recente para o mais antigo)
lista_anos <- sort(unique(dados_matriculas$ano), decreasing = TRUE) 

ui <- page_navbar(
  title = "Dashboard Educacional - Paraná",
  theme = bs_theme(preset = "flatly"),
  
  sidebar = sidebar(
    title = "Filtro do Gráfico",
    selectInput(
      inputId = "municipio_selecionado",
      label = "Escolha o Município:",
      choices = lista_municipios,
      selected = "Curitiba"
    )
  ),
  
  nav_panel(
    title = "Evolução das Matrículas", 
    card(
      card_header("Histórico por Rede de Ensino"),
      plotOutput("grafico_matriculas")
    )
  ),
  
  # --- ABA 2: Tabela Dinâmica por Ano ---
  nav_panel(
    title = "Tabela por Ano",
    card(
      card_header(
        # Colocamos o filtro de Ano diretamente no cabeçalho desta tabela
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

server <- function(input, output, session) {
  
  # Gráfico: Filtra pelo Município escolhido na barra lateral
  dados_filtrados <- reactive({
    dados_matriculas |> filter(municipio == input$municipio_selecionado)
  })
  
  output$grafico_matriculas <- renderPlot({
    ggplot(dados_filtrados(), aes(x = as.factor(ano), y = total_matriculas, fill = rede)) +
      geom_col(position = "dodge") +
      theme_minimal() +
      labs(x = "Ano", y = "Total de Matrículas", fill = "Rede de Ensino") +
      theme(text = element_text(size = 14))
  })
  
  # Tabela: Construção dinâmica baseada no Ano escolhido
  output$tabela_dinamica <- renderDT({
    
    # 1. Filtra a base apenas para o ano escolhido e seleciona as colunas de interesse
    dados_ano <- dados_matriculas |> 
      filter(ano == input$ano_selecionado) |>
      select(municipio, rede, total_matriculas, populacao_2024)
    
    # 2. Transforma as linhas de "Rede" em Colunas (Pivot)
    tabela_larga <- dados_ano |>
      pivot_wider(
        names_from = rede, 
        values_from = total_matriculas,
        values_fill = 0 # Se não houver matrículas numa rede, preenche com 0
      ) |>
      clean_names() 
    
    # 3. Garante que todas as colunas existam (evita erro se num ano não houver rede federal, por exemplo)
    if(!"federal" %in% names(tabela_larga)) tabela_larga$federal <- 0
    if(!"municipal" %in% names(tabela_larga)) tabela_larga$municipal <- 0
    if(!"estadual" %in% names(tabela_larga)) tabela_larga$estadual <- 0
    if(!"privada" %in% names(tabela_larga)) tabela_larga$privada <- 0
    
    # 4. Calcula os Totais (Público e Geral)
    tabela_final <- tabela_larga |>
      mutate(
        publico = estadual + municipal + federal,
        total = publico + privada
      ) |>
      # Organiza a ordem das colunas
      select(municipio, estadual, municipal, federal, publico, privada, total, populacao_2024) |>
      # Ordena a tabela do maior total para o menor
      arrange(desc(total))
    
    # 5. Gera a tabela interativa
    datatable(
      tabela_final,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json')
      ),
      colnames = c("Município", "Estadual", "Municipal", "Federal", "Público", "Privado", "Total", "População (2024)")
    )
  })
}

shinyApp(ui, server)