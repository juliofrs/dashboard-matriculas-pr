library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(janitor)
library(DT)
library(tidyr)
library(leaflet)
library(sf)
library(geobr)

# 1. Carregando os Dados do Excel
caminho_arquivo <- "Matriculas_Municipio_AI_AF_EM_populacao.xlsx"
dados_matriculas <- read_excel(caminho_arquivo, sheet = "Base") |> clean_names()

# Garante que o código do município seja numérico padrão nos dados
dados_matriculas$codigo_municipio <- as.numeric(dados_matriculas$codigo_municipio)

lista_anos <- sort(unique(dados_matriculas$ano), decreasing = TRUE) 

# 2. Carregando o Mapa Geográfico do Paraná (IBGE)
mapa_pr <- read_municipality(code_muni = "PR", year = 2020, showProgress = FALSE)
mapa_pr$code_muni <- as.numeric(mapa_pr$code_muni)

# 3. Interface (UI)
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
    radioButtons(
      inputId = "tipo_valor",
      label = "Formato da Tabela:",
      choices = c("Números Absolutos" = "absoluto", "Percentuais (%)" = "percentual"),
      selected = "absoluto"
    ),
    p(class = "text-muted", "Dica: O mapa exibe o volume total da etapa escolhida para cada município.")
  ),
  
  # --- ABA 1: MAPA ---
  nav_panel(
    title = "Mapa de Calor (Matrículas)",
    card(
      full_screen = TRUE,
      card_header("Densidade de Matrículas por Município (Passe o mouse)"),
      leafletOutput("mapa_interativo", height = "600px")
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

# 4. Lógica do Servidor
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
  
  # --- CONSTRUÇÃO DO MAPA ---
  output$mapa_interativo <- renderLeaflet({
    
    # Agrupa por código do município somando as matrículas do ano escolhido
    dados_mapa <- dados_reativos() |> 
      filter(ano == input$ano_selecionado) |>
      group_by(codigo_municipio) |> 
      summarise(total_mun = sum(valor_etapa, na.rm = TRUE)) |>
      ungroup()
    
    # Junta os dados geográficos com os dados numéricos
    mapa_pr_dados <- mapa_pr |>
      left_join(dados_mapa, by = c("code_muni" = "codigo_municipio"))
    
    # Trata eventuais valores NA para evitar erros no Leaflet
    mapa_pr_dados$total_mun[is.na(mapa_pr_dados$total_mun)] <- 0
    
    # Paleta de cores segura
    pal <- colorNumeric("YlGnBu", domain = mapa_pr_dados$total_mun)
    
    # Textos do pop-up ao passar o mouse
    labels_mapa <- sprintf(
      "<strong>%s</strong><br/>%s matrículas",
      mapa_pr_dados$name_muni, format(mapa_pr_dados$total_mun, big.mark = ".", scientific = FALSE)
    ) |> lapply(htmltools::HTML)
    
    leaflet(mapa_pr_dados) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal(total_mun),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.8,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        label = labels_mapa,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) |>
      addLegend(pal = pal, values = ~total_mun, opacity = 0.7, title = "Total", position = "bottomright")
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