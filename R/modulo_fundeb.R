# =============================================================================
# R/modulo_fundeb.R: MÓDULO INDEPENDENTE DE FINANCIAMENTO DA EDUCAÇÃO (FUNDEB)
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(leaflet)
  library(DT)
  library(htmltools)
})

# Dicionário de rótulos e descrições metodológicas dos indicadores
METADADOS_INDICADORES_FUNDEB <- list(
  receita_total_prevista = list(
    titulo = "Total das Receitas Previstas",
    curto  = "Receita Total Prevista",
    desc   = "Soma da receita de contribuição municipal ao Fundeb com as complementações da União previstas para o exercício."
  ),
  complementacao_uniao_total = list(
    titulo = "Complementação da União Total",
    curto  = "Complementação União Total",
    desc   = "Total de recursos federais previstos para complementar o fundo municipal (valor consolidado informado pela fonte oficial)."
  ),
  complementacao_vaar = list(
    titulo = "Complementação VAAR",
    curto  = "Complementação VAAR",
    desc   = "Valor Anual por Aluno-Resultado: complementação condicionada à melhoria de gestão e indicadores de aprendizagem e equidade."
  ),
  complementacao_vaat = list(
    titulo = "Complementação VAAT",
    curto  = "Complementação VAAT",
    desc   = "Valor Anual Total por Aluno: complementação destinada às redes cuja receita total por aluno não atinge o mínimo nacional."
  ),
  complementacao_vaaf = list(
    titulo = "Complementação VAAF",
    curto  = "Complementação VAAF",
    desc   = "Valor Anual por Aluno-Fundo: complementação destinada aos fundos estaduais abaixo do patamar mínimo nacional definido pela União."
  ),
  receita_contribuicao_fundeb = list(
    titulo = "Receita da Contribuição ao Fundeb",
    curto  = "Contribuição ao Fundeb",
    desc   = "Receita oriunda da cesta de impostos e transferências constitucionais vinculadas ao fundo no âmbito municipal."
  )
)

# =============================================================================
# 1. CONTROLES LATERAIS DO MÓDULO (EXIBIDOS NA SIDEBAR NA ABA FUNDEB)
# =============================================================================
fundeb_sidebar_ui <- function(id, lista_anos_fundeb) {
  ns <- NS(id)
  
  tagList(
    div(
      actionButton(ns("limpar_fundeb"), "↺ Restaurar Filtros FUNDEB", class = "btn btn-limpar-filtros mb-2")
    ),
    
    div(class = "sidebar-section-title", "Período & Território"),
    selectInput(
      ns("ano_fundeb"),
      "Ano de Referência:",
      choices = sort(unique(lista_anos_fundeb), decreasing = TRUE),
      selected = max(lista_anos_fundeb)
    ),
    selectInput(
      ns("nivel_territorial"),
      "Nível Territorial:",
      choices = c(
        "Paraná (399 Municípios)" = "parana",
        "Região do PPA"           = "ppa",
        "Núcleo Regional (NRE)"   = "nre",
        "Município Específico"    = "municipio"
      ),
      selected = "parana"
    ),
    uiOutput(ns("ui_localidade_dinamica")),
    
    div(class = "sidebar-section-title", "Indicador Financeiro"),
    selectInput(
      ns("indicador_fundeb"),
      "Variável de Análise:",
      choices = c(
        "Total das receitas previstas"       = "receita_total_prevista",
        "Complementação da União Total"      = "complementacao_uniao_total",
        "Complementação VAAR"                = "complementacao_vaar",
        "Complementação VAAT"                = "complementacao_vaat",
        "Complementação VAAF"                = "complementacao_vaaf",
        "Receita da contribuição ao Fundeb"  = "receita_contribuicao_fundeb"
      ),
      selected = "receita_total_prevista"
    ),
    
    div(class = "sidebar-section-title", "Cobertura da Fonte"),
    uiOutput(ns("sidebar_cobertura_fundeb"))
  )
}

# =============================================================================
# 2. INTERFACE PRINCIPAL DO MÓDULO (ÁREA DE CONTEÚDO)
# =============================================================================
fundeb_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Cabeçalho Institucional da Seção
    div(
      class = "mb-3 p-3 bg-white border rounded",
      style = "display: flex; justify-content: space-between; align-items: center; flex-wrap: wrap; gap: 12px;",
      div(
        h4(style = "margin: 0; font-weight: 700; color: var(--pr-green-dark); letter-spacing: -0.01em;",
           "FINANCIAMENTO DA EDUCAÇÃO | FUNDEB"),
        span(style = "color: var(--text-secondary); font-size: 0.88rem;",
             "Acompanhamento das receitas previstas e complementações da União nos municípios do Paraná.")
      ),
      div(
        style = "display: flex; align-items: center; gap: 8px; flex-wrap: wrap;",
        uiOutput(ns("badge_cobertura_geral"))
      )
    ),
    
    # ÁREA 1 — INDICADORES EXECUTIVOS (5 KPIS MUNICIPAIS RESPONSIVOS)
    layout_column_wrap(
      width = "210px",
      fill = FALSE,
      uiOutput(ns("card_kpi_vaaf")),
      uiOutput(ns("card_kpi_vaat")),
      uiOutput(ns("card_kpi_vaar")),
      uiOutput(ns("card_kpi_uniao")),
      uiOutput(ns("card_kpi_total"))
    ),
    
    # ÁREA 2 — REFERÊNCIA ESTADUAL SEPARADA + PERSPECTIVA TERRITORIAL
    layout_column_wrap(
      width = "420px",
      fill = FALSE,
      div(
        class = "card",
        style = "border-left: 4px solid #0284C7;",
        div(
          class = "card-header d-flex justify-content-between align-items-center flex-wrap gap-2",
          span("Referência Separada: GOVERNO DO ESTADO (Código IBGE 41)"),
          span(class = "badge", style = "background-color: #E0F2FE; color: #0369A1;", "Não somado aos municípios")
        ),
        div(
          class = "card-body py-3",
          uiOutput(ns("painel_governo_estado"))
        )
      ),
      div(
        class = "card",
        style = "border-left: 4px solid var(--pr-green-dark);",
        div(
          class = "card-header d-flex justify-content-between align-items-center flex-wrap gap-2",
          span("Análise da Perspectiva Territorial & Receita por Matrícula Municipal"),
          uiOutput(ns("badge_perspectiva"))
        ),
        div(
          class = "card-body py-3",
          uiOutput(ns("painel_perspectiva_ativa"))
        )
      )
    ),
    
    # ÁREA 3 — VISUALIZAÇÕES ANALÍTICAS (LINHA 1: COMPOSIÇÃO E RANKING)
    layout_column_wrap(
      width = "450px",
      fill = FALSE,
      div(
        class = "card",
        div(class = "card-header", "A. Composição das Complementações da União (VAAF, VAAT e VAAR)"),
        div(
          class = "card-body",
          uiOutput(ns("aviso_composicao")),
          plotlyOutput(ns("grafico_composicao_uniao"), height = "360px")
        )
      ),
      div(
        class = "card",
        div(
          class = "card-header d-flex justify-content-between align-items-center gap-2",
          span("B. Ranking de Municípios no Indicador Selecionado"),
          selectInput(
            ns("top_n_fundeb"),
            label = NULL,
            choices = c("Top 10" = "10", "Top 20" = "20", "Todos do recorte" = "todos"),
            selected = "10",
            width = "165px"
          )
        ),
        div(
          class = "card-body",
          uiOutput(ns("aviso_ranking_fundeb")),
          uiOutput(ns("ui_plot_ranking_fundeb"))
        )
      )
    ),
    
    # ÁREA 3 — VISUALIZAÇÕES ANALÍTICAS (LINHA 2: COMPARAÇÃO REGIONAL E MAPA)
    layout_column_wrap(
      width = "450px",
      fill = FALSE,
      div(
        class = "card",
        div(
          class = "card-header d-flex justify-content-between align-items-center flex-wrap gap-2",
          span("C. Comparação Regional (Macrorregiões PPA e NREs)"),
          radioButtons(
            ns("eixo_regional_fundeb"),
            label = NULL,
            choices = c("Por Região PPA" = "ppa", "Por NRE (Top 15)" = "nre"),
            selected = "ppa",
            inline = TRUE
          )
        ),
        div(
          class = "card-body",
          plotlyOutput(ns("grafico_regional_fundeb"), height = "380px")
        )
      ),
      div(
        class = "card",
        div(class = "card-header", "D. Distribuição Geográfica no Paraná (Valores Previstos em R$)"),
        div(
          class = "card-body",
          leafletOutput(ns("mapa_fundeb"), height = "380px")
        )
      )
    ),
    
    # ÁREA 4 — TABELA DETALHADA DOS MUNICÍPIOS
    div(
      class = "card",
      div(
        class = "card-header d-flex justify-content-between align-items-center flex-wrap gap-2",
        span("Tabela Detalhada do Financiamento Municipal (Ordenada pelo Indicador Selecionado)"),
        span(style = "font-size: 0.78rem; color: var(--text-secondary);",
             "Valores R$ 0,00 representam zero efetivo; 'Não informado' indica ausência na fonte oficial.")
      ),
      div(
        class = "card-body",
        DTOutput(ns("tabela_fundeb"))
      )
    )
  )
}

# =============================================================================
# 3. SERVIDOR DO MÓDULO (SERVER)
# =============================================================================
fundeb_server <- function(id, dados_fundeb_mun, dados_fundeb_estado, dados_matriculas, mapa_pr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    lista_ppa <- sort(unique(na.omit(dados_fundeb_mun$regiao_ppa)))
    lista_nre <- sort(unique(na.omit(dados_fundeb_mun$nome_nre)))
    cad_munis <- dados_fundeb_mun |>
      select(codigo_ibge, municipio) |>
      distinct() |>
      arrange(municipio)
    choices_munis <- setNames(cad_munis$codigo_ibge, cad_munis$municipio)
    
    observeEvent(input$limpar_fundeb, {
      updateSelectInput(session, "ano_fundeb", selected = max(dados_fundeb_mun$ano))
      updateSelectInput(session, "nivel_territorial", selected = "parana")
      updateSelectInput(session, "indicador_fundeb", selected = "receita_total_prevista")
      updateSelectInput(session, "top_n_fundeb", selected = "10")
    })
    
    output$ui_localidade_dinamica <- renderUI({
      nivel <- input$nivel_territorial
      if (is.null(nivel) || nivel == "parana") {
        return(div(
          class = "p-2 rounded mb-2",
          style = "background-color: var(--pr-green-light); color: var(--pr-green-dark); font-size: 0.78rem; border: 1px solid #C6E2D0;",
          "Escopo estadual ativo: consolidando todos os 399 municípios do Paraná."
        ))
      }
      if (nivel == "ppa") {
        return(selectInput(ns("localidade_fundeb"), "Região do PPA:", choices = lista_ppa, selected = lista_ppa[1]))
      }
      if (nivel == "nre") {
        return(selectInput(ns("localidade_fundeb"), "Núcleo Regional (NRE):", choices = lista_nre, selected = lista_nre[1]))
      }
      if (nivel == "municipio") {
        sel_default <- if ("4106902" %in% choices_munis) "4106902" else choices_munis[1]
        return(selectizeInput(ns("localidade_fundeb"), "Município:", choices = choices_munis, selected = sel_default))
      }
    })
    
    # Valida que a localidade pertence ao nível territorial selecionado (evita estados transitórios)
    localidade_ativa <- reactive({
      nivel <- input$nivel_territorial
      loc   <- input$localidade_fundeb
      if (is.null(nivel) || nivel == "parana" || is.null(loc)) return(NULL)
      if (nivel == "ppa" && loc %in% lista_ppa) return(loc)
      if (nivel == "nre" && loc %in% lista_nre) return(loc)
      if (nivel == "municipio" && loc %in% cad_munis$codigo_ibge) return(loc)
      NULL
    })
    
    base_ano <- reactive({
      ano_sel <- as.integer(input$ano_fundeb)
      dados_fundeb_mun |> filter(ano == ano_sel)
    })
    
    base_recorte <- reactive({
      df <- base_ano()
      nivel <- input$nivel_territorial
      loc   <- localidade_ativa()
      
      if (nivel == "ppa" && !is.null(loc)) {
        df <- df |> filter(regiao_ppa == loc)
      } else if (nivel == "nre" && !is.null(loc)) {
        df <- df |> filter(nome_nre == loc)
      } else if (nivel == "municipio" && !is.null(loc)) {
        df <- df |> filter(codigo_ibge == loc)
      }
      df
    })
    
    output$sidebar_cobertura_fundeb <- renderUI({
      df  <- base_recorte()
      ind <- input$indicador_fundeb
      res <- resumir_variavel_fundeb(df[[ind]])
      
      cor_bg <- if (res$n_validos == 0) "#FDE8E8" else if (res$parcial) "#FEF3C7" else "#E8F3EC"
      cor_tx <- if (res$n_validos == 0) "#9B1C1C" else if (res$parcial) "#92400E" else "#14532D"
      
      div(
        class = "p-2 rounded",
        style = sprintf("background-color: %s; color: %s; font-size: 0.78rem; border: 1px solid rgba(0,0,0,0.08);", cor_bg, cor_tx),
        strong("Indicador selecionado:"), br(),
        span(res$cobertura_txt)
      )
    })
    
    output$badge_cobertura_geral <- renderUI({
      df  <- base_recorte()
      ind <- input$indicador_fundeb
      res <- resumir_variavel_fundeb(df[[ind]])
      
      cor_bg <- if (res$n_validos == 0) "#FDE8E8" else if (res$parcial) "#FEF3C7" else "#E8F3EC"
      cor_tx <- if (res$n_validos == 0) "#9B1C1C" else if (res$parcial) "#92400E" else "#14532D"
      
      span(
        class = "badge",
        style = sprintf("background-color: %s; color: %s; font-size: 0.82rem; padding: 0.45rem 0.8rem; border: 1px solid rgba(0,0,0,0.08);", cor_bg, cor_tx),
        paste0("Exercício ", input$ano_fundeb, " • ", res$cobertura_txt)
      )
    })
    
    kpis_fundeb <- reactive({
      calcular_kpis_fundeb(
        dados_fundeb_mun,
        ano_sel    = as.integer(input$ano_fundeb),
        nivel      = input$nivel_territorial,
        localidade = localidade_ativa()
      )
    })
    
    render_card_fundeb <- function(chave_var, borda_cor) {
      meta <- METADADOS_INDICADORES_FUNDEB[[chave_var]]
      k    <- kpis_fundeb()[[chave_var]]
      
      valor_txt <- if (k$n_validos == 0) "Não informado" else formatar_moeda_br(k$soma)
      
      badge_class <- switch(k$var_tipo,
                            "positive" = "kpi-badge kpi-badge-positive",
                            "negative" = "kpi-badge kpi-badge-negative",
                            "kpi-badge kpi-badge-neutral"
      )
      
      cobertura_badge <- if (k$n_validos == 0) {
        div(style = "font-size: 0.72rem; color: #9B1C1C; font-weight: 600; margin-top: 2px;",
            sprintf("⚠️ Ausente em todos os %d municípios", k$n_total))
      } else if (k$parcial) {
        div(style = "font-size: 0.72rem; color: #92400E; font-weight: 600; margin-top: 2px;",
            sprintf("⚠️ Soma parcial (%d/%d municípios)", k$n_validos, k$n_total))
      } else {
        div(style = "font-size: 0.72rem; color: var(--pr-green-main); font-weight: 500; margin-top: 2px;",
            sprintf("✓ Integral (%d/%d municípios)", k$n_validos, k$n_total))
      }
      
      part_txt <- if (!is.na(k$participacao_pct)) {
        sprintf("Representa %.2f%% da receita prevista", k$participacao_pct)
      } else if (chave_var == "receita_total_prevista") {
        "Base consolidada do recorte territorial"
      } else {
        "Participação não calculada (dado ausente)"
      }
      
      div(
        class = "kpi-container",
        style = paste0("border-left-color: ", borda_cor, ";"),
        div(
          class = "kpi-title",
          span(meta$curto),
          span(style = "cursor: help; color: #94A3B8; font-size: 0.85rem;", title = meta$desc, "ⓘ")
        ),
        div(class = "kpi-value", style = "font-size: 1.28rem;", valor_txt),
        div(class = badge_class, style = "font-size: 0.70rem;", k$var_txt),
        div(class = "kpi-subtext", part_txt),
        cobertura_badge
      )
    }
    
    output$card_kpi_vaaf  <- renderUI({ render_card_fundeb("complementacao_vaaf", "#6366F1") })
    output$card_kpi_vaat  <- renderUI({ render_card_fundeb("complementacao_vaat", "#0284C7") })
    output$card_kpi_vaar  <- renderUI({ render_card_fundeb("complementacao_vaar", "#0D9488") })
    output$card_kpi_uniao <- renderUI({ render_card_fundeb("complementacao_uniao_total", "#D97706") })
    output$card_kpi_total <- renderUI({ render_card_fundeb("receita_total_prevista", "#14532D") })
    
    output$painel_governo_estado <- renderUI({
      ano_sel <- as.integer(input$ano_fundeb)
      gov <- dados_fundeb_estado |> filter(ano == ano_sel)
      if (nrow(gov) == 0) return(p("Registro estadual indisponível."))
      gov <- gov[1, ]
      
      div(
        style = "display: grid; grid-template-columns: repeat(2, 1fr); gap: 10px; font-size: 0.84rem;",
        div(
          span(style = "color: var(--text-secondary);", "Contribuição Estadual ao Fundeb:"), br(),
          strong(formatar_moeda_br(gov$receita_contribuicao_fundeb))
        ),
        div(
          span(style = "color: var(--text-secondary);", "Total das Receitas Previstas (Estado):"), br(),
          strong(style = "color: #0369A1;", formatar_moeda_br(gov$receita_total_prevista))
        ),
        div(
          span(style = "color: var(--text-secondary);", "Complementação VAAR (Estado):"), br(),
          strong(formatar_moeda_br(gov$complementacao_vaar))
        ),
        div(
          span(style = "color: var(--text-secondary);", "VAAF / VAAT (Estado):"), br(),
          strong(paste0(formatar_moeda_br(gov$complementacao_vaaf), " / ", formatar_moeda_br(gov$complementacao_vaat)))
        )
      )
    })
    
    output$badge_perspectiva <- renderUI({
      lbl <- switch(input$nivel_territorial,
                    "parana"    = "Perspectiva: Estado (399 Municípios)",
                    "ppa"       = paste("Perspectiva PPA:", localidade_ativa()),
                    "nre"       = paste("Perspectiva NRE:", localidade_ativa()),
                    "municipio" = "Perspectiva Municipal Individual"
      )
      span(class = "badge", style = "background-color: var(--pr-green-light); color: var(--pr-green-dark);", lbl)
    })
    
    output$painel_perspectiva_ativa <- renderUI({
      ano_sel <- as.integer(input$ano_fundeb)
      df_rec  <- base_recorte()
      df_all  <- base_ano()
      
      df_per_mat <- calcular_indicadores_por_matricula_municipal(dados_fundeb_mun, dados_matriculas, ano_sel)
      
      if (input$nivel_territorial == "municipio" && !is.null(localidade_ativa()) && nrow(df_rec) == 1) {
        m <- df_rec[1, ]
        ind <- input$indicador_fundeb
        
        validos_est <- df_all |> filter(!is.na(.data[[ind]])) |> arrange(desc(.data[[ind]]))
        pos_rank <- which(validos_est$codigo_ibge == m$codigo_ibge)
        rank_txt <- if (length(pos_rank) == 1) sprintf("%dº de %d municípios informados", pos_rank, nrow(validos_est)) else "Não ranqueado (valor não informado)"
        
        bloco_mat <- if (!is.null(df_per_mat)) {
          m_mat <- df_per_mat |> filter(codigo_ibge == m$codigo_ibge)
          if (nrow(m_mat) == 1 && !is.na(m_mat$receita_prevista_por_mat_mun[1])) {
            div(
              style = "margin-top: 8px; padding-top: 8px; border-top: 1px dashed #CBD5E1; font-size: 0.78rem;",
              strong("Previsão por Matrícula da Rede Municipal (", ano_sel, "): "),
              span(formatar_moeda_br(m_mat$receita_prevista_por_mat_mun[1])),
              span(style = "color: var(--text-secondary);",
                   sprintf(" (%s matrículas municipais; indicador de previsão orçamentária, não repasse líquido efetivo).",
                           format(m_mat$matriculas_rede_municipal[1], big.mark = ".")))
            )
          } else NULL
        } else {
          div(
            style = "margin-top: 8px; padding-top: 8px; border-top: 1px dashed #CBD5E1; font-size: 0.76rem; color: var(--text-secondary);",
            "Indicador por matrícula indisponível para 2026 (aguardando Censo Escolar 2026 para compatibilidade temporal)."
          )
        }
        
        return(div(
          style = "font-size: 0.83rem;",
          div(
            style = "display: flex; justify-content: space-between; flex-wrap: wrap; gap: 6px;",
            span(strong(m$municipio), sprintf(" — PPA %s | NRE %s", m$regiao_ppa, m$nome_nre)),
            span(style = "color: var(--pr-green-dark); font-weight: 600;", paste("Ranking Estadual:", rank_txt))
          ),
          div(
            style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px; margin-top: 8px;",
            div(span(style = "color: var(--text-secondary);", "Contribuição Fundeb:"), br(), strong(formatar_moeda_br(m$receita_contribuicao_fundeb))),
            div(span(style = "color: var(--text-secondary);", "Comp. União Total:"), br(), strong(formatar_moeda_br(m$complementacao_uniao_total))),
            div(span(style = "color: var(--text-secondary);", "% União / Receita:"), br(),
                strong(ifelse(is.na(m$pct_uniao_receita), "Não calculado", sprintf("%.2f%%", m$pct_uniao_receita))))
          ),
          bloco_mat
        ))
      }
      
      tot_contrib <- resumir_variavel_fundeb(df_rec$receita_contribuicao_fundeb)
      tot_uniao   <- resumir_variavel_fundeb(df_rec$complementacao_uniao_total)
      tot_prev    <- resumir_variavel_fundeb(df_rec$receita_total_prevista)
      
      pct_uniao_rec <- if (!tot_uniao$parcial && !is.na(tot_prev$soma) && tot_prev$soma > 0) {
        sprintf("%.2f%%", (tot_uniao$soma / tot_prev$soma) * 100)
      } else {
        "Não calculado (cobertura parcial)"
      }
      
      nota_mat <- if (!is.null(df_per_mat)) {
        sub_mat <- df_per_mat |> filter(codigo_ibge %in% df_rec$codigo_ibge)
        soma_rec <- sum(sub_mat$receita_total_prevista, na.rm = TRUE)
        soma_al  <- sum(sub_mat$matriculas_rede_municipal, na.rm = TRUE)
        if (soma_al > 0) {
          sprintf("Receita prevista / matrícula municipal (%d): %s (%s alunos na rede municipal; valor previsto).",
                  ano_sel, formatar_moeda_br(soma_rec / soma_al), format(soma_al, big.mark = "."))
        } else ""
      } else {
        "Indicadores por matrícula desabilitados em 2026 para evitar mistura de exercícios diferentes (Censo vai até 2025)."
      }
      
      div(
        style = "font-size: 0.83rem;",
        div(
          style = "display: grid; grid-template-columns: repeat(3, 1fr); gap: 8px;",
          div(span(style = "color: var(--text-secondary);", "Municípios no Recorte:"), br(), strong(sprintf("%d municípios", nrow(df_rec)))),
          div(span(style = "color: var(--text-secondary);", "Contribuição Municipal:"), br(), strong(formatar_moeda_br(tot_contrib$soma))),
          div(span(style = "color: var(--text-secondary);", "% União / Total Previsto:"), br(), strong(pct_uniao_rec))
        ),
        div(style = "margin-top: 8px; padding-top: 8px; border-top: 1px dashed #CBD5E1; font-size: 0.76rem; color: var(--text-secondary);",
            nota_mat)
      )
    })
    
    # A. COMPOSIÇÃO DAS COMPLEMENTAÇÕES (VAAF, VAAT, VAAR)
    output$aviso_composicao <- renderUI({
      df <- base_recorte()
      n_vaaf <- sum(!is.na(df$complementacao_vaaf))
      n_vaat <- sum(!is.na(df$complementacao_vaat))
      n_vaar <- sum(!is.na(df$complementacao_vaar))
      n_tot  <- nrow(df)
      
      if (n_vaaf < n_tot || n_vaat < n_tot || n_vaar < n_tot) {
        div(
          class = "alert alert-warning py-1 px-2 mb-2",
          style = "font-size: 0.76rem;",
          sprintf("⚠️ Cobertura parcial em %s: VAAF informado em %d/%d, VAAT em %d/%d e VAAR em %d/%d municípios. Valores ausentes não foram convertidos em zero.",
                  input$ano_fundeb, n_vaaf, n_tot, n_vaat, n_tot, n_vaar, n_tot)
        )
      } else NULL
    })
    
    output$grafico_composicao_uniao <- renderPlotly({
      df <- base_recorte()
      
      df_comp <- df |>
        mutate(
          soma_disp = rowSums(cbind(complementacao_vaaf, complementacao_vaat, complementacao_vaar), na.rm = TRUE),
          algum_valido = !is.na(complementacao_vaaf) | !is.na(complementacao_vaat) | !is.na(complementacao_vaar)
        ) |>
        filter(algum_valido, soma_disp > 0) |>
        arrange(desc(soma_disp)) |>
        head(12)
      
      if (nrow(df_comp) == 0) {
        return(
          plot_ly() |>
            layout(
              annotations = list(
                text = "Sem complementações positivas informadas para composição neste recorte.",
                xref = "paper", yref = "paper", showarrow = FALSE,
                font = list(size = 13, color = "#64748B")
              ),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF"
            ) |> config(displayModeBar = FALSE)
        )
      }
      
      df_comp$municipio <- factor(df_comp$municipio, levels = rev(df_comp$municipio))
      
      plot_ly(df_comp, y = ~municipio) |>
        add_bars(
          x = ~ifelse(is.na(complementacao_vaaf), 0, complementacao_vaaf),
          name = "VAAF",
          marker = list(color = "#6366F1"),
          customdata = ~formatar_moeda_br(complementacao_vaaf),
          hovertemplate = "<b>%{y}</b><br>VAAF: %{customdata}<extra></extra>"
        ) |>
        add_bars(
          x = ~ifelse(is.na(complementacao_vaat), 0, complementacao_vaat),
          name = "VAAT",
          marker = list(color = "#0284C7"),
          customdata = ~formatar_moeda_br(complementacao_vaat),
          hovertemplate = "<b>%{y}</b><br>VAAT: %{customdata}<extra></extra>"
        ) |>
        add_bars(
          x = ~ifelse(is.na(complementacao_vaar), 0, complementacao_vaar),
          name = "VAAR",
          marker = list(color = "#228B52"),
          customdata = ~formatar_moeda_br(complementacao_vaar),
          hovertemplate = "<b>%{y}</b><br>VAAR: %{customdata}<extra></extra>"
        ) |>
        layout(
          barmode = "stack",
          margin = list(l = 130, r = 20, t = 10, b = 40),
          xaxis = list(title = "Valor Previsto (R$)"),
          yaxis = list(title = "", dtick = 1),
          legend = list(orientation = "h", x = 0, y = 1.12),
          paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
          font = list(family = "Inter, sans-serif")
        ) |> config(displayModeBar = FALSE)
    })
    
    # B. RANKING DE MUNICÍPIOS (ALTURA RESPONSIVA PARA TOP 10, TOP 20 OU TODOS)
    output$aviso_ranking_fundeb <- renderUI({
      df  <- if (input$nivel_territorial == "municipio") base_ano() else base_recorte()
      ind <- input$indicador_fundeb
      n_aus <- sum(is.na(df[[ind]]))
      if (n_aus > 0) {
        div(
          style = "font-size: 0.75rem; color: #92400E; margin-bottom: 4px;",
          sprintf("Nota: %d município(s) com dado 'Não informado' omitido(s) das barras.", n_aus)
        )
      } else NULL
    })
    
    output$ui_plot_ranking_fundeb <- renderUI({
      if (input$top_n_fundeb == "todos") {
        div(
          style = "max-height: 360px; overflow-y: auto; border: 1px solid #F1F5F9; border-radius: 6px;",
          plotlyOutput(ns("grafico_ranking_fundeb"), height = "4800px")
        )
      } else {
        plotlyOutput(ns("grafico_ranking_fundeb"), height = "360px")
      }
    })
    
    output$grafico_ranking_fundeb <- renderPlotly({
      df  <- if (input$nivel_territorial == "municipio") base_ano() else base_recorte()
      ind <- input$indicador_fundeb
      
      df_valido <- df |>
        filter(!is.na(.data[[ind]])) |>
        mutate(valor_plot = as.numeric(.data[[ind]])) |>
        arrange(desc(valor_plot), municipio)
      
      if (nrow(df_valido) == 0) {
        return(
          plot_ly() |>
            layout(
              annotations = list(
                text = "Nenhum valor informado para este indicador no exercício selecionado.",
                xref = "paper", yref = "paper", showarrow = FALSE,
                font = list(size = 13, color = "#64748B")
              ),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF"
            ) |> config(displayModeBar = FALSE)
        )
      }
      
      if (input$top_n_fundeb != "todos") {
        n_lim <- as.integer(input$top_n_fundeb)
        df_valido <- head(df_valido, n_lim)
      }
      
      df_valido$municipio <- factor(df_valido$municipio, levels = rev(df_valido$municipio))
      n_rows <- nrow(df_valido)
      cores  <- rep("#228B52", n_rows)
      if (n_rows >= 1) cores[n_rows]     <- "#14532D"
      if (n_rows >= 2) cores[n_rows - 1] <- "#166534"
      if (n_rows >= 3) cores[n_rows - 2] <- "#15803D"
      
      if (input$nivel_territorial == "municipio" && !is.null(localidade_ativa())) {
        idx_sel <- which(df_valido$codigo_ibge == localidade_ativa())
        if (length(idx_sel) == 1) cores[idx_sel] <- "#D97706"
      }
      
      plot_ly(
        data = df_valido,
        x = ~valor_plot,
        y = ~municipio,
        type = "bar",
        orientation = "h",
        marker = list(color = cores),
        customdata = ~formatar_moeda_br(valor_plot),
        hovertemplate = "<b>%{y}</b><br>Valor Previsto: <b>%{customdata}</b><extra></extra>"
      ) |>
        layout(
          margin = list(l = 145, r = 25, t = 10, b = 40),
          xaxis = list(title = "Valor Previsto (R$)"),
          yaxis = list(title = "", dtick = 1),
          paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
          font = list(family = "Inter, sans-serif")
        ) |> config(displayModeBar = FALSE)
    })
    
    # C. COMPARAÇÃO REGIONAL (PPA E NRE)
    output$grafico_regional_fundeb <- renderPlotly({
      df  <- base_ano()
      ind <- input$indicador_fundeb
      eixo <- input$eixo_regional_fundeb
      
      grupo_col <- if (eixo == "ppa") "regiao_ppa" else "nome_nre"
      
      resumo_reg <- df |>
        group_by(territorio = .data[[grupo_col]]) |>
        summarise(
          n_total   = n(),
          n_validos = sum(!is.na(.data[[ind]])),
          soma_val  = ifelse(n_validos == 0, NA_real_, sum(.data[[ind]], na.rm = TRUE)),
          .groups   = "drop"
        ) |>
        filter(!is.na(soma_val)) |>
        arrange(desc(soma_val))
      
      if (eixo == "nre") {
        resumo_reg <- head(resumo_reg, 15)
      }
      
      if (nrow(resumo_reg) == 0) {
        return(
          plot_ly() |>
            layout(
              annotations = list(
                text = "Sem dados informados para agregação regional neste exercício.",
                xref = "paper", yref = "paper", showarrow = FALSE,
                font = list(size = 13, color = "#64748B")
              ),
              xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
              paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF"
            ) |> config(displayModeBar = FALSE)
        )
      }
      
      resumo_reg$territorio <- factor(resumo_reg$territorio, levels = rev(resumo_reg$territorio))
      resumo_reg$cobertura_label <- ifelse(
        resumo_reg$n_validos < resumo_reg$n_total,
        sprintf("Parcial (%d/%d mun.)", resumo_reg$n_validos, resumo_reg$n_total),
        sprintf("Integral (%d/%d mun.)", resumo_reg$n_validos, resumo_reg$n_total)
      )
      resumo_reg$hover_txt <- sprintf(
        "<b>%s</b><br>Soma: <b>%s</b><br>Cobertura: %s",
        resumo_reg$territorio,
        formatar_moeda_br(resumo_reg$soma_val),
        resumo_reg$cobertura_label
      )
      
      cores_reg <- ifelse(resumo_reg$n_validos < resumo_reg$n_total, "#D97706", "#228B52")
      
      plot_ly(
        data = resumo_reg,
        x = ~soma_val,
        y = ~territorio,
        type = "bar",
        orientation = "h",
        marker = list(color = cores_reg),
        text = ~hover_txt,
        hoverinfo = "text"
      ) |>
        layout(
          margin = list(l = 145, r = 20, t = 10, b = 40),
          xaxis = list(title = "Soma Municipal no Território (R$)"),
          yaxis = list(title = "", dtick = 1),
          paper_bgcolor = "#FFFFFF", plot_bgcolor = "#FFFFFF",
          font = list(family = "Inter, sans-serif")
        ) |> config(displayModeBar = FALSE)
    })
    
    # D. MAPA COROPLÉTICO INTERATIVO (LEAFLET)
    output$mapa_fundeb <- renderLeaflet({
      df_all <- base_ano()
      ind    <- input$indicador_fundeb
      nivel  <- input$nivel_territorial
      loc    <- localidade_ativa()
      
      df_mapa_join <- df_all |>
        mutate(
          code_muni = as.numeric(codigo_ibge),
          valor_num = as.numeric(.data[[ind]])
        ) |>
        select(code_muni, codigo_ibge, municipio, regiao_ppa, nome_nre, valor_num)
      
      mapa_dados <- mapa_pr |>
        left_join(df_mapa_join, by = "code_muni")
      
      if (nivel == "ppa" && !is.null(loc)) {
        mapa_dados <- mapa_dados |> filter(regiao_ppa == loc)
      } else if (nivel == "nre" && !is.null(loc)) {
        mapa_dados <- mapa_dados |> filter(nome_nre == loc)
      }
      
      valores_validos <- mapa_dados$valor_num[!is.na(mapa_dados$valor_num)]
      
      if (length(valores_validos) == 0) {
        return(
          leaflet(mapa_dados) |>
            addProviderTiles(providers$Esri.WorldGrayCanvas) |>
            addPolygons(
              fillColor = "#CBD5E1", weight = 1, opacity = 1, color = "#64748B", fillOpacity = 0.7,
              label = ~lapply(sprintf("<strong>%s</strong><br/>Valor: <em>Não informado</em>", name_muni), htmltools::HTML)
            )
        )
      }
      
      d_min <- min(valores_validos)
      d_max <- max(valores_validos)
      if (d_min == d_max) d_max <- d_min + 1
      
      pal <- colorNumeric("Greens", domain = c(d_min, d_max), na.color = "#CBD5E1")
      
      is_muni_sel <- if (nivel == "municipio" && !is.null(loc)) {
        mapa_dados$codigo_ibge == loc
      } else {
        rep(FALSE, nrow(mapa_dados))
      }
      
      borda_cor  <- ifelse(is_muni_sel, "#14532D", "#64748B")
      borda_peso <- ifelse(is_muni_sel, 3.5, 1)
      
      labels_html <- sprintf(
        "<strong>%s</strong><br/>PPA: %s | NRE: %s<br/>%s: <strong>%s</strong>",
        mapa_dados$name_muni,
        mapa_dados$regiao_ppa,
        mapa_dados$nome_nre,
        METADADOS_INDICADORES_FUNDEB[[ind]]$curto,
        formatar_moeda_br(mapa_dados$valor_num)
      )
      
      leaflet(mapa_dados) |>
        addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Cinza Minimalista") |>
        addProviderTiles(providers$Esri.WorldTopoMap, group = "Topográfico") |>
        addPolygons(
          fillColor = ~pal(valor_num),
          weight = borda_peso,
          opacity = 1,
          color = borda_cor,
          fillOpacity = 0.85,
          highlightOptions = highlightOptions(weight = 3, color = "#14532D", fillOpacity = 1, bringToFront = TRUE),
          label = lapply(labels_html, htmltools::HTML)
        ) |>
        addLegend(
          pal = pal,
          values = c(d_min, d_max),
          na.label = "Não informado",
          opacity = 0.85,
          title = paste0("<strong>", METADADOS_INDICADORES_FUNDEB[[ind]]$curto, " (R$)</strong>"),
          position = "bottomright",
          labFormat = labelFormat(prefix = "R$ ", big.mark = ".")
        ) |>
        addLayersControl(
          baseGroups = c("Cinza Minimalista", "Topográfico"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })
    
    # ÁREA 4 — TABELA DETALHADA DOS MUNICÍPIOS
    output$tabela_fundeb <- renderDT({
      df  <- base_recorte()
      ind <- input$indicador_fundeb
      
      df_ord <- df |>
        arrange(desc(!is.na(.data[[ind]])), desc(.data[[ind]]), municipio)
      
      tabela_exibicao <- df_ord |>
        transmute(
          `Município`              = municipio,
          `Região PPA`             = regiao_ppa,
          `NRE`                    = nome_nre,
          `Contribuição Fundeb`    = formatar_moeda_br(receita_contribuicao_fundeb),
          `VAAF`                   = formatar_moeda_br(complementacao_vaaf),
          `VAAT`                   = formatar_moeda_br(complementacao_vaat),
          `VAAR`                   = formatar_moeda_br(complementacao_vaar),
          `Comp. União Total`      = formatar_moeda_br(complementacao_uniao_total),
          `Receita Total Prevista` = formatar_moeda_br(receita_total_prevista),
          `% União / Receita`      = ifelse(is.na(pct_uniao_receita), "Não calculado",
                                            paste0(formatC(pct_uniao_receita, format = "f", digits = 2, decimal.mark = ","), "%"))
        )
      
      datatable(
        tabela_exibicao,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = list(
            list(extend = "csv", text = "Exportar CSV", filename = paste0("fundeb_parana_", input$ano_fundeb)),
            list(extend = "excel", text = "Exportar Excel", filename = paste0("fundeb_parana_", input$ano_fundeb))
          ),
          language = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/Portuguese-Brasil.json")
        )
      ) |>
        formatStyle(
          columns = c("Contribuição Fundeb", "VAAF", "VAAT", "VAAR", "Comp. União Total", "Receita Total Prevista", "% União / Receita"),
          textAlign = "right"
        ) |>
        formatStyle(
          columns = c("VAAF", "VAAT", "VAAR", "Comp. União Total"),
          color = styleEqual("Não informado", "#94A3B8"),
          fontStyle = styleEqual("Não informado", "italic")
        )
    })
  })
}