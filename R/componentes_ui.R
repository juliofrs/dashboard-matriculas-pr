# =============================================================================
# R/componentes_ui.R: DESIGN SYSTEM, ESTILOS, KPIS E VISUALIZAÇÃO INTERATIVA
# =============================================================================

suppressPackageStartupMessages({
  library(htmltools)
  library(plotly)
  library(leaflet)
  library(scales)
})

# Folha de Estilos Customizada (Tokens do Design System)
estilos_customizados <- function() {
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');
      
      :root {
        --pr-green-dark: #14532D;
        --pr-green-main: #228B52;
        --pr-green-light: #E8F3EC;
        --bg-canvas: #F5F7F6;
        --bg-surface: #FFFFFF;
        --text-primary: #26332C;
        --text-secondary: #64748B;
        --border-subtle: #E2E8F0;
        --accent-blue: #0284C7;
        --accent-teal: #0D9488;
        --accent-amber: #D97706;
        --accent-indigo: #6366F1;
      }
      
      body, .navbar, .card, .btn, .form-control, .selectize-input {
        font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif !important;
      }
      
      body {
        background-color: var(--bg-canvas) !important;
        color: var(--text-primary) !important;
      }
      
      .navbar {
        background-color: var(--pr-green-dark) !important;
        border-bottom: 2px solid var(--pr-green-main);
        box-shadow: 0 2px 4px rgba(0,0,0,0.08);
      }
      
      .navbar .navbar-brand {
        color: #FFFFFF !important;
        font-weight: 700;
        letter-spacing: -0.02em;
        font-size: 1.15rem;
      }
      
      .nav-link {
        color: #D1FAE5 !important;
        font-weight: 500;
        transition: all 0.2s ease;
      }
      
      .nav-link:hover, .nav-link.active {
        color: #FFFFFF !important;
        font-weight: 600;
      }
      
      .card {
        background: var(--bg-surface);
        border: 1px solid var(--border-subtle);
        border-radius: 10px;
        box-shadow: 0 1px 3px rgba(20, 83, 45, 0.05);
        margin-bottom: 1rem;
      }
      
      .card-header {
        background-color: #FAFCFB;
        border-bottom: 1px solid var(--border-subtle);
        font-weight: 600;
        font-size: 0.95rem;
        color: var(--text-primary);
        padding: 0.85rem 1.15rem;
      }
      
      /* Componente: Card de KPI Executivo */
      .kpi-container {
        padding: 1rem 1.15rem;
        background: #FFFFFF;
        border: 1px solid var(--border-subtle);
        border-radius: 10px;
        border-left: 4px solid var(--pr-green-main);
        height: 100%;
        display: flex;
        flex-direction: column;
        justify-content: space-between;
      }
      
      .kpi-title {
        font-size: 0.78rem;
        font-weight: 600;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: var(--text-secondary);
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      .kpi-value {
        font-size: 1.7rem;
        font-weight: 700;
        color: var(--text-primary);
        font-variant-numeric: tabular-nums;
        line-height: 1.2;
        margin: 0.35rem 0;
      }
      
      .kpi-badge {
        font-size: 0.75rem;
        font-weight: 500;
        padding: 0.2rem 0.5rem;
        border-radius: 4px;
        display: inline-flex;
        align-items: center;
        width: fit-content;
      }
      
      .kpi-badge-positive {
        background-color: #DEF7EC;
        color: #03543F;
      }
      
      .kpi-badge-negative {
        background-color: #FDE8E8;
        color: #9B1C1C;
      }
      
      .kpi-badge-neutral {
        background-color: #F3F4F6;
        color: #4B5563;
      }
      
      .kpi-subtext {
        font-size: 0.72rem;
        color: var(--text-secondary);
        margin-top: 0.25rem;
      }
      
      /* Filtros Laterais */
      .sidebar-section-title {
        font-size: 0.75rem;
        font-weight: 700;
        text-transform: uppercase;
        letter-spacing: 0.05em;
        color: var(--text-secondary);
        margin-top: 1rem;
        margin-bottom: 0.4rem;
        padding-bottom: 0.2rem;
        border-bottom: 1px solid var(--border-subtle);
      }
      
      .btn-limpar-filtros {
        background-color: var(--pr-green-light);
        color: var(--pr-green-dark);
        border: 1px solid #C6E2D0;
        font-weight: 600;
        font-size: 0.82rem;
        width: 100%;
        transition: all 0.2s ease;
      }
      
      .btn-limpar-filtros:hover {
        background-color: #D1EADE;
        color: var(--pr-green-dark);
      }
      
      /* Tabela DT Customizada */
      table.dataTable thead th {
        background-color: var(--pr-green-dark) !important;
        color: #FFFFFF !important;
        font-weight: 600 !important;
        border-color: var(--pr-green-dark) !important;
      }
      
      .linha-totalizador-dt {
        background-color: var(--pr-green-light) !important;
        font-weight: 700 !important;
        color: var(--pr-green-dark) !important;
      }
    "))
  )
}

# Construtor do Cartão de KPI
kpi_card_ui <- function(id, titulo, borda_cor = "var(--pr-green-main)", tooltip_desc = "") {
  div(
    class = "kpi-container",
    style = paste0("border-left-color: ", borda_cor, ";"),
    div(
      class = "kpi-title",
      span(titulo),
      if (nzchar(tooltip_desc)) {
        span(
          style = "cursor: help; color: #94A3B8;",
          title = tooltip_desc,
          "ⓘ"
        )
      } else NULL
    ),
    uiOutput(id)
  )
}

render_kpi_conteudo <- function(indicador, is_total = FALSE) {
  badge_class <- "kpi-badge-neutral"
  if (!is.na(indicador$dif_abs)) {
    if (indicador$dif_abs > 0) badge_class <- "kpi-badge-positive"
    if (indicador$dif_abs < 0) badge_class <- "kpi-badge-negative"
  }
  
  tagList(
    div(class = "kpi-value", formatar_numero_br(indicador$valor)),
    div(
      class = paste("kpi-badge", badge_class),
      indicador$label_var
    ),
    if (!is_total) {
      div(
        class = "kpi-subtext",
        paste("Participação no recorte:", formatar_percentual_br(indicador$participacao))
      )
    } else {
      div(class = "kpi-subtext", "Totalização do recorte selecionado")
    }
  )
}

# Gráfico Interativo do Ranking (Plotly)
render_plotly_ranking <- function(tabela_ordenada, tipo_valor, rede_nome) {
  tabela <- tabela_ordenada
  tabela$municipio <- factor(tabela$municipio, levels = rev(tabela$municipio))
  
  n_linhas <- nrow(tabela)
  cores <- rep("#228B52", n_linhas)
  if (n_linhas >= 1) cores[n_linhas]     <- "#14532D" # 1º lugar
  if (n_linhas >= 2) cores[n_linhas - 1] <- "#166534" # 2º lugar
  if (n_linhas >= 3) cores[n_linhas - 2] <- "#15803D" # 3º lugar
  
  if (tipo_valor == "percentual") {
    hover_template <- paste0(
      "<b>%{y}</b><br>",
      "Proporção: <b>%{x:.1%}</b><br>",
      "Total Matrículas: %{customdata:,}<extra></extra>"
    )
    eixo_x <- list(title = "Proporção de Matrículas", tickformat = ".0%", range = c(0, max(tabela$valor_final * 1.15, 0.1)))
  } else {
    hover_template <- paste0(
      "<b>%{y}</b><br>",
      "Matrículas: <b>%{x:,.0f}</b><br>",
      "Total Geral: %{customdata:,}<extra></extra>"
    )
    eixo_x <- list(title = "Total de Matrículas", separador = ".", range = c(0, max(tabela$valor_final * 1.18, 100)))
  }
  
  plot_ly(
    data = tabela,
    x = ~valor_final,
    y = ~municipio,
    type = "bar",
    orientation = "h",
    marker = list(color = cores),
    customdata = ~total,
    hovertemplate = hover_template
  ) |>
    layout(
      margin = list(l = 140, r = 30, t = 20, b = 40),
      xaxis = eixo_x,
      yaxis = list(title = "", dtick = 1),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF",
      font = list(family = "Inter, sans-serif")
    ) |>
    config(displayModeBar = FALSE)
}

# Gráfico Interativo de Evolução Histórica (Plotly)
render_plotly_historico <- function(df_plot, tipo_valor, rede_nome) {
  if (tipo_valor == "percentual") {
    hover_template <- paste0("<b>%{fullData.name}</b><br>Ano %{x}: <b>%{y:.1%}</b><extra></extra>")
    eixo_y <- list(title = paste("Proporção (", rede_nome, ")"), tickformat = ".1%")
  } else {
    hover_template <- paste0("<b>%{fullData.name}</b><br>Ano %{x}: <b>%{y:,.0f}</b> matrículas<extra></extra>")
    eixo_y <- list(title = paste("Total de Matrículas (", rede_nome, ")"))
  }
  
  p <- plot_ly(
    data = df_plot,
    x = ~ano,
    y = ~valor_final,
    color = ~local,
    type = "scatter",
    mode = "lines+markers",
    marker = list(size = 8),
    line = list(width = 2.5),
    hovertemplate = hover_template
  ) |>
    layout(
      margin = list(l = 60, r = 20, t = 30, b = 40),
      xaxis = list(title = "Ano do Censo Escolar", dtick = 1),
      yaxis = eixo_y,
      legend = list(orientation = "h", x = 0, y = 1.12),
      paper_bgcolor = "#FFFFFF",
      plot_bgcolor = "#FFFFFF",
      font = list(family = "Inter, sans-serif")
    ) |>
    config(displayModeBar = FALSE)
  
  p
}