# =====================================================================
# app.R  —  O "MAESTRO" DO APP
# ---------------------------------------------------------------------
# Junta todas as partes: carrega a despensa (dados.R), as receitas
# (analises.R), o empratamento (graficos.R) e o roteiro das missões
# (missoes.R). Depois define:
#   * a TELA (ui): o que o aluno vê;
#   * o CÉREBRO (server): o que acontece quando ele mexe nos controles.
# =====================================================================

library(shiny)
library(bslib)
library(psych)
library(GPArotation)

source("dados.R", encoding = "UTF-8")

# ---------------------------------------------------------------------
# A TELA
# ---------------------------------------------------------------------

# Barra lateral: todos os controles da análise.
barra_lateral <- sidebar(
  width = 300,
  title = "Controles da análise",

  sliderInput("nfatores", "Número de fatores",
              min = 1, max = 10, value = 2, step = 1),

  selectInput("extracao", "Método de extração",
              choices = c("Resíduos mínimos (minres)" = "minres",
                          "Máxima verossimilhança (ml)" = "ml",
                          "Eixos principais (pa)" = "pa"),
              selected = "minres"),

  selectInput("rotacao", "Rotação",
              choices = c("Nenhuma (none)" = "none",
                          "Varimax (ortogonal)" = "varimax",
                          "Oblimin (oblíqua)" = "oblimin",
                          "Promax (oblíqua)" = "promax"),
              selected = "varimax"),

  radioButtons("correlacao", "Tipo de correlação",
               choices = c("Pearson" = "pearson",
                           "Policórica" = "policorica"),
               selected = "pearson", inline = TRUE),

  sliderInput("corte", "Ponto de corte das cargas",
              min = 0.10, max = 0.60, value = 0.30, step = 0.05),

  sliderInput("n_amostra", "Tamanho da amostra",
              min = 50, max = N_TOTAL, value = 1000, step = 50),

  actionButton("sortear", "Sortear outra amostra",
               icon = icon("shuffle"), class = "btn-outline-primary")
)

# As seis abas de resultados (por enquanto, vazias).
abas <- navset_card_tab(
  id = "aba",
  nav_panel("1. Correlações", value = "correlacoes",
            p("Aqui vai aparecer o mapa de calor da matriz de correlação.")),
  nav_panel("2. KMO e Bartlett", value = "kmo",
            p("Aqui vão aparecer o KMO e o teste de Bartlett, com interpretação.")),
  nav_panel("3. Quantos fatores?", value = "scree",
            p("Aqui vai aparecer o scree plot com a análise paralela.")),
  nav_panel("4. Cargas fatoriais", value = "cargas",
            p("Aqui vai aparecer a tabela de cargas, com TLI, RMSEA e variância explicada.")),
  nav_panel("5. Rotação", value = "rotacao",
            p("Aqui vão aparecer os gráficos de cargas antes e depois da rotação.")),
  nav_panel("6. Fatores e alfa", value = "fatores",
            p("Aqui vão aparecer a correlação entre fatores e o alfa de cada fator."))
)

# Painel das missões guiadas (por enquanto, vazio).
painel_missoes <- card(
  card_header("Missões guiadas"),
  p("As missões vão aparecer aqui, uma de cada vez.")
)

ui <- page_sidebar(
  title = "Laboratório de Análise Fatorial Exploratória",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = barra_lateral,
  layout_columns(
    col_widths = c(8, 4),
    abas,
    painel_missoes
  )
)

# ---------------------------------------------------------------------
# O CÉREBRO
# ---------------------------------------------------------------------
server <- function(input, output, session) {
  # Os cálculos entram nas próximas etapas.
}

shinyApp(ui, server)
