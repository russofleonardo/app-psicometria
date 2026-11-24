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

options(mc.cores = 1)   # no navegador não há processamento paralelo

source("dados.R",    encoding = "UTF-8")
source("analises.R", encoding = "UTF-8")
source("graficos.R", encoding = "UTF-8")

# O controle deslizante anda de 50 em 50; o último passo (2.450)
# significa "todas as pessoas" (2.436).
N_MAX_SLIDER <- ceiling(N_TOTAL / 50) * 50

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
              min = 50, max = N_MAX_SLIDER, value = 1000, step = 50),
  div(class = "small text-muted", style = "margin-top:-.6rem;",
      textOutput("resumo_amostra", inline = TRUE)),

  actionButton("sortear", "Sortear outra amostra",
               icon = icon("shuffle"), class = "btn-outline-primary")
)

# --- Aba 1: correlações
aba_correlacoes <- nav_panel(
  "1. Correlações", value = "correlacoes",
  p("Cada quadradinho mostra a correlação entre dois itens. ",
    strong("Vermelho"), " = correlação positiva (quem concorda com um tende a concordar com o outro); ",
    strong("azul"), " = correlação negativa (quem concorda com um tende a discordar do outro); ",
    "cinza = quase nenhuma relação."),
  p(class = "text-muted small",
    "Os itens estão agrupados por domínio. Procure “blocos” de cor forte: são grupos de itens que andam juntos — a semente dos fatores."),
  legenda_dominios(),
  checkboxInput("mostrar_numeros", "Mostrar os valores dentro dos quadradinhos", value = FALSE),
  uiOutput("aviso_suavizada"),
  plotOutput("mapa_cor", height = "720px")
)

# --- Aba 2: KMO e Bartlett
aba_kmo <- nav_panel(
  "2. KMO e Bartlett", value = "kmo",
  p("Antes de procurar fatores, precisamos saber se os dados ", em("servem"),
    " para isso. Dois testes ajudam:"),
  uiOutput("aviso_suavizada2"),
  layout_columns(
    col_widths = c(6, 6),
    uiOutput("caixa_kmo"),
    uiOutput("caixa_bartlett")
  ),
  card(
    card_header("KMO de cada item"),
    p(class = "small text-muted",
      "Um item com KMO baixo (abaixo de 0,50) compartilha pouco com os demais e é candidato a sair da análise."),
    uiOutput("tabela_msa")
  )
)

# --- Aba 3: scree plot e análise paralela
aba_scree <- nav_panel(
  "3. Quantos fatores?", value = "scree",
  p("O ", strong("scree plot"), " mostra quanto cada fator explica (o autovalor). A ",
    strong("análise paralela"), " compara isso com o que dados ", em("totalmente aleatórios"),
    " do mesmo tamanho explicariam."),
  p(strong("Regra:"), " mantenha os fatores em que a linha azul (dados reais) fica ",
    em("acima"), " da linha laranja (acaso). Quando a azul cai abaixo da laranja, os fatores seguintes não explicam mais do que o acaso."),
  plotOutput("scree", height = "460px"),
  tags$details(
    class = "mt-2",
    tags$summary("Ver a sugestão da análise paralela (tente contar antes!)"),
    uiOutput("sugestao_paralela")
  )
)

# --- Abas 4 a 6 (próxima etapa)
aba_cargas <- nav_panel(
  "4. Cargas fatoriais", value = "cargas",
  p("Aqui vai aparecer a tabela de cargas, com TLI, RMSEA e variância explicada."))
aba_rotacao <- nav_panel(
  "5. Rotação", value = "rotacao",
  p("Aqui vão aparecer os gráficos de cargas antes e depois da rotação."))
aba_fatores <- nav_panel(
  "6. Fatores e alfa", value = "fatores",
  p("Aqui vão aparecer a correlação entre fatores e o alfa de cada fator."))

abas <- navset_card_tab(
  id = "aba",
  aba_correlacoes, aba_kmo, aba_scree, aba_cargas, aba_rotacao, aba_fatores
)

# Painel das missões guiadas (próximas etapas).
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
# Lembre da "fila de dominós": cada reactive() só é recalculado quando
# algo de que ele depende muda.
server <- function(input, output, session) {

  # --- A amostra -------------------------------------------------------
  # A semente começa igual para todos; cada clique em "sortear" muda.
  semente <- reactiveVal(2026)
  observeEvent(input$sortear, semente(semente() + 1))

  # Espera o aluno soltar o controle antes de recalcular (0,5 s)
  n_amostra <- debounce(reactive(min(input$n_amostra, N_TOTAL)), 500)

  amostra <- reactive(sortear_amostra(n_amostra(), semente()))

  output$resumo_amostra <- renderText({
    n <- n_amostra()
    if (n >= N_TOTAL) sprintf("Usando todas as %s pessoas.", fmt_int(N_TOTAL))
    else sprintf("%s de %s pessoas (sorteio nº %d).",
                 fmt_int(n), fmt_int(N_TOTAL), semente() - 2025)
  })

  # --- As correlações ----------------------------------------------------
  # Cada tipo é calculado só quando pedido e guardado até a amostra mudar.
  cor_pearson <- reactive(calcular_correlacao(amostra(), "pearson"))
  cor_policorica <- reactive({
    withProgress(message = "Calculando correlações policóricas…",
                 detail = "isso pode levar alguns segundos.", value = 0.3, {
      calcular_correlacao(amostra(), "policorica")
    })
  })
  correlacao <- reactive({
    if (input$correlacao == "policorica") cor_policorica() else cor_pearson()
  })

  # --- KMO / Bartlett e análise paralela --------------------------------
  kmo <- reactive(avaliar_kmo(correlacao()$R, nrow(amostra())))
  paralela <- reactive(analise_paralela(correlacao()$R, nrow(amostra()), semente()))

  # =====================================================================
  # Aba 1
  # =====================================================================
  aviso_correlacao <- function() {
    cr <- correlacao()
    avisos <- list()
    if (cr$falhou) avisos <- c(avisos, list(
      "⚠ Com esta amostra, não foi possível estimar as correlações policóricas (há respostas raras demais). O app está mostrando a correlação de Pearson. Tente uma amostra maior ou sorteie outra."))
    if (cr$suavizada) avisos <- c(avisos, list(
      "⚠ A matriz de correlação precisou de um pequeno ajuste (suavização) porque, com esta amostra, ela não era “bem-comportada” matematicamente. Isso costuma acontecer com amostras pequenas e correlação policórica: os resultados ficam instáveis."))
    if (length(avisos) == 0) return(NULL)
    div(class = "alert alert-warning py-2 small", lapply(avisos, p, class = "mb-1"))
  }
  output$aviso_suavizada  <- renderUI(aviso_correlacao())
  output$aviso_suavizada2 <- renderUI(aviso_correlacao())

  output$mapa_cor <- renderPlot({
    grafico_correlacao(correlacao()$R, mostrar_numeros = isTRUE(input$mostrar_numeros))
  }, res = 96)

  # =====================================================================
  # Aba 2
  # =====================================================================
  output$caixa_kmo <- renderUI({
    k <- kmo()
    value_box(
      title = "KMO geral (Kaiser-Meyer-Olkin)",
      value = fmt(k$geral, 2),
      p(strong(paste0("Classificação: ", k$classe, "."))),
      p(class = "small", k$texto_kmo),
      p(class = "small text-muted",
        "Referência: ≥ 0,90 maravilhoso; 0,80 meritório; 0,70 mediano; 0,60 medíocre; 0,50 miserável; < 0,50 inaceitável."),
      showcase = icon("gauge-high"),
      theme = if (k$geral >= 0.6) "primary" else "danger"
    )
  })

  output$caixa_bartlett <- renderUI({
    k <- kmo()
    value_box(
      title = "Teste de esfericidade de Bartlett",
      value = paste0("p ", if (k$p < 0.001) "< 0,001" else paste0("= ", fmt(k$p, 3))),
      p(sprintf("χ² = %s; gl = %s", fmt(k$qui2, 1), fmt_int(k$gl))),
      p(class = "small", k$texto_bartlett),
      p(class = "small text-muted",
        "Atenção: com amostras grandes, esse teste quase sempre dá significativo. Ele é um requisito mínimo, não uma prova de que há bons fatores."),
      showcase = icon("circle-nodes"),
      theme = if (k$p < 0.05) "primary" else "danger"
    )
  })

  output$tabela_msa <- renderUI(tabela_msa(kmo()))

  # =====================================================================
  # Aba 3
  # =====================================================================
  output$scree <- renderPlot(grafico_scree(paralela()), res = 96)

  output$sugestao_paralela <- renderUI({
    pa <- paralela()
    tagList(
      p(class = "mt-2",
        sprintf("Com esta amostra, a análise paralela sugere %s.",
                if (pa$sugeridos == 1) "1 fator" else paste(pa$sugeridos, "fatores"))),
      p(class = "small text-muted",
        sprintf("Para comparar: o antigo critério de Kaiser (autovalores maiores que 1) sugeriria %d. Ele costuma exagerar no número de fatores; a análise paralela é mais confiável. (Simulações: %d conjuntos de dados aleatórios.)",
                pa$kaiser, pa$n_iter))
    )
  })
}

shinyApp(ui, server)
