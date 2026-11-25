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
source("missoes.R",  encoding = "UTF-8")

# O controle deslizante anda de 50 em 50; o último passo (2.450)
# significa "todas as pessoas" (2.436).
N_MAX_SLIDER <- ceiling(N_TOTAL / 50) * 50

# ---------------------------------------------------------------------
# A TELA
# ---------------------------------------------------------------------

# Barra lateral: todos os controles da análise.
barra_lateral <- sidebar(
  width = 280,
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
              min = 0.10, max = 0.60, value = 0.30, step = 0.05, ticks = FALSE),

  sliderInput("n_amostra", "Tamanho da amostra",
              min = 50, max = N_MAX_SLIDER, value = 1000, step = 50, ticks = FALSE),
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
  checkboxInput("mostrar_numeros", "Mostrar os valores dentro dos quadradinhos",
                value = FALSE, width = "100%"),
  uiOutput("aviso_suavizada"),
  plotOutput("mapa_cor", height = "720px")
)

# --- Aba 2: KMO e Bartlett
aba_kmo <- nav_panel(
  "2. KMO e Bartlett", value = "kmo",
  p("Antes de procurar fatores, precisamos saber se os dados ", em("servem"),
    " para isso. Dois testes ajudam:"),
  uiOutput("aviso_suavizada2"),
  # Lado a lado em telas largas; um embaixo do outro em telas estreitas.
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(6, 6)),
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
  p(class = "small text-muted",
    "A linha laranja não é a média do acaso, e sim o seu ", strong("percentil 95"),
    ": o valor que dados aleatórios só superam em 5% das vezes. Assim, um fator só é aceito se for claramente maior do que o acaso."),
  checkboxInput("mostrar_kaiser", "Mostrar também os autovalores usados no critério de Kaiser",
                value = FALSE, width = "100%"),
  uiOutput("explica_kaiser"),
  plotOutput("scree", height = "460px"),
  tags$details(
    class = "mt-2",
    tags$summary("Ver a sugestão da análise paralela (tente contar antes!)"),
    uiOutput("sugestao_paralela")
  ),
  card(
    class = "mt-3",
    card_header("Quanto o ajuste melhora a cada fator?"),
    p("Os gráficos mostram o TLI, o RMSEA e a variância explicada para modelos de 1 a 10 fatores ",
      "(com o método de extração e o tipo de correlação escolhidos). As faixas coloridas marcam os valores bons (verde) e aceitáveis (amarelo)."),
    p(class = "small text-muted",
      "O anel preto marca o número de fatores escolhido agora; a linha laranja tracejada, a sugestão da análise paralela; ",
      "um ✖ vermelho, um caso de Heywood (resultado não confiável). ",
      "Repare que o ajuste sempre melhora, mas cada fator novo ajuda menos que o anterior: ",
      "o objetivo é parar quando o ganho fica pequeno — a ", em("parcimônia"), "."),
    card_body(plotOutput("grafico_ajuste_k", height = "380px"), fillable = FALSE)
  )
)

# --- Aba 4: cargas fatoriais e índices de ajuste
aba_cargas <- nav_panel(
  "4. Cargas fatoriais", value = "cargas",
  p("Agora o app procura os fatores, usando o número de fatores, o método de extração e a rotação escolhidos na barra lateral. ",
    "Primeiro, veja se o modelo se ajusta aos dados; depois, veja quais itens formam cada fator."),
  uiOutput("aviso_correlacao3"),
  uiOutput("aviso_afe"),
  layout_columns(
    col_widths = breakpoints(sm = 12, lg = c(4, 4, 4)),
    uiOutput("caixa_tli"),
    uiOutput("caixa_rmsea"),
    uiOutput("caixa_variancia")
  ),
  uiOutput("leitura_ajuste"),
  # Guardar a solução atual para comparar depois (sub-aba "Comparar")
  div(class = "d-flex flex-wrap align-items-center gap-2 mb-2",
      actionButton("guardar_solucao", "Guardar esta solução para comparar",
                   icon = icon("thumbtack"), class = "btn-sm btn-outline-primary"),
      uiOutput("descricao_guardada", inline = TRUE)),
  navset_card_underline(
    id = "sub_cargas",
    nav_panel(
      "Tabela de cargas", value = "tabela",
      p("A ", strong("carga"), " mostra o quanto cada item está ligado a cada fator. Ela vai de −1 a 1: ",
        "quanto mais longe de zero, mais forte a ligação. Cargas ", strong("negativas"),
        " indicam itens que andam no sentido contrário do fator (como os itens invertidos)."),
      p(class = "small text-muted",
        "Cargas acima do ponto de corte (barra lateral) aparecem em negrito e coloridas; as demais ficam apagadas. ",
        "O ideal é que cada item tenha carga forte em um só fator."),
      uiOutput("nota_obliqua"),
      conditionalPanel(
        "input.rotacao == 'oblimin' || input.rotacao == 'promax'",
        radioButtons("matriz_cargas", "Mostrar:",
                     choices = c("Matriz padrão (cargas)" = "padrao",
                                 "Matriz de estrutura (correlações item–fator)" = "estrutura"),
                     selected = "padrao", inline = TRUE, width = "100%")
      ),
      checkboxInput("agrupar_cargas", "Agrupar os itens pelo fator em que têm a maior carga",
                    value = TRUE, width = "100%"),
      uiOutput("tabela_cargas"),
      tags$details(
        class = "mt-3 small",
        tags$summary("O que são h², autovalor, SQ e “situação”?"),
        tags$ul(
          class = "mt-2",
          tags$li(strong("h² (comunalidade):"), " quanto da variação das respostas a um item os fatores conseguem explicar. ",
                  "Valores muito baixos (perto de 0,20 ou menos) indicam um item que pouco tem a ver com os fatores. ",
                  "Um valor igual ou maior que 1 é impossível na teoria: é o chamado ", em("caso de Heywood"), "."),
          tags$li(strong("Autovalor (sem rotação):"), " a soma das cargas ao quadrado de um fator; mostra o “tamanho” do fator."),
          tags$li(strong("Soma das cargas² — SQ (com rotação):"), " depois de rotacionar, a soma das cargas ao quadrado ",
                  em("não é mais"), " um autovalor: a rotação redistribui a variância entre os fatores. Por isso o nome muda. ",
                  "O total explicado pelos fatores juntos continua o mesmo."),
          tags$li(strong("Carga cruzada:"), " o item tem carga forte em mais de um fator — fica difícil dizer a qual pertence."),
          tags$li(strong("Sem fator:"), " nenhuma carga passou do ponto de corte — o item não se encaixou em nenhum fator.")
        )
      )
    ),
    nav_panel(
      "Diagrama do modelo", value = "diagrama",
      p("O diagrama mostra o modelo da análise fatorial: cada ", strong("fator"), " (os círculos, à direita) é uma variável que não ",
        "podemos observar diretamente, e as setas indicam que ele ", em("influencia"), " as respostas aos itens (os retângulos, à esquerda). ",
        "O número em cada seta é a carga."),
      p(class = "small text-muted",
        "Setas tracejadas e vermelhas são cargas negativas (como as dos itens invertidos). ",
        "Para o diagrama ficar legível, cada item aparece ligado só ao fator em que tem a maior carga, e só quando ela passa do ponto de corte. ",
        "Nas rotações oblíquas, os arcos à direita mostram as correlações entre fatores que passam do ponto de corte. ",
        "(O desenho é feito pelo pacote psych, que escreve os números com ponto em vez de vírgula.)"),
      card_body(plotOutput("diagrama", height = "720px"), fillable = FALSE, padding = 0)
    ),
    nav_panel(
      "Resíduos", value = "residuos",
      p("A análise fatorial tenta ", strong("reproduzir as correlações"), " entre os itens usando só os fatores. ",
        "O ", strong("resíduo"), " é a diferença entre a correlação observada e a que o modelo reproduz: ",
        "ele mostra o que o modelo ", em("não"), " conseguiu explicar."),
      p(class = "small text-muted",
        "Num bom modelo, quase tudo fica cinza (resíduos perto de zero). Blocos coloridos indicam relações que os fatores deixaram de fora — ",
        "um sinal de que podem faltar fatores. Atenção: a escala aqui vai só até ±0,20, porque os resíduos são números pequenos. ",
        "A rotação não muda os resíduos."),
      uiOutput("resumo_residuos"),
      card_body(plotOutput("mapa_residuos", height = "720px"), fillable = FALSE, padding = 0)
    ),
    nav_panel(
      "Comparar", value = "comparar",
      uiOutput("conteudo_comparar")
    )
  )
)

# --- Aba 5: rotação
aba_rotacao <- nav_panel(
  "5. Rotação", value = "rotacao",
  p("Imagine os itens como pontos num mapa. Os fatores são os eixos desse mapa. ",
    strong("Rotacionar"), " é girar os eixos para que cada um passe o mais perto possível de um grupo de pontos. ",
    "Os pontos não mudam de lugar entre si: só muda o ponto de vista, e a solução fica mais fácil de interpretar."),
  p(class = "small text-muted",
    "Cada ponto é um item, na cor do seu domínio. O quadrado cinza no centro é a “zona fraca”: ",
    "itens ali têm carga abaixo do ponto de corte nos dois fatores do gráfico. ",
    "Depois de uma boa rotação, os pontos se aproximam dos eixos — e se afastam do meio entre eles."),
  conditionalPanel(
    "input.nfatores < 2",
    div(class = "alert alert-info small",
        "Com 1 fator só existe um eixo, e não há o que girar. Escolha 2 ou mais fatores na barra lateral para ver a rotação.")
  ),
  conditionalPanel(
    "input.nfatores >= 2",
    conditionalPanel(
      "input.nfatores > 2",
      div(class = "d-flex flex-wrap gap-3 align-items-end",
          selectInput("eixo_x", "Fator no eixo horizontal", choices = c("F1", "F2"), selected = "F1", width = "200px"),
          selectInput("eixo_y", "Fator no eixo vertical", choices = c("F1", "F2"), selected = "F2", width = "200px"))
    ),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      card(card_header("Antes da rotação"),
           card_body(plotOutput("grafico_antes", height = "430px"), fillable = FALSE)),
      card(card_header(textOutput("titulo_depois", inline = TRUE)),
           card_body(plotOutput("grafico_depois", height = "430px"), fillable = FALSE))
    ),
    card(
      card_header("O que a rotação mudou — e o que não mudou"),
      uiOutput("tabela_rotacao"),
      uiOutput("texto_rotacao")
    )
  )
)

# --- Aba 6: correlação entre fatores e alfa
aba_fatores <- nav_panel(
  "6. Fatores e alfa", value = "fatores",
  card(
    card_header("Os fatores se correlacionam?"),
    uiOutput("conteudo_phi")
  ),
  card(
    card_header("Consistência interna (alfa e ômega) de cada fator"),
    p("O ", strong("alfa de Cronbach"), " indica se os itens de um fator “contam a mesma história”: ",
      "se as respostas a eles costumam andar juntas. Vai de 0 a 1; a partir de 0,70 costuma ser considerado aceitável."),
    p(class = "small text-muted",
      "Cada item entra no fator em que tem a maior carga (se ela passar do ponto de corte). ",
      "Itens com carga negativa — marcados com “(inv.)” — são invertidos antes do cálculo. ",
      "A coluna “Parece medir” mostra o domínio que mais aparece entre os itens do fator."),
    uiOutput("tabela_alfa"),
    uiOutput("itens_fora"),
    uiOutput("nota_alfa"),
    uiOutput("nota_omega")
  )
)

abas <- navset_card_tab(
  id = "aba",
  aba_correlacoes, aba_kmo, aba_scree, aba_cargas, aba_rotacao, aba_fatores
)

# Painel das missões guiadas (próximas etapas).
# Em telas muito largas ele fica à direita das abas; em telas menores
# (como a de um notebook) ele vai para cima, ocupando a largura toda,
# para não espremer os gráficos.
# Quando muda algum controle, só a parte "Conferência" é redesenhada:
# assim o comentário da reflexão, se estiver aberto, não fecha sozinho.
# O cabeçalho é um botão: um clique fecha o painel (sobra só uma linha
# com o resumo) e outro clique abre de novo.
painel_missoes <- card(
  class = "painel-missoes", fill = FALSE,
  card_header(
    class = "p-0",
    tags$button(
      type = "button", id = "botao_recolher_missoes",
      class = "botao-recolher btn w-100 d-flex align-items-center gap-2 text-start px-3 py-2",
      `data-bs-toggle` = "collapse", `data-bs-target` = "#corpo_missoes",
      `aria-expanded` = "false", `aria-controls` = "corpo_missoes",
      span(class = "seta", "▾"),
      span(class = "fw-semibold text-nowrap", "Missões guiadas"),
      span(class = "small text-muted text-truncate", style = "min-width:0;",
           uiOutput("missoes_resumo", inline = TRUE)),
      span(class = "rotulo ms-auto small text-nowrap")
    )
  ),
  card_body(
    padding = 0, gap = 0, fillable = FALSE,
    div(
      id = "corpo_missoes", class = "collapse",   # começa fechado
      div(
        class = "corpo-missoes p-3",
        uiOutput("missoes_topo"),
        uiOutput("missao_texto"),
        uiOutput("missao_conferencia"),
        uiOutput("missao_reflexao"),
        div(class = "mt-3 text-end",
            actionLink("recomecar_missoes", "Recomeçar as missões", class = "small text-muted"))
      )
    )
  )
)

# O progresso das missões fica guardado no navegador (localStorage), para
# não se perder ao recarregar a página — funciona também no Shinylive.
script_missoes <- tags$script(HTML("
  $(document).on('shiny:connected', function() {
    var salvo = null;
    try { salvo = JSON.parse(localStorage.getItem('afe_missoes')); } catch (e) {}
    Shiny.setInputValue('missoes_salvas', salvo || {concluidas: [], atual: 1});
  });
  Shiny.addCustomMessageHandler('salvar_missoes', function(x) {
    try { localStorage.setItem('afe_missoes', JSON.stringify(x)); } catch (e) {}
  });

  // O painel de missões começa fechado. Se a pessoa o deixou aberto da
  // última vez, ele abre sozinho. Enquanto ela nunca abriu, o botão pulsa.
  $(function() {
    var corpo = document.getElementById('corpo_missoes');
    var botao = document.getElementById('botao_recolher_missoes');
    var salvo = null;
    try { salvo = localStorage.getItem('afe_missoes_fechado'); } catch (e) {}
    if (salvo === '0') { corpo.classList.add('show'); botao.setAttribute('aria-expanded', 'true'); }
    if (salvo === null) botao.classList.add('nunca-aberto');
    corpo.addEventListener('hidden.bs.collapse', function() {
      try { localStorage.setItem('afe_missoes_fechado', '1'); } catch (e) {}
    });
    corpo.addEventListener('shown.bs.collapse', function() {
      botao.classList.remove('nunca-aberto');
      try { localStorage.setItem('afe_missoes_fechado', '0'); } catch (e) {}
    });
  });
"))

ui <- page_sidebar(
  title = "Laboratório de Análise Fatorial Exploratória",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = barra_lateral,
  # fillable = FALSE: a página rola normalmente, em vez de espremer tudo
  # na altura da janela (em telas baixas, os gráficos sumiam).
  fillable = FALSE,
  # Ajustes para telas menores (notebooks):
  #  * abas com menos espaço lateral, para caberem numa linha só;
  #  * abaixo de 1400 px de largura, as missões sobem para cima das abas.
  tags$style(HTML("
    .nav-tabs .nav-link { padding: .5rem .7rem; }
    .botao-missao { min-width: 2.3rem; }
    .botao-recolher { border: 0; border-radius: 0; }
    .botao-recolher:hover { background: rgba(0,0,0,.04); }
    .botao-recolher .seta { display: inline-block; transition: transform .2s; }
    .botao-recolher[aria-expanded='false'] .seta { transform: rotate(-90deg); }
    /* Aberto: um 'fechar' discreto. Fechado: um botão azul bem visível. */
    .botao-recolher .rotulo { color: #6c757d; }
    .botao-recolher .rotulo::after { content: 'fechar \\25B4'; }
    .botao-recolher[aria-expanded='false'] .rotulo {
      color: #fff; background: #2a78d6; border-radius: 999px;
      padding: .25rem .8rem; font-weight: 600;
    }
    .botao-recolher[aria-expanded='false'] .rotulo::after { content: 'Abrir as missões \\25BE'; }
    .botao-recolher[aria-expanded='false']:hover .rotulo { background: #1f62b3; }
    .botao-recolher[aria-expanded='false'] { background: #eef4fc; }
    /* Pulso suave até a pessoa abrir as missões pela primeira vez */
    @keyframes pulso-missoes {
      0%, 100% { box-shadow: 0 0 0 0 rgba(42,120,214,.55); }
      50%      { box-shadow: 0 0 0 7px rgba(42,120,214,0); }
    }
    .botao-recolher.nunca-aberto[aria-expanded='false'] .rotulo { animation: pulso-missoes 2s ease-in-out infinite; }
    @media (prefers-reduced-motion: reduce) {
      .botao-recolher.nunca-aberto .rotulo { animation: none; }
    }
    .painel-missoes { height: auto !important; }
    .painel-missoes .corpo-missoes { overflow-y: auto; max-height: 55vh; }
    @media (max-width: 1399.98px) {
      .bslib-grid > :has(> .painel-missoes) { order: -1; }
    }
    /* Em telas largas, o painel acompanha a rolagem da página */
    @media (min-width: 1400px) {
      .painel-missoes { position: sticky; top: 1rem; }
      .painel-missoes .corpo-missoes { max-height: calc(100vh - 7rem); }
    }
  ")),
  script_missoes,
  layout_columns(
    col_widths = breakpoints(sm = 12, xxl = c(8, 4)),
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

  # --- A análise fatorial -------------------------------------------------
  # Espera o aluno soltar o controle do nº de fatores (0,3 s).
  nfat <- debounce(reactive(input$nfatores), 300)

  # Sempre calculamos a solução SEM rotação (para a aba 5 comparar) e a
  # solução COM a rotação escolhida. Se a rotação for "nenhuma", são iguais.
  afe_sem <- reactive(rodar_afe(correlacao()$R, nrow(amostra()), nfat(), input$extracao, "none"))
  afe_com <- reactive({
    if (input$rotacao == "none") afe_sem()
    else rodar_afe(correlacao()$R, nrow(amostra()), nfat(), input$extracao, input$rotacao)
  })
  # Atalho: a solução rotacionada, só se deu certo (senão as saídas esperam)
  afe <- reactive({
    a <- afe_com()
    req(a$ok)
    a
  })

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
  output$aviso_correlacao3 <- renderUI(aviso_correlacao())

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
  output$scree <- renderPlot(
    grafico_scree(paralela(), mostrar_kaiser = isTRUE(input$mostrar_kaiser)), res = 96)

  output$explica_kaiser <- renderUI({
    if (!isTRUE(input$mostrar_kaiser)) return(NULL)
    pa <- paralela()
    acima_azul <- sum(pa$reais > 1)
    div(class = "small p-2 mb-2 rounded", style = "background:#f4f3ef;border-left:4px solid #898781;",
        p(class = "mb-1", strong("Atenção: são dois tipos de autovalor. "),
          "A linha azul mostra os autovalores ", em("fatoriais"), " (só da parte que os itens compartilham), usados na análise paralela. ",
          "A linha cinza mostra os autovalores da ", em("matriz de correlação completa"),
          " (com 1 na diagonal), que são maiores — e são estes que o critério de Kaiser compara com 1."),
        p(class = "mb-0", sprintf(
          "Por isso, contar os pontos azuis acima de 1 dá %d, enquanto o critério de Kaiser (pontos cinzas acima de 1) dá %d. Quando alguém cita o critério de Kaiser, está falando da linha cinza.",
          acima_azul, pa$kaiser)))
  })

  output$sugestao_paralela <- renderUI({
    pa <- paralela()
    tagList(
      p(class = "mt-2",
        sprintf("Com esta amostra, a análise paralela sugere %s.",
                if (pa$sugeridos == 1) "1 fator" else paste(pa$sugeridos, "fatores"))),
      p(class = "small text-muted",
        sprintf("Para comparar: o antigo critério de Kaiser (autovalores da matriz completa maiores que 1) sugeriria %d. Ele costuma exagerar no número de fatores e não é recomendado como critério único. (Análise paralela: %d conjuntos de dados aleatórios, comparando com o percentil %d.)",
                pa$kaiser, pa$n_iter, round(100 * pa$percentil)))
    )
  })

  ajuste_k <- reactive({
    withProgress(message = "Calculando modelos de 1 a 10 fatores…", value = 0.5, {
      ajuste_por_k(correlacao()$R, nrow(amostra()), input$extracao)
    })
  })

  output$grafico_ajuste_k <- renderPlot(
    grafico_ajuste_k(ajuste_k(), nfat(), paralela()$sugeridos), res = 96)

  # =====================================================================
  # Aba 4
  # =====================================================================
  nomes_extracao <- c(minres = "resíduos mínimos", ml = "máxima verossimilhança", pa = "eixos principais")

  output$aviso_afe <- renderUI({
    a <- afe_com()
    if (!a$ok) return(div(class = "alert alert-danger small",
      strong("Não foi possível estimar o modelo com estas escolhas. "),
      sprintf("Tente outro método de extração (agora: %s), menos fatores ou uma amostra maior.",
              nomes_extracao[[input$extracao]])))
    if (a$heywood) return(div(class = "alert alert-warning small",
      strong("⚠ Caso de Heywood. "),
      "Algum item ficou com comunalidade (h²) igual ou maior que 1 — o que, na teoria, é impossível. ",
      "Isso costuma acontecer quando há fatores demais ou a amostra é pequena. Os resultados abaixo não são confiáveis: ",
      "tente menos fatores, outro método de extração ou uma amostra maior."))
    NULL
  })

  leitura <- reactive(interpretar_ajuste(afe(), paralela()$sugeridos))

  tema_classe <- function(classe) switch(classe,
    bom = "success", aceitavel = "primary", mediocre = "warning", ruim = "danger", "secondary")

  output$caixa_tli <- renderUI({
    a <- afe(); l <- leitura()
    value_box(
      title = "TLI (índice de Tucker-Lewis)",
      value = fmt(a$TLI, 3),
      p(strong(paste0("Ajuste ", NOMES_CLASSE[[l$c_tli]], "."))),
      p(class = "small", l$texto_tli),
      p(class = "small opacity-75", "Referência: ≥ 0,95 bom; ≥ 0,90 aceitável; abaixo disso, ruim. Quanto maior, melhor."),
      theme = tema_classe(l$c_tli)
    )
  })

  output$caixa_rmsea <- renderUI({
    a <- afe(); l <- leitura()
    value_box(
      title = "RMSEA (erro de aproximação)",
      value = fmt(a$RMSEA, 3),
      p(strong(paste0("Ajuste ", NOMES_CLASSE[[l$c_rmsea]], ".")),
        if (!is.na(a$RMSEA_inf))
          span(class = "small", sprintf(" Intervalo de 90%%: %s a %s.", fmt(a$RMSEA_inf, 3), fmt(a$RMSEA_sup, 3)))),
      p(class = "small", l$texto_rmsea),
      p(class = "small opacity-75", "Referência: ≤ 0,05 bom; ≤ 0,08 aceitável; ≤ 0,10 medíocre; acima, ruim. Quanto menor, melhor."),
      theme = tema_classe(l$c_rmsea)
    )
  })

  output$caixa_variancia <- renderUI({
    a <- afe()
    value_box(
      title = "Variância explicada",
      value = fmt_pct(a$total),
      p(strong(sprintf("Pelos %s juntos.", if (a$nfatores == 1) "1 fator" else paste(a$nfatores, "fatores")))),
      p(class = "small",
        sprintf("Em média, os fatores explicam %s da variação das respostas aos itens. O resto é a parte de cada item que não é compartilhada com os outros (inclui o erro de medida).",
                fmt_pct(a$total))),
      p(class = "small opacity-75", "Em psicologia, valores a partir de 50% costumam ser considerados bons, mas é comum ficar abaixo disso com questionários de personalidade."),
      theme = if (a$total >= 0.5) "success" else "primary"
    )
  })

  output$leitura_ajuste <- renderUI({
    l <- leitura()
    # Fundo claro com uma barra colorida à esquerda (texto escuro, fácil de ler)
    cor <- switch(l$nivel, bom = "#0ca30c", ruim = "#d03b3b", "#c98500")
    fundo <- switch(l$nivel, bom = "#eef8ee", ruim = "#fbeeee", "#fdf5e6")
    div(class = "p-3 mb-3 rounded",
        style = sprintf("background:%s;border-left:5px solid %s;", fundo, cor),
        div(class = "fw-bold mb-1", "O que os índices dizem juntos: ",
            selo(l$nivel, l$titulo)),
        p(class = "mb-1", l$texto),
        p(class = "small mb-0 opacity-75",
          "Lembre: esses pontos de corte são convenções (Hu & Bentler, 1999), não leis. Um bom ajuste não garante que os fatores façam sentido — isso quem decide é você, olhando as cargas."))
  })

  output$nota_obliqua <- renderUI({
    if (!afe()$obliqua) return(NULL)
    div(class = "small text-muted mb-2",
      p(class = "mb-1", "Com rotação oblíqua, existem duas tabelas diferentes:"),
      tags$ul(class = "mb-1",
        tags$li(strong("Matriz padrão:"), " o efeito ", em("único"), " de cada fator sobre o item, descontando os outros fatores. ",
                "É a que usamos para decidir a que fator cada item pertence."),
        tags$li(strong("Matriz de estrutura:"), " a correlação simples entre o item e cada fator. ",
                "Como os fatores se correlacionam, um item acaba se correlacionando um pouco com fatores aos quais não pertence.")),
      p(class = "mb-0", "Só sem correlação entre os fatores (varimax) as duas são iguais — e só aí a carga é a correlação item–fator. ",
        "Nas oblíquas, as porcentagens de variância dos fatores se sobrepõem e não podem ser somadas."))
  })

  output$tabela_cargas <- renderUI(
    tabela_cargas(afe(), input$corte, isTRUE(input$agrupar_cargas),
                  matriz = if (is.null(input$matriz_cargas)) "padrao" else input$matriz_cargas))

  # --- Diagrama do modelo
  output$diagrama <- renderPlot({
    a <- afe()
    graphics::par(mar = c(0.5, 0.5, 0.5, 0.5))
    psych::fa.diagram(a$L, Phi = a$Phi, cut = input$corte, simple = TRUE,
                      digits = 2, main = "", cex = 0.9)
  }, res = 96)

  # --- Resíduos
  output$resumo_residuos <- renderUI({
    a <- afe()
    pct <- a$prop_res_grandes
    frase <- if (pct > 0.30) {
      "Muitos resíduos grandes: os fatores deixam várias relações sem explicar. Procure blocos coloridos — eles mostram grupos de itens que ainda “pedem” um fator."
    } else if (pct > 0.10) {
      "Uma parte das relações ficou sem explicação. Veja se os resíduos maiores se concentram em algum grupo de itens (um possível fator a mais) ou estão espalhados."
    } else if (pct > 0.02) {
      "Poucos resíduos grandes: o modelo reproduz bem a maior parte das correlações."
    } else {
      "Praticamente nenhum resíduo grande. Se o número de fatores for alto, cuidado: o modelo pode estar “decorando” os detalhes desta amostra."
    }
    nivel <- if (pct > 0.30) "ruim" else if (pct > 0.10) "atencao" else "bom"
    div(class = "mb-2",
        p(class = "mb-1",
          strong("RMSR: "), fmt(a$rmsr, 3),
          span(class = "text-muted small", " (raiz da média dos resíduos ao quadrado; quanto menor, melhor — abaixo de 0,05 costuma ser considerado bom)")),
        p(class = "mb-1",
          strong("Resíduos maiores que 0,05: "), fmt_pct(pct, 0),
          span(class = "text-muted small", " dos pares de itens")),
        p(class = "mb-0", selo(nivel, frase)))
  })

  output$mapa_residuos <- renderPlot(
    grafico_correlacao(afe()$residuos, limite = 0.2, rotulo = "resíduo"), res = 96)

  # --- Guardar e comparar soluções
  solucao_guardada <- reactiveVal(NULL)

  info_atual <- reactive(list(
    nfatores = nfat(), extracao = input$extracao, rotacao = input$rotacao,
    correlacao = input$correlacao, n = nrow(amostra()), sorteio = semente() - 2025))

  observeEvent(input$guardar_solucao, {
    a <- afe_com()
    if (!isTRUE(a$ok)) {
      showNotification("Não há solução válida para guardar.", type = "warning")
      return()
    }
    solucao_guardada(list(afe = a, info = info_atual()))
    showNotification("Solução guardada! Mude as escolhas e abra a sub-aba “Comparar”.", type = "message")
  })

  output$descricao_guardada <- renderUI({
    g <- solucao_guardada()
    if (is.null(g)) return(span(class = "small text-muted", "Nenhuma solução guardada ainda."))
    span(class = "small", strong("Guardada: "), descrever_solucao(g$info))
  })

  output$conteudo_comparar <- renderUI({
    g <- solucao_guardada()
    if (is.null(g)) return(div(class = "text-muted",
      p("Ainda não há solução guardada. Para comparar duas soluções:"),
      tags$ol(
        tags$li("Escolha uma solução (por exemplo, 5 fatores com varimax) e clique em ",
                strong("Guardar esta solução para comparar"), "."),
        tags$li("Mude o que quiser na barra lateral (rotação, número de fatores, tipo de correlação…)."),
        tags$li("Volte aqui para ver as duas lado a lado."))))
    a <- afe()
    tagList(
      p(class = "small", strong("Guardada: "), descrever_solucao(g$info), br(),
        strong("Atual: "), descrever_solucao(info_atual())),
      tabela_comparacao(comparar_solucoes(g$afe, a, input$corte), g$afe, a)
    )
  })

  # =====================================================================
  # Aba 5
  # =====================================================================
  # Quando muda o nº de fatores, atualiza as opções dos eixos.
  observeEvent(nfat(), {
    opcoes <- paste0("F", seq_len(nfat()))
    x <- if (isTRUE(input$eixo_x %in% opcoes)) input$eixo_x else "F1"
    y <- if (isTRUE(input$eixo_y %in% opcoes)) input$eixo_y else "F2"
    updateSelectInput(session, "eixo_x", choices = opcoes, selected = x)
    updateSelectInput(session, "eixo_y", choices = opcoes, selected = y)
  })

  eixos <- reactive({
    k <- nfat()
    req(k >= 2)
    x <- match(input$eixo_x, paste0("F", seq_len(k)))
    y <- match(input$eixo_y, paste0("F", seq_len(k)))
    if (is.na(x)) x <- 1
    if (is.na(y)) y <- 2
    if (x == y) y <- if (x == 1) 2 else 1
    c(x, y)
  })

  nomes_rotacao <- c(none = "nenhuma", varimax = "varimax", oblimin = "oblimin", promax = "promax")

  output$titulo_depois <- renderText({
    if (input$rotacao == "none") "Depois da rotação (nenhuma rotação escolhida)"
    else paste0("Depois da rotação ", nomes_rotacao[[input$rotacao]])
  })

  output$grafico_antes <- renderPlot({
    a <- afe_sem(); req(a$ok, ncol(a$L) >= 2)
    e <- eixos()
    grafico_cargas(a$L, e[1], e[2], input$corte)
  }, res = 96)

  output$grafico_depois <- renderPlot({
    a <- afe(); req(ncol(a$L) >= 2)
    e <- eixos()
    grafico_cargas(a$L, e[1], e[2], input$corte)
  }, res = 96)

  output$tabela_rotacao <- renderUI({
    s <- afe_sem(); req(s$ok, ncol(s$L) >= 2)
    tabela_comparacao_rotacao(s, afe(), input$corte)
  })

  output$texto_rotacao <- renderUI({
    req(nfat() >= 2)
    r <- input$rotacao
    textos <- list(
      none = "Nenhuma rotação escolhida: os dois gráficos são iguais. Escolha varimax, oblimin ou promax na barra lateral e compare.",
      varimax = "A varimax é uma rotação ortogonal: os eixos giram juntos, sempre em ângulo reto. Por isso os fatores continuam sem correlação entre si.",
      oblimin = "A oblimin é uma rotação oblíqua: cada eixo pode girar por conta própria, sem precisar ficar em ângulo reto. Por isso os fatores podem se correlacionar (veja a aba 6).",
      promax = "A promax é uma rotação oblíqua: cada eixo pode girar por conta própria, sem precisar ficar em ângulo reto. Por isso os fatores podem se correlacionar (veja a aba 6)."
    )
    tagList(
      p(class = "small", HTML(paste0(
        "<strong>Repare:</strong> a variância explicada pelos fatores juntos não muda — a rotação não melhora nem piora o ajuste do modelo (TLI e RMSEA continuam iguais). ",
        "Ela só redistribui a variância entre os fatores, buscando a chamada <em>estrutura simples</em>: ",
        "cada item com carga forte em um só fator."))),
      p(class = "small text-muted mb-0", textos[[r]])
    )
  })

  # =====================================================================
  # Aba 6
  # =====================================================================
  output$conteudo_phi <- renderUI({
    a <- afe()
    if (a$nfatores == 1) return(p("Com 1 fator só, não há correlação entre fatores para mostrar."))
    if (!a$obliqua) return(tagList(
      p(sprintf("Com %s, os fatores são ", if (input$rotacao == "none") "a solução sem rotação" else "a rotação varimax (ortogonal)"),
        strong("obrigados a ter correlação zero"), " entre si — é uma regra do método, não algo que os dados mostraram."),
      p(class = "small text-muted mb-0",
        "Para deixar os dados dizerem se os fatores se correlacionam, escolha uma rotação oblíqua (oblimin ou promax) na barra lateral.")))

    Phi <- a$Phi
    maior <- max(abs(Phi[lower.tri(Phi)]))
    tagList(
      p("Com uma rotação oblíqua, os fatores podem se correlacionar. A tabela mostra a correlação entre cada par de fatores ",
        "(cores fortes: |r| ≥ 0,32)."),
      tabela_phi(Phi),
      if (maior >= 0.32)
        p(selo("bom", sprintf("A maior correlação entre fatores é %s (em módulo).", fmt(maior, 2))),
          " Isso passa de 0,32, o valor que costuma ser usado como referência (Tabachnick & Fidell): os fatores se correlacionam o bastante para justificar uma rotação oblíqua.")
      else
        p(selo("atencao", sprintf("A maior correlação entre fatores é %s (em módulo).", fmt(maior, 2))),
          " Como fica abaixo de 0,32 (referência de Tabachnick & Fidell), uma rotação ortogonal (varimax) daria um resultado parecido — e um pouco mais simples de interpretar.")
    )
  })

  alfas <- reactive(alfa_por_fator(correlacao()$R, afe()$L, input$corte))

  output$tabela_alfa <- renderUI(tabela_alfa(alfas()))

  output$itens_fora <- renderUI({
    fora <- alfas()$fora
    if (length(fora) == 0) return(p(class = "small", selo("bom", "Todos os itens entraram em algum fator.")))
    p(class = "small",
      selo("atencao", sprintf("%s sem carga forte em nenhum fator (ficaram fora do alfa): ",
                              if (length(fora) == 1) "1 item ficou" else paste(length(fora), "itens ficaram"))),
      lapply(fora, chip_codigo))
  })

  output$nota_alfa <- renderUI({
    p(class = "small text-muted mb-0", paste0(
      "Referência: ≥ 0,90 excelente; 0,80 bom; 0,70 aceitável; 0,60 questionável; 0,50 pobre; abaixo, inaceitável. ",
      "O alfa é calculado a partir da matriz de correlação em uso (alfa padronizado)",
      if (input$correlacao == "policorica") "; como ela é policórica, este é o chamado alfa ordinal" else "",
      ". Fatores com poucos itens tendem a ter alfa mais baixo."))
  })

  output$nota_omega <- renderUI({
    div(class = "small mt-2 p-2 rounded", style = "background:#f4f3ef;",
      p(class = "mb-1", strong("Alfa ou ômega? "),
        "O alfa supõe que todos os itens do fator têm a mesma carga — o que raramente é verdade. ",
        "O ", strong("ômega"), " (de McDonald) é calculado a partir das cargas e não faz essa suposição; hoje ele é o mais recomendado. ",
        "Quando os dois ficam parecidos, as cargas dos itens são parecidas; quando o ômega fica bem acima do alfa, as cargas são desiguais."),
      p(class = "mb-0", strong("Cuidado: "),
        "um alfa (ou ômega) alto não prova que o fator mede uma coisa só. Ele também sobe só por haver mais itens. ",
        "A evidência de que o fator é unidimensional vem da análise fatorial (cargas e resíduos), não do alfa."))
  })

  # =====================================================================
  # Missões guiadas
  # =====================================================================
  missao_atual <- reactiveVal(1L)
  concluidas <- reactiveVal(integer(0))

  # Guarda o progresso no navegador
  salvar_missoes <- function() {
    session$sendCustomMessage("salvar_missoes",
      list(concluidas = as.list(concluidas()), atual = missao_atual()))
  }

  # Ao abrir o app, recupera o progresso guardado (se houver)
  observeEvent(input$missoes_salvas, {
    s <- input$missoes_salvas
    feitas <- suppressWarnings(as.integer(unlist(s$concluidas)))
    concluidas(sort(unique(feitas[!is.na(feitas) & feitas %in% seq_len(N_MISSOES)])))
    a <- suppressWarnings(as.integer(s$atual))
    if (length(a) == 1 && !is.na(a) && a >= 1 && a <= N_MISSOES) missao_atual(a)
  }, once = TRUE)

  # Trocar de missão (botões numerados e "Próxima missão")
  observeEvent(input$missao_escolhida, {
    k <- as.integer(input$missao_escolhida)
    if (!is.na(k) && k >= 1 && k <= N_MISSOES) {
      missao_atual(k)
      salvar_missoes()
    }
  })

  # "Concluí esta missão": o app confere de novo antes de aceitar
  observeEvent(input$missao_concluida, {
    k <- as.integer(input$missao_concluida)
    if (!is.na(k) && all(conferir_missao(MISSOES[[k]], input))) {
      concluidas(sort(unique(c(concluidas(), k))))
      salvar_missoes()
    }
  })

  observeEvent(input$ir_para_aba, nav_select("aba", input$ir_para_aba))

  observeEvent(input$recomecar_missoes, {
    concluidas(integer(0))
    missao_atual(1L)
    salvar_missoes()
  })

  output$missoes_topo <- renderUI(tagList(
    progresso_missoes(concluidas()),
    botoes_missoes(missao_atual(), concluidas())
  ))

  # Resumo de uma linha no cabeçalho (é o que aparece com o painel fechado)
  output$missoes_resumo <- renderUI({
    k <- missao_atual()
    span(sprintf("· Missão %d: %s%s · %d de %d concluídas",
                 k, MISSOES[[k]]$titulo, if (k %in% concluidas()) " ✔︎" else "",
                 length(concluidas()), N_MISSOES))
  })

  output$missao_texto <- renderUI(conteudo_missao(missao_atual()))

  output$missao_reflexao <- renderUI(reflexao_missao(missao_atual()))

  output$missao_conferencia <- renderUI({
    k <- missao_atual()
    m <- MISSOES[[k]]
    feita <- k %in% concluidas()
    ok <- conferir_missao(m, input)

    botao <- if (feita) {
      div(class = "d-flex flex-wrap align-items-center gap-2",
          span(class = "fw-bold", style = "color:#0ca30c;", "✔︎ Missão concluída!"),
          if (k < N_MISSOES) tags$button(
            type = "button", class = "btn btn-sm btn-primary",
            onclick = sprintf("Shiny.setInputValue('missao_escolhida', %d, {priority: 'event'})", k + 1),
            "Próxima missão →")
          else span(class = "small", "Você concluiu todas as missões. Parabéns!"))
    } else {
      tagList(
        tags$button(
          type = "button", class = "btn btn-sm btn-success",
          disabled = if (!all(ok)) NA,
          onclick = sprintf("Shiny.setInputValue('missao_concluida', %d, {priority: 'event'})", k),
          "✔︎ Concluí esta missão"),
        if (!all(ok)) div(class = "small text-muted mt-1",
                           "O botão é liberado quando todos os itens da conferência estiverem marcados.")
      )
    }
    tagList(lista_conferencia(m, ok), botao)
  })
}

shinyApp(ui, server)
