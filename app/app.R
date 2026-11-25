# Laboratório de Psicometria – Análise Fatorial Exploratória
# © 2026 Leonardo Ferreira Russo — licença CC BY-NC-ND 4.0 (veja o arquivo LICENSE)
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
  div(style = "margin-top:-.6rem;",
      uiOutput("nota_correlacao"),
      actionLink("por_que_policorica", "Pearson ou policórica? Entenda", class = "small")),
  # Desativa a opção "Policórica" quando a amostra não tem o cálculo pronto
  tags$script(HTML("
    Shiny.addCustomMessageHandler('policorica_disponivel', function(ok) {
      var el = $('input[name=correlacao][value=policorica]');
      el.prop('disabled', !ok);
      el.closest('label').css('opacity', ok ? 1 : 0.45)
        .attr('title', ok ? '' : 'Disponível só com a amostra completa ou a inicial (1.000 pessoas, sorteio nº 1)');
    });
  ")),

  sliderInput("corte", "Ponto de corte das cargas",
              min = 0.10, max = 0.60, value = 0.30, step = 0.05, ticks = FALSE),

  sliderInput("n_amostra", "Tamanho da amostra",
              min = 50, max = N_MAX_SLIDER, value = 1000, step = 50, ticks = FALSE),
  div(class = "small text-muted", style = "margin-top:-.6rem;",
      textOutput("resumo_amostra", inline = TRUE)),

  actionButton("sortear", "Sortear outra amostra",
               icon = icon("shuffle"), class = "btn-outline-primary"),

  div(class = "small mt-2",
      uiOutput("resumo_itens_lateral"),
      actionLink("editar_itens", "Escolher os itens (aba 1)", class = "small")),

  # Autoria e licença (sempre visível)
  div(class = "small text-muted mt-4 pt-2 border-top",
      HTML(paste0(
        "© 2026 <strong>Leonardo Ferreira Russo</strong><br>",
        "Licença <a href='https://creativecommons.org/licenses/by-nc-nd/4.0/deed.pt-br' target='_blank' rel='noopener'>CC BY-NC-ND 4.0</a>",
        " · <a href='#' onclick=\"document.querySelector('a[data-value=inicio]').click(); setTimeout(function(){document.getElementById('autoria').scrollIntoView({behavior:'smooth'});}, 150); return false;\">como citar</a>")))
)

# --- Aba 0: comece aqui (apresentação do laboratório)
# Diagrama simples do modelo fatorial: um fator latente (que não vemos)
# influencia as respostas a três itens (que vemos); cada item tem também
# uma parte própria (o "erro" ou parte única).
diagrama_modelo <- HTML('
<svg viewBox="0 0 560 230" role="img" aria-label="Diagrama: o fator Extroversão influencia as respostas aos itens E3, E4 e E5; cada item também tem uma parte própria" style="width:100%;max-width:560px;height:auto;font-family:inherit;">
  <defs><marker id="seta" viewBox="0 0 10 10" refX="9" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
    <path d="M0,0 L10,5 L0,10 z" fill="#52514e"/></marker></defs>
  <ellipse cx="110" cy="115" rx="92" ry="46" fill="#e3f4ec" stroke="#1baf7a" stroke-width="2.5"/>
  <text x="110" y="110" text-anchor="middle" font-size="16" font-weight="700" fill="#0b0b0b">Extroversão</text>
  <text x="110" y="131" text-anchor="middle" font-size="12" fill="#52514e">fator (não observado)</text>
  <g font-size="13" fill="#0b0b0b">
    <rect x="300" y="22" width="170" height="44" rx="6" fill="#fff" stroke="#c3c2b7" stroke-width="1.5"/>
    <text x="385" y="49" text-anchor="middle">E3 Sabe cativar as pessoas</text>
    <rect x="300" y="93" width="170" height="44" rx="6" fill="#fff" stroke="#c3c2b7" stroke-width="1.5"/>
    <text x="385" y="120" text-anchor="middle">E4 Faz amigos com facilidade</text>
    <rect x="300" y="164" width="170" height="44" rx="6" fill="#fff" stroke="#c3c2b7" stroke-width="1.5"/>
    <text x="385" y="191" text-anchor="middle">E5 Assume o comando</text>
  </g>
  <g stroke="#52514e" stroke-width="2" marker-end="url(#seta)" fill="none">
    <path d="M195,98 L296,46"/><path d="M203,115 L296,115"/><path d="M195,132 L296,184"/>
  </g>
  <g font-size="12" fill="#52514e" text-anchor="middle">
    <text x="240" y="62">carga</text>
  </g>
  <g stroke="#898781" stroke-width="1.5" marker-end="url(#seta)" fill="none">
    <path d="M520,44 L474,44"/><path d="M520,115 L474,115"/><path d="M520,186 L474,186"/>
  </g>
  <g font-size="12" fill="#52514e">
    <circle cx="534" cy="44" r="13" fill="#f4f3ef" stroke="#898781"/><text x="534" y="48" text-anchor="middle">u</text>
    <circle cx="534" cy="115" r="13" fill="#f4f3ef" stroke="#898781"/><text x="534" y="119" text-anchor="middle">u</text>
    <circle cx="534" cy="186" r="13" fill="#f4f3ef" stroke="#898781"/><text x="534" y="190" text-anchor="middle">u</text>
  </g>
</svg>')

caixa_info <- function(..., cor = "#f4f3ef", borda = NULL) div(
  class = "p-3 mb-3 rounded",
  style = paste0("background:", cor, ";", if (!is.null(borda)) paste0("border-left:5px solid ", borda, ";")),
  ...)

# Parágrafo escrito em HTML: assim o negrito fica "colado" à pontuação
# (sem os espaços extras que aparecem quando se juntam pedaços de texto).
ph <- function(texto, ...) p(HTML(texto), ...)

passo_aba <- function(numero, nome, pergunta) tags$li(
  class = "mb-1", HTML(sprintf("<strong>%d. %s</strong> <span class='text-muted'>—</span> %s", numero, nome, pergunta)))

DESCRICOES_DOMINIO <- c(
  A = "empatia, cooperação, consideração pelos outros",
  C = "organização, disciplina, responsabilidade",
  E = "sociabilidade, energia, gosto por estar com pessoas",
  N = "tendência a sentir emoções negativas, como ansiedade e irritação",
  O = "curiosidade intelectual, imaginação, interesse por ideias")

aba_inicio <- nav_panel(
  "0. Comece aqui", value = "inicio",
  div(
    style = "max-width:980px;",
    h4(class = "mt-1", "Bem-vindo ao Laboratório de Psicometria – Análise Fatorial Exploratória"),
    ph(class = "lead", style = "font-size:1.1rem;",
       "Aqui você vai aprender Análise Fatorial Exploratória (AFE) <strong>fazendo</strong>: analisando dados reais, tomando decisões como um pesquisador e vendo as consequências de cada escolha."),

    # --- O alerta sobre a aba 1
    caixa_info(cor = "#fdf5e6", borda = "#c98500",
      p(class = "fw-bold mb-1", "⚠︎ Antes de começar: não se assuste com a aba 1"),
      ph(class = "mb-1",
         "Na próxima aba (<strong>1. Itens</strong>) estão os 25 itens da escala. Ali também aparecem várias colunas e números (KMO, h², cargas, alfa…) que você <strong>ainda não sabe usar</strong> — e está tudo bem."),
      ph(class = "mb-0",
         "Por enquanto, <strong>apenas leia os itens</strong> para conhecê-los e siga o passo a passo das <strong>missões</strong>. Mais adiante, com mais conhecimento, você vai voltar a essa aba para tomar decisões sobre os itens.")),

    # --- A escala
    h5(class = "mt-4", "A escala que vamos analisar"),
    ph("Vamos trabalhar com <strong>25 itens de personalidade</strong> do modelo dos <strong>Cinco Grandes Fatores</strong> (Big Five), o modelo de personalidade mais estudado da psicologia. Os itens vêm do <em>International Personality Item Pool</em> (IPIP) e foram respondidos on-line por 2.800 pessoas no projeto de pesquisa <em>SAPA</em>. Os dados estão no pacote psych do R (conjunto <code>bfi</code>); usamos as <strong>2.436 pessoas</strong> que responderam a todos os itens."),
    ph("Cada pessoa disse o quanto cada frase a descreve, numa escala de <strong>1 (discordo totalmente) a 6 (concordo totalmente)</strong>. Os itens foram escritos para medir cinco traços, com 5 itens cada:"),
    tags$ul(
      class = "mb-2",
      lapply(names(DOMINIOS), function(l) tags$li(HTML(sprintf(
        "<span style='display:inline-block;width:.85em;height:.85em;border-radius:2px;margin-right:.4em;vertical-align:-.05em;background:%s;'></span><strong>%s — %s</strong>: %s",
        CORES_DOMINIO[[l]], l, DOMINIOS[[l]], DESCRICOES_DOMINIO[[l]]))))),
    ph(class = "small text-muted",
       "Alguns itens são <strong>invertidos</strong> (marcados com (−)): concordar com eles indica <em>menos</em> do traço — por exemplo, “E1 Fala pouco” indica menos Extroversão."),

    # --- O que é AFE
    h5(class = "mt-4", "O que é a Análise Fatorial Exploratória?"),
    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)),
      div(
        ph("Muitas coisas que a psicologia estuda — como a Extroversão — <strong>não podem ser observadas diretamente</strong>. O que observamos são as <strong>respostas aos itens</strong>."),
        ph("A ideia central da AFE é simples: se vários itens são influenciados pelo mesmo traço, as respostas a eles <strong>andam juntas</strong> (se correlacionam). Quem é extrovertido tende a concordar com “Faz amigos com facilidade” <em>e</em> com “Sabe cativar as pessoas”."),
        ph("A AFE olha para as correlações entre todos os itens e procura o <strong>menor número de fatores</strong> (variáveis “escondidas”) capaz de explicá-las. Depois, mostra o quanto cada item está ligado a cada fator — a <strong>carga fatorial</strong>.")
      ),
      div(
        diagrama_modelo,
        ph(class = "small text-muted mt-1",
           "O fator (oval) influencia as respostas aos itens (retângulos); a força de cada seta é a carga. Cada item tem também uma parte própria (<strong>u</strong>), que o fator não explica — inclui o erro de medida.")
      )
    ),
    h6(class = "mt-2", "Para que serve na psicometria?"),
    tags$ul(
      tags$li(HTML("<strong>Construir instrumentos:</strong> descobrir quais itens “andam juntos” e formam cada escala.")),
      tags$li(HTML("<strong>Buscar evidências de validade:</strong> verificar se a estrutura dos itens corresponde ao que o instrumento pretende medir (a chamada evidência baseada na estrutura interna).")),
      tags$li(HTML("<strong>Identificar itens problemáticos:</strong> itens que não se ligam a nenhum fator, ou que se ligam a vários.")),
      tags$li(HTML("<strong>Resumir:</strong> trocar dezenas de itens por poucos fatores, mais fáceis de interpretar e de pontuar."))),
    caixa_info(
      p(class = "small mb-1 fw-bold", "AFE, AFC e ACP — qual a diferença?"),
      tags$ul(class = "small mb-0",
        tags$li(HTML("<strong>AFE (exploratória):</strong> os dados mostram quantos fatores há e quais itens se ligam a cada um. É usada quando ainda estamos descobrindo a estrutura.")),
        tags$li(HTML("<strong>AFC (confirmatória):</strong> o pesquisador diz de antemão qual item pertence a qual fator, e o modelo testa se os dados concordam. Costuma vir depois da AFE, em outra amostra.")),
        tags$li(HTML("<strong>ACP (análise de componentes principais):</strong> parecida na aparência, mas apenas resume a variação dos itens, sem supor fatores “escondidos” por trás deles. Não é a mesma coisa que AFE.")))),

    # --- A atividade
    h5(class = "mt-4", "A atividade"),
    ph("Você vai fazer o papel de quem pesquisa. A pergunta é: <strong>esses 25 itens realmente formam cinco fatores, como a teoria prevê? E todos os itens funcionam bem?</strong> Para responder, você vai percorrer as etapas de uma AFE, na ordem em que elas são feitas na prática:"),
    tags$ul(
      class = "list-unstyled ms-3",
      passo_aba(1, "Itens", "quais são os itens? (por enquanto, só leia — voltaremos aqui no fim)"),
      passo_aba(2, "Correlações", "os itens se relacionam? Formam grupos?"),
      passo_aba(3, "KMO e Bartlett", "os dados servem para uma análise fatorial?"),
      passo_aba(4, "Quantos fatores?", "quantos fatores devemos extrair?"),
      passo_aba(5, "Cargas fatoriais", "o modelo se ajusta aos dados? Quais itens formam cada fator?"),
      passo_aba(6, "Rotação", "como deixar os fatores mais fáceis de interpretar?"),
      passo_aba(7, "Fatores e alfa", "os fatores se relacionam? As escalas são confiáveis?")),
    ph("No fim, com tudo isso aprendido, você <strong>volta à aba 1</strong> para tomar decisões sobre os itens — tirar um item fraco, decidir a escala de um item ambíguo — e ver como cada decisão muda os resultados."),

    # --- Como usar
    h5(class = "mt-4", "Como usar o laboratório"),
    tags$ul(
      tags$li(HTML("<strong>Missões guiadas:</strong> no alto da página. São 17 missões, do básico ao desafio, que guiam você pelas abas. Cada uma diz o que fazer, confere se você fez e termina com uma pergunta para refletir. O seu progresso fica salvo neste navegador.")),
      tags$li(HTML("<strong>Barra lateral:</strong> os controles da análise (número de fatores, rotação, tamanho da amostra…). Mexa à vontade: tudo é recalculado na hora, e nada se estraga.")),
      tags$li(HTML("<strong>Ponto de partida:</strong> comece pela missão 1 e siga a ordem."))),
    tags$button(
      type = "button", class = "btn btn-primary mt-1 mb-4",
      onclick = paste0(
        "Shiny.setInputValue('missao_escolhida', 1, {priority: 'event'});",
        "bootstrap.Collapse.getOrCreateInstance(document.getElementById('corpo_missoes')).show();",
        # Neste layout quem rola é a área principal (não a janela): por isso
        # pedimos ao próprio painel de missões que venha para o topo da tela.
        "setTimeout(function() {",
        "  document.querySelector('.painel-missoes').scrollIntoView({behavior: 'smooth', block: 'start'});",
        "}, 50);"),
      "Começar pela missão 1 →"),

    # --- Autoria e licença
    h5(class = "mt-4", id = "autoria", "Autoria, licença e como citar"),
    caixa_info(
      ph(class = "mb-2",
         "Este laboratório foi criado por <strong>Leonardo Ferreira Russo</strong> © 2026. O código, os textos, as missões e o material didático estão licenciados sob a licença <a href='https://creativecommons.org/licenses/by-nc-nd/4.0/deed.pt-br' target='_blank' rel='noopener'>Creative Commons Atribuição-NãoComercial-SemDerivações 4.0 (CC BY-NC-ND 4.0)</a>."),
      ph(class = "small mb-2",
         "Você pode <strong>usar e compartilhar</strong> o laboratório (por exemplo, em aulas), desde que <strong>cite o autor</strong>, <strong>não use para fins comerciais</strong> e <strong>não distribua versões modificadas</strong>. Outros usos dependem de autorização do autor."),
      ph(class = "small mb-2",
         "<strong>Como citar:</strong> Russo, L. F. (2026). <em>Laboratório de Psicometria – Análise Fatorial Exploratória</em> [Aplicativo web]. https://russofleonardo.github.io/app-psicometria/"),
      ph(class = "small text-muted mb-0",
         "Dados: conjunto <code>bfi</code> do pacote psych (W. Revelle), com itens do International Personality Item Pool, coletados no projeto SAPA. Feito com R, Shiny, bslib, psych, GPArotation e Shinylive, que mantêm suas próprias licenças."))
  )
)

# --- Aba 1: os itens (incluir/excluir e montar as escalas)
aba_itens <- nav_panel(
  "1. Itens", value = "itens",
  div(class = "small p-2 mb-2 rounded", style = "background:#fdf5e6;border-left:4px solid #c98500;",
      strong("Primeira vez aqui? "),
      "Por enquanto, só leia os itens. As colunas com números (KMO, h², cargas, alfa…) vão fazer sentido ao longo das missões — ",
      "você volta a esta aba no fim, para tomar decisões sobre os itens."),
  p("Antes de analisar, conheça os ", strong("25 itens"), " do questionário. Cada pessoa disse o quanto cada frase a descreve, ",
    "de 1 (discordo totalmente) a 6 (concordo totalmente). Os itens marcados com ", strong("(−)"), " são ", strong("invertidos"),
    ": concordar com eles indica ", em("menos"), " do traço."),
  p(class = "small text-muted",
    "Nesta aba você pode ", strong("tirar itens da análise"), " (desmarcando a caixinha) e decidir de ", strong("qual escala"),
    " cada item faz parte (para o alfa e o ômega da aba 7). Volte aqui quando quiser: todas as abas passam a usar só os itens marcados."),
  uiOutput("contexto_itens"),
  div(class = "d-flex flex-wrap align-items-center gap-2 mb-2",
      uiOutput("resumo_itens", inline = TRUE),
      actionButton("recolocar_itens", "Recolocar todos os itens", icon = icon("rotate-left"),
                   class = "btn-sm btn-outline-secondary")),
  uiOutput("avisos_itens"),
  legenda_dominios(),
  uiOutput("tabela_itens"),
  tags$details(
    class = "mt-2 mb-3",
    tags$summary(strong("Quando excluir um item? (leia antes de decidir)")),
    div(class = "small mt-2",
      p("Os ", strong("sinais para investigar"), " da tabela não são ordens de exclusão. Uma exclusão se justifica quando ",
        strong("duas evidências"), " apontam na mesma direção:"),
      tags$ol(
        tags$li(strong("Estatística:"), " KMO do item abaixo de 0,50; comunalidade (h²) muito baixa; nenhuma carga forte; carga cruzada que persiste; caso de Heywood."),
        tags$li(strong("Conteúdo:"), " ao reler o item, ele parece ambíguo, mal redigido ou medir outra coisa.")),
      p(strong("Cuidados:")),
      tags$ul(
        tags$li(strong("Uma decisão por vez."), " Excluir um item muda todas as cargas — às vezes outro item muda de fator."),
        tags$li(strong("Não “decore” a amostra."), " Excluir itens até o ajuste ficar bom pode só aproveitar o acaso desta amostra. Sorteie outra e veja se a decisão se sustenta."),
        tags$li(strong("Cada item retirado estreita o construto."), " Uma escala com menos itens pode deixar de medir partes importantes do traço."),
        tags$li(strong("Cada fator precisa de pelo menos 3 itens"), " para ficar bem definido."),
        tags$li(strong("Cuidado com os invertidos:"), " eles costumam ter cargas menores por um efeito de método. Tirá-los pode melhorar os números e piorar o instrumento (veja a missão 17)."),
        tags$li(strong("“Alfa sem o item” não decide sozinho:"), " um alfa maior não quer dizer uma escala melhor."),
        tags$li(strong("Registre e relate"), " cada decisão — o diário abaixo ajuda nisso."))
    )
  ),
  card(
    card_header(class = "d-flex align-items-center",
                "Diário de decisões",
                actionButton("reiniciar_diario", "Reiniciar o diário", icon = icon("eraser"),
                             class = "btn-sm btn-outline-secondary ms-auto")),
    p(class = "small text-muted",
      "Cada vez que você tira ou recoloca itens, o app registra como ficaram os principais resultados, com a configuração usada no momento. ",
      "Para comparar em detalhe duas situações, use “Guardar esta solução para comparar” na aba 5."),
    uiOutput("diario")
  )
)

# --- Aba 2: correlações
aba_correlacoes <- nav_panel(
  "2. Correlações", value = "correlacoes",
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

# --- Aba 3: KMO e Bartlett
aba_kmo <- nav_panel(
  "3. KMO e Bartlett", value = "kmo",
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

# --- Aba 4: scree plot e análise paralela
aba_scree <- nav_panel(
  "4. Quantos fatores?", value = "scree",
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
    p("Os gráficos abaixo comparam modelos de 1 a 10 fatores (com o método de extração e o tipo de correlação escolhidos na barra lateral), ",
      "usando três medidas. Antes de olhar, entenda o que cada uma diz:"),
    layout_columns(
      col_widths = breakpoints(sm = 12, md = c(6, 6), xl = c(3, 3, 3, 3)),
      div(class = "small p-2 rounded h-100", style = "background:#f4f3ef;",
          strong("TLI (índice de Tucker-Lewis)"), br(),
          "Compara o seu modelo com um modelo “sem fatores”, em que os itens não teriam nenhuma relação entre si. ",
          "Responde: ", em("quanto melhor do que o nada o meu modelo é?"), " Vai de 0 a 1 (às vezes passa um pouco de 1). ",
          strong("Quanto maior, melhor:"), " 0,95 ou mais é bom; de 0,90 a 0,95, aceitável."),
      div(class = "small p-2 rounded h-100", style = "background:#f4f3ef;",
          strong("RMSEA (erro de aproximação)"), br(),
          "Mede o erro médio do modelo ao reproduzir as correlações entre os itens, e ", em("penaliza"),
          " modelos com muitos fatores (complexidade tem preço). ",
          strong("Quanto menor, melhor:"), " até 0,05 é bom; até 0,08, aceitável; acima de 0,10, ruim."),
      div(class = "small p-2 rounded h-100", style = "background:#f4f3ef;",
          strong("Variância explicada"), br(),
          "Quanto da variação das respostas aos itens os fatores conseguem explicar, em média. ",
          "O resto é a parte própria de cada item (incluindo o erro de medida). ",
          HTML("<strong>Quanto maior, melhor</strong>, mas em questionários de personalidade é comum ficar abaixo de 50%.")),
      div(class = "small p-2 rounded h-100", style = "background:#fbeeee;",
          strong("Caso de Heywood ", span(style = "color:#d03b3b;", "✖︎")), br(),
          "Quando o modelo calcula que um item tem comunalidade (h²) igual ou maior que 1 — ou seja, que os fatores explicariam ",
          em("mais de 100%"), " do item, o que é impossível. É um sinal de que algo deu errado: geralmente ",
          strong("fatores demais"), " ou amostra pequena. Os resultados desse modelo não são confiáveis.")
    ),
    p(class = "small text-muted mt-3",
      "Nos gráficos, as faixas coloridas marcam os valores bons (verde) e aceitáveis (amarelo). ",
      "O anel preto marca o número de fatores escolhido agora; a linha laranja tracejada, a sugestão da análise paralela; ",
      "um ✖ vermelho, um caso de Heywood."),
    card_body(plotOutput("grafico_ajuste_k", height = "380px"), fillable = FALSE, padding = 0),
    div(class = "p-3 rounded mt-2", style = "background:#eef4fc;border-left:5px solid #2a78d6;",
        p(class = "fw-bold mb-1", "Os índices não decidem sozinhos"),
        p(class = "mb-2",
          "Repare que o ajuste ", strong("sempre"), " melhora quando acrescentamos fatores — no extremo, com um fator por item, ",
          "seria perfeito, e não teríamos resumido nada. Por isso, “o modelo com o melhor TLI” não é a resposta. ",
          "O número de fatores se decide juntando várias evidências:"),
        tags$ol(class = "mb-2",
          tags$li(strong("Análise paralela e scree plot"), " (acima): quantos fatores explicam mais do que o acaso."),
          tags$li(strong("Índices de ajuste e parcimônia"), " (estes gráficos): o ajuste é aceitável? ",
                  "A partir de que ponto cada fator novo quase não ajuda mais? Algum modelo tem caso de Heywood?"),
          tags$li(strong("As cargas de cada item"), " (aba 5): cada fator tem pelo menos 3 itens com cargas fortes? ",
                  "Há muitas cargas cruzadas ou itens sem fator? Um fator com um ou dois itens só costuma ser sinal de fatores demais."),
          tags$li(HTML("<strong>A teoria</strong>: os fatores fazem sentido? Dá para dar um nome a cada um? "),
                  "Eles correspondem ao que o instrumento foi feito para medir — ou a algo que a literatura conhece?")),
        p(class = "small mb-0",
          "Na prática, testamos os números de fatores candidatos (por exemplo, 5 e 6), olhamos as cargas de cada solução na aba 5 ",
          "e escolhemos a que é ao mesmo tempo adequada aos dados ", em("e"), " interpretável. ",
          "O botão “Guardar esta solução para comparar”, na aba 5, ajuda nessa comparação."))
  )
)

# --- Aba 5: cargas fatoriais e índices de ajuste
aba_cargas <- nav_panel(
  "5. Cargas fatoriais", value = "cargas",
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

# --- Aba 6: rotação
aba_rotacao <- nav_panel(
  "6. Rotação", value = "rotacao",
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

# --- Aba 7: correlação entre fatores e alfa
aba_fatores <- nav_panel(
  "7. Fatores e alfa", value = "fatores",
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
  aba_inicio, aba_itens, aba_correlacoes, aba_kmo, aba_scree, aba_cargas, aba_rotacao, aba_fatores
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
  title = "Laboratório de Psicometria – Análise Fatorial Exploratória",
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
  # A policórica é pesada demais para calcular no navegador a cada sorteio.
  # Por isso ela só está disponível nas amostras já calculadas (completa e
  # inicial); nas outras, o app usa Pearson.
  tem_policorica <- reactive(policorica_disponivel(n_amostra(), semente()))
  cor_policorica <- reactive(
    calcular_correlacao(amostra(), "policorica", chave_amostra(n_amostra(), semente())))
  correlacao <- reactive({
    if (identical(input$correlacao, "policorica") && tem_policorica()) cor_policorica() else cor_pearson()
  })

  observe({
    ok <- tem_policorica()
    session$sendCustomMessage("policorica_disponivel", ok)
    if (!ok && identical(isolate(input$correlacao), "policorica")) {
      updateRadioButtons(session, "correlacao", selected = "pearson")
      showNotification(
        "A correlação policórica só está disponível com a amostra completa ou a inicial (1.000 pessoas, sorteio nº 1). O app voltou para Pearson.",
        type = "warning", duration = 8)
    }
  })

  output$nota_correlacao <- renderUI({
    if (tem_policorica()) span(class = "small text-muted", "Policórica disponível para esta amostra.")
    else span(class = "small text-muted",
              "Policórica: só com a amostra completa ou a inicial (1.000 pessoas, sorteio nº 1).")
  })

  # --- Explicação "Pearson ou policórica?"
  comparacao_pp <- reactive(comparar_pearson_policorica())

  output$grafico_item_assimetrico <- renderPlot({
    cp <- comparacao_pp(); req(cp)
    grafico_respostas_item(cp$item_assim)
  }, res = 96)

  observeEvent(input$por_que_policorica, {
    cp <- comparacao_pp()
    showModal(modalDialog(
      title = "Pearson ou policórica?", size = "l", easyClose = TRUE,
      footer = modalButton("Fechar"),
      explicacao_policorica(cp)
    ))
  })

  # --- Os itens que entram na análise (aba 1) ---------------------------
  MIN_ITENS <- 6
  excluidos <- reactiveVal(character(0))
  incluidos <- reactive(setdiff(itens$codigo, excluidos()))
  # Espera um pouco (0,6 s) para juntar vários cliques antes de recalcular
  incluidos_d <- debounce(incluidos, 600)
  excluidos_d <- reactive(setdiff(itens$codigo, incluidos_d()))

  # A matriz de correlação usada em TODAS as análises: só os itens incluídos.
  # (Para Pearson e para a policórica pronta, tirar um item é o mesmo que
  # recalcular sem ele, porque as correlações são calculadas par a par.)
  R_analise <- reactive({
    inc <- incluidos_d()
    correlacao()$R[inc, inc, drop = FALSE]
  })

  # --- KMO / Bartlett e análise paralela --------------------------------
  kmo <- reactive(avaliar_kmo(R_analise(), nrow(amostra())))
  paralela <- reactive(analise_paralela(R_analise(), nrow(amostra()), semente()))

  # --- A análise fatorial -------------------------------------------------
  # Espera o aluno soltar o controle do nº de fatores (0,3 s).
  nfat <- debounce(reactive(input$nfatores), 300)

  # Sempre calculamos a solução SEM rotação (para a aba 6 comparar) e a
  # solução COM a rotação escolhida. Se a rotação for "nenhuma", são iguais.
  afe_sem <- reactive(rodar_afe(R_analise(), nrow(amostra()), nfat(), input$extracao, "none"))
  afe_com <- reactive({
    if (input$rotacao == "none") afe_sem()
    else rodar_afe(R_analise(), nrow(amostra()), nfat(), input$extracao, input$rotacao)
  })
  # Atalho: a solução rotacionada, só se deu certo (senão as saídas esperam)
  afe <- reactive({
    a <- afe_com()
    req(a$ok)
    a
  })

  # =====================================================================
  # Aba 2
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
    grafico_correlacao(R_analise(), mostrar_numeros = isTRUE(input$mostrar_numeros))
  }, res = 96)

  # =====================================================================
  # Aba 3
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
  # Aba 4
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
      ajuste_por_k(R_analise(), nrow(amostra()), input$extracao)
    })
  })

  output$grafico_ajuste_k <- renderPlot(
    grafico_ajuste_k(ajuste_k(), nfat(), paralela()$sugeridos), res = 96)

  # =====================================================================
  # Aba 5
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
    correlacao = input$correlacao, n = nrow(amostra()), sorteio = semente() - 2025,
    excluidos = excluidos_d()))

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
  # Aba 6
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
      oblimin = "A oblimin é uma rotação oblíqua: cada eixo pode girar por conta própria, sem precisar ficar em ângulo reto. Por isso os fatores podem se correlacionar (veja a aba 7).",
      promax = "A promax é uma rotação oblíqua: cada eixo pode girar por conta própria, sem precisar ficar em ângulo reto. Por isso os fatores podem se correlacionar (veja a aba 7)."
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
  # Aba 7
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

  alfas <- reactive(alfa_por_fator(R_analise(), afe()$L, input$corte, escalas_manuais()))

  output$tabela_alfa <- renderUI(tabela_alfa(alfas()))

  output$itens_fora <- renderUI({
    al <- alfas()
    fora_auto <- setdiff(al$fora, al$fora_manual)
    excl <- excluidos_d()
    tagList(
      if (length(al$fora) == 0) p(class = "small", selo("bom", "Todos os itens da análise entraram em alguma escala.")),
      if (length(fora_auto) > 0) p(class = "small",
        selo("atencao", sprintf("%s sem carga forte em nenhum fator (ficaram fora das escalas): ",
                                if (length(fora_auto) == 1) "1 item ficou" else paste(length(fora_auto), "itens ficaram"))),
        lapply(fora_auto, chip_codigo)),
      if (length(al$fora_manual) > 0) p(class = "small",
        selo("atencao", "Você deixou fora das escalas (aba 1): "), lapply(al$fora_manual, chip_codigo)),
      if (length(excl) > 0) p(class = "small text-muted",
        "Fora da análise (excluídos na aba 1): ", lapply(excl, chip_codigo)))
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
  # Aba 1 — os itens
  # =====================================================================
  escalas_manuais <- reactiveVal(stats::setNames(integer(0), character(0)))
  redesenhar_itens <- reactiveVal(0)   # força redesenhar a tabela (ex.: clique recusado)

  # Desmarcar / marcar um item
  observeEvent(input$alternar_item, {
    cod <- input$alternar_item
    ex <- excluidos()
    # Os cliques rápidos são juntados num só recálculo; por isso as decisões
    # se acumulam até o diário registrar (ex.: "Excluiu C4, C5").
    anotar <- function(tipo) acao_pendente(c(isolate(acao_pendente()), paste(tipo, cod)))
    if (cod %in% ex) {
      excluidos(setdiff(ex, cod))
      anotar("Recolocou")
    } else if (length(itens$codigo) - length(ex) - 1 < MIN_ITENS) {
      showNotification(sprintf("A análise precisa de pelo menos %d itens.", MIN_ITENS), type = "warning")
      redesenhar_itens(redesenhar_itens() + 1)
    } else {
      excluidos(c(ex, cod))
      anotar("Excluiu")
    }
  })

  observeEvent(input$recolocar_itens, {
    if (length(excluidos()) == 0) return()
    excluidos(character(0))
    acao_pendente("Recolocou todos os itens")
  })

  # "Excluiu C4", "Excluiu C5", "Recolocou A1" -> "Excluiu C4, C5; recolocou A1"
  juntar_acoes <- function(acoes) {
    if (length(acoes) == 1) return(acoes)
    tipo <- sub(" .*", "", acoes); cod <- sub("^\\S+ ", "", acoes)
    partes <- vapply(unique(tipo), function(t) paste(t, paste(cod[tipo == t], collapse = ", ")), "")
    paste(c(partes[1], tolower(substr(partes[-1], 1, 1)) |> paste0(substring(partes[-1], 2))), collapse = "; ")
  }

  observeEvent(input$editar_itens, nav_select("aba", "itens"))

  # Escolher a escala de um item ("auto", "0" = fora, ou o nº do fator)
  observeEvent(input$escala_item, {
    it <- input$escala_item$item
    v <- input$escala_item$escala
    m <- escalas_manuais()
    m <- m[names(m) != it]
    if (!identical(v, "auto")) m[it] <- as.integer(v)
    escalas_manuais(m)
  })

  # Quando a solução muda, os fatores podem mudar de número e de ordem:
  # as escolhas manuais de escala deixam de fazer sentido e são desfeitas.
  assinatura_solucao <- reactive(paste(
    nfat(), input$rotacao, input$extracao, input$correlacao, n_amostra(), semente(),
    paste(incluidos_d(), collapse = ",")))
  observeEvent(assinatura_solucao(), {
    if (length(escalas_manuais()) > 0) {
      escalas_manuais(stats::setNames(integer(0), character(0)))
      showNotification("A solução mudou, então as escolhas manuais de escala (aba 1) foram desfeitas: os fatores podem ter mudado de número e de ordem.",
                       type = "warning", duration = 8)
    }
  }, ignoreInit = TRUE)

  diagnostico <- reactive(diagnosticar_itens(R_analise(), kmo(), afe(), alfas(), input$corte))

  output$contexto_itens <- renderUI({
    p(class = "small p-2 rounded", style = "background:#eef4fc;",
      strong("Os números desta tabela usam a solução atual: "),
      sprintf("%s, extração %s, %s, ponto de corte %s. ",
              if (nfat() == 1) "1 fator" else paste(nfat(), "fatores"), input$extracao,
              NOMES_ROTACAO[[input$rotacao]], fmt(input$corte, 2)),
      "Escolha o número de fatores e a rotação (barra lateral e abas 4 e 5) antes de decidir sobre os itens.")
  })

  output$resumo_itens <- renderUI({
    excl <- excluidos()
    span(strong(sprintf("%d de %d itens na análise", length(itens$codigo) - length(excl), length(itens$codigo))),
         if (length(excl) > 0) span(class = "text-muted", " · excluídos: ", paste(excl, collapse = ", ")))
  })

  output$resumo_itens_lateral <- renderUI({
    excl <- excluidos()
    if (length(excl) == 0) return(span(class = "text-muted", sprintf("Itens na análise: todos os %d.", length(itens$codigo))))
    span(strong(sprintf("Itens na análise: %d de %d", length(itens$codigo) - length(excl), length(itens$codigo))),
         span(class = "text-muted", sprintf(" (sem %s)", paste(excl, collapse = ", "))))
  })

  output$avisos_itens <- renderUI({
    a <- afe_com()
    avisos <- list()
    if (!isTRUE(a$ok)) avisos <- c(avisos, list(selo("ruim",
      "Com estes itens e estas escolhas, o modelo não pôde ser estimado. Recoloque itens ou use menos fatores.")))
    if (isTRUE(a$ok)) {
      al <- alfas()
      pequenas <- vapply(al$fatores, function(f) length(f$itens) < 3, logical(1))
      if (any(pequenas)) avisos <- c(avisos, list(selo("atencao", sprintf(
        "%s com menos de 3 itens: %s. Um fator precisa de pelo menos 3 itens para ficar bem definido.",
        if (sum(pequenas) == 1) "Uma escala ficou" else "Algumas escalas ficaram",
        paste(vapply(al$fatores[pequenas], `[[`, "", "fator"), collapse = ", ")))))
    }
    if (length(intersect(excluidos(), INVERTIDOS)) >= 3) avisos <- c(avisos, list(selo("atencao",
      "Você excluiu vários itens invertidos. Cuidado: os números podem melhorar enquanto o instrumento piora (veja a missão 17).")))
    if (length(avisos) == 0) return(NULL)
    div(class = "small mb-2", lapply(avisos, div))
  })

  output$tabela_itens <- renderUI({
    redesenhar_itens()
    a <- afe_com()
    if (isTRUE(a$ok)) tabela_itens(diagnostico(), excluidos_d(), alfas(), escalas_manuais())
    else tabela_itens(NULL, excluidos_d(), NULL, escalas_manuais())
  })

  # --- Diário de decisões: registra o resultado depois de cada decisão
  diario <- reactiveVal(NULL)
  acao_pendente <- reactiveVal(sprintf("Início (%d itens)", length(itens$codigo)))

  observe({
    acao <- acao_pendente()
    req(length(acao) > 0)
    acao <- juntar_acoes(acao)
    # Espera a análise com o novo conjunto de itens ficar pronta
    req(setequal(incluidos_d(), incluidos()))
    a <- afe_com()
    nova <- if (isTRUE(a$ok)) {
      req(setequal(rownames(a$L), incluidos()))
      cls <- classificar_itens(a$L, isolate(input$corte))
      data.frame(acao = acao, n_itens = nrow(a$L),
                 config = sprintf("%d fatores · %s · %s", a$nfatores, NOMES_ROTACAO[[a$rotacao]],
                                  if (isolate(input$correlacao) == "policorica") "policórica" else "Pearson"),
                 TLI = a$TLI, RMSEA = a$RMSEA, total = a$total, kmo = kmo()$geral,
                 cruzadas = sum(cls$situacao == "cruzada"), sem_fator = sum(cls$situacao == "sem_fator"),
                 stringsAsFactors = FALSE)
    } else {
      data.frame(acao = paste(acao, "(modelo não estimado)"), n_itens = length(incluidos()), config = "—",
                 TLI = NA, RMSEA = NA, total = NA, kmo = NA, cruzadas = 0L, sem_fator = 0L, stringsAsFactors = FALSE)
    }
    isolate(diario(rbind(diario(), nova)))
    acao_pendente(NULL)
  })

  # Reiniciar: apaga o histórico e começa de novo a partir da situação atual
  observeEvent(input$reiniciar_diario, {
    diario(NULL)
    acao_pendente(sprintf("Novo começo (%d itens)", length(incluidos())))
  })

  output$diario <- renderUI({
    d <- diario()
    if (is.null(d)) return(p(class = "small text-muted", "Calculando…"))
    tabela_diario(d)
  })

  # =====================================================================
  # Missões guiadas
  # =====================================================================
  missao_atual <- reactiveVal(1L)
  concluidas <- reactiveVal(integer(0))

  # O que as missões podem conferir: os controles + o estado da aba 1
  estado_missoes <- reactive(c(
    reactiveValuesToList(input),
    list(excluidos = excluidos(), n_excluidos = length(excluidos()),
         n_manuais = length(escalas_manuais()), tem_guardada = !is.null(solucao_guardada()))))

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
    if (!is.na(k) && all(conferir_missao(MISSOES[[k]], estado_missoes()))) {
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
    ok <- conferir_missao(m, estado_missoes())

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
    aviso_itens <- if (k <= 14 && length(excluidos()) > 0) div(
      class = "small mt-2", selo("atencao", sprintf(
        "Você excluiu itens na aba 1 (%s). As missões 1 a 14 foram pensadas com os 25 itens: os resultados podem ficar diferentes dos comentários.",
        paste(excluidos(), collapse = ", "))))
    tagList(lista_conferencia(m, ok), botao, aviso_itens)
  })
}

shinyApp(ui, server)
