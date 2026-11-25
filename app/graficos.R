# =====================================================================
# graficos.R  —  O "EMPRATAMENTO" (GRÁFICOS E TABELAS)
# ---------------------------------------------------------------------
# Funções que transformam os resultados de analises.R em gráficos
# (R base) e tabelas para a tela.
# =====================================================================

# --- Cores de apoio (tinta, grade, superfícies) -----------------------
TINTA      <- "#0b0b0b"
TINTA_2    <- "#52514e"
TINTA_FRACA <- "#898781"
GRADE      <- "#e1e0d9"
EIXO       <- "#c3c2b7"
SUPERFICIE <- "#fcfcfb"

# Escala de cor para correlações: azul (negativa) — cinza (zero) — vermelho (positiva)
paleta_divergente <- grDevices::colorRampPalette(
  c("#104281", "#3987e5", "#b7d3f6", "#f0efec", "#f5b8b7", "#e34948", "#8f1f1f")
)

# Estilo comum dos gráficos
preparar_grafico <- function(mar = c(4, 4, 1, 1)) {
  graphics::par(mar = mar, bg = SUPERFICIE, fg = TINTA_2, col.axis = TINTA_2,
                col.lab = TINTA_2, family = "sans", las = 1, bty = "n")
}

# =====================================================================
# 1. MAPA DE CALOR DA MATRIZ DE CORRELAÇÃO
# =====================================================================
# "limite" é o valor do extremo da escala de cores (1 para correlações;
# menor para resíduos, que são números pequenos). Valores além do limite
# ficam com a cor do extremo.
grafico_correlacao <- function(R, mostrar_numeros = FALSE, limite = 1, rotulo = "r") {
  p   <- ncol(R)
  cod <- colnames(R)
  M <- R
  diag(M) <- NA                       # a diagonal (sempre 1) não ensina nada
  M_original <- M
  M <- pmax(pmin(M, limite), -limite)

  cores <- paleta_divergente(101)
  quebras <- seq(-limite, limite, length.out = 102)

  # Barra de cores com largura fixa (2,2 cm); o mapa ocupa o resto.
  graphics::layout(matrix(1:2, nrow = 1), widths = c(1, graphics::lcm(2.2)))

  # Cabem os nomes completos dos itens? Se a tela for estreita, usamos só os códigos.
  largura_mapa <- graphics::par("din")[1] - 2.2 / 2.54          # em polegadas
  nomes <- itens[cod, "nome"]
  largura_nomes <- max(graphics::strwidth(nomes, units = "inches", cex = 0.85)) + 0.45
  usar_nomes <- (largura_mapa - largura_nomes) > 3.5
  rotulos <- if (usar_nomes) nomes else cod
  margem_esq <- if (usar_nomes) largura_nomes else 0.6

  # --- Painel principal
  preparar_grafico()
  graphics::par(mai = c(0.8, margem_esq, 0.2, 0.05))
  graphics::image(1:p, 1:p, t(M[p:1, ]), col = cores, breaks = quebras,
                  axes = FALSE, xlab = "", ylab = "", useRaster = FALSE)

  # Linhas brancas separando os domínios
  divisas <- which(diff(match(itens[cod, "letra"], names(DOMINIOS))) != 0) + 0.5
  graphics::abline(v = divisas, col = SUPERFICIE, lwd = 3)
  graphics::abline(h = p + 1 - divisas, col = SUPERFICIE, lwd = 3)

  # Faixas coloridas do domínio ao lado de cada item
  graphics::par(xpd = TRUE)
  for (i in seq_len(p)) {
    cor_i <- itens[cod[i], "cor"]
    graphics::rect(-0.35, p - i + 0.55, 0.35, p - i + 1.45, col = cor_i, border = NA)  # esquerda
    graphics::rect(i - 0.45, -0.45, i + 0.45, 0.25, col = cor_i, border = NA)         # embaixo
  }
  graphics::text(-0.7, p:1, rotulos, adj = 1, cex = if (usar_nomes) 0.85 else 0.75,
                 col = if (usar_nomes) TINTA else TINTA_2)
  graphics::text(1:p, -1.1, cod, cex = 0.75, col = TINTA_2, srt = 90, adj = 1)
  graphics::par(xpd = FALSE)

  # Números dentro das células (opcional)
  if (mostrar_numeros) {
    for (i in seq_len(p)) for (j in seq_len(p)) {
      if (i == j) next
      r <- M_original[i, j]
      graphics::text(j, p - i + 1, sub("^(-?)0,", "\\1,", fmt(r, 2)),
                     cex = 0.55, col = if (abs(r) > 0.45 * limite) "#ffffff" else TINTA)
    }
  }

  # --- Barra de cores (legenda)
  preparar_grafico()
  graphics::par(mai = c(0.8, 0.05, 0.2, 0.45))
  valores <- seq(-limite, limite, length.out = 101)
  graphics::image(1, valores, matrix(valores, nrow = 1), col = cores,
                  breaks = quebras, axes = FALSE, xlab = "", ylab = "")
  marcas <- seq(-limite, limite, length.out = 5)
  graphics::axis(4, at = marcas, labels = sub("^-", "−", fmt(marcas, if (limite < 1) 2 else 1)),
                 col = EIXO, cex.axis = 0.8, tck = -0.3)
  graphics::mtext(rotulo, side = 3, line = 0.2, cex = 0.9, font = 3, col = TINTA_2)

  graphics::layout(1)
}

# =====================================================================
# 2. SCREE PLOT COM ANÁLISE PARALELA
# =====================================================================
grafico_scree <- function(par_res, ate = 15, mostrar_kaiser = FALSE) {
  k <- seq_len(min(ate, length(par_res$reais)))
  reais <- par_res$reais[k]
  acaso <- par_res$acaso[k]
  completos <- par_res$autovalores_R[k]
  azul <- "#2a78d6"; laranja <- "#eb6834"; cinza <- "#898781"

  preparar_grafico(mar = c(4.5, 4.5, 1, 9))
  y_lim <- range(c(0, reais, acaso, if (mostrar_kaiser) completos))
  graphics::plot(k, reais, type = "n", ylim = y_lim, xlim = c(1, max(k)),
                 axes = FALSE, xlab = "Número do fator", ylab = "Autovalor")
  graphics::abline(h = pretty(y_lim), col = GRADE, lwd = 1)
  graphics::abline(v = k, col = GRADE, lwd = 0.5, lty = 3)
  graphics::abline(h = 0, col = EIXO, lwd = 1.5)
  graphics::axis(1, at = k, col = EIXO, tck = -0.015)
  graphics::axis(2, at = pretty(y_lim), labels = fmt(pretty(y_lim), 1),
                 col = EIXO, tck = -0.015)

  # Autovalores da matriz completa (os do critério de Kaiser) e a linha do 1
  if (mostrar_kaiser) {
    graphics::abline(h = 1, col = cinza, lwd = 1.2, lty = 3)
    graphics::lines(k, completos, col = cinza, lwd = 1.8)
    graphics::points(k, completos, col = SUPERFICIE, bg = cinza, pch = 22, cex = 1.3, lwd = 1.2)
    graphics::par(xpd = TRUE)
    graphics::text(max(k) + 0.4, 1, "1 (Kaiser)", adj = 0, col = cinza, cex = 0.8)
    graphics::par(xpd = FALSE)
  }

  graphics::lines(k, acaso, col = laranja, lwd = 2, lty = 2)
  graphics::points(k, acaso, col = SUPERFICIE, bg = laranja, pch = 24, cex = 1.3, lwd = 1.5)
  graphics::lines(k, reais, col = azul, lwd = 2)
  graphics::points(k, reais, col = SUPERFICIE, bg = azul, pch = 21, cex = 1.5, lwd = 1.5)

  # Rótulos diretos no fim de cada linha (além da legenda)
  graphics::par(xpd = TRUE)
  ult <- max(k)
  desloc <- diff(y_lim) * 0.06
  real_em_cima <- reais[ult] >= acaso[ult]   # quem está por cima no fim leva o rótulo de cima
  graphics::text(ult + 0.4, reais[ult] + if (real_em_cima) desloc else -desloc,
                 "Dados reais", adj = 0, col = TINTA, cex = 0.85)
  graphics::text(ult + 0.4, acaso[ult] + if (real_em_cima) -desloc else desloc,
                 "Dados aleatórios\n(acaso)", adj = 0, col = TINTA, cex = 0.85)
  graphics::par(xpd = FALSE)

  rotulos <- c("Dados reais (autovalores fatoriais)",
               sprintf("Dados aleatórios (acaso: percentil %d)", round(100 * par_res$percentil)))
  cores_leg <- c(azul, laranja); pchs <- c(21, 24); ltys <- c(1, 2)
  if (mostrar_kaiser) {
    rotulos <- c(rotulos, "Matriz completa (critério de Kaiser)")
    cores_leg <- c(cores_leg, cinza); pchs <- c(pchs, 22); ltys <- c(ltys, 1)
  }
  graphics::legend("topright", bty = "n", inset = c(0.02, 0),
                   legend = rotulos, col = cores_leg, pt.bg = cores_leg,
                   pch = pchs, lty = ltys, lwd = 2, text.col = TINTA, cex = 0.9)
}

# =====================================================================
# 2a. DISTRIBUIÇÃO DAS RESPOSTAS A UM ITEM (explicação Pearson × policórica)
# =====================================================================
grafico_respostas_item <- function(codigo) {
  x <- dados_bfi[[codigo]]
  prop <- as.numeric(table(factor(x, levels = 1:6))) / length(x)
  preparar_grafico(mar = c(4.2, 4.2, 2.4, 0.8))
  meio <- graphics::barplot(prop, names.arg = 1:6, col = itens[codigo, "cor"], border = NA,
                            ylim = c(0, max(prop) * 1.18), axes = FALSE,
                            xlab = "Resposta (1 = discordo totalmente … 6 = concordo totalmente)", ylab = "")
  graphics::text(meio, prop, paste0(fmt(100 * prop, 0), "%"), pos = 3, cex = 0.85, col = TINTA)
  graphics::title(main = itens[codigo, "nome"], font.main = 1, cex.main = 1, col.main = TINTA, line = 0.8)
}

# =====================================================================
# 2a'. EXPLICAÇÃO "PEARSON OU POLICÓRICA?" (janela aberta pela barra lateral)
# =====================================================================
explicacao_policorica <- function(cp) {
  p <- htmltools::p; strong <- htmltools::strong; em <- htmltools::em; h6 <- htmltools::h6
  caixa <- function(...) htmltools::div(class = "p-2 mb-2 rounded small", style = "background:#f4f3ef;", ...)

  numeros <- if (!is.null(cp)) htmltools::tagList(
    htmltools::tags$table(
      class = "table table-sm align-middle", style = "width:auto;",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Amostra completa, 5 fatores, oblimin"),
        htmltools::tags$th(class = "text-end", "Pearson"), htmltools::tags$th(class = "text-end", "Policórica"))),
      htmltools::tags$tbody(
        htmltools::tags$tr(htmltools::tags$td("Correlação média entre os itens (em módulo)"),
          htmltools::tags$td(class = "text-end", fmt(cp$media_abs_r_p, 2)), htmltools::tags$td(class = "text-end", fmt(cp$media_abs_r_pc, 2))),
        htmltools::tags$tr(htmltools::tags$td(sprintf("Correlação entre %s e %s (a que mais muda)", cp$par[1], cp$par[2])),
          htmltools::tags$td(class = "text-end", fmt(cp$r_p, 2)), htmltools::tags$td(class = "text-end", fmt(cp$r_pc, 2))),
        htmltools::tags$tr(htmltools::tags$td("Carga principal média dos itens"),
          htmltools::tags$td(class = "text-end", fmt(cp$carga_p, 2)), htmltools::tags$td(class = "text-end", fmt(cp$carga_pc, 2))),
        htmltools::tags$tr(htmltools::tags$td("Variância explicada pelos fatores"),
          htmltools::tags$td(class = "text-end", fmt_pct(cp$total_p)), htmltools::tags$td(class = "text-end", fmt_pct(cp$total_pc)))
      )
    ),
    p(class = "small", sprintf(
      "Os fatores encontrados são os mesmos nas duas (congruência de Tucker de pelo menos %s): a estrutura não muda. O que muda é o tamanho das relações — com Pearson, tudo fica um pouco “encolhido”.",
      fmt(cp$congruencia_min, 2)))
  )

  htmltools::tagList(
    h6("1. O problema: as respostas não são números contínuos"),
    p("Os itens são respondidos numa escala de 1 a 6 (de “discordo totalmente” a “concordo totalmente”). ",
      "Essas respostas são ", htmltools::HTML("<strong>categorias ordenadas</strong>: "),
      "sabemos que 5 é mais do que 4, mas não que a distância entre 4 e 5 ",
      "seja igual à distância entre 1 e 2. A correlação de ", strong("Pearson"), " trata as respostas como se fossem medidas contínuas, como altura ou peso."),

    h6(class = "mt-3", "2. O que a correlação policórica faz"),
    p("A ", strong("policórica"), " imagina que, por trás de cada item, existe uma variável contínua “escondida” — por exemplo, o quanto a pessoa ",
      em("realmente"), " gosta de refletir sobre as coisas. Ela supõe que essa variável segue uma curva normal e que a pessoa escolhe a resposta ",
      "conforme o ponto em que seu valor cruza alguns ", strong("limiares"), " (entre 1 e 2, entre 2 e 3, e assim por diante). ",
      "Então ela estima a correlação entre essas variáveis escondidas, e não entre as categorias."),

    h6(class = "mt-3", "3. Por que isso importa: Pearson “encolhe” as correlações"),
    p("Quando há poucas categorias e as respostas se concentram num dos extremos (distribuição ",
      htmltools::HTML("<strong>assimétrica</strong>), "),
      "a correlação de Pearson fica menor do que a relação real. Veja o item mais assimétrico destes dados:"),
    shiny::plotOutput("grafico_item_assimetrico", height = "260px"),
    caixa(if (!is.null(cp)) sprintf(
      "Quase ninguém escolhe as respostas mais %s: as pessoas se concentram no lado do “%s” (assimetria = %s). Com isso, sobra pouca variação para Pearson captar.",
      if (cp$assimetria < 0) "baixas" else "altas", if (cp$assimetria < 0) "concordo" else "discordo",
      sub("^-", "−", fmt(cp$assimetria, 2)))),
    p("Nestes dados, a diferença aparece assim:"),
    numeros,

    h6(class = "mt-3", "4. Por que a policórica seria a ideal aqui"),
    p("Os itens têm só 6 categorias, e vários têm respostas assimétricas. Para escalas assim, a literatura recomenda a correlação policórica ",
      "na análise fatorial — ela estima melhor as cargas e a estrutura (por exemplo, Holgado-Tello et al., 2010). ",
      "Em uma pesquisa de verdade com estes dados, a escolha seria a policórica."),
    caixa(strong("Mas ela tem custos: "),
      "é muito mais pesada de calcular, precisa de amostras maiores e, com amostras pequenas, pode gerar uma matriz “mal-comportada” ",
      "que precisa ser ajustada (suavizada). Com 7 ou mais categorias e respostas simétricas, Pearson e policórica costumam dar resultados muito parecidos."),

    h6(class = "mt-3", "5. E por que o app usa Pearson?"),
    p("Este app roda inteiro no navegador, sem servidor. Ali, calcular a policórica levaria até ", strong("um minuto"),
      " a cada nova amostra — o que atrapalharia explorar o app. Por isso:"),
    htmltools::tags$ul(
      htmltools::tags$li("o app usa ", strong("Pearson"), " como padrão — a estrutura de fatores é a mesma, e as ideias da AFE são as mesmas;"),
      htmltools::tags$li("a ", strong("policórica"), " fica disponível nas duas amostras que já vêm calculadas: a ", strong("completa"),
                         " e a ", strong("inicial"), " (1.000 pessoas, sorteio nº 1). Use-as para comparar (a missão 13 faz isso).")),
    p(class = "small text-muted mb-0",
      "No R, a policórica é calculada com psych::polychoric(dados) ou diretamente na análise fatorial com psych::fa(dados, cor = \"poly\").")
  )
}

# =====================================================================
# 2b. AJUSTE CONFORME O NÚMERO DE FATORES (três gráficos pequenos)
# =====================================================================
grafico_ajuste_k <- function(ak, k_atual, k_sugerido) {
  azul <- "#2a78d6"; verde_claro <- "#e3f4e3"; amarelo_claro <- "#fbf1dc"
  graphics::layout(matrix(1:3, nrow = 1))
  on.exit(graphics::layout(1))

  painel <- function(y, titulo, faixas, formato, ylim) {
    preparar_grafico(mar = c(4.2, 4.2, 3.2, 0.8))
    graphics::par(cex = 1)    # o layout com 3 painéis encolhe o texto; voltamos ao normal
    graphics::plot(ak$k, y, type = "n", ylim = ylim, xlim = c(0.7, max(ak$k) + 0.3),
                   axes = FALSE, xlab = "Número de fatores", ylab = "")
    graphics::title(main = titulo, font.main = 1, cex.main = 1.05, col.main = TINTA, line = 1.6)
    # Faixas de referência (bom / aceitável)
    for (f in faixas) graphics::rect(0.7, f$de, max(ak$k) + 0.3, f$ate, col = f$cor, border = NA)
    graphics::abline(v = ak$k, col = GRADE, lwd = 0.5, lty = 3)
    # Linha vertical no número sugerido pela análise paralela
    if (k_sugerido >= 1 && k_sugerido <= max(ak$k)) {
      graphics::abline(v = k_sugerido, col = "#eb6834", lwd = 1.5, lty = 2)
      graphics::mtext("paralela", side = 3, at = k_sugerido, line = 0.2, cex = 0.8, col = "#eb6834")
    }
    graphics::axis(1, at = ak$k, col = EIXO, tck = -0.02, cex.axis = 0.85, gap.axis = 0)
    marcas <- pretty(ylim)
    graphics::axis(2, at = marcas, labels = formato(marcas), col = EIXO, tck = -0.02, cex.axis = 0.9)
    graphics::lines(ak$k, y, col = azul, lwd = 2)
    hey <- !is.na(ak$heywood) & ak$heywood
    graphics::points(ak$k[!hey], y[!hey], pch = 21, bg = azul, col = SUPERFICIE, cex = 1.4, lwd = 1.2)
    # Casos de Heywood: um X vermelho (resultado não confiável)
    graphics::points(ak$k[hey], y[hey], pch = 4, col = "#d03b3b", cex = 1.5, lwd = 2.5)
    # O número de fatores escolhido agora: um anel em volta do ponto
    i <- which(ak$k == k_atual)
    if (length(i) == 1 && !is.na(y[i]))
      graphics::points(ak$k[i], y[i], pch = 21, col = TINTA, bg = NA, cex = 2.6, lwd = 2)
  }

  tli_lim <- range(c(0, 1, ak$TLI), na.rm = TRUE)
  painel(ak$TLI, "TLI (maior = melhor)",
         list(list(de = 0.95, ate = max(1.05, tli_lim[2]), cor = verde_claro),
              list(de = 0.90, ate = 0.95, cor = amarelo_claro)),
         function(x) fmt(x, 1), tli_lim)
  rmsea_lim <- range(c(0, ak$RMSEA), na.rm = TRUE)
  painel(ak$RMSEA, "RMSEA (menor = melhor)",
         list(list(de = 0, ate = 0.05, cor = verde_claro),
              list(de = 0.05, ate = 0.08, cor = amarelo_claro)),
         function(x) fmt(x, 2), rmsea_lim)
  painel(ak$total, "Variância explicada",
         list(list(de = 0.5, ate = 1, cor = verde_claro)),
         function(x) paste0(fmt(100 * x, 0), "%"), c(0, max(0.6, ak$total, na.rm = TRUE)))
}

# =====================================================================
# 3. PEÇAS DE TABELA (HTML)
# =====================================================================

# Um item com o quadradinho colorido do domínio + nome
chip_item <- function(codigo) {
  htmltools::tags$span(
    htmltools::tags$span(style = sprintf(
      "display:inline-block;width:.8em;height:.8em;border-radius:2px;margin-right:.45em;vertical-align:-.05em;background:%s;",
      itens[codigo, "cor"])),
    itens[codigo, "nome"]
  )
}

# Legenda das cores dos domínios
legenda_dominios <- function() {
  htmltools::div(
    class = "d-flex flex-wrap gap-3 small mb-2",
    lapply(names(DOMINIOS), function(l) htmltools::tags$span(
      htmltools::tags$span(style = sprintf(
        "display:inline-block;width:.85em;height:.85em;border-radius:2px;margin-right:.35em;vertical-align:-.05em;background:%s;",
        CORES_DOMINIO[[l]])),
      paste0(l, " = ", DOMINIOS[[l]])
    )),
    htmltools::tags$span(class = "text-muted", "(−) = item invertido")
  )
}

# Selo de situação: ícone + texto (nunca só cor)
selo <- function(nivel, texto) {
  # "︎" pede ao navegador o símbolo de texto (que aceita cor),
  # e não o emoji colorido.
  estilos <- list(
    bom     = c("✔︎", "#0ca30c"),
    atencao = c("⚠︎", "#c98500"),
    ruim    = c("✖︎", "#d03b3b")
  )
  e <- estilos[[nivel]]
  htmltools::tags$span(
    htmltools::tags$span(style = sprintf("color:%s;font-weight:700;margin-right:.3em;", e[2]), e[1]),
    texto
  )
}

# Tabela do KMO de cada item
tabela_msa <- function(kmo_res) {
  linhas <- lapply(seq_len(nrow(kmo_res$itens)), function(i) {
    it <- kmo_res$itens[i, ]
    nivel <- if (it$msa >= 0.60) "bom" else if (it$msa >= 0.50) "atencao" else "ruim"
    htmltools::tags$tr(
      htmltools::tags$td(chip_item(it$codigo)),
      htmltools::tags$td(class = "text-end", style = "font-variant-numeric:tabular-nums;",
                         fmt(it$msa, 2)),
      htmltools::tags$td(selo(nivel, it$classe))
    )
  })
  htmltools::tags$table(
    class = "table table-sm align-middle",
    htmltools::tags$thead(htmltools::tags$tr(
      htmltools::tags$th("Item"), htmltools::tags$th(class = "text-end", "KMO do item"),
      htmltools::tags$th("Classificação"))),
    htmltools::tags$tbody(linhas)
  )
}

# =====================================================================
# 4. PEÇAS DAS ABAS 4 A 6 (CARGAS, ROTAÇÃO, FATORES E ALFA)
# =====================================================================

# Número com sinal de menos tipográfico: -0,45 -> "−0,45"
fmt_sinal <- function(x, d = 2) {
  x <- round(x, d)
  x[x == 0] <- 0          # evita "−0,00"
  sub("^-", "−", fmt(x, d))
}

# Fundo de uma célula: vermelho (positivo) ou azul (negativo), mais forte
# quanto mais longe de zero — as mesmas cores do mapa de calor.
cor_fundo <- function(x) {
  a <- round(0.10 + 0.45 * min(abs(x), 1), 2)
  if (x >= 0) sprintf("rgba(227,73,72,%s)", a) else sprintf("rgba(57,135,229,%s)", a)
}

# Código do item com o quadradinho do domínio (versão curta do chip_item)
chip_codigo <- function(codigo, invertido = FALSE) {
  htmltools::tags$span(
    class = "text-nowrap me-2",
    title = itens[codigo, "nome"],
    htmltools::tags$span(style = sprintf(
      "display:inline-block;width:.75em;height:.75em;border-radius:2px;margin-right:.3em;vertical-align:-.05em;background:%s;",
      itens[codigo, "cor"])),
    codigo,
    if (invertido) htmltools::tags$span(class = "text-muted", " (inv.)")
  )
}

estilo_num <- "font-variant-numeric:tabular-nums;white-space:nowrap;"

# --- Tabela de cargas (aba 4) -----------------------------------------
# matriz = "padrao" (as cargas) ou "estrutura" (correlação item–fator,
# só faz diferença nas rotações oblíquas). A situação dos itens e o
# agrupamento vêm sempre da matriz padrão.
tabela_cargas <- function(afe, corte, agrupar = TRUE, matriz = "padrao") {
  estrutura <- identical(matriz, "estrutura") && afe$obliqua
  cls <- classificar_itens(afe$L, corte)
  L <- if (estrutura) afe$S else afe$L
  k <- ncol(L)
  ordem <- if (agrupar) order(cls$principal, -abs(cls$carga)) else seq_len(nrow(L))

  linhas <- lapply(seq_along(ordem), function(pos) {
    i <- ordem[pos]
    cod <- rownames(L)[i]
    # Linha mais grossa quando começa um novo grupo de fator
    novo_grupo <- agrupar && pos > 1 && cls$principal[i] != cls$principal[ordem[pos - 1]]
    borda <- if (novo_grupo) sprintf("border-top:2px solid %s;", EIXO) else ""

    celulas <- lapply(seq_len(k), function(f) {
      x <- L[i, f]
      estilo <- if (abs(x) >= corte) sprintf("font-weight:700;background:%s;", cor_fundo(x))
                else sprintf("color:%s;", TINTA_FRACA)
      htmltools::tags$td(class = "text-end", style = paste0(estilo_num, estilo, borda), fmt_sinal(x))
    })

    h2 <- afe$h2[[cod]]
    celula_h2 <- htmltools::tags$td(
      class = "text-end", style = paste0(estilo_num, borda),
      if (h2 >= 0.995) selo("ruim", fmt(h2, 2)) else fmt(h2, 2))

    situacao <- if (estrutura) "" else switch(cls$situacao[i],
      ok = "",
      cruzada = selo("atencao", "carga cruzada"),
      sem_fator = selo("ruim", "sem fator"))

    htmltools::tags$tr(
      htmltools::tags$td(style = paste0("white-space:nowrap;", borda), chip_item(cod)),
      celulas, celula_h2,
      htmltools::tags$td(class = "small", style = paste0("white-space:nowrap;", borda), situacao)
    )
  })

  rodape <- function(rotulo, valores) htmltools::tags$tr(
    htmltools::tags$th(class = "fw-normal text-muted", rotulo),
    lapply(valores, function(v) htmltools::tags$td(class = "text-end text-muted", style = estilo_num, v)),
    htmltools::tags$td(), htmltools::tags$td()
  )

  htmltools::div(
    style = "overflow-x:auto;",
    htmltools::tags$table(
      class = "table table-sm align-middle mb-0",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Item"),
        lapply(colnames(L), function(f) htmltools::tags$th(class = "text-end", f)),
        htmltools::tags$th(class = "text-end",
                           title = "Comunalidade: quanto da variância do item os fatores explicam", "h²"),
        htmltools::tags$th(if (estrutura) "" else "Situação"))),
      htmltools::tags$tbody(linhas),
      # Sem rotação, a soma das cargas² de cada fator É o seu autovalor.
      # Depois de rotacionar, não é mais: chamamos de "soma das cargas² (SQ)".
      if (!estrutura) htmltools::tags$tfoot(
        rodape(if (afe$rotacao == "none") "Autovalor (soma das cargas²)" else "Soma das cargas² (SQ)",
               fmt(afe$ss, 2)),
        rodape("% da variância", fmt_pct(afe$prop)),
        if (!afe$obliqua) rodape("% acumulada", fmt_pct(cumsum(afe$prop)))
      )
    )
  )
}

# --- Gráfico de cargas: um fator contra outro (aba 5) -------------------
grafico_cargas <- function(L, x = 1, y = 2, corte = 0.3) {
  preparar_grafico(mar = c(4.2, 4.2, 0.8, 0.8))
  graphics::plot(NA, xlim = c(-1, 1), ylim = c(-1, 1), asp = 1, axes = FALSE,
                 xlab = paste("Carga em", colnames(L)[x]),
                 ylab = paste("Carga em", colnames(L)[y]))
  # "Zona fraca": cargas abaixo do ponto de corte nos dois fatores
  graphics::rect(-corte, -corte, corte, corte, col = "#efeee9", border = NA)
  marcas <- seq(-1, 1, 0.5)
  graphics::abline(h = marcas, v = marcas, col = GRADE, lwd = 0.8)
  graphics::abline(h = 0, v = 0, col = EIXO, lwd = 1.5)
  graphics::axis(1, at = marcas, labels = fmt_sinal(marcas, 1), col = EIXO, tck = -0.015, cex.axis = 0.85)
  graphics::axis(2, at = marcas, labels = fmt_sinal(marcas, 1), col = EIXO, tck = -0.015, cex.axis = 0.85)

  cores <- itens[rownames(L), "cor"]
  graphics::points(L[, x], L[, y], pch = 21, bg = cores, col = SUPERFICIE, cex = 1.7, lwd = 1.2)
  graphics::text(L[, x], L[, y], rownames(L), pos = 3, offset = 0.5, cex = 0.72, col = TINTA_2)
}

# --- O que a rotação mudou (aba 5) --------------------------------------
tabela_comparacao_rotacao <- function(sem, com, corte) {
  niveis <- c("ok", "cruzada", "sem_fator")
  cs <- table(factor(classificar_itens(sem$L, corte)$situacao, niveis))
  cc <- table(factor(classificar_itens(com$L, corte)$situacao, niveis))

  linha <- function(rotulo, a, b, destaque = FALSE) htmltools::tags$tr(
    htmltools::tags$td(if (destaque) htmltools::strong(rotulo) else rotulo),
    htmltools::tags$td(class = "text-end", style = estilo_num, a),
    htmltools::tags$td(class = "text-end", style = estilo_num, b))

  htmltools::div(
    style = "overflow-x:auto;",
    htmltools::tags$table(
      class = "table table-sm align-middle",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(""), htmltools::tags$th(class = "text-end", "Antes"),
        htmltools::tags$th(class = "text-end", "Depois"))),
      htmltools::tags$tbody(
        lapply(seq_len(ncol(sem$L)), function(f)
          linha(paste0("% da variância em ", colnames(sem$L)[f]),
                fmt_pct(sem$prop[f]), fmt_pct(com$prop[f]))),
        linha("Variância explicada pelos fatores juntos", fmt_pct(sem$total), fmt_pct(com$total), TRUE),
        linha("Itens com carga forte em um só fator", cs[["ok"]], cc[["ok"]], TRUE),
        linha("Itens com carga cruzada", cs[["cruzada"]], cc[["cruzada"]]),
        linha("Itens sem nenhuma carga forte", cs[["sem_fator"]], cc[["sem_fator"]])
      )
    )
  )
}

# --- Correlação entre fatores (aba 6) -----------------------------------
tabela_phi <- function(Phi) {
  k <- ncol(Phi)
  htmltools::div(
    style = "overflow-x:auto;",
    htmltools::tags$table(
      class = "table table-sm align-middle", style = "width:auto;",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th(""),
        lapply(colnames(Phi), function(f)
          htmltools::tags$th(class = "text-end", style = "min-width:4.5em;", f)))),
      htmltools::tags$tbody(lapply(seq_len(k), function(i) htmltools::tags$tr(
        htmltools::tags$th(rownames(Phi)[i]),
        lapply(seq_len(k), function(j) {
          if (j > i) return(htmltools::tags$td())
          if (j == i) return(htmltools::tags$td(class = "text-end text-muted", style = estilo_num, "1"))
          r <- Phi[i, j]
          estilo <- if (abs(r) >= 0.32) sprintf("font-weight:700;background:%s;", cor_fundo(r)) else ""
          htmltools::tags$td(class = "text-end", style = paste0(estilo_num, estilo), fmt_sinal(r))
        })
      )))
    )
  )
}

# =====================================================================
# 5. PEÇAS DAS MISSÕES GUIADAS
# =====================================================================
CORES_NIVEL <- c("Básico" = "#1baf7a", "Intermediário" = "#2a78d6",
                 "Avançado" = "#6250d6", "Desafio" = "#d03b3b")

selo_nivel <- function(nivel) htmltools::tags$span(
  class = "badge rounded-pill", style = sprintf("background:%s;", CORES_NIVEL[[nivel]]), nivel)

# Fileira de botões 1, 2, 3... (verde = concluída; cheio = missão atual)
botoes_missoes <- function(atual, concluidas) {
  htmltools::div(
    class = "d-flex flex-wrap gap-1 mb-2",
    lapply(seq_len(N_MISSOES), function(k) {
      m <- MISSOES[[k]]
      feita <- k %in% concluidas
      classe <- if (k == atual && feita) "btn-success"
                else if (k == atual) "btn-primary"
                else if (feita) "btn-outline-success"
                else "btn-outline-secondary"
      htmltools::tags$button(
        type = "button",
        class = paste("btn btn-sm botao-missao", classe),
        title = sprintf("Missão %d (%s): %s%s", k, m$nivel, m$titulo, if (feita) " — concluída" else ""),
        onclick = sprintf("Shiny.setInputValue('missao_escolhida', %d, {priority: 'event'})", k),
        if (feita) paste0(k, " ✔︎") else as.character(k)
      )
    })
  )
}

# Barra de progresso
progresso_missoes <- function(concluidas) {
  n <- length(concluidas)
  htmltools::div(
    class = "mb-2",
    htmltools::div(class = "small text-muted mb-1",
                   sprintf("%d de %d missões concluídas", n, N_MISSOES)),
    htmltools::div(
      class = "progress", style = "height:6px;",
      htmltools::div(class = "progress-bar bg-success", role = "progressbar",
                     style = sprintf("width:%.0f%%;", 100 * n / N_MISSOES)))
  )
}

# Texto da missão (não depende dos controles, para não "piscar")
conteudo_missao <- function(k) {
  m <- MISSOES[[k]]
  htmltools::tagList(
    htmltools::div(
      class = "d-flex flex-wrap align-items-center gap-2 mb-2",
      selo_nivel(m$nivel),
      htmltools::tags$strong(sprintf("Missão %d: %s", k, m$titulo))
    ),
    htmltools::tags$button(
      type = "button", class = "btn btn-sm btn-outline-primary mb-2",
      onclick = sprintf("Shiny.setInputValue('ir_para_aba', '%s', {priority: 'event'})", m$aba),
      paste0("Ir para a aba “", NOMES_ABAS[[m$aba]], "” →")),
    htmltools::p(class = "mb-2", htmltools::strong("Objetivo: "), m$objetivo),
    htmltools::div(class = "fw-bold small text-uppercase text-muted", "O que fazer"),
    htmltools::tags$ol(class = "ps-3 mb-2", lapply(m$passos, htmltools::tags$li))
  )
}

# Lista de conferência (✔ cumprido / ○ falta)
lista_conferencia <- function(m, ok) {
  htmltools::tagList(
    htmltools::div(class = "fw-bold small text-uppercase text-muted", "Conferência do app"),
    htmltools::tags$ul(
      class = "list-unstyled mb-2 small",
      lapply(seq_along(m$condicoes), function(j) htmltools::tags$li(
        if (ok[j]) htmltools::tags$span(style = "color:#0ca30c;font-weight:700;", "✔︎ ")
        else htmltools::tags$span(style = sprintf("color:%s;font-weight:700;", TINTA_FRACA), "○ "),
        htmltools::tags$span(class = if (!ok[j]) "text-muted", m$condicoes[[j]]$rotulo)
      ))
    )
  )
}

# Pergunta de reflexão com o comentário escondido
reflexao_missao <- function(k) {
  m <- MISSOES[[k]]
  htmltools::div(
    class = "mt-3 p-2 rounded", style = "background:#f4f3ef;",
    htmltools::div(class = "fw-bold small text-uppercase text-muted", "Para refletir"),
    htmltools::p(class = "mb-1", m$reflexao),
    htmltools::tags$details(
      htmltools::tags$summary(class = "small", "Pense um pouco… e depois clique para ver o comentário"),
      htmltools::p(class = "small mt-2 mb-0", m$comentario)
    )
  )
}

# --- Alfa de cada fator (aba 6) -----------------------------------------
tabela_alfa <- function(alfas) {
  linhas <- lapply(alfas$fatores, function(f) {
    n_itens <- length(f$itens)
    parece <- if (n_itens == 0) htmltools::span(class = "text-muted", "nenhum item")
      else htmltools::tags$span(
        htmltools::tags$span(style = sprintf(
          "display:inline-block;width:.8em;height:.8em;border-radius:2px;margin-right:.4em;vertical-align:-.05em;background:%s;",
          CORES_DOMINIO[[f$dominio]])),
        DOMINIOS[[f$dominio]],
        htmltools::tags$span(class = "text-muted small",
                             sprintf(" (%d de %d %s)", f$qtd_dominio, n_itens,
                                     if (n_itens == 1) "item" else "itens")))
    alfa_txt <- if (is.na(f$alfa)) htmltools::span(class = "text-muted", "—") else fmt(f$alfa, 2)
    omega_txt <- if (is.na(f$omega)) htmltools::span(class = "text-muted", title = "O ômega precisa de 3 ou mais itens", "—")
                 else fmt(f$omega, 2)
    classe <- if (n_itens < 2) htmltools::span(class = "small text-muted", "precisa de 2 ou mais itens")
      else selo(if (f$alfa >= 0.7) "bom" else if (f$alfa >= 0.6) "atencao" else "ruim", f$classe)

    htmltools::tags$tr(
      htmltools::tags$th(f$fator),
      htmltools::tags$td(style = "white-space:nowrap;", parece),
      htmltools::tags$td(lapply(f$itens, function(cod) htmltools::tags$span(
        class = "text-nowrap d-inline-block",
        chip_codigo(cod, cod %in% f$invertidos),
        if (cod %in% f$manuais) htmltools::tags$span(
          class = "badge rounded-pill text-bg-light me-2", style = "font-weight:500;",
          title = if (cod %in% f$carga_baixa) "Colocado à mão nesta escala, mas a carga dele neste fator é baixa" else "Colocado à mão nesta escala",
          if (cod %in% f$carga_baixa) "manual ⚠︎" else "manual")))),
      htmltools::tags$td(class = "text-end", style = estilo_num, alfa_txt),
      htmltools::tags$td(class = "text-end", style = estilo_num, omega_txt),
      htmltools::tags$td(style = "white-space:nowrap;", classe)
    )
  })
  htmltools::div(
    style = "overflow-x:auto;",
    htmltools::tags$table(
      class = "table table-sm align-middle",
      htmltools::tags$thead(htmltools::tags$tr(
        htmltools::tags$th("Fator"), htmltools::tags$th("Parece medir"),
        htmltools::tags$th("Itens (do mais forte ao mais fraco)"),
        htmltools::tags$th(class = "text-end", "Alfa"),
        htmltools::tags$th(class = "text-end", "Ômega"),
        htmltools::tags$th("Classificação (alfa)"))),
      htmltools::tags$tbody(linhas)
    )
  )
}

# =====================================================================
# 6. COMPARAÇÃO ENTRE A SOLUÇÃO GUARDADA E A ATUAL (aba 4)
# =====================================================================
NOMES_EXTRACAO <- c(minres = "minres", ml = "ml", pa = "pa")
NOMES_ROTACAO <- c(none = "sem rotação", varimax = "varimax", oblimin = "oblimin", promax = "promax")

# "5 fatores · minres · varimax · Pearson · n = 1.000 (sorteio 1)"
descrever_solucao <- function(info) {
  paste(c(
    if (info$nfatores == 1) "1 fator" else paste(info$nfatores, "fatores"),
    NOMES_EXTRACAO[[info$extracao]], NOMES_ROTACAO[[info$rotacao]],
    if (info$correlacao == "policorica") "policórica" else "Pearson",
    sprintf("n = %s (sorteio %d)", fmt_int(info$n), info$sorteio),
    if (length(info$excluidos) > 0) paste("sem", paste(info$excluidos, collapse = ", "))
  ), collapse = " · ")
}

tabela_comparacao <- function(cmp, guardada, atual) {
  th <- htmltools::tags$th; td <- htmltools::tags$td; tr <- htmltools::tags$tr
  num <- function(x) td(class = "text-end", style = estilo_num, x)

  # (a) Escolhas e índices, lado a lado
  linha <- function(rotulo, a, b) tr(td(rotulo), num(a), num(b))
  indices <- htmltools::tags$table(
    class = "table table-sm align-middle",
    htmltools::tags$thead(tr(th(""), th(class = "text-end", "Guardada"), th(class = "text-end", "Atual"))),
    htmltools::tags$tbody(
      linha("Itens na análise", nrow(guardada$L), nrow(atual$L)),
      linha("Número de fatores", guardada$nfatores, atual$nfatores),
      linha("TLI (maior = melhor)", fmt(guardada$TLI, 3), fmt(atual$TLI, 3)),
      linha("RMSEA (menor = melhor)", fmt(guardada$RMSEA, 3), fmt(atual$RMSEA, 3)),
      linha("RMSR dos resíduos (menor = melhor)", fmt(guardada$rmsr, 3), fmt(atual$rmsr, 3)),
      linha("Variância explicada", fmt_pct(guardada$total), fmt_pct(atual$total)),
      linha("Itens com carga cruzada", cmp$contagem_guardada[["cruzadas"]], cmp$contagem_atual[["cruzadas"]]),
      linha("Itens sem fator", cmp$contagem_guardada[["sem_fator"]], cmp$contagem_atual[["sem_fator"]])
    )
  )

  # (b) Correspondência entre os fatores (congruência de Tucker)
  pares <- htmltools::tags$table(
    class = "table table-sm align-middle", style = "width:auto;",
    htmltools::tags$thead(tr(th("Fator atual"), th("Mais parecido na guardada"),
                             th(class = "text-end", "Congruência"), th("Leitura"))),
    htmltools::tags$tbody(lapply(seq_len(nrow(cmp$pares)), function(i) {
      pr <- cmp$pares[i, ]
      leitura <- classificar_congruencia(pr$congruencia)
      nivel <- switch(leitura, "o mesmo fator" = "bom", "parecido" = "atencao", "ruim")
      tr(th(pr$atual), td(pr$guardada),
         num(paste0(fmt(abs(pr$congruencia), 2), if (pr$congruencia < 0) " (sinal invertido)" else "")),
         td(selo(nivel, leitura)))
    }))
  )

  # (c) Item a item
  itens_tab <- htmltools::tags$table(
    class = "table table-sm align-middle",
    htmltools::tags$thead(tr(
      th("Item"), th(class = "text-end", "h² guardada"), th(class = "text-end", "h² atual"),
      th("Fator na guardada"), th("Fator na atual"), th(""))),
    htmltools::tags$tbody(lapply(seq_len(nrow(cmp$itens)), function(i) {
      it <- cmp$itens[i, ]
      excluido <- htmltools::tags$span(class = "text-muted fst-italic", "excluído")
      fator_txt <- function(presente, f, carga, sit) if (!presente) excluido else htmltools::tags$span(
        f, htmltools::tags$span(class = "text-muted", sprintf(" (%s)", fmt_sinal(carga))),
        if (sit == "sem_fator") htmltools::tags$span(class = "small text-muted", " — sem carga forte"))
      aviso <- if (it$na_guardada != it$na_atual) selo("atencao", if (it$na_atual) "voltou para a análise" else "foi excluído")
               else if (isTRUE(it$mudou)) selo("atencao", "mudou de fator")
      tr(td(style = "white-space:nowrap;", chip_item(it$codigo)),
         num(if (it$na_guardada) fmt(it$h2_guardada, 2) else "—"),
         num(if (it$na_atual) fmt(it$h2_atual, 2) else "—"),
         td(style = "white-space:nowrap;", fator_txt(it$na_guardada, it$fator_guardada, it$carga_guardada, it$situacao_guardada)),
         td(style = "white-space:nowrap;", fator_txt(it$na_atual, it$fator_atual, it$carga_atual, it$situacao_atual)),
         td(class = "small", style = "white-space:nowrap;", aviso))
    }))
  )

  htmltools::tagList(
    htmltools::h6("Escolhas e índices"),
    htmltools::div(style = "overflow-x:auto;", indices),
    htmltools::h6(class = "mt-3", "Os fatores são os mesmos?"),
    htmltools::p(class = "small text-muted",
      "A congruência de Tucker compara as cargas de dois fatores (vai de 0 a 1). ",
      "Referência usual: 0,95 ou mais = o mesmo fator; de 0,85 a 0,94 = parecido; abaixo disso = diferente. ",
      "Os números dos fatores podem mudar de uma solução para outra — por isso comparamos pelo conteúdo, não pelo número. ",
      "Se as soluções têm itens diferentes, a comparação usa só os itens que estão nas duas."),
    htmltools::div(style = "overflow-x:auto;", pares),
    htmltools::h6(class = "mt-3", "Item a item"),
    htmltools::div(style = "overflow-x:auto;", itens_tab)
  )
}

# =====================================================================
# 7. ABA 0 — A LISTA DE ITENS (incluir/excluir, escala, sinais)
# =====================================================================
# As caixinhas e os seletores são HTML simples que avisam o servidor com
# Shiny.setInputValue (o mesmo jeito dos botões das missões). Assim a tabela
# pode ser redesenhada à vontade sem perder as escolhas, que ficam no servidor.
tabela_itens <- function(diag, excluidos, alfas, manual) {
  th <- htmltools::tags$th; td <- htmltools::tags$td; tr <- htmltools::tags$tr
  # Sem diagnóstico (o modelo não pôde ser estimado): só as caixinhas
  sem_diag <- is.null(diag) || is.null(alfas)
  k <- if (sem_diag) 0 else length(alfas$fatores)
  # Rótulo de cada escala: "F2 (parece Extroversão)"
  rotulo_fator <- if (sem_diag) character(0) else vapply(alfas$fatores, function(f)
    if (is.na(f$dominio)) f$fator else sprintf("%s · %s", f$fator, DOMINIOS[[f$dominio]]), "")

  linhas <- lapply(itens$codigo, function(cod) {
    incluido <- !(cod %in% excluidos)
    caixa <- htmltools::tags$input(
      type = "checkbox", class = "form-check-input", style = "cursor:pointer;",
      title = if (incluido) "Tirar este item da análise" else "Recolocar este item na análise",
      checked = if (incluido) NA,
      onclick = sprintf("Shiny.setInputValue('alternar_item', '%s', {priority: 'event'})", cod))
    if (!incluido) {
      return(tr(style = "opacity:.5;",
        td(caixa), td(style = "white-space:nowrap;", chip_item(cod)),
        td(colspan = 6, class = "small fst-italic", "fora da análise")))
    }
    d <- if (sem_diag) NULL else diag[diag$codigo == cod, ]
    if (is.null(d) || nrow(d) == 0) {
      return(tr(td(caixa), td(style = "white-space:nowrap;", chip_item(cod)),
                td(colspan = 6, class = "small text-muted", "—")))
    }
    auto <- composicao_escalas_auto(d)   # escala automática deste item
    atual <- if (cod %in% names(manual)) as.character(manual[[cod]]) else "auto"
    opcoes <- c(
      list(htmltools::tags$option(value = "auto", selected = if (atual == "auto") NA,
           sprintf("%s (auto)", if (auto == 0) "Fora das escalas" else rotulo_fator[auto]))),
      lapply(seq_len(k), function(j) htmltools::tags$option(
        value = j, selected = if (atual == as.character(j)) NA, rotulo_fator[j])),
      list(htmltools::tags$option(value = "0", selected = if (atual == "0") NA, "Fora das escalas")))
    seletor <- htmltools::tags$select(
      class = "form-select form-select-sm", style = "min-width:12.5rem;",
      title = "De qual escala este item faz parte (para o alfa e o ômega da aba 6). “Auto” = pela maior carga.",
      onchange = sprintf("Shiny.setInputValue('escala_item', {item: '%s', escala: this.value}, {priority: 'event'})", cod),
      opcoes)

    alfa_txt <- if (is.na(d$alfa_sem)) htmltools::span(class = "text-muted", "—") else htmltools::tags$span(
      fmt(d$alfa_sem, 2),
      htmltools::tags$span(
        class = "small", style = if (d$delta > 0.01) "color:#c98500;font-weight:700;" else "color:#898781;",
        sprintf(" (%s%s)", if (d$delta >= 0) "+" else "−", fmt(abs(d$delta), 2))))

    sinais <- if (nzchar(d$sinais)) strsplit(d$sinais, "|", fixed = TRUE)[[1]] else character(0)
    nivel_sinal <- function(s) if (grepl("Heywood|baixo \\(< 0,50\\)|nenhuma", s)) "ruim" else "atencao"

    tr(td(caixa),
       td(style = "white-space:nowrap;", chip_item(cod)),
       td(class = "text-end", style = estilo_num,
          if (!is.na(d$msa) && d$msa < 0.5) htmltools::span(style = "color:#d03b3b;font-weight:700;", fmt(d$msa, 2)) else fmt(d$msa, 2)),
       td(class = "text-end", style = estilo_num, fmt(d$h2, 2)),
       td(style = estilo_num, sprintf("%s (%s)", d$fator, fmt_sinal(d$carga))),
       td(seletor),
       td(class = "text-end", style = estilo_num, alfa_txt),
       td(class = "small", style = "min-width:11rem;",
          if (length(sinais) == 0) htmltools::span(class = "text-muted", "—")
          else lapply(sinais, function(s) htmltools::div(selo(nivel_sinal(s), s)))))
  })

  htmltools::div(
    style = "overflow-x:auto;",
    htmltools::tags$table(
      class = "table table-sm align-middle",
      htmltools::tags$thead(tr(
        th(title = "Desmarque para tirar o item da análise", "Incluir"),
        th("Item"),
        th(class = "text-end", title = "KMO do item: o quanto ele compartilha com os demais", "KMO"),
        th(class = "text-end", title = "Comunalidade: quanto do item os fatores explicam", "h²"),
        th("Maior carga"),
        th("Escala (para o alfa)"),
        th(class = "text-end", title = "Alfa que a escala do item teria sem ele (entre parênteses, a diferença)", "Alfa sem o item"),
        th("Sinais para investigar"))),
      htmltools::tags$tbody(linhas)
    )
  )
}

# Escala automática de um item a partir do diagnóstico (0 = fora)
composicao_escalas_auto <- function(d) {
  if (d$situacao == "sem_fator") 0L else as.integer(sub("^F", "", d$fator))
}

# Diário de decisões
tabela_diario <- function(diario) {
  th <- htmltools::tags$th; td <- htmltools::tags$td; tr <- htmltools::tags$tr
  num <- function(x) td(class = "text-end", style = estilo_num, x)
  htmltools::div(
    style = "overflow-x:auto;",
    htmltools::tags$table(
      class = "table table-sm align-middle small",
      htmltools::tags$thead(tr(
        th("#"), th("Decisão"), th(class = "text-end", "Itens"), th("Configuração"),
        th(class = "text-end", "TLI"), th(class = "text-end", "RMSEA"),
        th(class = "text-end", "Variância"), th(class = "text-end", "KMO"),
        th(class = "text-end", title = "Itens com carga cruzada / itens sem carga forte", "Cruzadas / sem fator"))),
      htmltools::tags$tbody(lapply(seq_len(nrow(diario)), function(i) {
        d <- diario[i, ]
        tr(td(i), td(htmltools::strong(d$acao)), num(d$n_itens), td(class = "text-muted", d$config),
           num(fmt(d$TLI, 3)), num(fmt(d$RMSEA, 3)), num(fmt_pct(d$total)), num(fmt(d$kmo, 2)),
           num(sprintf("%d / %d", d$cruzadas, d$sem_fator)))
      }))
    )
  )
}
