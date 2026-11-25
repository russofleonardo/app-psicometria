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
grafico_correlacao <- function(R, mostrar_numeros = FALSE) {
  p   <- ncol(R)
  cod <- colnames(R)
  M <- R
  diag(M) <- NA                       # a diagonal (sempre 1) não ensina nada

  cores <- paleta_divergente(101)
  quebras <- seq(-1, 1, length.out = 102)

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
      r <- M[i, j]
      graphics::text(j, p - i + 1, sub("^(-?)0,", "\\1,", fmt(r, 2)),
                     cex = 0.55, col = if (abs(r) > 0.45) "#ffffff" else TINTA)
    }
  }

  # --- Barra de cores (legenda)
  preparar_grafico()
  graphics::par(mai = c(0.8, 0.05, 0.2, 0.45))
  valores <- seq(-1, 1, length.out = 101)
  graphics::image(1, valores, matrix(valores, nrow = 1), col = cores,
                  breaks = quebras, axes = FALSE, xlab = "", ylab = "")
  graphics::axis(4, at = seq(-1, 1, 0.5), labels = fmt(seq(-1, 1, 0.5), 1),
                 col = EIXO, cex.axis = 0.8, tck = -0.3)
  graphics::mtext("r", side = 3, line = 0.2, cex = 0.9, font = 3, col = TINTA_2)

  graphics::layout(1)
}

# =====================================================================
# 2. SCREE PLOT COM ANÁLISE PARALELA
# =====================================================================
grafico_scree <- function(par_res, ate = 15) {
  k <- seq_len(min(ate, length(par_res$reais)))
  reais <- par_res$reais[k]
  acaso <- par_res$acaso[k]
  azul <- "#2a78d6"; laranja <- "#eb6834"

  preparar_grafico(mar = c(4.5, 4.5, 1, 9))
  y_lim <- range(c(0, reais, acaso))
  graphics::plot(k, reais, type = "n", ylim = y_lim, xlim = c(1, max(k)),
                 axes = FALSE, xlab = "Número do fator", ylab = "Autovalor")
  graphics::abline(h = pretty(y_lim), col = GRADE, lwd = 1)
  graphics::abline(v = k, col = GRADE, lwd = 0.5, lty = 3)
  graphics::abline(h = 0, col = EIXO, lwd = 1.5)
  graphics::axis(1, at = k, col = EIXO, tck = -0.015)
  graphics::axis(2, at = pretty(y_lim), labels = fmt(pretty(y_lim), 1),
                 col = EIXO, tck = -0.015)

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

  graphics::legend("topright", bty = "n", inset = c(0.02, 0),
                   legend = c("Dados reais", "Dados aleatórios (acaso)"),
                   col = c(azul, laranja), pt.bg = c(azul, laranja),
                   pch = c(21, 24), lty = c(1, 2), lwd = 2, text.col = TINTA)
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
tabela_cargas <- function(afe, corte, agrupar = TRUE) {
  L <- afe$L
  k <- ncol(L)
  cls <- classificar_itens(L, corte)
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

    situacao <- switch(cls$situacao[i],
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
        htmltools::tags$th("Situação"))),
      htmltools::tags$tbody(linhas),
      htmltools::tags$tfoot(
        rodape("Autovalor (soma das cargas²)", fmt(afe$ss, 2)),
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
    classe <- if (n_itens < 2) htmltools::span(class = "small text-muted", "precisa de 2 ou mais itens")
      else selo(if (f$alfa >= 0.7) "bom" else if (f$alfa >= 0.6) "atencao" else "ruim", f$classe)

    htmltools::tags$tr(
      htmltools::tags$th(f$fator),
      htmltools::tags$td(style = "white-space:nowrap;", parece),
      htmltools::tags$td(lapply(f$itens, function(cod) chip_codigo(cod, cod %in% f$invertidos))),
      htmltools::tags$td(class = "text-end", style = estilo_num, alfa_txt),
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
        htmltools::tags$th(class = "text-end", "Alfa"), htmltools::tags$th("Classificação"))),
      htmltools::tags$tbody(linhas)
    )
  )
}
