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
  estilos <- list(
    bom     = c("✔", "#0ca30c"),
    atencao = c("⚠", "#c98500"),
    ruim    = c("✖", "#d03b3b")
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
