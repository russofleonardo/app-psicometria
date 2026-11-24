# =====================================================================
# analises.R  —  AS "RECEITAS" (CÁLCULOS DA AFE)
# ---------------------------------------------------------------------
# Cada função é uma receita: recebe ingredientes e devolve um resultado.
# Nenhuma delas desenha nada; os desenhos ficam em graficos.R.
# =====================================================================

# --- Utilidades: números no formato brasileiro (vírgula decimal) -----
fmt <- function(x, d = 2) {
  ifelse(is.na(x), "—",
         formatC(x, format = "f", digits = d, decimal.mark = ",", big.mark = "."))
}
fmt_int <- function(x) formatC(x, format = "d", big.mark = ".")
fmt_p <- function(p) if (p < 0.001) "< 0,001" else fmt(p, 3)

# --- 1. Sortear a amostra ---------------------------------------------
# "semente" é o número que faz o sorteio ser reproduzível: a mesma
# semente sempre dá a mesma amostra.
sortear_amostra <- function(n, semente) {
  n <- min(n, N_TOTAL)
  set.seed(semente)
  dados_bfi[sort(sample.int(N_TOTAL, n)), , drop = FALSE]
}

# --- 2. Matriz de correlação ------------------------------------------
# Pearson: trata as respostas (1 a 6) como números contínuos.
# Policórica: supõe que por trás de cada resposta há uma variável
# contínua "escondida"; costuma ser mais adequada para escalas Likert.
# Se a matriz não for "positiva definida" (pode acontecer com amostras
# pequenas na policórica), ela é levemente ajustada (suavizada).
policorica_segura <- function(dados, correcao) {
  tryCatch(
    suppressWarnings(suppressMessages(
      psych::polychoric(dados, smooth = FALSE, global = FALSE, correct = correcao)$rho
    )),
    error = function(e) NULL
  )
}

calcular_correlacao <- function(dados, tipo = c("pearson", "policorica")) {
  tipo <- match.arg(tipo)
  falhou <- FALSE
  if (tipo == "pearson") {
    R <- stats::cor(dados)
  } else {
    # Com amostras pequenas, a correção padrão às vezes falha;
    # tentamos de novo sem ela e, em último caso, usamos Pearson.
    R <- policorica_segura(dados, 0.5)
    if (is.null(R) || anyNA(R)) R <- policorica_segura(dados, 0)
    if (is.null(R) || anyNA(R)) {
      R <- stats::cor(dados)
      falhou <- TRUE
    }
  }
  suavizada <- FALSE
  if (min(eigen(R, symmetric = TRUE, only.values = TRUE)$values) <= 1e-8) {
    R <- suppressWarnings(psych::cor.smooth(R))
    suavizada <- TRUE
  }
  dimnames(R) <- list(colnames(dados), colnames(dados))
  list(R = R, tipo = tipo, suavizada = suavizada, falhou = falhou)
}

# --- 3. KMO e teste de Bartlett ---------------------------------------
# KMO: os itens compartilham o bastante para formar fatores? (0 a 1)
# Bartlett: as correlações são todas zero? (queremos rejeitar isso)
classificar_kmo <- function(valor) {
  cut(valor,
      breaks = c(-Inf, 0.50, 0.60, 0.70, 0.80, 0.90, Inf),
      labels = c("inaceitável", "miserável", "medíocre",
                 "mediano", "meritório", "maravilhoso"),
      right = FALSE)
}

avaliar_kmo <- function(R, n) {
  k <- suppressWarnings(psych::KMO(R))
  b <- suppressWarnings(psych::cortest.bartlett(R, n = n))
  geral <- unname(k$MSA)
  classe <- as.character(classificar_kmo(geral))

  texto_kmo <- if (geral >= 0.80) {
    "Os itens compartilham bastante variância entre si: os dados são muito adequados para a análise fatorial."
  } else if (geral >= 0.60) {
    "Os itens compartilham variância suficiente: dá para seguir com a análise fatorial, com alguma cautela."
  } else if (geral >= 0.50) {
    "O compartilhamento entre os itens é fraco: a análise fatorial é arriscada."
  } else {
    "Os itens quase não compartilham variância: a análise fatorial não é recomendada."
  }

  texto_bartlett <- if (b$p.value < 0.05) {
    "Rejeitamos a hipótese de que todas as correlações são zero: existem relações entre os itens que valem a pena investigar."
  } else {
    "Não dá para descartar que as correlações sejam todas zero: não há o que fatorar."
  }

  list(
    geral = geral, classe = classe, texto_kmo = texto_kmo,
    itens = data.frame(codigo = names(k$MSAi), msa = unname(k$MSAi),
                       classe = as.character(classificar_kmo(k$MSAi)),
                       stringsAsFactors = FALSE),
    qui2 = b$chisq, gl = b$df, p = b$p.value,
    texto_bartlett = texto_bartlett
  )
}

# --- 4. Análise paralela ----------------------------------------------
# Compara os autovalores dos dados reais com os de dados ALEATÓRIOS do
# mesmo tamanho. Um fator só "vale a pena" se explica mais do que o
# acaso explicaria. Os autovalores são os da análise fatorial (o mesmo
# critério usado por psych::fa.parallel).
autovalores_fatoriais <- function(R, n) {
  suppressWarnings(suppressMessages(
    psych::fa(R, nfactors = 1, fm = "minres", n.obs = n, warnings = FALSE)$values
  ))
}

analise_paralela <- function(R, n, semente, n_iter = 20) {
  p <- ncol(R)
  reais <- autovalores_fatoriais(R, n)

  set.seed(semente + 1000)
  simulados <- replicate(n_iter, {
    aleatorio <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
    autovalores_fatoriais(stats::cor(aleatorio), n)
  })
  acaso <- rowMeans(simulados)

  # Quantos fatores seguidos ficam ACIMA do acaso?
  acima <- reais > acaso
  sugeridos <- if (!acima[1]) 0 else (which(!acima)[1] - 1)
  if (is.na(sugeridos)) sugeridos <- p

  # Para comparação: o critério de Kaiser (autovalores > 1 na matriz completa)
  kaiser <- sum(eigen(R, symmetric = TRUE, only.values = TRUE)$values > 1)

  list(reais = reais, acaso = acaso, sugeridos = sugeridos,
       kaiser = kaiser, n_iter = n_iter)
}
