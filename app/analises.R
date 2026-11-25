# Laboratório de Psicometria – Análise Fatorial Exploratória
# © 2026 Leonardo Ferreira Russo — licença CC BY-NC-ND 4.0 (veja o arquivo LICENSE)
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
fmt_int <- function(x) formatC(x, format = "d", big.mark = ".", decimal.mark = ",")
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

# Correlações policóricas JÁ CALCULADAS para as duas amostras mais usadas
# (a completa e a inicial: 1.000 pessoas, sorteio 1). O cálculo é lento,
# principalmente na versão do navegador; como os dados nunca mudam, o
# resultado guardado é idêntico ao calculado na hora.
# O arquivo é gerado por ferramentas/gerar_policoricas.R.
POLICORICAS_PRONTAS <- if (file.exists("policoricas_prontas.rds")) readRDS("policoricas_prontas.rds") else list()

chave_amostra <- function(n, semente) if (n >= N_TOTAL) "completa" else paste0("n", n, "_s", semente)

policorica_disponivel <- function(n, semente) !is.null(POLICORICAS_PRONTAS[[chave_amostra(n, semente)]])

# Compara Pearson e policórica na amostra completa (para a explicação
# "Pearson ou policórica?"): mesma AFE (5 fatores, oblimin) nas duas.
comparar_pearson_policorica <- function() {
  pc <- POLICORICAS_PRONTAS[["completa"]]
  if (is.null(pc)) return(NULL)
  Rp <- stats::cor(dados_bfi)
  Rpc <- pc$R
  ap <- rodar_afe(Rp, N_TOTAL, 5, "minres", "oblimin")
  apc <- rodar_afe(Rpc, N_TOTAL, 5, "minres", "oblimin")
  if (!ap$ok || !apc$ok) return(NULL)

  # O par de itens em que as duas correlações mais diferem
  dif <- abs(Rpc) - abs(Rp)
  dif[lower.tri(dif, diag = TRUE)] <- NA
  ij <- which(dif == max(dif, na.rm = TRUE), arr.ind = TRUE)[1, ]

  # O item mais assimétrico (respostas concentradas num extremo)
  assimetria <- apply(dados_bfi, 2, function(x) mean((x - mean(x))^3) / stats::sd(x)^3)
  item_assim <- names(which.max(abs(assimetria)))

  cong <- psych::factor.congruence(apc$L, ap$L)
  list(
    h2_p = mean(ap$h2), h2_pc = mean(apc$h2),
    total_p = ap$total, total_pc = apc$total,
    carga_p = mean(abs(classificar_itens(ap$L, 0)$carga)),
    carga_pc = mean(abs(classificar_itens(apc$L, 0)$carga)),
    congruencia_min = min(apply(abs(cong), 1, max)),
    par = colnames(Rp)[ij], r_p = Rp[ij[1], ij[2]], r_pc = Rpc[ij[1], ij[2]],
    media_abs_r_p = mean(abs(Rp[upper.tri(Rp)])), media_abs_r_pc = mean(abs(Rpc[upper.tri(Rpc)])),
    item_assim = item_assim, assimetria = assimetria[[item_assim]]
  )
}

calcular_correlacao <- function(dados, tipo = c("pearson", "policorica"), chave = NULL) {
  tipo <- match.arg(tipo)
  if (tipo == "policorica" && !is.null(chave) && !is.null(POLICORICAS_PRONTAS[[chave]])) {
    return(POLICORICAS_PRONTAS[[chave]])
  }
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

# Para cada fator, o "acaso" é o percentil 95 dos autovalores simulados
# (o valor que o acaso só supera em 5% das vezes) — mais rigoroso do que
# a média, que tende a aceitar fatores demais.
analise_paralela <- function(R, n, semente, n_iter = 50, percentil = 0.95) {
  p <- ncol(R)
  reais <- autovalores_fatoriais(R, n)

  set.seed(semente + 1000)
  simulados <- replicate(n_iter, {
    aleatorio <- matrix(stats::rnorm(n * p), nrow = n, ncol = p)
    autovalores_fatoriais(stats::cor(aleatorio), n)
  })
  acaso <- apply(simulados, 1, stats::quantile, probs = percentil, names = FALSE)

  # Quantos fatores seguidos ficam ACIMA do acaso?
  acima <- reais > acaso
  sugeridos <- if (!acima[1]) 0 else (which(!acima)[1] - 1)
  if (is.na(sugeridos)) sugeridos <- p

  # Para comparação: o critério de Kaiser usa OUTROS autovalores — os da
  # matriz de correlação completa (com 1 na diagonal), que são maiores.
  autovalores_R <- eigen(R, symmetric = TRUE, only.values = TRUE)$values
  kaiser <- sum(autovalores_R > 1)

  list(reais = reais, acaso = acaso, sugeridos = sugeridos,
       kaiser = kaiser, autovalores_R = autovalores_R,
       n_iter = n_iter, percentil = percentil)
}

# Porcentagem no formato brasileiro: 0,4237 -> "42,4%"
fmt_pct <- function(x, d = 1) paste0(fmt(100 * x, d), "%")

# --- 5. A análise fatorial em si ----------------------------------------
# Roda psych::fa() e devolve só o que o app usa, com nomes simples.
# Se o modelo não puder ser estimado, devolve ok = FALSE (o app avisa).
ROTACOES_OBLIQUAS <- c("oblimin", "promax")

rodar_afe <- function(R, n, nfatores, extracao = "minres", rotacao = "none") {
  ajuste <- tryCatch(
    suppressWarnings(suppressMessages(
      psych::fa(R, nfactors = nfatores, fm = extracao, rotate = rotacao,
                n.obs = n, warnings = FALSE)
    )),
    error = function(e) NULL
  )
  if (is.null(ajuste)) return(list(ok = FALSE))

  # Cargas: uma linha por item, uma coluna por fator (F1, F2, ...)
  L <- unclass(ajuste$loadings)
  L <- matrix(L, nrow = nrow(L),
              dimnames = list(rownames(R), paste0("F", seq_len(ncol(L)))))
  if (anyNA(L)) return(list(ok = FALSE))
  p <- nrow(L)

  # O sinal de um fator é arbitrário (F e −F são a mesma solução). Viramos
  # cada fator para que a maioria das cargas fique positiva: assim o fator
  # de Extroversão aparece como "Extroversão", e não como "Introversão".
  # (Só contam os itens cuja maior carga está naquele fator. Os itens
  # invertidos contam "ao contrário": o fator aponta para o lado em que os
  # itens comuns têm carga positiva e os invertidos, negativa.)
  principal <- apply(abs(L), 1, which.max)
  chave <- ifelse(rownames(L) %in% INVERTIDOS, -1, 1)
  sinal <- vapply(seq_len(ncol(L)), function(f) {
    proprios <- principal == f
    soma <- if (any(proprios)) sum(L[proprios, f] * chave[proprios]) else sum(L[, f] * chave)
    if (soma < 0) -1 else 1
  }, numeric(1))
  L <- sweep(L, 2, sinal, `*`)

  # Correlação entre fatores (só existe nas rotações oblíquas)
  Phi <- NULL
  if (rotacao %in% ROTACOES_OBLIQUAS && nfatores > 1 && !is.null(ajuste$Phi)) {
    Phi <- ajuste$Phi * outer(sinal, sinal)
    dimnames(Phi) <- list(colnames(L), colnames(L))
  }

  # Variância explicada por fator (autovalor do fator = soma das cargas²;
  # nas oblíquas, leva em conta a correlação entre os fatores)
  ss <- if (is.null(Phi)) colSums(L^2) else diag(Phi %*% crossprod(L))
  h2 <- ajuste$communality[rownames(L)]

  rmsea <- ajuste$RMSEA
  pegar <- function(x, i) if (is.null(x) || length(x) < i) NA_real_ else unname(x[i])

  # Matriz de estrutura: correlação entre cada item e cada fator.
  # Sem correlação entre fatores (Phi = identidade), ela é igual às cargas.
  S <- if (is.null(Phi)) L else L %*% Phi
  dimnames(S) <- dimnames(L)

  # Resíduos: correlação observada − correlação que o modelo reproduz.
  # (A diagonal não interessa: ela guarda a parte única de cada item.)
  Rhat <- L %*% (if (is.null(Phi)) diag(ncol(L)) else Phi) %*% t(L)
  residuos <- R - Rhat
  diag(residuos) <- NA
  fora_diag <- residuos[upper.tri(residuos)]

  list(
    ok = TRUE, L = L, S = S, h2 = h2, u2 = 1 - h2,
    ss = ss, prop = ss / p, total = sum(h2) / p,
    Phi = Phi, obliqua = !is.null(Phi),
    residuos = residuos,
    rmsr = sqrt(mean(fora_diag^2)),
    prop_res_grandes = mean(abs(fora_diag) > 0.05),
    TLI = pegar(ajuste$TLI, 1),
    RMSEA = pegar(rmsea, 1), RMSEA_inf = pegar(rmsea, 2), RMSEA_sup = pegar(rmsea, 3),
    heywood = any(h2 >= 0.995),
    nfatores = nfatores, extracao = extracao, rotacao = rotacao
  )
}

# --- 6. Situação de cada item -------------------------------------------
# "ok": carga forte (acima do corte) em um só fator — o ideal;
# "cruzada": carga forte em dois ou mais fatores;
# "sem_fator": nenhuma carga forte.
classificar_itens <- function(L, corte) {
  fortes <- rowSums(abs(L) >= corte)
  principal <- apply(abs(L), 1, which.max)
  data.frame(
    codigo = rownames(L),
    principal = principal,
    carga = L[cbind(seq_len(nrow(L)), principal)],
    situacao = ifelse(fortes == 0, "sem_fator", ifelse(fortes > 1, "cruzada", "ok")),
    stringsAsFactors = FALSE
  )
}

# --- 7. Os índices de ajuste e o que eles dizem ------------------------
# Pontos de corte usuais (Hu & Bentler, 1999). São convenções, não leis.
classificar_tli <- function(tli) {
  if (is.na(tli)) "indisponivel"
  else if (tli >= 0.95) "bom" else if (tli >= 0.90) "aceitavel" else "ruim"
}
classificar_rmsea <- function(r) {
  if (is.na(r)) "indisponivel"
  else if (r <= 0.05) "bom" else if (r <= 0.08) "aceitavel"
  else if (r <= 0.10) "mediocre" else "ruim"
}
NOMES_CLASSE <- c(bom = "bom", aceitavel = "aceitável", mediocre = "medíocre",
                  ruim = "ruim", indisponivel = "indisponível")

interpretar_ajuste <- function(afe, sugeridos) {
  c_tli <- classificar_tli(afe$TLI)
  c_rmsea <- classificar_rmsea(afe$RMSEA)
  k <- afe$nfatores

  texto_tli <- switch(c_tli,
    bom = "O modelo reproduz bem as correlações entre os itens — muito melhor do que um modelo sem nenhum fator.",
    aceitavel = "O ajuste é razoável, mas ainda há relações entre os itens que os fatores não explicam.",
    ruim = "Os fatores deixam muita coisa sem explicar: o modelo reproduz mal as correlações entre os itens.",
    indisponivel = "Não foi possível calcular o TLI para este modelo.")

  texto_rmsea <- switch(c_rmsea,
    bom = "O erro médio do modelo é pequeno: ele se aproxima bem dos dados.",
    aceitavel = "O erro médio do modelo é aceitável, mas não desprezível.",
    mediocre = "O erro médio do modelo é grande: o ajuste é fraco.",
    ruim = "O erro médio do modelo é grande demais: o modelo não representa bem os dados.",
    indisponivel = "Não foi possível calcular o RMSEA para este modelo.")

  # O intervalo de 90% atravessa mais de uma faixa? Então a estimativa é incerta.
  if (!is.na(afe$RMSEA_inf) && !is.na(afe$RMSEA_sup) &&
      classificar_rmsea(afe$RMSEA_inf) != classificar_rmsea(afe$RMSEA_sup)) {
    texto_rmsea <- paste(texto_rmsea,
      "Mas atenção: o intervalo de 90% vai de uma faixa a outra, então o valor é incerto (isso é comum com amostras pequenas).")
  }

  bom_tli <- c_tli == "bom"
  bom_rmsea <- c_rmsea %in% c("bom", "aceitavel")
  ruim_tli <- c_tli == "ruim"
  ruim_rmsea <- c_rmsea %in% c("mediocre", "ruim")
  fator_s <- function(x) if (x == 1) "1 fator" else paste(x, "fatores")

  excesso <- (!is.na(afe$TLI) && afe$TLI > 1.01) ||
             (!is.na(afe$RMSEA) && afe$RMSEA == 0 && k > sugeridos)

  if (excesso) {
    nivel <- "atencao"
    titulo <- "Ajuste “bom demais”: cuidado com o excesso de fatores"
    texto <- sprintf(
      "Com %s, o modelo se ajusta quase perfeitamente — às vezes isso quer dizer que ele está “decorando” os detalhes desta amostra, em vez de encontrar algo que se repetiria em outras. A análise paralela sugere %s. Tente reduzir o número de fatores e veja se o ajuste continua bom.",
      fator_s(k), fator_s(sugeridos))
  } else if (c_tli == "indisponivel" || c_rmsea == "indisponivel") {
    nivel <- "atencao"
    titulo <- "Índices incompletos"
    texto <- "Algum índice não pôde ser calculado. Olhe com atenção a tabela de cargas antes de tirar conclusões."
  } else if (bom_tli && bom_rmsea) {
    nivel <- "bom"
    titulo <- "Os dois índices indicam bom ajuste"
    texto <- if (k > sugeridos) sprintf(
      "O modelo se ajusta bem aos dados. Mas note que você está usando %s, mais do que a análise paralela sugere (%s): fatores a mais costumam melhorar o ajuste sem trazer nada novo. Veja na tabela se todos os fatores têm itens com cargas fortes e fazem sentido.",
      fator_s(k), fator_s(sugeridos))
    else "O modelo se ajusta bem aos dados. O próximo passo é ver se as cargas fazem sentido: cada fator reúne itens com um tema em comum?"
  } else if (ruim_tli && ruim_rmsea) {
    nivel <- "ruim"
    titulo <- "Os dois índices indicam ajuste fraco"
    texto <- if (k < sugeridos) sprintf(
      "Provavelmente faltam fatores: você está usando %s, e a análise paralela (aba 4) sugere %s. Tente aumentar o número de fatores na barra lateral e veja os índices melhorarem.",
      fator_s(k), fator_s(sugeridos))
    else sprintf(
      "Você já está usando %s (a análise paralela sugere %s). Mais fatores podem melhorar os índices, mas também podem criar fatores sem sentido. Com escalas de concordância (Likert) e amostras grandes, esses índices costumam ser exigentes: use a tabela de cargas para decidir se a solução faz sentido.",
      fator_s(k), fator_s(sugeridos))
  } else {
    nivel <- "atencao"
    titulo <- "Os índices não contam a mesma história"
    texto <- sprintf(
      "Um índice está melhor que o outro. Isso acontece porque cada um olha para um aspecto diferente: o TLI compara o modelo com um modelo sem fatores, e o RMSEA mede o erro médio, levando em conta quantos fatores você usou. Quando eles discordam, o ajuste é razoável, mas não excelente. Olhe os dois e a tabela de cargas antes de concluir.%s",
      if (k < sugeridos) sprintf(" Vale testar mais fatores: a análise paralela sugere %s.", fator_s(sugeridos)) else "")
  }

  list(c_tli = c_tli, c_rmsea = c_rmsea, texto_tli = texto_tli, texto_rmsea = texto_rmsea,
       nivel = nivel, titulo = titulo, texto = texto)
}

# --- 8. Consistência interna (alfa) de cada fator ----------------------
# Alfa padronizado: k * r_médio / (1 + (k - 1) * r_médio), onde r_médio é a
# correlação média entre os itens do fator. Itens com carga negativa são
# invertidos antes (é como "virar" a escala deles).
alfa_padronizado <- function(R, codigos, sinais) {
  k <- length(codigos)
  if (k < 2) return(NA_real_)
  Rk <- R[codigos, codigos] * outer(sinais, sinais)
  r_medio <- mean(Rk[upper.tri(Rk)])
  k * r_medio / (1 + (k - 1) * r_medio)
}

classificar_alfa <- function(a) {
  if (is.na(a)) return(NA_character_)
  as.character(cut(a, breaks = c(-Inf, 0.5, 0.6, 0.7, 0.8, 0.9, Inf),
                   labels = c("inaceitável", "pobre", "questionável",
                              "aceitável", "bom", "excelente"),
                   right = FALSE))
}

# Ômega de McDonald: ajusta um modelo de 1 fator só com os itens do fator e
# calcula ω = (Σλ)² / ((Σλ)² + Σ(1 − λ²)). Diferente do alfa, ele NÃO supõe
# que todos os itens tenham a mesma carga. Precisa de pelo menos 3 itens.
omega_fator <- function(R, codigos, sinais) {
  if (length(codigos) < 3) return(NA_real_)
  Rk <- R[codigos, codigos] * outer(sinais, sinais)
  ajuste <- tryCatch(
    suppressWarnings(suppressMessages(
      psych::fa(Rk, nfactors = 1, fm = "minres", warnings = FALSE))),
    error = function(e) NULL)
  if (is.null(ajuste)) return(NA_real_)
  lambda <- as.numeric(ajuste$loadings)
  if (sum(lambda) < 0) lambda <- -lambda
  lambda <- pmin(lambda, 0.999)             # protege contra casos de Heywood
  sum(lambda)^2 / (sum(lambda)^2 + sum(1 - lambda^2))
}

# Composição das escalas: para cada item, o nº do fator da escala de que ele
# faz parte (0 = fora das escalas). Automática: o fator da maior carga, se
# ela passar do ponto de corte. "manual" (vetor nomeado: item -> nº do fator,
# ou 0 = fora) substitui a escolha automática nos itens indicados.
composicao_escalas <- function(L, corte, manual = NULL) {
  cls <- classificar_itens(L, corte)
  comp <- stats::setNames(ifelse(cls$situacao == "sem_fator", 0L, as.integer(cls$principal)), cls$codigo)
  if (length(manual) > 0) {
    m <- manual[names(manual) %in% names(comp) & manual <= ncol(L)]
    comp[names(m)] <- as.integer(m)
  }
  comp
}

# Sinal de cada item dentro da sua escala (para "desvirar" os invertidos)
sinais_escala <- function(L, sel, f) {
  s <- sign(L[sel, f])
  s[s == 0] <- 1
  s
}

alfa_por_fator <- function(R, L, corte, manual = NULL) {
  comp <- composicao_escalas(L, corte, manual)
  fatores <- lapply(seq_len(ncol(L)), function(f) {
    sel <- names(comp)[comp == f]
    sel <- sel[order(-abs(L[sel, f]))]
    sinais <- sinais_escala(L, sel, f)
    a <- alfa_padronizado(R, sel, sinais)

    # Domínio que mais aparece entre os itens do fator
    dominio <- NA_character_; qtd <- 0
    if (length(sel) > 0) {
      contagem <- table(itens[sel, "letra"])
      dominio <- names(contagem)[which.max(contagem)]
      qtd <- max(contagem)
    }
    w <- omega_fator(R, sel, sinais)
    list(fator = colnames(L)[f], itens = sel, invertidos = sel[sinais < 0],
         alfa = a, classe = classificar_alfa(a), omega = w,
         dominio = dominio, qtd_dominio = qtd,
         manuais = intersect(sel, names(manual)),
         carga_baixa = sel[abs(L[sel, f]) < corte])
  })
  list(fatores = fatores, fora = names(comp)[comp == 0], composicao = comp,
       fora_manual = intersect(names(comp)[comp == 0], names(manual)))
}

# "Alfa se o item for excluído": para cada item que está numa escala, o alfa
# que essa escala teria sem ele (e a diferença em relação ao alfa atual).
alfa_se_excluido <- function(R, L, alfas) {
  linhas <- lapply(alfas$fatores, function(f) {
    if (length(f$itens) < 3) return(NULL)   # sem ele sobraria 1 item: não há alfa
    j <- match(f$fator, colnames(L))
    do.call(rbind, lapply(f$itens, function(cod) {
      resto <- setdiff(f$itens, cod)
      a_sem <- alfa_padronizado(R, resto, sinais_escala(L, resto, j))
      data.frame(codigo = cod, fator = f$fator, alfa = f$alfa, alfa_sem = a_sem,
                 delta = a_sem - f$alfa, stringsAsFactors = FALSE)
    }))
  })
  do.call(rbind, linhas)
}

# --- Diagnóstico de cada item (aba 1) -----------------------------------
# Junta, para cada item incluído, os sinais que merecem investigação.
# São SINAIS PARA INVESTIGAR, não ordens de exclusão.
diagnosticar_itens <- function(R, kmo, afe, alfas, corte) {
  L <- afe$L
  cls <- classificar_itens(L, corte)
  ase <- alfa_se_excluido(R, L, alfas)
  do.call(rbind, lapply(seq_len(nrow(L)), function(i) {
    cod <- rownames(L)[i]
    msa <- kmo$itens$msa[match(cod, kmo$itens$codigo)]
    h2 <- afe$h2[[cod]]
    linha_ase <- if (!is.null(ase)) ase[ase$codigo == cod, ] else NULL
    delta <- if (!is.null(linha_ase) && nrow(linha_ase) == 1) linha_ase$delta else NA_real_
    sinais <- c(
      if (!is.na(msa) && msa < 0.5) "KMO do item baixo (< 0,50)"
      else if (!is.na(msa) && msa < 0.6) "KMO do item fraco (< 0,60)",
      if (h2 >= 0.995) "caso de Heywood (h² ≥ 1)"
      else if (h2 < 0.2) "h² baixa (< 0,20)",
      switch(cls$situacao[i], sem_fator = "nenhuma carga forte", cruzada = "carga cruzada", NULL),
      if (!is.na(delta) && delta > 0.01) "o alfa da escala subiria sem ele"
    )
    data.frame(codigo = cod, msa = msa, h2 = h2,
               fator = colnames(L)[cls$principal[i]], carga = cls$carga[i],
               situacao = cls$situacao[i],
               escala = alfas$composicao[[cod]],
               alfa_sem = if (!is.null(linha_ase) && nrow(linha_ase) == 1) linha_ase$alfa_sem else NA_real_,
               delta = delta,
               sinais = paste(sinais, collapse = "|"),
               stringsAsFactors = FALSE)
  }))
}

# --- 9. Ajuste conforme o número de fatores ----------------------------
# Roda a AFE de 1 a k_max fatores (sem rotação: o ajuste não depende dela).
ajuste_por_k <- function(R, n, extracao, k_max = 10) {
  linhas <- lapply(seq_len(k_max), function(k) {
    a <- rodar_afe(R, n, k, extracao, "none")
    if (!a$ok) return(data.frame(k = k, TLI = NA, RMSEA = NA, total = NA, heywood = NA))
    data.frame(k = k, TLI = a$TLI, RMSEA = a$RMSEA, total = a$total, heywood = a$heywood)
  })
  do.call(rbind, linhas)
}

# --- 10. Comparar duas soluções ------------------------------------------
# Congruência de Tucker: o "quão parecidas" são as cargas de dois fatores
# (de −1 a 1). Regra usual: ≥ 0,95 = mesmo fator; 0,85 a 0,94 = parecido.
classificar_congruencia <- function(x) {
  x <- abs(x)
  if (is.na(x)) "—" else if (x >= 0.95) "o mesmo fator"
  else if (x >= 0.85) "parecido" else "diferente"
}

comparar_solucoes <- function(guardada, atual, corte) {
  cg <- classificar_itens(guardada$L, corte)
  ca <- classificar_itens(atual$L, corte)

  # Para cada fator atual, o fator guardado mais parecido. As duas soluções
  # podem ter itens diferentes (se algum foi excluído): comparamos nos itens
  # que estão nas duas.
  comuns <- intersect(rownames(atual$L), rownames(guardada$L))
  cong <- psych::factor.congruence(atual$L[comuns, , drop = FALSE], guardada$L[comuns, , drop = FALSE])
  pares <- data.frame(
    atual = colnames(atual$L),
    guardada = colnames(guardada$L)[apply(abs(cong), 1, which.max)],
    congruencia = apply(cong, 1, function(r) r[which.max(abs(r))]),
    stringsAsFactors = FALSE
  )

  # Uma linha por item (os 25), dizendo se está em cada solução
  todos <- itens$codigo
  pega <- function(v, nomes) unname(v[match(todos, nomes)])
  itens_cmp <- data.frame(
    codigo = todos,
    na_guardada = todos %in% rownames(guardada$L),
    na_atual = todos %in% rownames(atual$L),
    h2_guardada = pega(guardada$h2, names(guardada$h2)),
    h2_atual = pega(atual$h2, names(atual$h2)),
    fator_guardada = pega(colnames(guardada$L)[cg$principal], cg$codigo),
    carga_guardada = pega(cg$carga, cg$codigo), situacao_guardada = pega(cg$situacao, cg$codigo),
    fator_atual = pega(colnames(atual$L)[ca$principal], ca$codigo),
    carga_atual = pega(ca$carga, ca$codigo), situacao_atual = pega(ca$situacao, ca$codigo),
    stringsAsFactors = FALSE
  )
  # O item "mudou de fator"? Comparamos pelo par mais parecido.
  equivalente <- stats::setNames(pares$guardada, pares$atual)
  itens_cmp$mudou <- itens_cmp$na_guardada & itens_cmp$na_atual &
    equivalente[itens_cmp$fator_atual] != itens_cmp$fator_guardada

  contar <- function(cls) c(cruzadas = sum(cls$situacao == "cruzada"),
                            sem_fator = sum(cls$situacao == "sem_fator"))
  list(pares = pares, itens = itens_cmp,
       contagem_guardada = contar(cg), contagem_atual = contar(ca))
}
