# =====================================================================
# gerar_policoricas.R — calcula e guarda as correlações policóricas das
# duas amostras mais usadas no app (a completa e a inicial), para que elas
# apareçam na hora, sem esperar o cálculo (que é lento no navegador).
#
# Como usar (no R, a partir da pasta do projeto):
#   source("ferramentas/gerar_policoricas.R")
#
# Só é preciso rodar de novo se mudarem os dados, o tamanho inicial da
# amostra, a semente inicial ou a função calcular_correlacao().
# =====================================================================
local({
  dir_original <- setwd("app")
  on.exit(setwd(dir_original))
  source("dados.R", encoding = "UTF-8", local = TRUE)
  source("analises.R", encoding = "UTF-8", local = TRUE)

  N_INICIAL <- 1000        # valor inicial do controle "Tamanho da amostra" (app.R)
  SEMENTE_INICIAL <- 2026  # semente inicial do sorteio (app.R)

  prontas <- list()
  prontas[[chave_amostra(N_TOTAL, SEMENTE_INICIAL)]] <-
    calcular_correlacao(sortear_amostra(N_TOTAL, SEMENTE_INICIAL), "policorica")
  prontas[[chave_amostra(N_INICIAL, SEMENTE_INICIAL)]] <-
    calcular_correlacao(sortear_amostra(N_INICIAL, SEMENTE_INICIAL), "policorica")

  saveRDS(prontas, "policoricas_prontas.rds")
  cat("Guardadas:", paste(names(prontas), collapse = ", "), "\n")
})
