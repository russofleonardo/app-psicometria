# Laboratório de Psicometria – Análise Fatorial Exploratória
# © 2026 Leonardo Ferreira Russo — licença CC BY-NC-ND 4.0 (veja o arquivo LICENSE)
# =====================================================================
# dados.R  —  A "DESPENSA" DO APP
# ---------------------------------------------------------------------
# Este arquivo prepara, uma única vez, tudo o que o app usa como
# ingrediente: os dados, os rótulos dos itens em português, os domínios,
# os itens invertidos e as cores.
# =====================================================================

# --- 1. Os dados ------------------------------------------------------
# psych::bfi tem 2.800 pessoas que responderam 25 itens do Big Five
# (escala de 1 = discordo totalmente a 6 = concordo totalmente).
# Ficamos só com os 25 itens e com quem respondeu todos (2.436 pessoas).
dados_bfi <- psych::bfi[, 1:25]
dados_bfi <- dados_bfi[stats::complete.cases(dados_bfi), ]
rownames(dados_bfi) <- NULL
N_TOTAL <- nrow(dados_bfi)

# --- 2. Os domínios ---------------------------------------------------
# Cada letra do código do item indica o domínio da personalidade.
DOMINIOS <- c(
  A = "Amabilidade",
  C = "Conscienciosidade",
  E = "Extroversão",
  N = "Neuroticismo",
  O = "Abertura"
)

# Uma cor fixa por domínio (a mesma em todos os gráficos e tabelas).
# Paleta escolhida para ser distinguível também por daltônicos; mesmo
# assim, o código do item sempre aparece escrito, para não depender só da cor.
CORES_DOMINIO <- c(
  A = "#2a78d6",  # azul
  C = "#eb6834",  # laranja
  E = "#1baf7a",  # verde-água
  N = "#6250d6",  # violeta
  O = "#e87ba4"   # rosa
)

# --- 3. A tabela de itens ---------------------------------------------
# Rótulo curto em português de cada item.
ROTULOS <- c(
  A1 = "Indiferente aos sentimentos alheios",
  A2 = "Pergunta como os outros estão",
  A3 = "Sabe consolar os outros",
  A4 = "Gosta de crianças",
  A5 = "Deixa as pessoas à vontade",
  C1 = "Exigente no trabalho",
  C2 = "Insiste até ficar perfeito",
  C3 = "Segue um plano",
  C4 = "Faz as coisas pela metade",
  C5 = "Desperdiça tempo",
  E1 = "Fala pouco",
  E2 = "Tem dificuldade de se aproximar",
  E3 = "Sabe cativar as pessoas",
  E4 = "Faz amigos com facilidade",
  E5 = "Assume o comando",
  N1 = "Fica com raiva facilmente",
  N2 = "Irrita-se facilmente",
  N3 = "Muda muito de humor",
  N4 = "Sente-se triste com frequência",
  N5 = "Entra em pânico facilmente",
  O1 = "Cheio de ideias",
  O2 = "Evita leituras difíceis",
  O3 = "Eleva o nível da conversa",
  O4 = "Reflete sobre as coisas",
  O5 = "Não se aprofunda nos assuntos"
)

# Itens invertidos: concordar com eles indica MENOS do traço.
INVERTIDOS <- c("A1", "C4", "C5", "E1", "E2", "O2", "O5")

itens <- data.frame(
  codigo    = names(ROTULOS),
  rotulo    = unname(ROTULOS),
  letra     = substr(names(ROTULOS), 1, 1),
  stringsAsFactors = FALSE
)
itens$dominio   <- unname(DOMINIOS[itens$letra])
itens$cor       <- unname(CORES_DOMINIO[itens$letra])
itens$invertido <- itens$codigo %in% INVERTIDOS
# Nome para exibir: "A1 (−) Indiferente aos sentimentos alheios"
itens$nome <- paste0(
  itens$codigo,
  ifelse(itens$invertido, " (−) ", " "),
  itens$rotulo
)
rownames(itens) <- itens$codigo
