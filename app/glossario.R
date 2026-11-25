# Laboratório de Psicometria – Análise Fatorial Exploratória
# © 2026 Leonardo Ferreira Russo — licença CC BY-NC-ND 4.0 (veja o arquivo LICENSE)
# =====================================================================
# glossario.R  —  O "DICIONÁRIO" DO APP
# ---------------------------------------------------------------------
# Definições curtas dos termos principais. Onde um termo aparece marcado
# com termo(), ele ganha um sublinhado pontilhado e, ao passar o mouse,
# mostra a definição num balão.
# =====================================================================

GLOSSARIO <- c(
  fator          = "Variável que não pode ser observada diretamente (como a Extroversão), mas que influencia as respostas a vários itens.",
  carga          = "Força da ligação entre um item e um fator, de −1 a 1. Quanto mais longe de zero, mais o item é influenciado pelo fator.",
  comunalidade   = "h²: quanto da variação das respostas a um item é explicado pelos fatores juntos (de 0 a 1). O resto é a parte própria do item.",
  autovalor      = "Quanto da variação total dos itens um fator explica. Na solução sem rotação, é a soma das cargas ao quadrado do fator.",
  kmo            = "Kaiser-Meyer-Olkin: indica o quanto os itens compartilham entre si (de 0 a 1). A partir de 0,60 a análise fatorial é aceitável; acima de 0,80, muito boa.",
  bartlett       = "Teste que verifica se as correlações entre os itens são todas zero. Precisa dar significativo (p < 0,05), mas isso é só um requisito mínimo.",
  paralela       = "Compara os autovalores dos dados reais com os de dados aleatórios do mesmo tamanho. Mantém-se o fator que explica mais do que o acaso.",
  scree          = "Gráfico dos autovalores em ordem. O “cotovelo”, onde a linha para de cair rápido, sugere quantos fatores manter.",
  kaiser         = "Critério antigo: manter os fatores com autovalor maior que 1 (na matriz de correlação completa). Costuma exagerar o número de fatores.",
  extracao       = "O método usado para estimar os fatores a partir das correlações (ex.: resíduos mínimos, máxima verossimilhança, eixos principais).",
  rotacao        = "Girar os eixos dos fatores para que cada item fique ligado a um único fator. Não muda o ajuste, só facilita a interpretação.",
  ortogonal      = "Rotação que mantém os fatores sem correlação entre si (ex.: varimax).",
  obliqua        = "Rotação que permite que os fatores se correlacionem (ex.: oblimin, promax). Costuma ser mais realista em psicologia.",
  estrutura_simples = "O ideal da rotação: cada item com carga forte em um só fator e cargas perto de zero nos demais.",
  matriz_padrao  = "Nas rotações oblíquas, as cargas que mostram o efeito único de cada fator sobre o item. É a usada para interpretar.",
  matriz_estrutura = "Nas rotações oblíquas, a correlação simples entre cada item e cada fator (inclui o efeito da correlação entre os fatores).",
  carga_cruzada  = "Quando um item tem carga forte em dois ou mais fatores ao mesmo tempo — fica difícil dizer a qual ele pertence.",
  ponto_corte    = "Valor mínimo para considerar uma carga “forte” (o mais comum é entre 0,30 e 0,40). É uma decisão do pesquisador.",
  tli            = "Índice de Tucker-Lewis: compara o modelo com um modelo sem fatores. Quanto maior, melhor: 0,95 ou mais é bom; de 0,90 a 0,95, aceitável.",
  rmsea          = "Erro médio de aproximação do modelo, que penaliza modelos complexos. Quanto menor, melhor: até 0,05 é bom; até 0,08, aceitável.",
  residuo        = "Diferença entre a correlação observada e a que o modelo reproduz: o que os fatores não explicaram. O RMSR é a média desses resíduos.",
  heywood        = "Resultado impossível: um item com comunalidade igual ou maior que 1. Costuma indicar fatores demais ou amostra pequena.",
  variancia      = "Quanto da variação das respostas aos itens os fatores explicam, em média. O resto é a parte própria de cada item (inclui o erro de medida).",
  alfa           = "Alfa de Cronbach: indica se os itens de uma escala “contam a mesma história”. Supõe que todos os itens têm a mesma carga.",
  omega          = "Ômega de McDonald: medida de confiabilidade calculada a partir das cargas, sem supor que elas sejam iguais. Hoje é a mais recomendada.",
  congruencia    = "Congruência de Tucker: o quanto dois fatores têm cargas parecidas (de 0 a 1). A partir de 0,95, são considerados o mesmo fator.",
  invertido      = "Item escrito no sentido contrário ao traço: concordar com ele indica MENOS do traço (ex.: “Fala pouco”, para Extroversão).",
  policorica     = "Correlação que supõe uma variável contínua “escondida” por trás de cada resposta ordinal (como 1 a 6). Mais indicada para escalas Likert."
)

# Termo com sublinhado pontilhado e definição ao passar o mouse.
# termo("carga") mostra "carga"; termo("carga", "cargas fatoriais") mostra o texto pedido.
termo <- function(chave, texto = chave) {
  htmltools::tags$span(
    class = "termo", tabindex = "0",
    `data-bs-toggle` = "tooltip", `data-bs-title` = GLOSSARIO[[chave]],
    texto)
}

# A mesma coisa, mas como texto HTML (para usar dentro de frases escritas em HTML)
termo_html <- function(chave, texto = chave) {
  sprintf("<span class='termo' tabindex='0' data-bs-toggle='tooltip' data-bs-title='%s'>%s</span>",
          htmltools::htmlEscape(GLOSSARIO[[chave]], attribute = TRUE), texto)
}
