# =====================================================================
# missoes.R  —  O "ROTEIRO" DAS MISSÕES GUIADAS
# ---------------------------------------------------------------------
# Cada missão tem:
#   titulo, nivel, aba (onde ela acontece), objetivo, passos (o que fazer),
#   condicoes (o que o app confere antes de liberar o "Concluí"),
#   reflexao (a pergunta) e comentario (a resposta comentada).
#
# Os comentários falam em "provavelmente" e "nesta amostra" porque os
# números mudam um pouco a cada sorteio de amostra.
# =====================================================================

NOMES_ABAS <- c(
  correlacoes = "1. Correlações",
  kmo         = "2. KMO e Bartlett",
  scree       = "3. Quantos fatores?",
  cargas      = "4. Cargas fatoriais",
  rotacao     = "5. Rotação",
  fatores     = "6. Fatores e alfa"
)

NIVEIS <- c("Básico", "Intermediário", "Avançado", "Desafio")

# --- Condições prontas (cada uma: um rótulo e um teste) -----------------
# O teste recebe "i" (os controles do app) e devolve TRUE ou FALSE.
cond <- function(rotulo, teste) list(rotulo = rotulo, teste = teste)

cond_aba <- function(aba) cond(
  sprintf("Abrir a aba “%s”", NOMES_ABAS[[aba]]),
  function(i) identical(i$aba, aba))

cond_fatores <- function(k) cond(
  sprintf("Número de fatores = %d", k),
  function(i) isTRUE(i$nfatores == k))

cond_rotacao <- function(r) cond(
  sprintf("Rotação: %s", c(none = "nenhuma", varimax = "varimax",
                           oblimin = "oblimin", promax = "promax")[[r]]),
  function(i) identical(i$rotacao, r))

cond_amostra_completa <- function() cond(
  "Tamanho da amostra no máximo (todas as pessoas)",
  function(i) isTRUE(i$n_amostra >= N_TOTAL))

# --- As missões ---------------------------------------------------------
MISSOES <- list(

  # ------------------------------------------------------------- BÁSICO
  list(
    titulo = "Primeiro olhar nas correlações",
    nivel = "Básico", aba = "correlacoes",
    objetivo = "Descobrir, só olhando o mapa de calor, se os itens formam grupos — a ideia que está por trás de toda a análise fatorial.",
    passos = c(
      "Na barra lateral, leve o “Tamanho da amostra” até o máximo, para usar todas as pessoas.",
      "Abra a aba 1 e observe o mapa de calor: os itens estão agrupados por domínio (A, C, E, N, O).",
      "Procure os “blocos” de cor forte ao longo da diagonal. Quantos você encontra? Qual é o bloco mais forte?"
    ),
    condicoes = list(cond_aba("correlacoes"), cond_amostra_completa()),
    reflexao = "Dentro do bloco da Amabilidade (A) aparecem alguns quadradinhos azuis, de correlação negativa. Por que itens do mesmo domínio teriam correlação negativa?",
    comentario = "Porque o item A1 (“Indiferente aos sentimentos alheios”) é invertido: concordar com ele indica MENOS amabilidade. Quem é amável tende a concordar com A2–A5 e a discordar de A1 — por isso a correlação negativa. O bloco mais forte costuma ser o do Neuroticismo (N): seus itens falam de emoções muito parecidas (raiva, irritação, mudanças de humor), e por isso andam muito juntos."
  ),

  list(
    titulo = "Os itens invertidos",
    nivel = "Básico", aba = "correlacoes",
    objetivo = "Entender como os itens invertidos aparecem nos números, e por que eles não atrapalham a análise.",
    passos = c(
      "Na aba 1, ligue a opção “Mostrar os valores dentro dos quadradinhos”.",
      "Ache a correlação entre E1 (“Fala pouco”) e E2 (“Tem dificuldade de se aproximar”). Ela é positiva ou negativa?",
      "Agora ache a correlação entre E1 e E4 (“Faz amigos com facilidade”). E esta?"
    ),
    condicoes = list(
      cond_aba("correlacoes"),
      cond("Ligar “Mostrar os valores dentro dos quadradinhos”", function(i) isTRUE(i$mostrar_numeros))
    ),
    reflexao = "E1 e E4 medem o mesmo traço (Extroversão), mas têm correlação negativa. Isso é um problema para a análise fatorial?",
    comentario = "Não. A análise fatorial olha para a FORÇA da relação, não para o sentido. E1 e E2 são invertidos (ambos falam de introversão), então andam juntos (correlação positiva). Já E1 e E4 andam em sentidos opostos (correlação negativa), mas a ligação é forte do mesmo jeito. Mais adiante você vai ver que os itens invertidos aparecem com carga NEGATIVA no fator — e que, para calcular o alfa, eles precisam ser “desvirados”."
  ),

  list(
    titulo = "Os dados servem para análise fatorial?",
    nivel = "Básico", aba = "kmo",
    objetivo = "Usar o KMO e o teste de Bartlett para decidir se vale a pena procurar fatores nestes dados.",
    passos = c(
      "Mantenha a amostra completa (tamanho da amostra no máximo).",
      "Na aba 2, leia o KMO geral e a sua classificação.",
      "Leia o resultado do teste de Bartlett.",
      "Na tabela de baixo, encontre o item com o MENOR KMO."
    ),
    condicoes = list(cond_aba("kmo"), cond_amostra_completa()),
    reflexao = "O teste de Bartlett deu significativo (p < 0,001). Isso prova que vamos encontrar bons fatores?",
    comentario = "Não. O Bartlett só testa se as correlações são TODAS zero — um requisito mínimo. Com milhares de pessoas, ele dá significativo quase sempre, até com correlações bem fracas. O KMO é mais informativo: ele indica o quanto os itens compartilham entre si. Aqui ele deve ficar na faixa “meritório” (perto de 0,85), o que é um bom sinal. Itens com KMO individual mais baixo compartilham menos com os outros: são os primeiros a observar com atenção."
  ),

  list(
    titulo = "O peso do tamanho da amostra",
    nivel = "Básico", aba = "kmo",
    objetivo = "Ver como os resultados ficam instáveis quando a amostra é pequena.",
    passos = c(
      "Na aba 2, anote o KMO geral com a amostra completa.",
      "Reduza o “Tamanho da amostra” para 100 pessoas ou menos.",
      "Clique em “Sortear outra amostra” algumas vezes e observe o KMO e a tabela dos itens mudando."
    ),
    condicoes = list(
      cond_aba("kmo"),
      cond("Tamanho da amostra com 100 pessoas ou menos", function(i) isTRUE(i$n_amostra <= 100)),
      cond("Clicar em “Sortear outra amostra” pelo menos uma vez", function(i) isTRUE(i$sortear >= 1))
    ),
    reflexao = "Por que os resultados “pulam” tanto de um sorteio para outro quando a amostra é pequena? Quantas pessoas você acha que seriam suficientes para estes 25 itens?",
    comentario = "Cada correlação é uma estimativa, e com poucas pessoas ela tem muito erro: basta uma ou outra pessoa diferente para mudar o número. Somando o erro de 300 correlações (25 itens), o KMO e os fatores ficam instáveis. Não existe um número mágico, mas regras comuns pedem pelo menos 5 a 10 pessoas por item (aqui, 125 a 250) e, idealmente, 300 ou mais. Quanto mais fortes e claras as cargas, menor a amostra necessária."
  ),

  # ------------------------------------------------------- INTERMEDIÁRIO
  list(
    titulo = "Quantos fatores?",
    nivel = "Intermediário", aba = "scree",
    objetivo = "Decidir o número de fatores com o scree plot e a análise paralela — antes de olhar a resposta pronta.",
    passos = c(
      "Volte a amostra para o máximo (todas as pessoas).",
      "Na aba 3, conte quantos pontos azuis (dados reais) ficam ACIMA dos laranjas (acaso), da esquerda para a direita.",
      "Só depois abra “Ver a sugestão da análise paralela” e compare com a sua contagem.",
      "Ligue “Mostrar também os autovalores usados no critério de Kaiser” e compare a linha cinza com a azul."
    ),
    condicoes = list(cond_aba("scree"), cond_amostra_completa()),
    reflexao = "O questionário foi feito para medir 5 traços, mas a análise paralela provavelmente sugere 6 fatores. Quem está “certo”: a teoria ou os dados?",
    comentario = "Nenhum dos dois sozinho. A análise paralela é um guia estatístico: ela diz quantos fatores explicam mais do que o acaso. Com uma amostra grande, até fatores pequenos (por exemplo, dois itens muito parecidos) passam nesse teste. A decisão final junta estatística e teoria: teste 5 e 6 fatores e veja qual solução faz mais sentido. O critério de Kaiser (autovalor > 1) é o mais antigo e costuma exagerar no número de fatores; hoje não é recomendado como critério único. Repare também que ele usa outros autovalores (os da matriz completa, linha cinza), maiores que os da análise fatorial (linha azul) — por isso contar os pontos azuis acima de 1 dá outro número. Por fim, o gráfico “Quanto o ajuste melhora a cada fator?” ajuda a ver o que se ganha indo de 5 para 6."
  ),

  list(
    titulo = "Um fator só",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Ver o que acontece quando forçamos todos os itens a caber em um único fator.",
    passos = c(
      "Na barra lateral, escolha 1 fator e rotação “Nenhuma”.",
      "Na aba 4, leia o TLI, o RMSEA e o comentário “O que os índices dizem juntos”.",
      "Na tabela, veja quais itens têm carga forte nesse fator único — e quais ficaram de fora.",
      "Abra a sub-aba “Resíduos”: que blocos de itens o fator único deixou sem explicar?"
    ),
    condicoes = list(cond_aba("cargas"), cond_fatores(1), cond_rotacao("none")),
    reflexao = "Se um único fator se ajustasse bem, o que isso diria sobre a personalidade das pessoas?",
    comentario = "Diria que existe um único traço geral por trás de todas as respostas — que daria para resumir a personalidade em um número só. Os índices ruins mostram que não é o caso: um fator deixa a maior parte das relações sem explicar. O fator único acaba reunindo os itens que mais se correlacionam entre si (provavelmente os de Neuroticismo e alguns de Extroversão), e os outros domínios ficam quase de fora. Os resíduos mostram isso com clareza: sobram blocos coloridos justamente nos domínios que o fator único não conseguiu explicar — cada bloco é um fator que está faltando."
  ),

  list(
    titulo = "Aumentando o número de fatores",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Acompanhar como o ajuste muda a cada fator acrescentado.",
    passos = c(
      "Na aba 4, comece com 1 fator e vá subindo, um por um, até 5, observando o TLI, o RMSEA e os resíduos.",
      "Depois, na aba 3, veja o gráfico “Quanto o ajuste melhora a cada fator?”, que resume os modelos de 1 a 10 fatores.",
      "Em qual passo o ajuste melhorou mais? E menos?",
      "Volte para a aba 4 com 5 fatores."
    ),
    condicoes = list(cond_aba("cargas"), cond_fatores(5)),
    reflexao = "O TLI sempre melhora quando acrescentamos fatores. Então por que não usar logo 10 fatores?",
    comentario = "Porque mais fatores SEMPRE ajustam melhor: no extremo, com um fator por item, o ajuste seria perfeito — e não teríamos resumido nada. O objetivo da análise fatorial é a parcimônia: explicar bem os dados com o MENOR número de fatores que façam sentido. Repare que os primeiros fatores melhoram muito o ajuste, e os seguintes, cada vez menos. Quando acrescentar um fator quase não muda nada, é sinal de que ele não é necessário."
  ),

  list(
    titulo = "Dando nome aos fatores",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Interpretar os fatores a partir dos itens que têm carga forte em cada um.",
    passos = c(
      "Escolha 5 fatores e rotação varimax.",
      "Na aba 4, deixe ligada a opção “Agrupar os itens pelo fator em que têm a maior carga”.",
      "Para cada fator (F1 a F5), leia os itens do seu grupo e dê um nome a ele.",
      "Procure os itens marcados com “carga cruzada”."
    ),
    condicoes = list(
      cond_aba("cargas"), cond_fatores(5), cond_rotacao("varimax"),
      cond("Ligar “Agrupar os itens pelo fator em que têm a maior carga”", function(i) isTRUE(i$agrupar_cargas))
    ),
    reflexao = "Algum item tem carga forte em dois fatores ao mesmo tempo? Pelo conteúdo do item, isso faz sentido?",
    comentario = "Provavelmente sim: os nomes saem quase iguais aos domínios do Big Five (Neuroticismo, Extroversão, Conscienciosidade, Amabilidade, Abertura). Itens com carga cruzada costumam ter conteúdo “misto”. Por exemplo, N4 (“Sente-se triste com frequência”) pode carregar em Neuroticismo e, negativamente, em Extroversão — pessoas tristes tendem a ser menos sociáveis. Uma carga cruzada não é um erro: ela mostra que o item mede um pouco de cada coisa, o que é importante saber antes de montar uma escala."
  ),

  list(
    titulo = "O ponto de corte",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Entender como o ponto de corte muda a leitura da tabela de cargas.",
    passos = c(
      "Na aba 4, com 5 fatores, observe quantos itens têm a marca “sem fator” com o corte em 0,30.",
      "Suba o “Ponto de corte das cargas” para 0,50 ou mais.",
      "Conte de novo os itens “sem fator” e as cargas cruzadas."
    ),
    condicoes = list(
      cond_aba("cargas"), cond_fatores(5),
      cond("Ponto de corte em 0,50 ou mais", function(i) isTRUE(i$corte >= 0.5 - 1e-9))
    ),
    reflexao = "Um ponto de corte mais exigente é sempre melhor? O que se ganha e o que se perde?",
    comentario = "Não necessariamente. Com um corte alto, cada fator fica com poucos itens, mas muito “puros”: as cargas cruzadas desaparecem, só que mais itens ficam sem fator. Com um corte baixo, quase todo item entra em algum fator, mas aparecem ligações fracas e cargas cruzadas. Valores entre 0,30 e 0,40 são os mais usados. O corte é uma decisão do pesquisador e deve ser informada no relatório. Volte o corte para 0,30 antes de seguir."
  ),

  # ------------------------------------------------------------ AVANÇADO
  list(
    titulo = "Girando os eixos",
    nivel = "Avançado", aba = "rotacao",
    objetivo = "Ver o que a rotação faz com as cargas — e o que ela NÃO muda.",
    passos = c(
      "Com 5 fatores, escolha rotação “Nenhuma” e abra a aba 5. Observe os dois gráficos (são iguais).",
      "Agora escolha a rotação varimax e compare “antes” e “depois”.",
      "Na tabela de baixo, compare as cargas cruzadas antes e depois, e a variância explicada total.",
      "Se quiser, troque os fatores dos eixos (ex.: F3 × F4) e veja outros grupos de itens."
    ),
    condicoes = list(cond_aba("rotacao"), cond_fatores(5), cond_rotacao("varimax")),
    reflexao = "Se a rotação não muda o ajuste do modelo (TLI, RMSEA e variância total continuam iguais), para que serve rotacionar?",
    comentario = "Para INTERPRETAR. A solução sem rotação é matematicamente correta, mas difícil de ler: o primeiro fator “puxa” quase todos os itens, e aparecem muitas cargas cruzadas. A rotação gira os eixos até que cada item fique perto de um único eixo (a “estrutura simples”). É como girar um mapa até o norte ficar para cima: o território é o mesmo, mas fica muito mais fácil de se orientar."
  ),

  list(
    titulo = "Ortogonal ou oblíqua?",
    nivel = "Avançado", aba = "fatores",
    objetivo = "Descobrir se os fatores se correlacionam e escolher o tipo de rotação.",
    passos = c(
      "Com 5 fatores e rotação varimax, abra a aba 6 e leia o que aparece sobre a correlação entre fatores.",
      "Troque para a rotação oblimin.",
      "Na tabela de correlações, encontre o par de fatores com a maior correlação (em módulo). Quais traços são esses?",
      "Na aba 4, alterne entre “Matriz padrão” e “Matriz de estrutura”. Onde aparecem as diferenças?"
    ),
    condicoes = list(cond_aba("fatores"), cond_fatores(5), cond_rotacao("oblimin")),
    reflexao = "Faz sentido supor que traços de personalidade são totalmente independentes uns dos outros? Que rotação você escolheria e por quê?",
    comentario = "Na psicologia, quase nunca. Pessoas mais ansiosas (Neuroticismo) tendem, em média, a ser um pouco menos extrovertidas e menos amáveis, por exemplo. A varimax obriga as correlações a serem zero, o que é uma suposição forte. A oblimin deixa os dados mostrarem as correlações: se elas forem baixas (abaixo de 0,32), as duas rotações dão resultados parecidos; se forem altas, a oblíqua representa melhor a realidade. Por isso muitos autores recomendam começar pela oblíqua. Com fatores correlacionados, surgem duas tabelas: a matriz padrão (o efeito único de cada fator, usada para interpretar) e a de estrutura (a correlação item–fator). Na estrutura, os itens parecem “carregar” um pouco em fatores aos quais não pertencem — é só reflexo da correlação entre os fatores."
  ),

  list(
    titulo = "Os fatores são confiáveis?",
    nivel = "Avançado", aba = "fatores",
    objetivo = "Avaliar a consistência interna (alfa) de cada fator.",
    passos = c(
      "Com 5 fatores, rotação oblimin e ponto de corte 0,30, abra a aba 6.",
      "Leia o alfa, o ômega e a classificação de cada fator.",
      "Qual fator tem o alfa mais baixo? Quais itens dele aparecem marcados com “(inv.)”?",
      "Compare o alfa com o ômega de cada fator: eles ficam parecidos ou diferentes?"
    ),
    condicoes = list(
      cond_aba("fatores"), cond_fatores(5), cond_rotacao("oblimin"),
      cond("Ponto de corte em 0,30", function(i) isTRUE(abs(i$corte - 0.30) < 1e-6))
    ),
    reflexao = "Por que o fator da Abertura costuma ter o alfa mais baixo dos cinco?",
    comentario = "O alfa depende de duas coisas: do número de itens e de quanto eles se correlacionam. Os itens de Abertura falam de coisas bem diferentes entre si (ter ideias, evitar leituras difíceis, elevar o nível da conversa, refletir), então se correlacionam menos — e o alfa cai. Repare também nos itens “(inv.)”: são os itens invertidos (O2 e O5), que precisam ser desvirados antes do cálculo. Com só 5 itens por fator, alfas entre 0,60 e 0,80 são comuns. Nestes dados, alfa e ômega ficam muito parecidos porque as cargas dentro de cada fator são semelhantes; quando as cargas são desiguais, o ômega fica acima do alfa — e é o mais recomendado. E lembre: alfa alto não prova que o fator mede uma coisa só."
  ),

  list(
    titulo = "Pearson ou policórica?",
    nivel = "Avançado", aba = "cargas",
    objetivo = "Comparar os dois tipos de correlação e entender quando usar cada um.",
    passos = c(
      "Com 5 fatores e correlação de Pearson, clique em “Guardar esta solução para comparar” na aba 4.",
      "Troque para correlação policórica (pode levar alguns segundos).",
      "Abra a sub-aba “Comparar”: as comunalidades (h²) ficaram maiores ou menores? E os índices de ajuste? Os fatores continuam os mesmos?",
      "Se quiser, veja também o alfa na aba 6 — com policórica ele vira o “alfa ordinal”."
    ),
    condicoes = list(
      cond_aba("cargas"), cond_fatores(5),
      cond("Tipo de correlação: policórica", function(i) identical(i$correlacao, "policorica"))
    ),
    reflexao = "As respostas vão de 1 a 6 (discordo totalmente → concordo totalmente). Por que isso pode fazer a correlação de Pearson “subestimar” as relações?",
    comentario = "A correlação de Pearson trata as respostas como números contínuos, mas com apenas 6 opções boa parte da informação se perde — e as correlações tendem a sair menores do que as reais. A policórica supõe que, por trás de cada resposta, existe uma variável contínua “escondida” e estima a correlação entre elas. Por isso as cargas costumam sair um pouco maiores. Ela é a mais indicada para escalas de concordância com poucas opções, mas é mais lenta e pode ficar instável com amostras pequenas."
  ),

  # ------------------------------------------------------------- DESAFIO
  list(
    titulo = "Fatores demais — e o seu modelo final",
    nivel = "Desafio", aba = "cargas",
    objetivo = "Reconhecer os sinais de excesso de fatores e, no fim, escolher e defender o seu modelo.",
    passos = c(
      "Use a amostra completa e escolha 10 fatores.",
      "Na aba 4, leia os índices de ajuste e o comentário. Algum aviso apareceu?",
      "Na tabela, procure fatores com apenas um item de carga forte.",
      "Por fim, volte ao número de fatores que você acha melhor e escolha extração, rotação e tipo de correlação."
    ),
    condicoes = list(cond_aba("cargas"), cond_fatores(10), cond_amostra_completa()),
    reflexao = "Se você fosse escrever um relatório, qual modelo escolheria (número de fatores, extração, rotação e correlação)? Que evidências do app você usaria para justificar cada escolha?",
    comentario = "Com 10 fatores, os índices ficam “bons”, mas aparecem fatores com um só item (um fator precisa de pelo menos 3 itens para ser interpretável) e, às vezes, casos de Heywood: sinais de que o modelo está “decorando” a amostra. Uma boa justificativa junta várias evidências: (1) KMO e Bartlett adequados; (2) número de fatores apoiado pela análise paralela E pela teoria; (3) índices de ajuste aceitáveis; (4) cargas claras, com poucas cargas cruzadas; (5) rotação coerente com a correlação entre os fatores; (6) alfas aceitáveis. Para estes dados, 5 fatores com rotação oblíqua e correlação policórica é uma escolha bem defensável — mas o importante é que a sua escolha esteja bem argumentada."
  )
)

N_MISSOES <- length(MISSOES)

# Confere as condições de uma missão: devolve um vetor TRUE/FALSE.
conferir_missao <- function(m, i) {
  vapply(m$condicoes, function(cd) isTRUE(tryCatch(cd$teste(i), error = function(e) FALSE)),
         logical(1))
}
