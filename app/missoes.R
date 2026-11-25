# Laboratório de Psicometria – Análise Fatorial Exploratória
# © 2026 Leonardo Ferreira Russo — licença CC BY-NC-ND 4.0 (veja o arquivo LICENSE)
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
  inicio      = "0. Comece aqui",
  itens       = "1. Itens",
  correlacoes = "2. Correlações",
  kmo         = "3. KMO e Bartlett",
  scree       = "4. Quantos fatores?",
  cargas      = "5. Cargas fatoriais",
  rotacao     = "6. Rotação",
  fatores     = "7. Fatores e alfa"
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
# Cada missão tem um "id" FIXO: é por ele que o progresso fica salvo no
# navegador. Por isso as missões podem ser reordenadas à vontade, mas o id
# de uma missão nunca deve mudar.
# "usa_todos_itens" = a missão foi pensada com os 25 itens (o app avisa se
# houver itens excluídos na aba 1).
MISSOES <- list(

  # ============================================ BÁSICO — conhecer os dados
  list(
    id = "itens_conhecer", usa_todos_itens = TRUE,
    titulo = "Conhecendo os itens",
    nivel = "Básico", aba = "itens",
    objetivo = "Conhecer os 25 itens antes de qualquer análise — e fazer uma previsão sobre eles.",
    passos = c(
      "Se ainda não leu, leia a aba “0. Comece aqui”.",
      "Na aba 1, leia o texto dos 25 itens. Ignore os números das outras colunas por enquanto.",
      "Encontre os 7 itens invertidos (marcados com (−)). Para cada um, pense: concordar com ele indica mais ou menos do traço?",
      "Faça uma previsão: quais itens você acha que vão “andar juntos” nas respostas? Algum item parece poder pertencer a dois domínios?"
    ),
    condicoes = list(cond_aba("itens")),
    reflexao = "Qual item você acha que vai se comportar “mal” na análise — ligando-se fraco ao seu domínio, ou a dois domínios ao mesmo tempo? Por quê?",
    comentario = "Guarde a sua previsão: você vai poder conferi-la ao longo das missões. Alguns candidatos comuns: itens de conteúdo amplo ou ambíguo, como O4 (“Reflete sobre as coisas”), que pode ter a ver com Abertura mas também com outros traços; N4 (“Sente-se triste com frequência”), que mistura emoção negativa com retraimento social; e itens invertidos, como A1, que costumam ter ligações mais fracas por causa da forma negativa da frase. Ler os itens com atenção antes de analisar faz parte do trabalho: na análise fatorial, os números só fazem sentido junto com o conteúdo."
  ),

  list(
    id = "correlacoes_olhar", usa_todos_itens = TRUE,
    titulo = "Primeiro olhar nas correlações",
    nivel = "Básico", aba = "correlacoes",
    objetivo = "Descobrir, só olhando o mapa de calor, se os itens formam grupos — a ideia que está por trás de toda a análise fatorial.",
    passos = c(
      "Confira, na barra lateral, que o “Tamanho da amostra” está no máximo (todas as pessoas — é assim que o app abre).",
      "Abra a aba 2 e observe o mapa de calor: os itens estão agrupados por domínio (A, C, E, N, O).",
      "Procure os “blocos” de cor forte ao longo da diagonal. Quantos você encontra? Qual é o bloco mais forte?"
    ),
    condicoes = list(cond_aba("correlacoes"), cond_amostra_completa()),
    reflexao = "Dentro do bloco da Amabilidade (A) aparecem alguns quadradinhos azuis, de correlação negativa. Por que itens do mesmo domínio teriam correlação negativa?",
    comentario = "Porque o item A1 (“Indiferente aos sentimentos alheios”) é invertido: concordar com ele indica MENOS amabilidade. Quem é amável tende a concordar com A2–A5 e a discordar de A1 — por isso a correlação negativa. O bloco mais forte costuma ser o do Neuroticismo (N): seus itens falam de emoções muito parecidas (raiva, irritação, mudanças de humor), e por isso andam muito juntos."
  ),

  list(
    id = "itens_invertidos", usa_todos_itens = TRUE,
    titulo = "Os itens invertidos",
    nivel = "Básico", aba = "correlacoes",
    objetivo = "Entender como os itens invertidos aparecem nos números, e por que eles não atrapalham a análise.",
    passos = c(
      "Na aba 2, ligue a opção “Mostrar os valores dentro dos quadradinhos”.",
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
    id = "kmo_bartlett", usa_todos_itens = TRUE,
    titulo = "Os dados servem para análise fatorial?",
    nivel = "Básico", aba = "kmo",
    objetivo = "Usar o KMO e o teste de Bartlett para decidir se vale a pena procurar fatores nestes dados.",
    passos = c(
      "Na aba 3, leia o KMO geral e a sua classificação.",
      "Leia o resultado do teste de Bartlett.",
      "Na tabela de baixo, encontre o item com o MENOR KMO."
    ),
    condicoes = list(cond_aba("kmo"), cond_amostra_completa()),
    reflexao = "O teste de Bartlett deu significativo (p < 0,001). Isso prova que vamos encontrar bons fatores?",
    comentario = "Não. O Bartlett só testa se as correlações são TODAS zero — um requisito mínimo. Com milhares de pessoas, ele dá significativo quase sempre, até com correlações bem fracas. O KMO é mais informativo: ele indica o quanto os itens compartilham entre si. Aqui ele deve ficar na faixa “meritório” (perto de 0,85), o que é um bom sinal. Itens com KMO individual mais baixo compartilham menos com os outros: são os primeiros a observar com atenção."
  ),

  list(
    id = "tamanho_amostra", usa_todos_itens = TRUE,
    titulo = "O peso do tamanho da amostra",
    nivel = "Básico", aba = "kmo",
    objetivo = "Ver como os resultados ficam instáveis quando a amostra é pequena.",
    passos = c(
      "Na aba 3, anote o KMO geral com a amostra completa.",
      "Reduza o “Tamanho da amostra” para 100 pessoas ou menos.",
      "Clique em “Sortear outra amostra” algumas vezes e observe o KMO e a tabela dos itens mudando."
    ),
    condicoes = list(
      cond_aba("kmo"),
      cond("Tamanho da amostra com 100 pessoas ou menos", function(i) isTRUE(i$n_amostra <= 100)),
      cond("Clicar em “Sortear outra amostra” pelo menos uma vez", function(i) isTRUE(i$sortear >= 1))
    ),
    reflexao = "Por que os resultados “pulam” tanto de um sorteio para outro quando a amostra é pequena? Quantas pessoas você acha que seriam suficientes para estes 25 itens?",
    comentario = "Cada correlação é uma estimativa, e com poucas pessoas ela tem muito erro: basta uma ou outra pessoa diferente para mudar o número. Somando o erro de 300 correlações (25 itens), o KMO e os fatores ficam instáveis. Não existe um número mágico, mas regras comuns pedem pelo menos 5 a 10 pessoas por item (aqui, 125 a 250) e, idealmente, 300 ou mais. Quanto mais fortes e claras as cargas, menor a amostra necessária. Volte a amostra para o máximo antes de seguir."
  ),

  # ================================= INTERMEDIÁRIO — extrair e interpretar
  list(
    id = "quantos_fatores", usa_todos_itens = TRUE,
    titulo = "Quantos fatores?",
    nivel = "Intermediário", aba = "scree",
    objetivo = "Decidir o número de fatores com o scree plot e a análise paralela — antes de olhar a resposta pronta.",
    passos = c(
      "Volte a amostra para o máximo (todas as pessoas).",
      "Na aba 4, conte quantos pontos azuis (dados reais) ficam ACIMA dos laranjas (acaso), da esquerda para a direita.",
      "Só depois abra “Ver a sugestão da análise paralela” e compare com a sua contagem.",
      "Ligue “Mostrar também os autovalores usados no critério de Kaiser” e compare a linha cinza com a azul."
    ),
    condicoes = list(cond_aba("scree"), cond_amostra_completa()),
    reflexao = "O questionário foi feito para medir 5 traços, mas a análise paralela provavelmente sugere 6 fatores. Quem está “certo”: a teoria ou os dados?",
    comentario = "Nenhum dos dois sozinho. A análise paralela é um guia estatístico: ela diz quantos fatores explicam mais do que o acaso. Com uma amostra grande, até fatores pequenos (por exemplo, dois itens muito parecidos) passam nesse teste. A decisão final junta estatística e teoria: teste 5 e 6 fatores e veja qual solução faz mais sentido. O critério de Kaiser (autovalor > 1) é o mais antigo e costuma exagerar no número de fatores; hoje não é recomendado como critério único. Repare também que ele usa outros autovalores (os da matriz completa, linha cinza), maiores que os da análise fatorial (linha azul) — por isso contar os pontos azuis acima de 1 dá outro número. Nas próximas missões, você vai ver o que se ganha (e o que se perde) a cada fator acrescentado."
  ),

  list(
    id = "um_fator", usa_todos_itens = TRUE,
    titulo = "Um fator só",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Ver o que acontece quando forçamos todos os itens a caber em um único fator.",
    passos = c(
      "Na barra lateral, escolha 1 fator e rotação “Nenhuma”.",
      "Na aba 5, leia o TLI, o RMSEA e o comentário “O que os índices dizem juntos”.",
      "Na tabela, veja quais itens têm carga forte nesse fator único — e quais ficaram de fora.",
      "Abra a sub-aba “Resíduos”: que blocos de itens o fator único deixou sem explicar?"
    ),
    condicoes = list(cond_aba("cargas"), cond_fatores(1), cond_rotacao("none")),
    reflexao = "Se um único fator se ajustasse bem, o que isso diria sobre a personalidade das pessoas?",
    comentario = "Diria que existe um único traço geral por trás de todas as respostas — que daria para resumir a personalidade em um número só. Os índices ruins mostram que não é o caso: um fator deixa a maior parte das relações sem explicar. O fator único acaba reunindo os itens que mais se correlacionam entre si (provavelmente os de Neuroticismo e alguns de Extroversão), e os outros domínios ficam quase de fora. Os resíduos mostram isso com clareza: sobram blocos coloridos justamente nos domínios que o fator único não conseguiu explicar — cada bloco é um fator que está faltando."
  ),

  list(
    id = "aumentando_fatores", usa_todos_itens = TRUE,
    titulo = "Aumentando o número de fatores",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Acompanhar como o ajuste muda a cada fator acrescentado.",
    passos = c(
      "Na aba 5, comece com 1 fator e vá subindo, um por um, até 5, observando o TLI, o RMSEA e os resíduos.",
      "Depois, na aba 4, veja o gráfico “Quanto o ajuste melhora a cada fator?”, que resume os modelos de 1 a 10 fatores.",
      "Em qual passo o ajuste melhorou mais? E menos?",
      "Volte para a aba 5 com 5 fatores."
    ),
    condicoes = list(cond_aba("cargas"), cond_fatores(5)),
    reflexao = "O TLI sempre melhora quando acrescentamos fatores. Então por que não usar logo 10 fatores?",
    comentario = "Porque mais fatores SEMPRE ajustam melhor: no extremo, com um fator por item, o ajuste seria perfeito — e não teríamos resumido nada. O objetivo da análise fatorial é a parcimônia: explicar bem os dados com o MENOR número de fatores que façam sentido. Repare que os primeiros fatores melhoram muito o ajuste, e os seguintes, cada vez menos. Quando acrescentar um fator quase não muda nada, é sinal de que ele não é necessário. Na próxima missão, você vai ver o que acontece quando se exagera."
  ),

  list(
    id = "fatores_demais", usa_todos_itens = TRUE,
    titulo = "Fatores demais",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Reconhecer os sinais de que há fatores demais no modelo.",
    passos = c(
      "Use a amostra completa e escolha 10 fatores.",
      "Na aba 5, leia os índices de ajuste e o comentário “O que os índices dizem juntos”. Algum aviso apareceu?",
      "Na tabela, procure fatores com apenas um ou dois itens de carga forte.",
      "Na aba 4, procure no gráfico “Quanto o ajuste melhora a cada fator?” algum caso de Heywood (✖)."
    ),
    condicoes = list(cond_aba("cargas"), cond_fatores(10), cond_amostra_completa()),
    reflexao = "Os índices ficaram “bons” com 10 fatores. Por que, ainda assim, esse modelo não é uma boa escolha?",
    comentario = "Com 10 fatores, os índices melhoram, mas aparecem fatores com um ou dois itens só — e um fator precisa de pelo menos 3 itens para ser interpretável. Podem surgir também casos de Heywood (comunalidade igual ou maior que 1), um sinal de que o modelo está “decorando” os detalhes desta amostra em vez de encontrar algo que se repetiria em outras. Lembre da parcimônia: o melhor modelo não é o de melhor ajuste, e sim o menor que explica bem os dados e faz sentido. Volte para 5 fatores antes de seguir."
  ),

  list(
    id = "girando_eixos", usa_todos_itens = TRUE,
    titulo = "Girando os eixos",
    nivel = "Intermediário", aba = "rotacao",
    objetivo = "Ver o que a rotação faz com as cargas — e o que ela NÃO muda.",
    passos = c(
      "Com 5 fatores, escolha rotação “Nenhuma” e abra a aba 6. Observe os dois gráficos (são iguais).",
      "Agora escolha a rotação varimax e compare “antes” e “depois”.",
      "Na tabela de baixo, compare as cargas cruzadas antes e depois, e a variância explicada total.",
      "Se quiser, troque os fatores dos eixos (ex.: F3 × F4) e veja outros grupos de itens."
    ),
    condicoes = list(cond_aba("rotacao"), cond_fatores(5), cond_rotacao("varimax")),
    reflexao = "Se a rotação não muda o ajuste do modelo (TLI, RMSEA e variância total continuam iguais), para que serve rotacionar?",
    comentario = "Para INTERPRETAR. A solução sem rotação é matematicamente correta, mas difícil de ler: o primeiro fator “puxa” quase todos os itens, e aparecem muitas cargas cruzadas. A rotação gira os eixos até que cada item fique perto de um único eixo (a “estrutura simples”). É como girar um mapa até o norte ficar para cima: o território é o mesmo, mas fica muito mais fácil de se orientar. Por isso, na prática, primeiro rotacionamos e só depois interpretamos os fatores — que é o que você vai fazer na próxima missão."
  ),

  list(
    id = "nomeando_fatores", usa_todos_itens = TRUE,
    titulo = "Dando nome aos fatores",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Interpretar os fatores a partir dos itens que têm carga forte em cada um.",
    passos = c(
      "Mantenha 5 fatores e a rotação varimax (a da missão anterior).",
      "Na aba 5, deixe ligada a opção “Agrupar os itens pelo fator em que têm a maior carga”.",
      "Para cada fator (F1 a F5), leia os itens do seu grupo e escreva um nome para ele nos campos logo acima da tabela (tecle Enter depois de cada nome).",
      "Procure os itens marcados com “carga cruzada”.",
      "Abra a sub-aba “Diagrama do modelo”: os mesmos grupos aparecem desenhados como um modelo (fatores → itens), já com os nomes que você deu."
    ),
    condicoes = list(
      cond_aba("cargas"), cond_fatores(5), cond_rotacao("varimax"),
      cond("Ligar “Agrupar os itens pelo fator em que têm a maior carga”", function(i) isTRUE(i$agrupar_cargas)),
      cond("Dar um nome a cada um dos fatores (campos acima da tabela)",
           function(i) isTRUE(i$n_nomes >= i$nfatores))
    ),
    reflexao = "Algum item tem carga forte em dois fatores ao mesmo tempo? Pelo conteúdo do item, isso faz sentido? E a sua previsão da missão 1 se confirmou?",
    comentario = "Provavelmente os nomes saem quase iguais aos domínios do Big Five (Neuroticismo, Extroversão, Conscienciosidade, Amabilidade, Abertura). Itens com carga cruzada costumam ter conteúdo “misto”. Por exemplo, N4 (“Sente-se triste com frequência”) pode carregar em Neuroticismo e, negativamente, em Extroversão — pessoas tristes tendem a ser menos sociáveis. Uma carga cruzada não é um erro: ela mostra que o item mede um pouco de cada coisa, o que é importante saber antes de montar uma escala."
  ),

  list(
    id = "ponto_corte", usa_todos_itens = TRUE,
    titulo = "O ponto de corte",
    nivel = "Intermediário", aba = "cargas",
    objetivo = "Entender como o ponto de corte muda a leitura da tabela de cargas.",
    passos = c(
      "Na aba 5, com 5 fatores, observe quantos itens têm a marca “sem fator” com o corte em 0,30.",
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

  # ===================================== AVANÇADO — refinar a análise
  list(
    id = "ortogonal_obliqua", usa_todos_itens = TRUE,
    titulo = "Ortogonal ou oblíqua?",
    nivel = "Avançado", aba = "fatores",
    objetivo = "Descobrir se os fatores se correlacionam e escolher o tipo de rotação.",
    passos = c(
      "Com 5 fatores e rotação varimax, abra a aba 7 e leia o que aparece sobre a correlação entre fatores.",
      "Troque para a rotação oblimin.",
      "Na tabela de correlações, encontre o par de fatores com a maior correlação (em módulo). Quais traços são esses?",
      "Na aba 5, alterne entre “Matriz padrão” e “Matriz de estrutura”. Onde aparecem as diferenças?"
    ),
    condicoes = list(cond_aba("fatores"), cond_fatores(5), cond_rotacao("oblimin")),
    reflexao = "Faz sentido supor que traços de personalidade são totalmente independentes uns dos outros? Que rotação você escolheria e por quê?",
    comentario = "Na psicologia, quase nunca. Pessoas mais ansiosas (Neuroticismo) tendem, em média, a ser um pouco menos extrovertidas e menos amáveis, por exemplo. A varimax obriga as correlações a serem zero, o que é uma suposição forte. A oblimin deixa os dados mostrarem as correlações: se elas forem baixas (abaixo de 0,32), as duas rotações dão resultados parecidos; se forem altas, a oblíqua representa melhor a realidade. Por isso muitos autores recomendam começar pela oblíqua. Com fatores correlacionados, surgem duas tabelas: a matriz padrão (o efeito único de cada fator, usada para interpretar) e a de estrutura (a correlação item–fator). Na estrutura, os itens parecem “carregar” um pouco em fatores aos quais não pertencem — é só reflexo da correlação entre os fatores."
  ),

  list(
    id = "confiabilidade", usa_todos_itens = TRUE,
    titulo = "Os fatores são confiáveis?",
    nivel = "Avançado", aba = "fatores",
    objetivo = "Avaliar a consistência interna (alfa e ômega) de cada fator.",
    passos = c(
      "Com 5 fatores, rotação oblimin e ponto de corte 0,30, abra a aba 7.",
      "Leia o alfa, o ômega e a classificação de cada fator (os nomes que você deu aos fatores na missão 11 aparecem na tabela).",
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
    id = "pearson_policorica", usa_todos_itens = TRUE,
    titulo = "Pearson ou policórica?",
    nivel = "Avançado", aba = "cargas",
    objetivo = "Comparar os dois tipos de correlação e entender quando usar cada um.",
    passos = c(
      "Confira que o tamanho da amostra está no máximo (a policórica da amostra completa já vem calculada e aparece na hora).",
      "Com 5 fatores e correlação de Pearson, clique em “Guardar esta solução para comparar” na aba 5.",
      "Troque para correlação policórica.",
      "Abra a sub-aba “Comparar”: as comunalidades (h²) ficaram maiores ou menores? E os índices de ajuste? Os fatores continuam os mesmos?",
      "Se quiser, leia a explicação “Pearson ou policórica? Entenda”, na barra lateral, e veja o alfa na aba 7 — com policórica ele vira o “alfa ordinal”."
    ),
    condicoes = list(
      cond_aba("cargas"), cond_fatores(5), cond_amostra_completa(),
      cond("Tipo de correlação: policórica", function(i) identical(i$correlacao, "policorica"))
    ),
    reflexao = "As respostas vão de 1 a 6 (discordo totalmente → concordo totalmente). Por que isso pode fazer a correlação de Pearson “subestimar” as relações?",
    comentario = "A correlação de Pearson trata as respostas como números contínuos, mas com apenas 6 opções boa parte da informação se perde — e as correlações tendem a sair menores do que as reais. A policórica supõe que, por trás de cada resposta, existe uma variável contínua “escondida” e estima a correlação entre elas. Por isso as cargas costumam sair um pouco maiores. Ela é a mais indicada para escalas de concordância com poucas opções, mas é mais lenta e pode ficar instável com amostras pequenas."
  ),

  list(
    id = "extracao_metodos", usa_todos_itens = TRUE,
    titulo = "Métodos de extração",
    nivel = "Avançado", aba = "cargas",
    objetivo = "Comparar métodos de extração e ver o quanto essa escolha muda os resultados.",
    passos = c(
      "Com a amostra completa, 5 fatores, rotação oblimin e extração “Resíduos mínimos (minres)”, clique em “Guardar esta solução para comparar” (aba 5).",
      "Troque o “Método de extração” para “Máxima verossimilhança (ml)”.",
      "Abra a sub-aba “Comparar”: os índices mudaram muito? Os fatores são os mesmos (congruência)?",
      "Se quiser, teste também “Eixos principais (pa)”."
    ),
    condicoes = list(
      cond_aba("cargas"), cond_fatores(5), cond_amostra_completa(),
      cond("Guardar uma solução para comparar (aba 5)", function(i) isTRUE(i$tem_guardada)),
      cond("Método de extração: máxima verossimilhança (ml)", function(i) identical(i$extracao, "ml"))
    ),
    reflexao = "Se os resultados ficaram quase iguais, a escolha do método de extração importa?",
    comentario = "Nestes dados, os três métodos dão resultados praticamente iguais: o TLI muda pouco (de cerca de 0,877 para 0,881) e os fatores são os mesmos (congruência perto de 1). Repare que a ordem dos fatores pode mudar de um método para outro — por isso comparamos pelo conteúdo, não pelo número. Isso é comum com amostras grandes e fatores bem definidos. A escolha pesa mais em situações difíceis: a máxima verossimilhança (ml) supõe que as respostas seguem uma distribuição normal e oferece testes estatísticos de ajuste, mas pode ter dificuldade para convergir ou gerar resultados impossíveis (casos de Heywood) com amostras pequenas ou respostas muito assimétricas; resíduos mínimos (minres) e eixos principais (pa) não fazem essa suposição e costumam ser mais robustos. Seja qual for, o método escolhido deve ser informado no relatório."
  ),

  # ============================== DESAFIO — decidir como pesquisador
  list(
    id = "item_fraco", usa_todos_itens = FALSE,
    titulo = "Decidindo sobre um item fraco",
    nivel = "Desafio", aba = "itens",
    objetivo = "Decidir se um item deve sair da análise — usando os números E o conteúdo do item — e ver todas as consequências.",
    passos = c(
      "Use a amostra completa, 5 fatores e rotação oblimin.",
      "Volte à aba 1: agora você já sabe ler as colunas! Veja os “sinais para investigar”. Qual item tem a menor comunalidade (h²)? Leia o texto dele.",
      "Na aba 5, clique em “Guardar esta solução para comparar”.",
      "Volte à aba 1 e desmarque esse item.",
      "Veja o “Diário de decisões” (aba 1) e a sub-aba “Comparar” (aba 5): o que melhorou? O que piorou? Algum OUTRO item mudou de fator?"
    ),
    condicoes = list(
      cond_fatores(5), cond_rotacao("oblimin"), cond_amostra_completa(),
      cond("Guardar uma solução para comparar (aba 5)", function(i) isTRUE(i$tem_guardada)),
      cond("Excluir exatamente 1 item (aba 1)", function(i) isTRUE(i$n_excluidos == 1))
    ),
    reflexao = "O ajuste e o alfa da escala do item melhoraram um pouco. Isso basta para justificar a exclusão? O que se perde?",
    comentario = "Não basta. Uma exclusão se justifica quando a estatística (comunalidade baixa, nenhuma carga forte, KMO baixo) E o conteúdo apontam na mesma direção — por exemplo, um item ambíguo ou que mede outra coisa. Nestes dados, o item mais fraco costuma ser A1 (“Indiferente aos sentimentos alheios”): sem ele, o TLI e o alfa da Amabilidade sobem um pouco. Mas repare: outro item pode mudar de fator (E3, por exemplo, pode passar para a Amabilidade), e o alfa de outra escala pode cair. Além disso, cada item retirado estreita o que a escala mede. Boas práticas: uma decisão por vez, sempre justificada e relatada; e conferir se ela se sustenta em outra amostra (sorteie outra!) — senão, você pode estar só “decorando” esta amostra."
  ),

  list(
    id = "montando_escalas", usa_todos_itens = FALSE,
    titulo = "Montando as escalas",
    nivel = "Desafio", aba = "fatores",
    objetivo = "Decidir de qual escala um item com carga cruzada deve fazer parte, e ver como isso muda o alfa e o ômega.",
    passos = c(
      "Com 5 fatores e rotação oblimin, procure na aba 1 um item com “carga cruzada” (ou cuja maior carga está num fator de outro domínio).",
      "Na coluna “Escala (para o alfa)”, mude esse item para outra escala — ou para “Fora das escalas”.",
      "Veja na aba 7 como mudaram o alfa e o ômega das escalas envolvidas. O item aparece marcado como “manual”."
    ),
    condicoes = list(
      cond_aba("fatores"), cond_fatores(5), cond_rotacao("oblimin"),
      cond("Mudar a escala de pelo menos 1 item (aba 1)", function(i) isTRUE(i$n_manuais >= 1))
    ),
    reflexao = "Quando a carga de um item aponta para um fator, mas o conteúdo (e a teoria) aponta para outro, a qual escala ele deve pertencer?",
    comentario = "Não há regra automática. O ponto de partida é a maior carga, mas a decisão final também considera o conteúdo do item e o objetivo do instrumento. Se as duas cargas são parecidas, o item mede um pouco de cada coisa: muitas vezes a melhor escolha é deixá-lo fora das escalas (e relatar isso). Repare que mudar um item de escala altera o alfa e o ômega das duas escalas — mas escolher a composição só para maximizar o alfa é um erro: a escala deve medir o construto, não apenas ter itens parecidos entre si. Atenção: as escolhas manuais são desfeitas quando você muda a solução (número de fatores, rotação, itens…), porque os fatores podem mudar de número e de ordem."
  ),

  list(
    id = "armadilha_invertidos", usa_todos_itens = FALSE,
    titulo = "A armadilha dos itens invertidos",
    nivel = "Desafio", aba = "itens",
    objetivo = "Ver o que acontece quando se excluem todos os itens invertidos — e entender por que isso é uma má ideia, mesmo quando os números melhoram.",
    passos = c(
      "Com 5 fatores e rotação oblimin, guarde a solução (aba 5, “Guardar esta solução para comparar”).",
      "Na aba 1, desmarque os 7 itens invertidos — os marcados com (−): A1, C4, C5, E1, E2, O2 e O5.",
      "Compare: o TLI e o RMSEA melhoraram? E a tabela de cargas (aba 5) e a coluna “Parece medir” (aba 7) — os 5 domínios continuam lá?"
    ),
    condicoes = list(
      cond_fatores(5), cond_rotacao("oblimin"),
      cond("Guardar uma solução para comparar (aba 5)", function(i) isTRUE(i$tem_guardada)),
      cond("Excluir os 7 itens invertidos (aba 1)", function(i) isTRUE(all(INVERTIDOS %in% i$excluidos)))
    ),
    reflexao = "Se alguns números melhoraram, por que não é recomendado tirar os itens invertidos?",
    comentario = "Itens invertidos costumam ter cargas mais fracas por um “efeito de método”: algumas pessoas tendem a concordar com tudo (aquiescência), leem com pressa ou se confundem com a negação. Tirá-los pode melhorar alguns índices — nestes dados, o TLI provavelmente sobe —, mas a estrutura se desfaz: a Extroversão, que perde dois de seus itens, tende a desaparecer, e outro fator surge no lugar. Mesmo quando a estrutura sobrevive, uma escala só com itens “no mesmo sentido” fica vulnerável a quem concorda com tudo, cujos escores ficam inflados. Por isso, a boa prática é manter itens nos dois sentidos e, se preciso, tratar o efeito de método em modelos confirmatórios. Lição geral: um índice melhor não significa um instrumento melhor. Para continuar, recoloque os itens (botão “Recolocar todos os itens”, na aba 1)."
  ),

  list(
    id = "modelo_final", usa_todos_itens = FALSE,
    titulo = "O seu modelo final",
    nivel = "Desafio", aba = "itens",
    objetivo = "Juntar tudo o que você aprendeu: escolher, registrar e defender o seu modelo final.",
    passos = c(
      "Com a amostra completa, escolha o modelo que você defenderia: número de fatores, método de extração, rotação e tipo de correlação.",
      "Na aba 1, decida se algum item deve sair ou mudar de escala — ou se todos ficam (essa também é uma decisão a justificar).",
      "Na aba 5, guarde essa solução (“Guardar esta solução para comparar”).",
      "Volte à aba 1 e confira o “Diário de decisões”: ele resume o caminho que você percorreu.",
      "Escreva a justificativa de cada escolha, usando o que o app mostrou.",
      "Por fim, clique em “Gerar o relatório da sua pesquisa” (logo abaixo): cole a sua justificativa, confira o relatório e salve em PDF."
    ),
    condicoes = list(
      cond_aba("itens"), cond_amostra_completa(),
      cond("Guardar a sua solução final (aba 5)", function(i) isTRUE(i$tem_guardada))
    ),
    reflexao = "Qual modelo você escolheu (número de fatores, extração, rotação, correlação e itens)? Que evidências do app você usaria para justificar cada escolha num relatório?",
    comentario = "Uma boa justificativa junta várias evidências: (1) KMO e Bartlett adequados; (2) número de fatores apoiado pela análise paralela, pelo ganho de ajuste e pela teoria; (3) índices de ajuste aceitáveis e resíduos pequenos; (4) cargas claras, com poucas cargas cruzadas e pelo menos 3 itens por fator; (5) rotação coerente com a correlação entre os fatores; (6) confiabilidade (alfa e ômega) aceitável; (7) decisões sobre itens tomadas com estatística E conteúdo, uma de cada vez — de preferência conferidas em outra amostra. Para estes dados, 5 fatores com rotação oblíqua (e, idealmente, correlação policórica), mantendo todos os itens, é uma escolha bem defensável — mas o importante é que a sua escolha esteja bem argumentada. Relate também o que você testou e descartou: isso faz parte de uma análise transparente. Parabéns por chegar até aqui!"
  )
)

IDS_MISSOES <- vapply(MISSOES, `[[`, "", "id")
stopifnot(!anyDuplicated(IDS_MISSOES))

# Progresso salvo no formato antigo (antes de as missões terem id): o
# número antigo de cada missão -> o id dela. Serve para ninguém perder o
# que já tinha feito quando as missões foram reorganizadas.
MAPA_IDS_V1 <- c(
  "correlacoes_olhar", "itens_invertidos", "kmo_bartlett", "tamanho_amostra",
  "quantos_fatores", "um_fator", "aumentando_fatores", "nomeando_fatores",
  "ponto_corte", "girando_eixos", "ortogonal_obliqua", "confiabilidade",
  "pearson_policorica", "fatores_demais", "item_fraco", "montando_escalas",
  "armadilha_invertidos")

N_MISSOES <- length(MISSOES)

# Confere as condições de uma missão: devolve um vetor TRUE/FALSE.
conferir_missao <- function(m, i) {
  vapply(m$condicoes, function(cd) isTRUE(tryCatch(cd$teste(i), error = function(e) FALSE)),
         logical(1))
}
