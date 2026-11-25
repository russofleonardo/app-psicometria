# Laboratório de Psicometria – Análise Fatorial Exploratória

App interativo, em português, para ensinar Análise Fatorial Exploratória (AFE) a iniciantes. Ele usa os 25 itens do Big Five do conjunto `psych::bfi` e traz missões guiadas que o próprio app confere.

A aba “0. Comece aqui” apresenta a escala, a atividade, uma explicação conceitual da AFE e o caminho pelas abas.

Na aba “1. Itens”, a pessoa pode tirar itens da análise e decidir de qual escala cada item faz parte, vendo o efeito em todas as abas, com diagnósticos de cada item (sinais para investigar) e um diário de decisões.

São 17 missões, do básico ao desafio, que passam por todas as etapas da AFE: correlações, KMO e Bartlett, número de fatores, cargas e índices de ajuste, rotação, correlação entre fatores e alfa. Cada missão tem objetivo, passos, uma conferência automática dos controles (o botão “Concluí” só é liberado quando tudo está certo) e uma pergunta de reflexão com comentário. O progresso fica salvo no navegador.

## Como abrir no computador
No R, com os pacotes `shiny`, `bslib`, `psych` e `GPArotation` instalados:

```r
shiny::runApp("app")
```

## Versão para o navegador
A pasta `docs/` contém a versão Shinylive, que roda direto no navegador, sem servidor. Ela é gerada com:

```r
shinylive::export("app", "docs")
```
