# Laboratório de Análise Fatorial Exploratória

App interativo, em português, para ensinar Análise Fatorial Exploratória (AFE) a iniciantes. Ele usa os 25 itens do Big Five do conjunto `psych::bfi` e traz missões guiadas que o próprio app confere.

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
