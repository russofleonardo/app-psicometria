# Regras do projeto

App Shiny em R para ensinar Análise Fatorial Exploratória (AFE) a iniciantes. O foco é ensinar AFE, não R. O usuário não é programador: explique tudo de forma simples, em português.

## Git e GitHub
- O projeto fica num repositório no GitHub do usuário. Se ainda não existir, ajude a criar e a conectar, explicando cada passo.
- Ao terminar cada etapa que esteja funcionando, faça um commit com mensagem clara em português e dê push para o GitHub.
- Antes de qualquer mudança grande, confirme que o que já funciona está salvo no GitHub.
- Nunca suba senhas, chaves ou arquivos `.env`. Garanta que estejam no `.gitignore`.
- No fim de cada sessão, diga ao usuário o que foi salvo e se ficou algo pendente.

## Publicação (GitHub Pages)
- O app está publicado em https://russofleonardo.github.io/app-psicometria/, a partir da pasta `docs/` (GitHub Pages, branch `main`, pasta `/docs`).
- **Sempre que alterar algo em `app/`**, antes do commit: teste no R, gere de novo a versão do navegador com `shinylive::export("app", "docs")` e inclua a pasta `docs/` no mesmo commit. Sem isso, o link público continua mostrando a versão antiga.
- Depois do push, o GitHub leva 1 a 2 minutos para atualizar o link. Quem já abriu o app pode precisar recarregar com Ctrl+F5.
- O progresso das missões fica salvo no navegador pelo número da missão: acrescente missões novas no fim da lista; não reordene nem insira no meio.
- A policórica só está disponível nas amostras pré-calculadas em `app/policoricas_prontas.rds`. Se mudar os dados, o tamanho inicial da amostra, a semente inicial ou `calcular_correlacao()`, rode de novo `ferramentas/gerar_policoricas.R`.

## Notas técnicas
- O app é publicado com Shinylive (roda no navegador via webR, sem servidor). Use apenas pacotes compatíveis com webR: `shiny`, `bslib`, `psych`, `GPArotation`. Gráficos em R base.
- Toda a interface fica em português.
- O código do app fica em `app/` (`app.R`, `dados.R`, `analises.R`, `graficos.R`, `missoes.R`). A versão exportada para o navegador fica em `docs/`.
