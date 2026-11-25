# Laboratório de Psicometria – Análise Fatorial Exploratória
# © 2026 Leonardo Ferreira Russo — licença CC BY-NC-ND 4.0 (veja o arquivo LICENSE)
# =====================================================================
# exportar.R — gera a versão para o navegador (pasta docs/) e ajusta a
# página de abertura:
#   * título da aba do navegador e idioma (português);
#   * uma tela de "Carregando…" explicando que a primeira abertura demora
#     (o navegador baixa o R inteiro), que some sozinha quando o app aparece.
#
# Como usar (no R, a partir da pasta do projeto):
#   source("ferramentas/exportar.R")
# =====================================================================

shinylive::export("app", "docs")

pagina <- "docs/index.html"
html <- paste(readLines(pagina, encoding = "UTF-8", warn = FALSE), collapse = "\n")

html <- sub('<html lang="en">', '<html lang="pt-BR">', html, fixed = TRUE)
html <- sub("<title>Shiny App</title>",
            "<title>Laboratório de Psicometria – Análise Fatorial Exploratória</title>", html, fixed = TRUE)

tela_carregando <- '
    <div id="carregando" role="status" aria-live="polite"
         style="position:fixed;inset:0;z-index:9999;background:#f8f9fa;display:flex;align-items:center;justify-content:center;font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif;">
      <div style="max-width:540px;text-align:center;padding:2rem;">
        <div class="girando"></div>
        <h1 style="font-size:1.35rem;color:#2c3e50;margin:0 0 .6rem;">Laboratório de Psicometria – Análise Fatorial Exploratória</h1>
        <p style="font-size:1.05rem;margin:0 0 .6rem;color:#1f1f1f;">Carregando…</p>
        <p style="font-size:.95rem;color:#52514e;margin:0;">Na primeira vez, o navegador baixa o R inteiro (cerca de 70 MB): pode levar <strong>cerca de 1 minuto</strong>. Nas próximas visitas, abre bem mais rápido.</p>
      </div>
    </div>
    <style>
      @keyframes girar { to { transform: rotate(360deg); } }
      .girando { width: 42px; height: 42px; margin: 0 auto 1.1rem; border: 4px solid #d6e4f5;
                 border-top-color: #2a78d6; border-radius: 50%; animation: girar 1s linear infinite; }
      @media (prefers-reduced-motion: reduce) { .girando { animation: none; } }
    </style>
    <script>
      // Some quando o app aparece (ou, por segurança, depois de 4 minutos)
      (function () {
        var inicio = Date.now();
        var espera = setInterval(function () {
          var pronto = false;
          try {
            var quadro = document.querySelector("#root iframe");
            var doc = quadro && quadro.contentDocument;
            pronto = !!(doc && doc.getElementById("botao_recolher_missoes"));
          } catch (e) {}
          if (pronto || Date.now() - inicio > 240000) {
            clearInterval(espera);
            var tela = document.getElementById("carregando");
            if (tela) { tela.style.transition = "opacity .4s"; tela.style.opacity = "0";
                        setTimeout(function () { tela.remove(); }, 450); }
          }
        }, 500);
      })();
    </script>'

if (!grepl('id="carregando"', html, fixed = TRUE)) {
  html <- sub("<body>", paste0("<body>", tela_carregando), html, fixed = TRUE)
}

writeLines(enc2utf8(html), pagina, useBytes = TRUE)
cat("Versão para o navegador gerada em docs/ (com tela de carregamento).\n")
