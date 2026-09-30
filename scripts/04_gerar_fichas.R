# =============================================================================
# 04_gerar_fichas.R — gera as fichas municipais em PDF (Typst) em arquivos/fichas/
# O modelo fica em _ficha/ficha.qmd (pasta ignorada pelo site) e é renderizado
# numa pasta temporária, fora do projeto, uma vez por município.
# Rodar da raiz do repositório (PowerShell):
#   & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/04_gerar_fichas.R
# =============================================================================
raiz <- normalizePath(".", winslash = "/")
quarto <- Sys.which("quarto")
if (!nzchar(quarto)) quarto <- "C:/Users/lindo/AppData/Local/Programs/Quarto/bin/quarto.exe"
mun <- data.table::fread("dados_site/municipios.csv", encoding = "UTF-8")
mun <- mun[mun$pn13 == TRUE, ]
tmp <- file.path(tempdir(), "dsip_ficha"); dir.create(tmp, showWarnings = FALSE)
file.copy("_ficha/ficha.qmd", file.path(tmp, "ficha.qmd"), overwrite = TRUE)
dir.create("arquivos/fichas", recursive = TRUE, showWarnings = FALSE)
orig <- getwd(); on.exit(setwd(orig))
for (i in seq_len(nrow(mun))) {
  setwd(tmp)
  saida <- paste0(mun$slug[i], ".pdf")
  st <- system2(quarto, c("render", shQuote(file.path(tmp, "ficha.qmd")), "--to", "typst",
                          "-P", paste0("id:", mun$id_municipio[i]), "-P", paste0("raiz:", raiz),
                          "--output", saida), stdout = FALSE, stderr = FALSE)
  setwd(orig)
  ok <- st == 0 && file.exists(file.path(tmp, saida))
  if (ok) file.copy(file.path(tmp, saida), file.path("arquivos/fichas", saida), overwrite = TRUE)
  cat(sprintf("%-32s %s\n", mun$municipio[i], if (ok) "ok" else "FALHOU"))
}
