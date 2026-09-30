# =============================================================================
# 01_preparar_dados.R — Observatório DSIP-MT
# Lê as bases congeladas no Google Drive (fonte oficial) e grava no repositório
# apenas o que é publicável e o que as páginas usam na renderização.
#
# Rodar a partir da raiz do repositório (PowerShell):
#   & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/01_preparar_dados.R
# (Não rodar pelo Bash do Git: o R falha com o caminho do Google Drive.)
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr); library(purrr); library(sf); library(data.table)
})

DRIVE <- "C:/Users/lindo/Meu Drive (lindomar.pegorini@unemat.br)/_RESEARCH/(26-27) PROJETO SUDECO"
TRAT  <- file.path(DRIVE, "07_Entrega_Fase_I/02_Bases_Tratadas")
REL   <- file.path(DRIVE, "07_Entrega_Fase_I/03_Relatorios")
BRUT  <- file.path(DRIVE, "07_Entrega_Fase_I/01_Bases_Brutas")
ECON  <- file.path(DRIVE, "06_Pesquisa/02_Revisao_FOFA/04_Entrega_Area_Economica")
BANCO <- file.path(DRIVE, "06_Pesquisa/01_Coleta_Dados/Banco_Ideias_TransfereGov")

stopifnot(dir.exists(TRAT))
dir.create("dados_site", showWarnings = FALSE)
for (d in c("arquivos/relatorios", "arquivos/dados", "arquivos/dados/dicionarios", "arquivos/geo"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) cat(sprintf(...), "\n")
# readr não é usado: a política de Controle de Aplicativos do Windows bloqueia a DLL do tzdb.
ler <- function(f, ...) tibble::as_tibble(data.table::fread(f, encoding = "UTF-8", ...))
gravar <- function(x, f) data.table::fwrite(x, f, na = "")

# ---- 1. Municípios ----------------------------------------------------------
slugify <- function(x) {
  x <- iconv(x, to = "ASCII//TRANSLIT")
  x <- tolower(gsub("[^A-Za-z0-9]+", "-", x))
  gsub("(^-|-$)", "", x)
}
mun <- ler(file.path(TRAT, "painel_mestre/municipios_ref.csv")) |>
  mutate(id_municipio = as.integer(id_municipio), slug = slugify(municipio),
         pn13 = grupo == "pantanal_norte")
stopifnot(nrow(mun) == 141, sum(mun$pn13) == 13)
gravar(mun, "dados_site/municipios.csv")
msg("Municípios: %d (PN-13: %d)", nrow(mun), sum(mun$pn13))

# ---- 2. Indicadores em formato longo ----------------------------------------
ind <- ler("dados_site/indicadores.csv", na.strings = "")

# Valores que são retratos de um único ano, repetidos nas bases (ver NOTAS_D3)
ANO_FIXO <- c(taxa_urbanizacao = 2022, cagr_pop_2010_2025 = 2025)

ler_ind <- function(i) {
  r <- ind[i, ]
  x <- ler(file.path(TRAT, "por_dimensao", r$arquivo))
  if (!r$variavel %in% names(x)) stop("Variável ausente: ", r$variavel, " em ", r$arquivo)
  ano <- if (is.na(r$ano_col) || r$ano_col == "") NA_integer_ else suppressWarnings(as.integer(x[[r$ano_col]]))
  out <- tibble(id_municipio = as.integer(x$id_municipio), indicador = r$id,
                ano = ano, valor = suppressWarnings(as.numeric(x[[r$variavel]])))
  if (r$id %in% names(ANO_FIXO)) out <- filter(out, ano == ANO_FIXO[[r$id]])
  distinct(out, id_municipio, indicador, ano, .keep_all = TRUE)
}
serie <- map_dfr(seq_len(nrow(ind)), ler_ind) |> filter(!is.na(valor))
saveRDS(serie, "dados_site/serie.rds")
msg("Série longa: %d linhas, %d indicadores", nrow(serie), n_distinct(serie$indicador))

# ---- 3. Ano de referência e último valor ------------------------------------
# Ano de referência = ano mais recente em que pelo menos 80% do máximo de
# municípios do PN-13 com dado naquele indicador têm valor.
pn_ids <- mun$id_municipio[mun$pn13]
ano_ref <- serie |>
  filter(id_municipio %in% pn_ids) |>
  count(indicador, ano) |>
  group_by(indicador) |>
  filter(n >= 0.8 * max(n)) |>
  summarise(ano_ref = if (all(is.na(ano))) NA_integer_ else max(ano, na.rm = TRUE), .groups = "drop")

ultimos <- serie |>
  left_join(ano_ref, by = "indicador") |>
  filter(is.na(ano_ref) | is.na(ano) | ano <= ano_ref) |>
  group_by(indicador, id_municipio) |>
  slice_max(order_by = coalesce(ano, 0L), n = 1, with_ties = FALSE) |>
  ungroup()
saveRDS(list(ano_ref = ano_ref, ultimos = ultimos), "dados_site/ultimos.rds")
msg("Últimos valores: %d linhas", nrow(ultimos))

# ---- 4. Malha municipal simplificada -----------------------------------------
geo <- st_read(file.path(BRUT, "dados_coletados/shapefiles/municipios_mt.gpkg"), quiet = TRUE) |>
  st_transform(5880) |> st_simplify(dTolerance = 400, preserveTopology = TRUE) |>
  st_transform(4326) |>
  mutate(id_municipio = as.integer(code_muni)) |>
  select(id_municipio) |>
  left_join(select(mun, id_municipio, municipio, grupo, slug), by = "id_municipio")
saveRDS(geo, "dados_site/geo_mt.rds")
if (file.exists("arquivos/geo/mt_municipios.geojson")) file.remove("arquivos/geo/mt_municipios.geojson")
st_write(geo, "arquivos/geo/mt_municipios.geojson", quiet = TRUE)
msg("Malha: %d polígonos", nrow(geo))

# ---- 5. Downloads: relatórios em PDF -----------------------------------------
pdfs <- c(list.files(file.path(REL, "PDF"), "\\.pdf$", full.names = TRUE),
          list.files(file.path(REL, "Notas_Metodologicas"), "\\.pdf$", full.names = TRUE),
          file.path(REL, "SINTESE_EXECUTIVA_Parciais_I.pdf"),
          list.files(file.path(ECON, "01_Relatorios/PDF"), "\\.pdf$", full.names = TRUE))
ok <- file.copy(pdfs, "arquivos/relatorios", overwrite = TRUE)
msg("PDFs copiados: %d de %d", sum(ok), length(pdfs))

# ---- 6. Downloads: bases tratadas (sem IFDM, cuja redistribuição depende da FIRJAN)
csvs <- list.files(file.path(TRAT, "por_dimensao"), "\\.csv$", recursive = TRUE, full.names = TRUE)
csvs <- c(csvs, list.files(file.path(TRAT, "painel_mestre"), "\\.csv$", full.names = TRUE))
csvs <- csvs[!grepl("_backup", csvs) & basename(csvs) != "d6_ifdm.csv"]
for (f in csvs) {
  x <- ler(f, colClasses = "character")
  x <- select(x, -any_of(grep("ifdm|rank_uf|rank_br", names(x), value = TRUE)))
  gravar(x, file.path("arquivos/dados", basename(f)))
}
dics <- list.files(file.path(TRAT, "dicionarios_variaveis"), "\\.csv$", full.names = TRUE)
for (f in dics) {
  d <- ler(f) |>
    filter(!(arquivo == "d6_ifdm.csv" | grepl("ifdm|rank_uf|rank_br", variavel)))
  gravar(d, file.path("arquivos/dados/dicionarios", basename(f)))
}
file.copy(file.path(TRAT, "CATALOGO_BASES_TRATADAS.csv"), "arquivos/dados", overwrite = TRUE)
file.copy(file.path(BRUT, "FONTES_E_LICENCAS.csv"), "dados_site/fontes_licencas.csv", overwrite = TRUE)
msg("Bases copiadas: %d CSV + %d dicionários", length(csvs), length(dics))

# ---- 7. Banco de Ideias: projetos com proponente em município do PN-13 ------
norm <- function(x) tolower(iconv(x, to = "ASCII//TRANSLIT")) |> str_replace_all("[^a-z]", "")
bi <- ler(file.path(BANCO, "banco_ideias_pantanal_consolidado.csv"))
alias <- c(santoantoniodeleverger = "santoantoniodoleverger")
bi <- bi |>
  mutate(chave = norm(municipio), chave = coalesce(alias[chave], chave)) |>
  inner_join(mun |> filter(pn13) |> transmute(id_municipio, chave = norm(municipio)), by = "chave") |>
  filter(uf == "MT") |>
  select(id_municipio, ano, fonte, instrumento, proponente_executor, ideia_sintese, tema, eixo,
         situacao, celebrado, valor_global)
gravar(bi, "dados_site/banco_ideias_pn13.csv")
msg("Banco de Ideias (PN-13): %d projetos", nrow(bi))

# ---- 8. Páginas de município (13 arquivos gerados a partir de _perfil.qmd) --
for (i in which(mun$pn13)) {
  m <- mun[i, ]
  txt <- c("---",
           sprintf('title: "%s"', m$municipio),
           sprintf('description: "Perfil do município de %s (MT) no Pantanal Norte — Projeto DSIP-MT"', m$municipio),
           "---", "",
           "```{r}", "#| include: false", sprintf("MUN_ID <- %dL", m$id_municipio), "```", "",
           "{{< include _perfil.qmd >}}", "")
  writeLines(txt, file.path("municipios", paste0(m$slug, ".qmd")), useBytes = FALSE)
}
msg("Páginas de município geradas: %d", sum(mun$pn13))

# ---- 9. CSV de indicadores por município (sem IFDM, que depende da FIRJAN) --
dir.create("arquivos/municipios", recursive = TRUE, showWarnings = FALSE)
for (i in which(mun$pn13)) {
  m <- mun[i, ]
  x <- serie |>
    filter(id_municipio == m$id_municipio, !grepl("^ifdm", indicador)) |>
    left_join(select(ind, indicador = id, dimensao = dim, rotulo, unidade), by = "indicador") |>
    transmute(id_municipio, municipio = m$municipio, dimensao, indicador, rotulo, unidade, ano, valor) |>
    arrange(dimensao, indicador, ano)
  gravar(x, file.path("arquivos/municipios", paste0(m$slug, "_indicadores.csv")))
}
msg("CSV por município: %d", sum(mun$pn13))
msg("Concluído.")
