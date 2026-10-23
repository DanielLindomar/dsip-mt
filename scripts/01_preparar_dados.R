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
OST   <- file.path(DRIVE, "06_Pesquisa/01_Coleta_Dados/Sebrae_Observatorio")   # dados complementares (OST/Sebrae)

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
# Anos parciais que não entram (coleta do OST em 30/09/2026)
# quebras de série nas bases da Fase I (apontadas pelo site do Cleiton em 16/10/2026): focos de calor mudam de escala
# em 2023-2024 (d4_queimadas.csv) e o FPM muda de classificação em 2022 (d6_financas.csv); o site para no último ano comparável
ANO_MAX <- c(aberturas_por_1000_hab = 2025, empresas_cadastur = 2025, leitos_hospedagem = 2025, leitos_por_1000_hab = 2025,
             densidade_focos_por_1000km2 = 2022, dep_fpm_pct = 2021)

ler_ind <- function(i) {
  r <- ind[i, ]
  x <- ler(if (identical(r$origem, "sebrae")) file.path(OST, "02_tratados", r$arquivo)
           else file.path(TRAT, "por_dimensao", r$arquivo))
  if (!r$variavel %in% names(x)) stop("Variável ausente: ", r$variavel, " em ", r$arquivo)
  ano <- if (is.na(r$ano_col) || r$ano_col == "") NA_integer_ else suppressWarnings(as.integer(x[[r$ano_col]]))
  out <- tibble(id_municipio = as.integer(x$id_municipio), indicador = r$id,
                ano = ano, valor = suppressWarnings(as.numeric(x[[r$variavel]])))
  if (r$id %in% names(ANO_FIXO)) out <- filter(out, ano == ANO_FIXO[[r$id]])
  if (r$id %in% names(ANO_MAX)) out <- filter(out, ano <= ANO_MAX[[r$id]])
  distinct(out, id_municipio, indicador, ano, .keep_all = TRUE)
}
serie <- map_dfr(seq_len(nrow(ind)), ler_ind) |> filter(!is.na(valor))
fora <- setdiff(unique(serie$id_municipio), mun$id_municipio)
if (length(fora)) msg("Códigos fora dos 141 municípios do projeto (descartados): %s", paste(fora, collapse = ", "))
serie <- filter(serie, id_municipio %in% mun$id_municipio)
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
  st_transform(5880) |> st_simplify(dTolerance = 700, preserveTopology = TRUE) |>
  st_transform(4326) |>
  mutate(id_municipio = as.integer(code_muni)) |>
  select(id_municipio) |>
  left_join(select(mun, id_municipio, municipio, grupo, slug), by = "id_municipio")
# Coordenadas com 3 casas decimais (~100 m): suficiente para mapas estaduais e reduz o peso das páginas
st_geometry(geo) <- st_sfc(lapply(st_geometry(geo), function(g) {
  st_multipolygon(lapply(unclass(st_cast(g, "MULTIPOLYGON")), function(pol) lapply(pol, function(anel) round(anel, 3))))
}), crs = 4326)
geo <- st_make_valid(geo)
saveRDS(geo, "dados_site/geo_mt.rds")
if (file.exists("arquivos/geo/mt_municipios.geojson")) file.remove("arquivos/geo/mt_municipios.geojson")
st_write(geo, "arquivos/geo/mt_municipios.geojson", quiet = TRUE)
msg("Malha: %d polígonos", nrow(geo))

# ---- 4b. Agregados regionais: PN-13, PN-13 sem Rondonópolis e MT ---------------
# Pedido do coordenador em 16/10/2026 (a partir do site do Cleiton Franco). Rondonópolis tem mais da metade da
# população do recorte e domina os totais; o agregado sem ele mostra o restante da região. Regra por indicador
# (coluna `agregacao` de indicadores.csv): soma dos municípios para totais; para taxas e proporções, média ponderada
# pela base do indicador (população, área, PIB, emprego formal, empresas ativas, receita ou despesa), o que equivale a
# recalcular a razão a partir das somas quando o denominador é a própria base; média simples para o IFDM (notas do D6);
# mediana quando não há base adequada.
# Usa o último valor de cada município até o ano de referência (o mesmo que o site mostra); o peso é o do mesmo ano ou,
# na falta, o do ano mais próximo.
RONDONOPOLIS <- 5107602L
area_km2 <- st_read(file.path(BRUT, "dados_coletados/shapefiles/municipios_mt.gpkg"), quiet = TRUE) |>
  st_transform(5880) |> mutate(id_municipio = as.integer(code_muni), area = as.numeric(st_area(geom)) / 1e6) |>
  st_drop_geometry() |> select(id_municipio, area)
peso_de <- function(var, ids, anos) {
  s <- serie[serie$indicador == var, c("id_municipio", "ano", "valor")]
  mapply(function(i, a) {
    x <- s[s$id_municipio == i, ]
    if (!nrow(x)) return(NA_real_)
    if (is.na(a)) return(x$valor[which.max(x$ano)])
    x$valor[which.min(abs(x$ano - a))]
  }, ids, anos)
}
pesos <- function(regra, ids, anos) switch(regra,
  pop = peso_de("populacao", ids, anos),
  pib = peso_de("pib_real", ids, anos),
  emprego = peso_de("empregos_formais", ids, anos),
  empresas = peso_de("empresas_ativas", ids, anos),
  receita = peso_de("receita_pc_real", ids, anos) * peso_de("populacao", ids, anos),
  despesa = peso_de("despesa_pc_real", ids, anos) * peso_de("populacao", ids, anos),
  area = area_km2$area[match(ids, area_km2$id_municipio)],
  stop("regra de agregação desconhecida: ", regra))
agregar <- function(u, regra) {
  u <- u[is.finite(u$valor), ]
  if (!nrow(u)) return(list(valor = NA_real_, n = 0L))
  v <- switch(regra,
    soma = sum(u$valor),
    mediana = median(u$valor),
    media = mean(u$valor),   # índices entre 0 e 1 (IFDM): média simples, conforme as notas metodológicas do D6
    { w <- pesos(regra, u$id_municipio, u$ano); ok <- is.finite(w) & w > 0
      if (!any(ok)) NA_real_ else sum(u$valor[ok] * w[ok]) / sum(w[ok]) })
  list(valor = unname(as.numeric(v))[1], n = nrow(u))
}
agregados <- map_dfr(seq_len(nrow(ind)), function(i) {
  r <- ind[i, ]; u <- ultimos[ultimos$indicador == r$id, ]
  a13 <- agregar(u[u$id_municipio %in% pn_ids, ], r$agregacao)
  a12 <- agregar(u[u$id_municipio %in% setdiff(pn_ids, RONDONOPOLIS), ], r$agregacao)
  amt <- agregar(u, r$agregacao)
  tibble(indicador = r$id, regra = r$agregacao, ano = ano_ref$ano_ref[match(r$id, ano_ref$indicador)],
         pn13 = a13$valor, n_pn13 = a13$n, pn12 = a12$valor, n_pn12 = a12$n, mt = amt$valor, n_mt = amt$n)
})
saveRDS(agregados, "dados_site/agregados.rds")
chk <- agregados[agregados$indicador == "pib_real_pc", ]
msg("Agregados: %d indicadores; PIB per capita PN-13 = %.0f, sem Rondonópolis = %.0f, MT = %.0f (ano %s)",
    nrow(agregados), chk$pn13, chk$pn12, chk$mt, chk$ano)

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

# ---- 6b. Dados complementares do OST/Sebrae (sem o ISDEL, índice do Sebrae) --
dir.create("arquivos/dados/sebrae", recursive = TRUE, showWarnings = FALSE)
ost <- list.files(file.path(OST, "02_tratados"), "^sebrae_.*[.]csv$", full.names = TRUE)
ost <- ost[basename(ost) != "sebrae_isdel.csv"]
file.copy(ost, "arquivos/dados/sebrae", overwrite = TRUE)
dic_ost <- ler(file.path(OST, "02_tratados", "dicionario_sebrae.csv")) |> filter(arquivo != "sebrae_isdel.csv")
gravar(dic_ost, "arquivos/dados/sebrae/dicionario_sebrae.csv")
file.copy(file.path(OST, "LEIA-ME.md"), "arquivos/dados/sebrae/LEIA-ME.md", overwrite = TRUE)
file.copy(file.path(OST, "consultas.csv"), "dados_site/ost_consultas.csv", overwrite = TRUE)
# Contexto turístico para os perfis (região turística e cluster do Mapa do Turismo; categorização)
tm <- ler(file.path(OST, "02_tratados", "sebrae_turismo_mapa.csv")) |> group_by(id_municipio) |>
  slice_max(ano, n = 1, with_ties = FALSE) |> ungroup() |>
  transmute(id_municipio, ano_mapa = ano, regiao_turistica = trimws(gsub("[[:space:]]+", " ", regiao_turistica)), cluster_mtur)
tc <- ler(file.path(OST, "02_tratados", "sebrae_turismo_categorizacao.csv")) |> select(id_municipio, categoria_turistica)
gravar(left_join(tm, tc, by = "id_municipio"), "dados_site/turismo_info.csv")
msg("OST/Sebrae: %d bases copiadas (ISDEL fora)", length(ost))

# ---- 8. Páginas de município (13 arquivos gerados a partir de _perfil.qmd) --
for (i in which(mun$pn13)) {
  m <- mun[i, ]
  txt <- c("---",
           sprintf('title: "%s"', m$municipio),
           sprintf('description: "Perfil do município de %s (MT) no Pantanal Norte — Projeto DSIP-MT"', m$municipio),
           "toc: false", "page-layout: full", "---", "",
           "```{r}", "#| include: false", sprintf("MUN_ID <- %dL", m$id_municipio), "```", "",
           "{{< include _perfil.qmd >}}", "")
  writeLines(txt, file.path("municipios", paste0(m$slug, ".qmd")), useBytes = FALSE)
}

# ---- 8b. Páginas de indicador (uma por indicador, geradas a partir de _indicador.qmd)
dir.create("indicadores", showWarnings = FALSE)
for (f in list.files("indicadores", "[.]qmd$", full.names = TRUE)) if (!startsWith(basename(f), "_") && basename(f) != "index.qmd") file.remove(f)
for (i in seq_len(nrow(ind))) {
  r <- ind[i, ]
  txt <- c("---", sprintf('title: "%s"', r$rotulo),
           sprintf('description: "%s — %s. Mapa dos 141 municípios de Mato Grosso, ranking do Pantanal Norte e série histórica."', r$dim, r$unidade),
           "toc: false", "page-layout: full", "---", "",
           "```{r}", "#| include: false", sprintf('IND_ID <- "%s"', r$id), "```", "",
           "{{< include _indicador.qmd >}}", "")
  writeLines(txt, file.path("indicadores", paste0(r$id, ".qmd")))
}
msg("Páginas de indicador geradas: %d", nrow(ind))
msg("Páginas de município geradas: %d", sum(mun$pn13))

# ---- 9. CSV de indicadores por município (sem IFDM, que depende da FIRJAN) --
dir.create("arquivos/municipios", recursive = TRUE, showWarnings = FALSE)
for (i in which(mun$pn13)) {
  m <- mun[i, ]
  x <- serie |>
    filter(id_municipio == m$id_municipio, indicador %in% ind$id[ind$download == 1]) |>
    left_join(select(ind, indicador = id, dimensao = dim, rotulo, unidade), by = "indicador") |>
    transmute(id_municipio, municipio = m$municipio, dimensao, indicador, rotulo, unidade, ano, valor) |>
    arrange(dimensao, indicador, ano)
  gravar(x, file.path("arquivos/municipios", paste0(m$slug, "_indicadores.csv")))
}
msg("CSV por município: %d", sum(mun$pn13))
msg("Concluído.")
