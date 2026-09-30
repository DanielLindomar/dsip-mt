# =============================================================================
# Funções compartilhadas pelas páginas do Observatório DSIP-MT.
# As páginas leem somente dados_site/ (gerado por scripts/01_preparar_dados.R).
# readr não é usado: a política de Controle de Aplicativos do Windows bloqueia a
# DLL do tzdb; a leitura de CSV usa data.table::fread.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(htmltools); library(reactable); library(plotly); library(leaflet); library(sf)
})

# Raiz do projeto: o Quarto define QUARTO_PROJECT_DIR durante a renderização
RAIZ <- Sys.getenv("QUARTO_PROJECT_DIR", unset = ".")
ds   <- function(...) file.path(RAIZ, "dados_site", ...)
ler  <- function(f, ...) tibble::as_tibble(data.table::fread(f, encoding = "UTF-8", ...))

IND  <- ler(ds("indicadores.csv"), na.strings = "")
MUN  <- ler(ds("municipios.csv"))
SER  <- readRDS(ds("serie.rds"))
ULT  <- readRDS(ds("ultimos.rds"))
GEO  <- readRDS(ds("geo_mt.rds"))
PN   <- MUN$id_municipio[MUN$pn13]

DIMS <- c(D1 = "Perfil econômico-produtivo", D2 = "Áreas de especial interesse",
          D3 = "Perfil sociodemográfico", D4 = "Perfil ambiental",
          D5 = "Infraestrutura", D6 = "Gestão pública")

COR <- c(mun = "#1E2761", pn = "#2E86AB", mt = "#9CA3AF")

# ---- Formatação pt-BR -------------------------------------------------------
num <- function(x, d = 0) ifelse(is.na(x), "—",
  formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ","))

escala <- function(x, prefixo) {
  a <- abs(x)
  ifelse(is.na(x), "—",
  ifelse(a >= 1e9, paste0(prefixo, num(x / 1e9, 1), " bi"),
  ifelse(a >= 1e6, paste0(prefixo, num(x / 1e6, 1), " mi"),
  ifelse(a >= 1e4, paste0(prefixo, num(x / 1e3, 1), " mil"),
         paste0(prefixo, num(x, 0))))))
}

fmt_valor <- function(x, fmt) {
  switch(fmt,
    int     = num(x, 0),
    dec1    = num(x, 1), dec2 = num(x, 2), dec3 = num(x, 3),
    pct1    = ifelse(is.na(x), "—", paste0(num(x, 1), "%")),
    pct2    = ifelse(is.na(x), "—", paste0(num(x, 2), "%")),
    brl     = escala(x, "R$ "),
    brl_mil = escala(x * 1000, "R$ "),
    usd     = escala(x, "US$ "),
    num(x, 2))
}

info_ind <- function(id) IND[IND$id == id, ]

# ---- Consultas -------------------------------------------------------------
ultimo <- function(id, mun_id) {
  u <- ULT$ultimos[ULT$ultimos$indicador == id & ULT$ultimos$id_municipio == mun_id, ]
  if (nrow(u) == 0) list(valor = NA_real_, ano = NA_integer_) else list(valor = u$valor, ano = u$ano)
}

resumo_ind <- function(id, mun_id) {
  u  <- ULT$ultimos[ULT$ultimos$indicador == id, ]
  up <- u[u$id_municipio %in% PN, ]
  me <- ultimo(id, mun_id)
  pos <- if (is.na(me$valor)) NA_integer_ else sum(up$valor > me$valor, na.rm = TRUE) + 1L
  list(valor = me$valor, ano = me$ano, pos = pos, n_pn = sum(!is.na(up$valor)),
       med_pn = median(up$valor, na.rm = TRUE), med_mt = median(u$valor, na.rm = TRUE),
       soma_pn = sum(up$valor, na.rm = TRUE))
}

# ---- Cartões de indicador ---------------------------------------------------
kpi <- function(id, mun_id) {
  r <- info_ind(id); s <- resumo_ind(id, mun_id)
  comp <- if (isTRUE(r$soma == 1) && s$soma_pn > 0 && !is.na(s$valor)) {
    sprintf("%s%% do total do Pantanal Norte", num(100 * s$valor / s$soma_pn, 1))
  } else {
    sprintf("Mediana PN-13: %s · MT: %s", fmt_valor(s$med_pn, r$fmt), fmt_valor(s$med_mt, r$fmt))
  }
  div(class = "kpi",
      div(class = "kpi-rotulo", r$rotulo),
      div(class = "kpi-valor", fmt_valor(s$valor, r$fmt)),
      div(class = "kpi-meta", paste0(r$unidade, if (!is.na(s$ano)) paste0(" · ", s$ano) else "")),
      div(class = "kpi-comp", comp))
}

grade_kpis <- function(mun_id) {
  ids <- IND$id[IND$kpi == 1]
  div(class = "kpi-grade", lapply(ids, kpi, mun_id = mun_id))
}

# ---- Tabela por dimensão ----------------------------------------------------
tabela_dim <- function(dim, mun_id) {
  ids <- IND$id[IND$dim == dim]
  linhas <- lapply(ids, function(id) {
    r <- info_ind(id); s <- resumo_ind(id, mun_id)
    tibble(Indicador = r$rotulo, Unidade = r$unidade,
           Valor = fmt_valor(s$valor, r$fmt), Ano = ifelse(is.na(s$ano), "—", as.character(s$ano)),
           `Posição no PN-13` = ifelse(is.na(s$pos), "—", sprintf("%dº de %d", s$pos, s$n_pn)),
           `Mediana PN-13` = fmt_valor(s$med_pn, r$fmt), `Mediana MT` = fmt_valor(s$med_mt, r$fmt),
           Nota = ifelse(is.na(r$nota), "", r$nota))
  })
  d <- bind_rows(linhas)
  reactable(d, compact = TRUE, striped = TRUE, highlight = TRUE, sortable = TRUE,
            defaultPageSize = 30, wrap = TRUE,
            columns = list(
              Indicador = colDef(minWidth = 190, style = list(fontWeight = 600)),
              Unidade = colDef(minWidth = 120, style = list(color = "#6b7280", fontSize = "0.85em")),
              Valor = colDef(align = "right", minWidth = 95),
              Ano = colDef(align = "center", minWidth = 55),
              `Posição no PN-13` = colDef(align = "center", minWidth = 95,
                                           header = "Posição no PN-13 (1º = maior)"),
              `Mediana PN-13` = colDef(align = "right", minWidth = 95),
              `Mediana MT` = colDef(align = "right", minWidth = 95),
              Nota = colDef(minWidth = 200, style = list(color = "#6b7280", fontSize = "0.82em"))))
}

# ---- Gráficos de série -----------------------------------------------------
grafico_serie <- function(id, mun_id, nome_mun) {
  r <- info_ind(id)
  s <- SER[SER$indicador == id & !is.na(SER$ano), ]
  if (nrow(s) == 0) return(NULL)
  meds <- s |> group_by(ano) |>
    summarise(pn = median(valor[id_municipio %in% PN], na.rm = TRUE),
              mt = median(valor, na.rm = TRUE), .groups = "drop")
  me <- s[s$id_municipio == mun_id, c("ano", "valor")]
  if (nrow(me) < 2) return(NULL)
  meds <- meds[meds$ano >= min(me$ano) & meds$ano <= max(me$ano), ]
  unidade <- r$unidade
  if (r$fmt == "brl_mil") {            # eixo em reais, não em milhares de reais
    me$valor <- me$valor * 1000; meds$pn <- meds$pn * 1000; meds$mt <- meds$mt * 1000
    unidade <- sub("R\\$ mil", "R$", unidade)
  }
  mx <- max(abs(c(me$valor, meds$pn, meds$mt)), na.rm = TRUE)
  fator <- if (mx >= 1e9) 1e9 else if (mx >= 1e6) 1e6 else 1
  if (fator > 1) {
    me$valor <- me$valor / fator; meds$pn <- meds$pn / fator; meds$mt <- meds$mt / fator
    unidade <- paste0(unidade, if (fator == 1e9) " — em bilhões" else " — em milhões")
  }
  r$unidade <- unidade
  plot_ly(height = 280) |>
    add_lines(data = me, x = ~ano, y = ~valor, name = nome_mun,
              line = list(color = COR[["mun"]], width = 3)) |>
    add_lines(data = meds, x = ~ano, y = ~pn, name = "Mediana PN-13",
              line = list(color = COR[["pn"]], width = 1.8, dash = "dash")) |>
    add_lines(data = meds, x = ~ano, y = ~mt, name = "Mediana MT",
              line = list(color = COR[["mt"]], width = 1.8, dash = "dot")) |>
    layout(title = list(text = paste0("<b>", r$rotulo, "</b><br><sup>", r$unidade, "</sup>"),
                        font = list(size = 13), x = 0.02),
           separators = ",.", hovermode = "x unified",
           xaxis = list(title = "", tickformat = "d"),
           yaxis = list(title = "", exponentformat = "none", separatethousands = TRUE),
           legend = list(orientation = "h", y = -0.18, font = list(size = 10)),
           margin = list(t = 55, l = 50, r = 10, b = 30)) |>
    config(displaylogo = FALSE, locale = "pt-BR",
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
           toImageButtonOptions = list(format = "png", filename = paste0(id, "_", mun_id)))
}

graficos_dim <- function(dim, mun_id, nome_mun) {
  ids <- IND$id[IND$dim == dim & IND$serie == 1]
  gs <- Filter(Negate(is.null), lapply(ids, grafico_serie, mun_id = mun_id, nome_mun = nome_mun))
  if (length(gs) == 0) return(invisible(NULL))
  div(class = "grade-graficos", lapply(gs, div))
}

# ---- Mapa -----------------------------------------------------------------
mapa_municipio <- function(mun_id = NULL, altura = 360) {
  g <- GEO |> mutate(
    cor = case_when(!is.null(mun_id) & id_municipio %in% mun_id ~ COR[["mun"]],
                    grupo == "pantanal_norte" ~ "#8FB8DE",
                    grupo == "polo_metropolitano" ~ "#E8A0A0",
                    TRUE ~ "#E5E7EB"),
    rot = ifelse(grupo == "pantanal_norte",
                 sprintf("<a href='%s.html'>%s</a>", slug, municipio), municipio))
  pn <- g[g$grupo == "pantanal_norte", ]
  bb <- st_bbox(pn)
  leaflet(g, height = altura, options = leafletOptions(zoomControl = TRUE)) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addPolygons(fillColor = ~cor, fillOpacity = 0.75, color = "#ffffff", weight = 0.8,
                label = ~municipio, popup = ~rot) |>
    fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
}

fonte_rodape <- function() {
  p(class = "nota-fonte",
    "Fonte: bases tratadas da Fase I do Projeto DSIP-MT (IBGE, RAIS/MTE, CAGED, BCB/SICOR, PPM/PAM, ",
    "ComexStat, INEP, DATASUS, PRODES/INPE, MapBiomas, SNIS, ANEEL, Siconfi/STN, FIRJAN e outras). ",
    "Medianas calculadas no ano de referência de cada indicador. Detalhes em ",
    a(href = "../dados/index.html", "Dados"), " e nas notas metodológicas da ",
    a(href = "../biblioteca/index.html", "Biblioteca"), ".")
}
