# =============================================================================
# Funções compartilhadas pelas páginas do Observatório DSIP-MT.
# As páginas leem somente dados_site/ (gerado por scripts/01_preparar_dados.R).
# readr não é usado: a política de Controle de Aplicativos do Windows bloqueia a
# DLL do tzdb; a leitura de CSV usa data.table::fread.
# Os gráficos são SVG gerados aqui (sem biblioteca JavaScript), para páginas leves.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(htmltools); library(reactable); library(leaflet); library(sf)
})

RAIZ <- Sys.getenv("QUARTO_PROJECT_DIR", unset = ".")
source(file.path(RAIZ, "R", "idioma.R"))
ds   <- function(...) file.path(RAIZ, "dados_site", ...)
ler  <- function(f, ...) tibble::as_tibble(data.table::fread(f, encoding = "UTF-8", ...))

IND  <- ler(ds("indicadores.csv"), na.strings = "")
MUN  <- ler(ds("municipios.csv"))
SER  <- readRDS(ds("serie.rds"))
ULT  <- readRDS(ds("ultimos.rds"))
GEO  <- readRDS(ds("geo_mt.rds"))
PN   <- MUN$id_municipio[MUN$pn13]
M13  <- MUN[MUN$pn13, ] |> arrange(municipio)

DIMS <- c(D1 = "Perfil econômico-produtivo", D2 = "Áreas de especial interesse",
          D3 = "Perfil sociodemográfico", D4 = "Perfil ambiental",
          D5 = "Infraestrutura", D6 = "Gestão pública")

COR <- c(mun = "#1E2761", pn = "#2E86AB", mt = "#9CA3AF", bom = "#1b7f4b", ruim = "#b42318")

# ---- Formatação pt-BR -------------------------------------------------------
num <- function(x, d = 0) ifelse(is.na(x), "—",
  formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ","))

escala <- function(x, prefixo = "") {
  a <- abs(x)
  ifelse(is.na(x), "—",
  ifelse(a >= 1e9, paste0(prefixo, num(x / 1e9, 1), " bi"),
  ifelse(a >= 1e6, paste0(prefixo, num(x / 1e6, 1), " mi"),
  ifelse(a >= 1e4, paste0(prefixo, num(x / 1e3, 1), " mil"),
         paste0(prefixo, num(x, 0))))))
}

fmt_valor <- function(x, fmt) {
  switch(fmt,
    int = num(x, 0), dec1 = num(x, 1), dec2 = num(x, 2), dec3 = num(x, 3),
    pct1 = ifelse(is.na(x), "—", paste0(num(x, 1), "%")),
    pct2 = ifelse(is.na(x), "—", paste0(num(x, 2), "%")),
    brl = escala(x, "R$ "), brl_mil = escala(x * 1000, "R$ "), usd = escala(x, "US$ "),
    num(x, 2))
}
# Valor em escala "natural" (reais, não milhares de reais) e formato correspondente
escala_natural <- function(r) if (r$fmt == "brl_mil") list(f = 1000, fmt = "brl") else list(f = 1, fmt = r$fmt)

fmt_eixo <- function(v) {
  a <- max(abs(v), na.rm = TRUE)
  if (a >= 1e9) return(paste0(num(v / 1e9, 1), " bi"))
  if (a >= 1e6) return(paste0(num(v / 1e6, 1), " mi"))
  if (a >= 1e4) return(paste0(num(v / 1e3, 0), " mil"))
  d <- if (all(abs(v - round(v)) < 1e-9)) 0 else if (a >= 10) 1 else 2
  num(v, d)
}

esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE); gsub("\"", "&quot;", x, fixed = TRUE) }

info_ind <- function(id) IND[IND$id == id, ]
selo_ost <- function(r) if (identical(r$origem, "sebrae")) ' <span class="selo-ost" data-tip="Dado complementar obtido via Observatório Setorial Territorial do Sebrae (OST); fonte original na nota">OST</span>' else ""
url_ind  <- function(id, prefixo = "../indicadores/") paste0(prefixo, id, ".html")

# ---- Consultas -------------------------------------------------------------
ultimo <- function(id, mun_id) {
  u <- ULT$ultimos[ULT$ultimos$indicador == id & ULT$ultimos$id_municipio == mun_id, ]
  if (nrow(u) == 0) list(valor = NA_real_, ano = NA_integer_) else list(valor = u$valor, ano = u$ano)
}
ano_ref <- function(id) ULT$ano_ref$ano_ref[ULT$ano_ref$indicador == id]

resumo_ind <- function(id, mun_id) {
  u  <- ULT$ultimos[ULT$ultimos$indicador == id, ]
  up <- u[u$id_municipio %in% PN, ]
  me <- ultimo(id, mun_id)
  pos <- if (is.na(me$valor)) NA_integer_ else sum(up$valor > me$valor, na.rm = TRUE) + 1L
  list(valor = me$valor, ano = me$ano, pos = pos, n_pn = sum(!is.na(up$valor)),
       med_pn = median(up$valor, na.rm = TRUE), med_mt = median(u$valor, na.rm = TRUE),
       soma_pn = sum(up$valor, na.rm = TRUE), vals_pn = setNames(up$valor, up$id_municipio))
}

# Situação em relação à mediana do PN-13, considerando o sentido do indicador
situacao <- function(valor, mediana, sentido) {
  if (is.na(valor) || is.na(mediana)) return(list(txt = "—", cls = "sit-neutra"))
  if (isTRUE(all.equal(valor, mediana))) return(list(txt = "= mediana", cls = "sit-neutra"))
  acima <- valor > mediana
  txt <- if (acima) "▲ acima" else "▼ abaixo"
  cls <- if (is.na(sentido) || sentido == 0) "sit-neutra"
         else if ((acima && sentido > 0) || (!acima && sentido < 0)) "sit-boa" else "sit-ruim"
  list(txt = txt, cls = cls)
}

# ---- SVG: minissérie, posição entre os 13 e gráfico de linhas ---------------
svg_sparkline <- function(x, y, w = 110, h = 28, cor = COR[["mun"]]) {
  ok <- is.finite(y); x <- x[ok]; y <- y[ok]
  if (length(y) < 2) return("")
  o <- order(x); x <- x[o]; y <- y[o]
  xr <- range(x); yr <- range(y); if (diff(yr) == 0) yr <- yr + c(-1, 1)
  px <- 3 + (x - xr[1]) / diff(xr) * (w - 6); py <- h - 3 - (y - yr[1]) / diff(yr) * (h - 6)
  sprintf('<svg class="spark" viewBox="0 0 %d %d" width="%d" height="%d" role="img" aria-label="Série %d–%d"><title>Série %d–%d</title><polyline fill="none" stroke="%s" stroke-width="1.6" points="%s"/><circle cx="%.1f" cy="%.1f" r="2.4" fill="%s"/></svg>',
          w, h, w, h, xr[1], xr[2], xr[1], xr[2], cor,
          paste(sprintf("%.1f,%.1f", px, py), collapse = " "), tail(px, 1), tail(py, 1), cor)
}

svg_posicao <- function(vals, destaque, med_mt, fmt, w = 130, h = 26) {
  vals <- vals[is.finite(vals)]
  if (length(vals) < 2 || !destaque %in% names(vals)) return("")
  r <- range(c(vals, med_mt), na.rm = TRUE); if (diff(r) == 0) r <- r + c(-1, 1)
  sx <- function(v) 6 + (v - r[1]) / diff(r) * (w - 12)
  nomes <- MUN$municipio[match(as.integer(names(vals)), MUN$id_municipio)]
  outros <- names(vals) != destaque
  pts <- paste(sprintf('<circle cx="%.1f" cy="13" r="3.4" fill="#b8c4d6" stroke="transparent" stroke-width="6" data-tip="%s: %s"/>',
                       sx(vals[outros]), esc(nomes[outros]), fmt_valor(vals[outros], fmt)), collapse = "")
  mt <- if (is.finite(med_mt)) sprintf('<line x1="%.1f" x2="%.1f" y1="4" y2="22" stroke="#6b7280" stroke-dasharray="2,2" data-tip="Mediana MT: %s"/>',
                                       sx(med_mt), sx(med_mt), fmt_valor(med_mt, fmt)) else ""
  eu <- sprintf('<circle cx="%.1f" cy="13" r="5.5" fill="%s" stroke="#fff" stroke-width="1.5" data-tip="%s: %s"/>',
                sx(vals[[destaque]]), COR[["mun"]], esc(nomes[!outros]), fmt_valor(vals[[destaque]], fmt))
  sprintf('<svg class="strip" viewBox="0 0 %d %d" width="%d" height="%d" role="img" aria-label="Posição entre os municípios do Pantanal Norte"><line x1="6" x2="%d" y1="13" y2="13" stroke="#e5e7eb" stroke-width="2"/>%s%s%s</svg>',
          w, h, w, h, w - 6, mt, pts, eu)
}

# series: lista de list(nome, cor, larg, dash, x, y)
svg_linhas <- function(series, fmt, titulo = "", w = 680, h = 260) {
  ys <- unlist(lapply(series, `[[`, "y")); xs <- unlist(lapply(series, `[[`, "x"))
  ys <- ys[is.finite(ys)]
  if (length(ys) < 2) return("")
  pl <- 62; pr <- 14; pt <- 12; pb <- 26
  xr <- range(xs); if (diff(xr) == 0) xr <- xr + c(-1, 1)
  yt <- pretty(range(ys), n = 5); yr <- range(yt)
  sx <- function(x) pl + (x - xr[1]) / diff(xr) * (w - pl - pr)
  sy <- function(y) h - pb - (y - yr[1]) / diff(yr) * (h - pt - pb)
  grade <- paste(sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#eef0f4"/><text x="%d" y="%.1f" text-anchor="end" font-size="11" fill="#6b7280">%s</text>',
                         pl, w - pr, sy(yt), sy(yt), pl - 6, sy(yt) + 4, fmt_eixo(yt)), collapse = "")
  xt <- pretty(xr, n = 6); xt <- xt[xt >= xr[1] & xt <= xr[2] & xt == round(xt)]
  eixo_x <- paste(sprintf('<text x="%.1f" y="%d" text-anchor="middle" font-size="11" fill="#6b7280">%d</text>',
                          sx(xt), h - 8, as.integer(xt)), collapse = "")
  zero <- if (yr[1] < 0 && yr[2] > 0) sprintf('<line x1="%d" x2="%d" y1="%.1f" y2="%.1f" stroke="#9ca3af"/>', pl, w - pr, sy(0), sy(0)) else ""
  linhas <- vapply(series, function(s) {
    ok <- is.finite(s$y); o <- order(s$x); x <- s$x[o]; y <- s$y[o]; ok <- ok[o]
    if (sum(ok) == 0) return("")
    seg <- paste0(ifelse(c(TRUE, !ok[-length(ok)]) & ok, "M", "L")[ok], sprintf("%.1f %.1f", sx(x[ok]), sy(y[ok])), collapse = " ")
    pts <- paste(sprintf('<circle class="pt" cx="%.1f" cy="%.1f" r="%s" fill="%s" stroke="transparent" stroke-width="9" data-tip="%s · %d: %s"/>',
                         sx(x[ok]), sy(y[ok]), if (s$larg >= 2.5) "3" else "2.2", s$cor, esc(s$nome), as.integer(x[ok]),
                         fmt_valor(y[ok], fmt)), collapse = "")
    sprintf('<g class="serie"><path class="linha" d="%s" fill="none" stroke="%s" stroke-width="%s" %s data-tip="%s"/>%s</g>', seg, s$cor, s$larg,
            if (nzchar(s$dash)) sprintf('stroke-dasharray="%s"', s$dash) else "", esc(s$nome), pts)
  }, character(1))
  series_leg <- Filter(function(s) !isFALSE(s$leg), series)
  if (length(series_leg) < length(series)) series_leg <- c(list(list(nome = "Municípios do PN-13 (um por linha)", cor = "#cfd8e6", larg = 1.5, dash = "")), series_leg)
  leg <- paste(vapply(series_leg, function(s) sprintf('<span class="leg-item"><svg width="22" height="8"><line x1="0" x2="22" y1="4" y2="4" stroke="%s" stroke-width="%s" %s/></svg>%s</span>',
                                                  s$cor, s$larg, if (nzchar(s$dash)) sprintf('stroke-dasharray="%s"', s$dash) else "", esc(s$nome)), character(1)), collapse = "")
  sprintf('<figure class="grafico-svg"><svg viewBox="0 0 %d %d" width="100%%" role="img" aria-label="%s">%s%s%s%s</svg><figcaption class="legenda-svg">%s</figcaption></figure>',
          w, h, esc(titulo), grade, zero, eixo_x, paste(linhas, collapse = ""), leg)
}

serie_mun_medianas <- function(id, mun_id, nome_mun) {
  r <- info_ind(id); e <- escala_natural(r)
  s <- SER[SER$indicador == id & !is.na(SER$ano), ]
  me <- s[s$id_municipio == mun_id, c("ano", "valor")]
  if (nrow(me) < 2) return("")
  meds <- s |> filter(ano >= min(me$ano), ano <= max(me$ano)) |> group_by(ano) |>
    summarise(pn = median(valor[id_municipio %in% PN], na.rm = TRUE), mt = median(valor, na.rm = TRUE), .groups = "drop")
  svg_linhas(list(
    list(nome = nome_mun, cor = COR[["mun"]], larg = 2.8, dash = "", x = me$ano, y = me$valor * e$f),
    list(nome = "Mediana PN-13", cor = COR[["pn"]], larg = 1.8, dash = "6,4", x = meds$ano, y = meds$pn * e$f),
    list(nome = "Mediana MT", cor = COR[["mt"]], larg = 1.8, dash = "2,3", x = meds$ano, y = meds$mt * e$f)),
    fmt = e$fmt, titulo = paste0(r$rotulo, " — ", nome_mun))
}

# ---- Cartões de indicador ---------------------------------------------------
kpi <- function(id, mun_id) {
  r <- info_ind(id); s <- resumo_ind(id, mun_id)
  comp <- if (isTRUE(r$soma == 1) && s$soma_pn > 0 && !is.na(s$valor)) {
    sprintf("%s%% do total do Pantanal Norte", num(100 * s$valor / s$soma_pn, 1))
  } else {
    sprintf("Mediana PN-13: %s · MT: %s", fmt_valor(s$med_pn, r$fmt), fmt_valor(s$med_mt, r$fmt))
  }
  sit <- situacao(s$valor, s$med_pn, r$sentido)
  div(class = "kpi",
      div(class = "kpi-rotulo", a(href = url_ind(id), r$rotulo)),
      div(class = "kpi-valor", fmt_valor(s$valor, r$fmt)),
      div(class = "kpi-meta", paste0(r$unidade, if (!is.na(s$ano)) paste0(" · ", s$ano) else "")),
      div(class = "kpi-comp", comp,
          if (!isTRUE(r$soma == 1) && sit$cls != "sit-neutra") span(class = paste("sit", sit$cls), sit$txt)))
}

grade_kpis <- function(mun_id) div(class = "kpi-grade", lapply(IND$id[IND$kpi == 1], kpi, mun_id = mun_id))

# ---- Tabela por dimensão (perfil municipal) ----------------------------------
tabela_dim <- function(dim, mun_id, nome_mun) {
  ids <- IND$id[IND$dim == dim]
  linhas <- lapply(ids, function(id) {
    r <- info_ind(id); s <- resumo_ind(id, mun_id); sit <- situacao(s$valor, s$med_pn, r$sentido)
    ser <- SER[SER$indicador == id & SER$id_municipio == mun_id & !is.na(SER$ano), ]
    nota <- if (!is.na(r$nota) && nzchar(r$nota)) sprintf(' <span class="nota-i" title="%s">ⓘ</span>', esc(r$nota)) else ""
    tibble(
      id = id,
      Indicador = sprintf('<a href="%s">%s</a>%s%s<div class="sub">%s</div>', url_ind(id), esc(r$rotulo), selo_ost(r), nota, esc(r$unidade)),
      Valor = sprintf('<b>%s</b><div class="sub">%s</div>', fmt_valor(s$valor, r$fmt), ifelse(is.na(s$ano), "", s$ano)),
      Posicao = sprintf('%s<div class="sub">%s</div>', svg_posicao(s$vals_pn, as.character(mun_id), s$med_mt, r$fmt),
                        ifelse(is.na(s$pos), "", sprintf("%dº maior de %d", s$pos, s$n_pn))),
      Serie = svg_sparkline(ser$ano, ser$valor),
      PN = sprintf('<span class="sit %s">%s</span><div class="sub">mediana %s</div>', sit$cls, sit$txt, fmt_valor(s$med_pn, r$fmt)),
      MT = fmt_valor(s$med_mt, r$fmt),
      nota_txt = ifelse(is.na(r$nota), "", r$nota))
  })
  d <- bind_rows(linhas)
  reactable(d, compact = TRUE, highlight = TRUE, sortable = FALSE, pagination = FALSE, wrap = TRUE,
            class = "tabela-dim",
            columns = list(
              id = colDef(show = FALSE), nota_txt = colDef(show = FALSE),
              Indicador = colDef(html = TRUE, minWidth = 200),
              Valor = colDef(html = TRUE, align = "right", minWidth = 90),
              Posicao = colDef(name = "Posição no PN-13", html = TRUE, align = "center", minWidth = 145),
              Serie = colDef(name = "Série", html = TRUE, align = "center", minWidth = 120),
              PN = colDef(name = "Frente ao PN-13", html = TRUE, align = "center", minWidth = 110),
              MT = colDef(name = "Mediana MT", align = "right", minWidth = 90)),
            details = function(i) {
              g <- serie_mun_medianas(d$id[i], mun_id, nome_mun)
              div(class = "detalhe-ind",
                  if (nzchar(g)) HTML(g) else p(class = "sub", "Indicador sem série histórica (retrato de um ano)."),
                  if (nzchar(d$nota_txt[i])) p(class = "nota-fonte", strong("Nota: "), d$nota_txt[i]),
                  p(class = "nota-fonte", a(href = url_ind(d$id[i]), "Ver este indicador para todos os municípios →")))
            })
}

# ---- Navegação entre municípios ---------------------------------------------
navegacao_municipio <- function(mun_id) {
  i <- match(mun_id, M13$id_municipio)
  ant <- M13[ifelse(i == 1, nrow(M13), i - 1), ]; prox <- M13[ifelse(i == nrow(M13), 1, i + 1), ]
  div(class = "nav-mun",
      a(class = "btn btn-sm btn-outline-primary", href = paste0(ant$slug, ".html"), paste("←", ant$municipio)),
      tags$select(class = "form-select form-select-sm", `aria-label` = "Ir para outro município",
                  onchange = "if (this.value) window.location = this.value",
                  lapply(seq_len(nrow(M13)), function(j) tags$option(value = paste0(M13$slug[j], ".html"),
                         selected = if (M13$id_municipio[j] == mun_id) NA else NULL, M13$municipio[j]))),
      a(class = "btn btn-sm btn-outline-primary", href = paste0(prox$slug, ".html"), paste(prox$municipio, "→")))
}

# ---- Mapas (sem camada de fundo externa) --------------------------------------
mapa_vazio <- function(altura) leaflet(height = altura, options = leafletOptions(zoomSnap = 0.25, attributionControl = TRUE))

js_clique <- function(prefixo) sprintf("function(el, x) { var map = this; map.eachLayer(function(l) {
  if (l.options && l.options.layerId) { l.on('click', function() { window.location = '%s' + l.options.layerId + '.html'; });
    l.on('mouseover', function() { l.setStyle({weight: 2.5}); }); l.on('mouseout', function() { l.setStyle({weight: 1}); }); } }); }", prefixo)

limites_pn <- function() { bb <- st_bbox(GEO[GEO$grupo == "pantanal_norte", ]); as.numeric(bb) }

mapa_municipio <- function(mun_id = NULL, altura = 360, prefixo = "") {
  g <- GEO
  fora <- g[g$grupo != "pantanal_norte", ]; pn <- g[g$grupo == "pantanal_norte", ]
  cor_pn <- ifelse(!is.null(mun_id) & pn$id_municipio %in% mun_id, COR[["mun"]], "#7FAEDB")
  bb <- limites_pn()
  mapa_vazio(altura) |>
    addPolygons(data = fora, fillColor = ifelse(fora$grupo == "polo_metropolitano", "#F2B8B5", "#E8EBF0"),
                fillOpacity = 1, color = "#ffffff", weight = 0.6, label = ~municipio) |>
    addPolygons(data = pn, layerId = ~slug, fillColor = cor_pn, fillOpacity = 1, color = "#ffffff", weight = 1,
                label = ~paste0(municipio, " — clique para abrir o perfil")) |>
    fitBounds(bb[1] - 0.3, bb[2] - 0.3, bb[3] + 0.3, bb[4] + 0.3) |>
    htmlwidgets::onRender(js_clique(prefixo))
}

mapa_indicador <- function(id, altura = 460, prefixo = "../municipios/") {
  r <- info_ind(id)
  u <- ULT$ultimos[ULT$ultimos$indicador == id, c("id_municipio", "valor", "ano")]
  g <- left_join(GEO, u, by = "id_municipio")
  v <- g$valor
  br <- unique(quantile(v, probs = seq(0, 1, 0.2), na.rm = TRUE))
  pal_nome <- if (isTRUE(r$sentido < 0)) "Reds" else "Blues"
  pal <- if (length(br) >= 4) colorBin(pal_nome, domain = v, bins = br, na.color = "#f3f4f6")
         else colorNumeric(pal_nome, domain = v, na.color = "#f3f4f6")
  rot <- sprintf("<b>%s</b><br>%s%s", esc(g$municipio), fmt_valor(g$valor, r$fmt),
                 ifelse(is.na(g$ano), "", paste0(" (", g$ano, ")")))
  pn <- g$grupo == "pantanal_norte"
  bb <- st_bbox(g)
  m <- mapa_vazio(altura) |>
    addPolygons(data = g[!pn, ], fillColor = pal(v[!pn]), fillOpacity = 0.9, color = "#ffffff", weight = 0.5,
                label = lapply(rot[!pn], HTML)) |>
    addPolygons(data = g[pn, ], layerId = ~slug, fillColor = pal(v[pn]), fillOpacity = 0.95, color = COR[["mun"]], weight = 1.6,
                label = lapply(paste0(rot[pn], "<br><i>Pantanal Norte — clique para abrir o perfil</i>"), HTML)) |>
    addLegend("bottomright", pal = pal, values = v, title = esc(r$unidade), opacity = 0.9,
              na.label = "sem dado",
              labFormat = function(type, cuts, p) {
                if (type == "bin") { n <- length(cuts); paste0(fmt_valor(cuts[-n], r$fmt), " – ", fmt_valor(cuts[-1], r$fmt)) }
                else fmt_valor(cuts, r$fmt) }) |>
    fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]]) |>
    htmlwidgets::onRender(js_clique(prefixo))
  m
}

# ---- Página de indicador ------------------------------------------------------
ranking_pn13 <- function(id, prefixo = "../municipios/") {
  r <- info_ind(id); e <- escala_natural(r)
  s <- resumo_ind(id, PN[1])
  u <- ULT$ultimos[ULT$ultimos$indicador == id & ULT$ultimos$id_municipio %in% PN, ] |>
    left_join(select(MUN, id_municipio, municipio, slug), by = "id_municipio") |>
    arrange(desc(valor))
  sem <- setdiff(M13$municipio, u$municipio)
  if (nrow(u) == 0) return(p("Sem dados para o Pantanal Norte."))
  w <- 680; lh <- 24; pl <- 190; pr <- 110; h <- nrow(u) * lh + 44
  vv <- u$valor * e$f; lim <- range(c(0, vv, s$med_pn * e$f, s$med_mt * e$f), na.rm = TRUE)
  sx <- function(x) pl + (x - lim[1]) / diff(lim) * (w - pl - pr)
  barras <- paste(sprintf('<a href="%s%s.html"><text x="%d" y="%.1f" text-anchor="end" font-size="12" fill="#1f2937">%s</text></a><rect x="%.1f" y="%.1f" width="%.1f" height="%d" fill="%s" data-tip="%s: %s (%s)"/><text x="%.1f" y="%.1f" font-size="11" fill="#374151">%s</text>',
                          prefixo, u$slug, pl - 8, (seq_len(nrow(u)) - 1) * lh + 17, esc(u$municipio),
                          pmin(sx(0), sx(vv)), (seq_len(nrow(u)) - 1) * lh + 5, abs(sx(vv) - sx(0)), lh - 8, COR[["pn"]],
                          esc(u$municipio), fmt_valor(u$valor, r$fmt), u$ano,
                          pmax(sx(0), sx(vv)) + 5, (seq_len(nrow(u)) - 1) * lh + 17, fmt_valor(u$valor, r$fmt)), collapse = "")
  ref <- function(v, cor, rot, dy) if (is.finite(v)) sprintf('<line x1="%.1f" x2="%.1f" y1="0" y2="%d" stroke="%s" stroke-dasharray="4,3" stroke-width="1.5"/><text x="%.1f" y="%d" font-size="11" fill="%s" text-anchor="middle">%s</text>',
                                                          sx(v), sx(v), h - 26 + dy, cor, sx(v), h - 14 + dy, cor, rot) else ""
  svg <- sprintf('<svg viewBox="0 0 %d %d" width="100%%" role="img" aria-label="Ranking dos municípios do Pantanal Norte">%s%s%s</svg>',
                 w, h, barras, ref(s$med_pn * e$f, COR[["pn"]], "mediana PN-13", 0), ref(s$med_mt * e$f, "#6b7280", "mediana MT", 13))
  tagList(HTML(svg), if (length(sem)) p(class = "nota-fonte", "Sem registro no ano de referência: ", paste(sem, collapse = ", "), "."))
}

series_pn13 <- function(id) {
  r <- info_ind(id); e <- escala_natural(r)
  s <- SER[SER$indicador == id & !is.na(SER$ano), ]
  if (n_distinct(s$ano) < 2) return("")
  meds <- s |> group_by(ano) |> summarise(pn = median(valor[id_municipio %in% PN], na.rm = TRUE),
                                          mt = median(valor, na.rm = TRUE), .groups = "drop")
  fin <- lapply(PN, function(m) { x <- s[s$id_municipio == m, ]
    list(nome = MUN$municipio[MUN$id_municipio == m], cor = "#cfd8e6", larg = 1, dash = "", leg = FALSE, x = x$ano, y = x$valor * e$f) })
  svg_linhas(c(fin, list(
    list(nome = "Mediana PN-13", cor = COR[["pn"]], larg = 3, dash = "", x = meds$ano, y = meds$pn * e$f),
    list(nome = "Mediana MT", cor = COR[["mun"]], larg = 2, dash = "2,3", x = meds$ano, y = meds$mt * e$f))),
    fmt = e$fmt, titulo = paste0(r$rotulo, " — municípios do Pantanal Norte e medianas"))
}

tabela_indicador <- function(id) {
  r <- info_ind(id)
  u <- ULT$ultimos[ULT$ultimos$indicador == id, ] |>
    left_join(select(MUN, id_municipio, municipio, grupo, slug), by = "id_municipio") |>
    mutate(Grupo = recode(grupo, pantanal_norte = "Pantanal Norte", polo_metropolitano = "Polo Metropolitano", resto_mt = "Resto de MT"),
           ordem = grupo != "pantanal_norte") |>
    arrange(ordem, desc(valor))
  d <- transmute(u, Município = municipio, slug, Grupo, Valor = valor, Ano = ano)
  reactable(d, searchable = TRUE, compact = TRUE, striped = TRUE, defaultPageSize = 13,
            showPageSizeOptions = TRUE, pageSizeOptions = c(13, 50, 141),
            columns = list(slug = colDef(show = FALSE),
              Município = colDef(minWidth = 170, cell = function(v, i)
                if (d$Grupo[i] == "Pantanal Norte") a(href = paste0("../municipios/", d$slug[i], ".html"), v) else v),
              Valor = colDef(align = "right", cell = function(v) fmt_valor(v, r$fmt)),
              Ano = colDef(align = "center", minWidth = 60)))
}

# ---- Resumo da dimensão (páginas D1–D6) ----------------------------------------
resumo_dimensao <- function(dim, prefixo = "../indicadores/") {
  ids <- IND$id[IND$dim == dim]
  d <- bind_rows(lapply(ids, function(id) {
    r <- info_ind(id)
    u <- ULT$ultimos[ULT$ultimos$indicador == id & ULT$ultimos$id_municipio %in% PN, ] |>
      left_join(select(MUN, id_municipio, municipio), by = "id_municipio")
    s <- resumo_ind(id, PN[1])
    mx <- u[which.max(u$valor), ]; mn <- u[which.min(u$valor), ]
    tibble(Indicador = sprintf('<a href="%s%s.html">%s</a>%s<div class="sub">%s</div>', prefixo, id, esc(r$rotulo), selo_ost(r), esc(r$unidade)),
           Ano = as.character(ano_ref(id)),
           `Mediana PN-13` = fmt_valor(s$med_pn, r$fmt), `Mediana MT` = fmt_valor(s$med_mt, r$fmt),
           Maior = if (nrow(mx)) sprintf("%s<div class='sub'>%s</div>", fmt_valor(mx$valor, r$fmt), esc(mx$municipio)) else "—",
           Menor = if (nrow(mn)) sprintf("%s<div class='sub'>%s</div>", fmt_valor(mn$valor, r$fmt), esc(mn$municipio)) else "—")
  }))
  reactable(d, compact = TRUE, pagination = FALSE, sortable = FALSE, wrap = TRUE,
            columns = list(Indicador = colDef(html = TRUE, minWidth = 220), Ano = colDef(align = "center", minWidth = 60),
                           `Mediana PN-13` = colDef(align = "right"), `Mediana MT` = colDef(align = "right"),
                           Maior = colDef(html = TRUE, align = "right", name = "Maior no PN-13"),
                           Menor = colDef(html = TRUE, align = "right", name = "Menor no PN-13")))
}

fonte_rodape <- function() {
  p(class = "nota-fonte",
    "Fonte: bases tratadas da Fase I do Projeto DSIP-MT (IBGE, RAIS/MTE, CAGED, BCB/SICOR, PPM/PAM, ",
    "ComexStat, INEP, DATASUS, PRODES/INPE, MapBiomas, SNIS, ANEEL, Siconfi/STN, FIRJAN e outras). ",
    "Medianas calculadas no ano de referência de cada indicador. Detalhes em ",
    a(href = "../dados/index.html", "Dados"), " e nas notas metodológicas da ",
    a(href = "../biblioteca/index.html", "Biblioteca"), ".")
}
