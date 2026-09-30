# Funções comuns às páginas de revisao/ (lidas com source(); não é página do site).
# As páginas leem apenas os CSV de dados_site/revisao, gravados por scripts/02_preparar_revisao.R.
suppressPackageStartupMessages({
  library(dplyr); library(reactable); library(htmltools)
})

RAIZ_REV <- Sys.getenv("QUARTO_PROJECT_DIR", "..")
DIR_REV  <- file.path(RAIZ_REV, "dados_site", "revisao")

ler_rev <- function(f) {
  x <- tibble::as_tibble(data.table::fread(file.path(DIR_REV, f), encoding = "UTF-8"))
  mutate(x, across(where(is.character), ~ ifelse(is.na(.x), "", .x)))
}

meta_rev <- function() { m <- ler_rev("metadados_pacote.csv"); setNames(m$valor, m$chave) }

num_br <- function(x) format(x, big.mark = ".", decimal.mark = ",", trim = TRUE, scientific = FALSE)
pct_br <- function(x, d = 1) paste0(formatC(100 * x, format = "f", digits = d, decimal.mark = ","), "%")

kpi_rev <- function(rotulo, valor, meta = NULL)
  div(class = "kpi", div(class = "kpi-rotulo", rotulo), div(class = "kpi-valor", valor),
      if (!is.null(meta)) div(class = "kpi-meta", meta))

ORDEM_DIM <- c("Forças", "Oportunidades", "Fraquezas", "Ameaças")
ORDEM_STATUS <- c("DOCUMENTADO", "REGIONAL", "INFERIDO", "CONTRAINDICADO", "NÃO AVALIADO")

# Filtro de seleção para colunas do reactable (valor exato)
filtro_select <- function(id_tabela, ordem = NULL) function(values, name) {
  v <- unique(as.character(values)); v <- v[v != ""]
  v <- if (is.null(ordem)) sort(v) else c(intersect(ordem, v), sort(setdiff(v, ordem)))
  tags$select(
    onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", id_tabela, name),
    `aria-label` = paste("Filtrar", name),
    style = "width:100%; height:28px; font-size:.85rem;",
    tags$option(value = "", "Todos"),
    lapply(v, function(x) tags$option(value = x, x)))
}
filtro_igual <- JS("function(rows, columnId, filterValue) {
  return rows.filter(function(row) { return String(row.values[columnId]) === filterValue })
}")

tema_rev <- reactableTheme(
  headerStyle = list(background = "#f3f4f8", color = "#1E2761", fontWeight = 600),
  style = list(fontSize = "0.88rem"))

link_ext <- function(url, texto) if (nzchar(url)) a(href = url, target = "_blank", rel = "noopener", texto) else texto
