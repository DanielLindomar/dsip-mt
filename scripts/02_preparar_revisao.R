# =============================================================================
# 02_preparar_revisao.R — Observatório DSIP-MT · seção "Revisão de literatura"
# Lê os pacotes de entrega das cinco áreas-chave no Google Drive (somente leitura)
# e grava em dados_site/revisao/<área>/ e arquivos/revisao/<área>/ apenas o que é
# publicável e o que as páginas de revisao/ (e as páginas de município) usam.
#
# Rodar a partir da raiz do repositório (PowerShell):
#   & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/02_preparar_revisao.R
# (Não rodar pelo Bash do Git: o R falha com o caminho do Google Drive.)
#
# Não é publicado (direitos de terceiros e privacidade): PDFs e textos integrais
# do acervo e da busca dirigida, os campos abstract e keywords do .bib, as bases de
# evidências, os identificadores EVC da matriz complementar, a Nota Técnica de
# encaminhamento e qualquer identificador do Google Drive.
#
# Áreas: a econômica (pacote regerado em 23/09/2026) tem conferências com os
# números do pacote; as outras quatro (publicadas no Drive em 07/10/2026) têm
# conferências de coerência interna entre a matriz, os anexos e o BibTeX.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readxl); library(jsonlite); library(data.table)
})

DRIVE <- "C:/Users/lindo/Meu Drive (lindomar.pegorini@unemat.br)/_RESEARCH/(26-27) PROJETO SUDECO"
FOFA  <- file.path(DRIVE, "06_Pesquisa/02_Revisao_FOFA")

AREAS <- tibble::tribble(
  ~slug,              ~nome,              ~de_area,            ~dim_fase_i,  ~pasta,                                                   ~extracao,                       ~data_drive,
  "economica",        "Econômica",        "econômica",         "D1 · PESM",  "04_Entrega_Area_Economica",                              "03_Extracao_FOFA",              "24/09/2026",
  "infraestrutura",   "Infraestrutura",   "de infraestrutura", "D5 · PIM",   "05_Infraestrutura/04_Entrega_Area_Infraestrutura",       "05_Infraestrutura/03_Extracao", "07/10/2026",
  "setor-publico",    "Setor Público",    "do setor público",  "D6 · PGPM",  "06_Setor_Publico/04_Entrega_Area_Setor_Publico",         "06_Setor_Publico/03_Extracao",  "07/10/2026",
  "sociodemografica", "Sociodemográfica", "sociodemográfica",  "D3 · PSDM",  "07_Sociodemografica/04_Entrega_Area_Sociodemografica",   "07_Sociodemografica/03_Extracao", "07/10/2026",
  "ambiental",        "Ambiental",        "ambiental",         "D4 · PAM",   "08_Ambiental/04_Entrega_Area_Ambiental",                 "08_Ambiental/03_Extracao",      "07/10/2026")

OUT_RAIZ <- "dados_site/revisao"
ARQ_RAIZ <- "arquivos/revisao"
REL      <- "arquivos/relatorios"
dir.create(OUT_RAIZ, recursive = TRUE, showWarnings = FALSE)
# os CSV da versão 0.3 ficavam na raiz de dados_site/revisao e arquivos/revisao; agora cada área tem sua pasta
for (d in c(OUT_RAIZ, ARQ_RAIZ)) unlink(list.files(d, pattern = "[.](csv|bib)$", full.names = TRUE))

msg     <- function(...) cat(sprintf(...), "\n")
confere <- function(cond, texto) { if (!isTRUE(cond)) stop("Falha de conferência: ", texto); msg("  ok: %s", texto) }

# ---- Normalizações ----------------------------------------------------------
sem_acento <- function(x) stringi::stri_trans_general(x, "Latin-ASCII")

# Dimensão: FORÇAS, FORCAS, FORÇA, Forças... -> Forças | Oportunidades | Fraquezas | Ameaças
norm_dim <- function(x) {
  k <- substr(toupper(sem_acento(trimws(x))), 1, 3)
  out <- c(FOR = "Forças", OPO = "Oportunidades", FRA = "Fraquezas", AME = "Ameaças")[k]
  if (anyNA(out)) stop("Dimensão não reconhecida: ", paste(unique(x[is.na(out)]), collapse = ", "))
  unname(out)
}
PREFIXO <- c("Forças" = "FOR", "Oportunidades" = "OPO", "Fraquezas" = "FRA", "Ameaças" = "AME")
ORDEM_DIM <- names(PREFIXO)

# Convergência: 3, "3/3" -> "3/3"
norm_conv <- function(x) { x <- trimws(as.character(x)); ifelse(grepl("/", x), x, paste0(x, "/3")) }

# Confiança: alta | média | baixa (a fonte grafa "media" sem acento)
norm_conf <- function(x) { x <- tolower(sem_acento(trimws(x))); c(alta = "alta", media = "média", baixa = "baixa")[x] |> unname() }

# Ancoragem geográfica: um único conjunto de rótulos (o Anexo C das áreas da Fase B tem também "Pantanal generico")
ANCORAGEM <- c("Mato Grosso", "Mato Grosso (parcial)", "Pantanal Sul (MS) e outros", "Pantanal genérico")
norm_anc <- function(x) {
  k <- tolower(sem_acento(trimws(x)))
  out <- ifelse(grepl("generico", k), ANCORAGEM[4],
         ifelse(grepl("parcial", k), ANCORAGEM[2],
         ifelse(grepl("ms|sul|outros", k), ANCORAGEM[3],
         ifelse(grepl("mato grosso|ancorado em mt", k), ANCORAGEM[1], NA))))
  if (anyNA(out)) stop("Ancoragem não reconhecida: ", paste(unique(x[is.na(out)]), collapse = ", "))
  out
}

# Abrangência dos itens da matriz complementar
ABRANG <- c(municipal = "Municipal", intermunicipal = "Intermunicipal", `pantanal-mt` = "Pantanal-MT",
            bioma = "Bioma", multiescala = "Multiescala", `mato-grosso` = "Mato Grosso")

# Município: chave sem acento, caixa, apóstrofo; "de Leverger" = "do Leverger"
chave_mun <- function(x) {
  k <- tolower(sem_acento(trimws(x)))
  k <- gsub("[^a-z0-9]+", " ", k)
  k <- gsub("\\bde leverger\\b", "do leverger", k)
  trimws(gsub("\\s+", " ", k))
}

primeira_maiuscula <- function(x) ifelse(is.na(x) | x == "", x, paste0(toupper(substr(x, 1, 1)), substring(x, 2)))
txt <- function(x) ifelse(is.na(x), "", as.character(x))

# ---- Municípios do recorte -----------------------------------------------
mun <- data.table::fread("dados_site/municipios.csv", encoding = "UTF-8") |>
  as_tibble() |> filter(pn13) |>
  transmute(id_municipio = as.integer(id_municipio), municipio, slug, k = chave_mun(municipio))
confere(nrow(mun) == 13, "13 municípios do recorte em dados_site/municipios.csv")
id_de <- function(nomes) {
  k <- chave_mun(nomes); i <- match(k, mun$k)
  if (anyNA(i)) stop("Município sem correspondência: ", paste(unique(nomes[is.na(i)]), collapse = ", "))
  mun$id_municipio[i]
}
nome_de <- function(ids) mun$municipio[match(ids, mun$id_municipio)]
# lista "caceres; pocone" -> nomes oficiais "Cáceres; Poconé"; nome de fora do recorte (a infraestrutura cita
# Cuiabá e Várzea Grande) fica como veio
nomes_oficiais <- function(x) vapply(x, function(s) {
  if (is.na(s) || trimws(s) == "") return("")
  p <- trimws(strsplit(s, ";")[[1]]); p <- p[p != ""]
  i <- match(chave_mun(p), mun$k)
  paste(ifelse(is.na(i), p, mun$municipio[i]), collapse = "; ")
}, character(1), USE.NAMES = FALSE)

# ---- BibTeX ------------------------------------------------------------------
PARTICULAS <- c("de", "da", "do", "dos", "das", "e", "d", "del", "van", "von", "der")
iniciais <- function(nome) {
  p <- strsplit(trimws(nome), "\\s+")[[1]]
  p <- p[!tolower(p) %in% PARTICULAS & p != ""]
  if (!length(p)) return("")
  paste0(vapply(p, function(w) {
    partes <- strsplit(w, "-")[[1]]
    paste0(toupper(substr(partes, 1, 1)), ".", collapse = "-")
  }, ""), collapse = " ")
}
autores_lista <- function(a) {
  pes <- trimws(strsplit(a, "\\s+and\\s+")[[1]])
  vapply(pes, function(p) {
    if (grepl(",", p)) {
      s <- trimws(sub(",.*$", "", p)); n <- trimws(sub("^[^,]*,", "", p))
      ini <- iniciais(n); if (ini == "") s else paste0(s, ", ", ini)
    } else p
  }, "", USE.NAMES = FALSE)
}
sobrenomes <- function(a) vapply(trimws(strsplit(a, "\\s+and\\s+")[[1]]),
                                 function(p) trimws(sub(",.*$", "", p)), "", USE.NAMES = FALSE)
autor_curto <- function(a) {
  if (is.na(a) || !nzchar(a)) return("Autoria não informada")
  s <- sobrenomes(a)
  if (length(s) == 1) s else if (length(s) == 2) paste(s[1], "e", s[2]) else paste(s[1], "et al.")
}
desescapa <- function(x) {
  x <- gsub("\\\\([_&%$#])", "\\1", x)
  gsub("[{}]", "", x)
}

ler_bib <- function(bib_f, arq_pub, nome_area) {
  L <- readLines(bib_f, encoding = "UTF-8", warn = FALSE)
  re_ini   <- "^@([A-Za-z]+)\\{([^,]+),\\s*$"
  re_campo <- "^\\s+([A-Za-z]+)\\s*=\\s*\\{(.*)\\},?\\s*$"
  estranhas <- which(!(grepl(re_ini, L) | grepl(re_campo, L) | L == "}" | L == "" | grepl("^%", L)))
  confere(length(estranhas) == 0, "todas as linhas do .bib seguem o formato um campo por linha")
  # versão sem abstract e keywords (os campos ocupam uma linha cada)
  L_pub <- L[!grepl("^\\s+(abstract|keywords)\\s*=", L, ignore.case = TRUE)]
  n_cab <- which(!grepl("^%", L_pub))[1] - 1
  cab <- c("% Versão publicada no Observatório DSIP-MT sem os campos de resumo e de palavras-chave",
           sprintf("%% (direitos de terceiros). Demais campos idênticos ao arquivo do pacote da área %s.", nome_area))
  L_pub <- c(L_pub[seq_len(n_cab)], cab, L_pub[-seq_len(n_cab)])
  writeLines(enc2utf8(L_pub), arq_pub, useBytes = TRUE)
  confere(!any(grepl("^\\s+(abstract|keywords)\\s*=", L_pub, ignore.case = TRUE)), "arquivo .bib publicado sem abstract/keywords")

  ents <- list(); cur <- NULL
  for (ln in L) {
    if (grepl(re_ini, ln)) {
      cur <- list(tipo = sub(re_ini, "\\1", ln), chave = sub(re_ini, "\\2", ln))
    } else if (!is.null(cur) && grepl(re_campo, ln)) {
      cur[[tolower(sub(re_campo, "\\1", ln))]] <- desescapa(sub(re_campo, "\\2", ln))
    } else if (ln == "}" && !is.null(cur)) {
      ents[[length(ents) + 1]] <- cur; cur <- NULL
    }
  }
  campo <- function(e, f) if (is.null(e[[f]])) NA_character_ else e[[f]]
  bib <- tibble(
    chave     = vapply(ents, campo, "", "chave"),
    tipo      = vapply(ents, campo, "", "tipo"),
    author    = vapply(ents, campo, "", "author"),
    titulo    = vapply(ents, campo, "", "title"),
    ano       = suppressWarnings(as.integer(vapply(ents, campo, "", "year"))),
    journal   = vapply(ents, campo, "", "journal"),
    booktitle = vapply(ents, campo, "", "booktitle"),
    howpub    = vapply(ents, campo, "", "howpublished"),
    doi       = vapply(ents, campo, "", "doi"),
    url_bib   = vapply(ents, campo, "", "url"),
    idioma    = vapply(ents, campo, "", "language"),
    note      = vapply(ents, campo, "", "note"))
  confere(!anyDuplicated(bib$chave), "chaves BibTeX únicas")
  bib |>
    mutate(autores   = vapply(author, function(a) if (is.na(a)) "" else paste(autores_lista(a), collapse = "; "), ""),
           autor_ano = paste0(vapply(author, autor_curto, ""), " (", ano, ")"),
           periodico = coalesce(journal, booktitle, howpub),
           url       = coalesce(ifelse(is.na(doi), NA, paste0("https://doi.org/", doi)), url_bib, ""),
           chave_interna = trimws(sub(";.*$", "", sub("^.*chave interna:\\s*", "", note))),
           redundantes   = ifelse(!is.na(note) & grepl("registros redundantes", note),
                                  trimws(gsub("\\s*\\(G[0-9]+\\)", "", sub("^.*registros redundantes do mesmo estudo:\\s*", "", note))),
                                  ""))
}

ROT_CAT <- c(MT_municipio_do_recorte = "Cita município do recorte (MT)",
             MT_pantanal_norte = "Pantanal Norte (MT), sem município do recorte",
             MT_e_MS = "Pantanal de MT e MS",
             MS_pantanal_sul = "Pantanal Sul (MS)",
             Pantanal_generico_ou_indefinido = "Pantanal genérico ou indefinido",
             fora_do_Brasil = "Fora do Brasil")

# Fluxo PRISMA: a aba "PRISMA" do Anexo B, com uma chave por linha conhecida
CHAVES_PRISMA <- c("^Brutos CAPES" = "capes", "^Brutos SciELO" = "scielo", "^Brutos Scopus" = "scopus",
                   "^Total bruto" = "total", "^Únicos após" = "unicos", "^Excluídos fora" = "fora_periodo",
                   "^Duplicatas do mesmo estudo removidas" = "dup_removidas", "^Submetidos à triagem" = "triados",
                   "^Incluir / Excluir / Incerto" = "titulo", "^[Rr][0-9]+c? —" = "rodada",
                   "^CORPUS INCLU" = "corpus", "^Excluídos por título" = "exc_titulo", "^Excluídos por resumo" = "exc_resumo",
                   "^Incluídos por arbitragem" = "arbitragem", "^Excluídos por identificação" = "exc_ident",
                   "^Excluídos por duplicata" = "exc_dup", "^Registros sem veredito" = "sem_veredito")
ETAPA_PRISMA <- function(x) {
  k <- toupper(sem_acento(x))
  if (grepl("^IDENTIFICA", k)) "Identificação" else if (grepl("^TRIAGEM POR TITULO", k)) "Triagem por título"
  else if (grepl("^TRIAGEM POR RESUMO", k)) "Triagem por resumo" else if (grepl("^RESULTADO", k)) "Resultado" else NA_character_
}

# ---- Uma área ---------------------------------------------------------------
preparar_area <- function(A) {
  msg("\n===== Área %s", A$nome)
  PAC   <- file.path(FOFA, A$pasta)
  ANX_S <- file.path(PAC, "05_Anexos/Revisao_Sistematica")
  ANX_C <- file.path(PAC, "05_Anexos/Revisao_Complementar")
  stopifnot(dir.exists(PAC))
  OUT <- file.path(OUT_RAIZ, A$slug); ARQ <- file.path(ARQ_RAIZ, A$slug)
  dir.create(OUT, recursive = TRUE, showWarnings = FALSE); dir.create(ARQ, recursive = TRUE, showWarnings = FALSE)
  unlink(list.files(OUT, full.names = TRUE)); unlink(list.files(ARQ, full.names = TRUE))
  gravar <- function(x, f) data.table::fwrite(x, file.path(OUT, f), na = "")
  econ <- A$slug == "economica"

  # 1. BibTeX
  bib_f <- list.files(file.path(PAC, "04_Corpus_Bibliografico"), pattern = "[.]bib$", full.names = TRUE)
  confere(length(bib_f) == 1, "um arquivo .bib no pacote")
  bib_pub <- sub("[.]bib$", "_sem_resumos.bib", basename(bib_f))
  bib <- ler_bib(bib_f, file.path(ARQ, bib_pub), A$de_area)
  if (econ) confere(nrow(bib) == 225, "225 entradas no .bib")
  msg("  entradas: %d; com DOI: %d", nrow(bib), sum(!is.na(bib$doi)))

  # 2. Ancoragem por registro (analise_geografica.json da extração da área)
  geo <- jsonlite::fromJSON(file.path(FOFA, A$extracao, "analise_geografica.json"), simplifyVector = FALSE)
  if (econ) confere(geo$n_registros == 236 && geo$n_estudos_distintos == 225, "analise_geografica.json: 236 registros, 225 estudos distintos")
  reg <- tibble(registro = names(geo$detalhe_corpus),
                cat      = vapply(geo$detalhe_corpus, function(d) d$cat, ""),
                mun13    = lapply(geo$detalhe_corpus, function(d) unlist(d$mun13)))
  mapa <- bind_rows(
    bib |> transmute(registro = chave_interna, chave),
    bib |> filter(redundantes != "") |>
      transmute(chave, registro = strsplit(redundantes, "\\s*[;,]\\s*")) |> tidyr::unnest(registro))
  confere(!anyDuplicated(mapa$registro), "cada registro do corpus aponta para uma só entrada do .bib")
  if (econ) {
    confere(nrow(mapa) == 236, "236 registros mapeados em 225 entradas do .bib (11 vínculos)")
    confere(all(reg$registro %in% mapa$registro), "todos os registros da análise geográfica têm entrada no .bib")
  }
  fora <- setdiff(reg$registro, mapa$registro)
  if (length(fora)) msg("  aviso: %d registro(s) da análise geográfica fora do .bib (saíram do corpus depois dela): %s",
                        length(fora), paste(fora, collapse = ", "))
  reg <- inner_join(reg, mapa, by = "registro")
  confere(all(bib$chave %in% reg$chave), "todo estudo do .bib tem classificação geográfica")

  est_mun <- reg |> select(chave, mun13) |> tidyr::unnest(mun13) |>
    filter(!is.na(mun13), mun13 != "") |>
    mutate(id_municipio = id_de(mun13)) |> distinct(chave, id_municipio)
  cat_est <- reg |> inner_join(bib |> select(chave, chave_interna), by = "chave") |>
    filter(registro == chave_interna) |> select(chave, cat)
  mun_por_est <- est_mun |> mutate(nome = nome_de(id_municipio)) |>
    arrange(nome) |> group_by(chave) |> summarise(municipios = paste(nome, collapse = "; "), .groups = "drop")
  estudos <- bib |>
    left_join(cat_est, by = "chave") |> left_join(mun_por_est, by = "chave") |>
    transmute(area = A$nome, chave, autores, autor_ano, ano, titulo, periodico,
              doi = txt(doi), url, tipo, idioma = txt(idioma),
              recorte_geografico = unname(ROT_CAT[cat]),
              municipios = coalesce(municipios, "")) |>
    arrange(ano, autor_ano)
  confere(nrow(estudos) == nrow(bib), "estudos.csv com uma linha por entrada do .bib")
  confere(!anyNA(estudos$recorte_geografico), "recorte geográfico atribuído a todos os estudos")
  gravar(estudos, "estudos.csv")

  estudos_municipio <- est_mun |>
    inner_join(bib |> select(chave, autores = autor_ano, ano, titulo, url), by = "chave") |>
    mutate(autores = sub("\\s*\\([0-9]{4}\\)$", "", autores)) |>
    transmute(id_municipio, chave, autores, ano, titulo, url) |>
    arrange(id_municipio, ano, autores)
  gravar(estudos_municipio, "estudos_municipio.csv")
  msg("  estudos que citam ao menos um município do recorte: %d; pares estudo × município: %d",
      n_distinct(estudos_municipio$chave), nrow(estudos_municipio))

  # 3. Matriz FOFA integrada
  mi <- read_excel(file.path(PAC, "02_Matrizes_FOFA/02_Matriz_Formato_Integrado.xlsx"), "Matriz_Integrada")
  metodo <- read_excel(file.path(PAC, "02_Matrizes_FOFA/02_Matriz_Formato_Integrado.xlsx"), "Metodo", col_names = FALSE, .name_repair = "minimal")
  metodo_txt <- paste(unlist(metodo), collapse = " ")
  n_itens <- nrow(mi)
  if (econ) confere(n_itens == 40 && ncol(mi) == 15, "Matriz_Integrada com 40 itens × 15 colunas")
  anc_c <- data.table::fread(file.path(ANX_S, "Anexo_C_Ancoragem_geografica_dos_itens.csv"), encoding = "UTF-8", sep = ";") |>
    as_tibble() |>
    transmute(dimensao = norm_dim(Dimensao),
              # o Anexo C da infraestrutura e do setor público numera os itens em sequência (1 a n por dimensão),
              # e não pelo ID validado, que tem lacunas: a ligação com a matriz é pela expressão original
              expressao_original = Expressao,
              item_seq = sprintf("%s-%02d", PREFIXO[dimensao], as.integer(Item)),
              anc_c = norm_anc(`Ancoragem geografica`),
              conv_c = norm_conv(Convergencia),
              mun_c = nomes_oficiais(`Municipios do recorte citados`))
  itens <- mi |>
    transmute(item_id = ID, dimensao = norm_dim(`Dimensão`),
              ordem = as.integer(sub("^.*-", "", ID)),
              proposicao = `Item (proposição verificável)`,
              expressao_original = `Expressão original (matriz validada)`,
              citacao_principal = `Evidência (citação literal)`,
              fonte_principal = Fonte, local_principal = Local,
              lastro_estudos = as.integer(`Nº de fontes`),
              abrangencia_detalhe = if (econ) "" else txt(`Abrangência`),
              anc_matriz = if (econ) norm_anc(`Abrangência`) else NA_character_,
              municipios_citados = nomes_oficiais(`Municípios do recorte citados`),
              confianca = norm_conf(`Confiança`),
              convergencia = norm_conv(`Convergência`),
              verificavel_campo = ifelse(tolower(sem_acento(`Verificável em campo`)) == "sim", "Sim", "Não"),
              ressalva = txt(Ressalva), como_verificar = txt(`Como verificar`)) |>
    left_join(anc_c |> select(-item_seq), by = c("dimensao", "expressao_original"))
  # na econômica a expressão do Anexo C difere da matriz em grafia (aspas), mas a numeração coincide com o ID
  sem_par <- is.na(itens$anc_c)
  if (any(sem_par)) {
    por_num <- anc_c[match(itens$item_id[sem_par], anc_c$item_seq), ]
    if (any(itens$expressao_original[!sem_par] %in% por_num$expressao_original)) stop("ligação do Anexo C ambígua")
    itens$anc_c[sem_par] <- por_num$anc_c; itens$conv_c[sem_par] <- por_num$conv_c; itens$mun_c[sem_par] <- por_num$mun_c
    msg("  Anexo C ligado pela expressão em %d itens e pelo número em %d", sum(!sem_par), sum(sem_par))
  }
  confere(nrow(anc_c) == n_itens && !anyNA(itens$anc_c), sprintf("Anexo C casa com os %d itens da matriz", n_itens))
  if (econ) confere(all(itens$anc_matriz == itens$anc_c), "ancoragem da matriz = ancoragem do Anexo C, item a item")
  confere(all(itens$convergencia == itens$conv_c), "convergência da matriz = convergência do Anexo C, item a item")
  if (econ) {
    confere(all(itens$municipios_citados == itens$mun_c), "municípios citados da matriz = Anexo C, item a item")
  } else {
    # nas demais áreas a coluna da matriz traz também municípios de fora do recorte (Corumbá, Aquidauana...) e não
    # coincide com o Anexo C, que lista os 13 do recorte citados em todas as citações do item: o site usa o Anexo C
    msg("  municípios citados: matriz difere do Anexo C em %d itens; usado o Anexo C", sum(itens$municipios_citados != itens$mun_c))
    itens$municipios_citados <- itens$mun_c
  }
  itens <- itens |> mutate(ancoragem = anc_c) |> select(-anc_c, -conv_c, -mun_c, -anc_matriz) |>
    mutate(dimensao = factor(dimensao, ORDEM_DIM)) |> arrange(dimensao, ordem) |>
    mutate(dimensao = as.character(dimensao))
  if (econ) confere(all(table(itens$dimensao) == 10), "10 itens por dimensão")

  # 4. Citações exibidas (Anexo A)
  qa <- read_excel(file.path(ANX_S, "Anexo_A_Quadros_FOFA_planilha.xlsx"), "Quadros_FOFA")
  refs <- read_excel(file.path(ANX_S, "Anexo_A_Quadros_FOFA_planilha.xlsx"), "Referencias")
  if (econ) { confere(nrow(qa) == 196, "196 citações exibidas no Anexo A"); confere(nrow(refs) == 114, "114 obras citadas (aba Referencias)") }
  if (!"Vínculo de estudo" %in% names(qa)) qa$`Vínculo de estudo` <- NA_character_
  qa <- qa |> tidyr::fill(`Dimensão`, `Nº`, `Palavra-chave / expressão`, `Convergência`, .direction = "down")
  cit <- qa |>
    transmute(dimensao = norm_dim(`Dimensão`),
              # Nº é o número (econômica) ou o ID completo, como FOR-01 (demais áreas)
              item_id = ifelse(grepl("^[A-Z]{3}-", `Nº`), `Nº`, sprintf("%s-%02d", PREFIXO[dimensao], suppressWarnings(as.integer(`Nº`)))),
              expressao = `Palavra-chave / expressão`,
              autor_ano = `Fonte (obra utilizada)`,
              citacao = `Citação (trecho de onde saiu o resultado)`,
              local = txt(Local), chave_fonte = `Chave da fonte`,
              conferencia = txt(`Conferência`),
              vinculo_estudo = txt(`Vínculo de estudo`)) |>
    group_by(item_id) |> mutate(ordem_citacao = row_number()) |> ungroup()
  chk <- cit |> distinct(item_id, expressao) |> left_join(itens |> select(item_id, expressao_original), by = "item_id")
  confere(nrow(chk) == n_itens && all(chk$expressao == chk$expressao_original),
          sprintf("Anexo A e matriz: mesma expressão original nos %d itens", n_itens))
  refs_link <- refs |> transmute(chave_fonte = Chave, doi_ref = DOI, titulo_obra = `Título`, veiculo = `Veículo`)
  confere(all(cit$chave_fonte %in% refs_link$chave_fonte), "toda citação tem obra na aba Referencias")
  cit <- cit |>
    left_join(refs_link, by = "chave_fonte") |>
    left_join(mapa |> rename(chave_fonte = registro, chave_bib = chave), by = "chave_fonte") |>
    left_join(bib |> select(chave_bib = chave, url_bib2 = url), by = "chave_bib") |>
    mutate(url = coalesce(ifelse(is.na(doi_ref) | doi_ref == "", NA, paste0("https://doi.org/", doi_ref)),
                          na_if(url_bib2, ""), "")) |>
    transmute(item_id, ordem_citacao, autor_ano, citacao, local, conferencia, titulo_obra, veiculo,
              url, chave_bib = coalesce(chave_bib, ""), vinculo_estudo)
  confere(n_distinct(cit$item_id) == n_itens, sprintf("citações cobrem os %d itens", n_itens))
  gravar(cit, "citacoes_fofa.csv")
  msg("  citações: %d (com link: %d)", nrow(cit), sum(cit$url != ""))
  itens <- itens |> left_join(cit |> count(item_id, name = "n_citacoes"), by = "item_id")
  gravar(itens, "itens_fofa.csv")

  # 5. FOFA municipal (Anexo B da complementar); a 2ª coluna é o grupo da tipologia da área
  mb <- read_excel(file.path(ANX_C, "Anexo_B_Matriz_Cruzada_Municipal.xlsx"), 1)
  rotulo_grupo <- names(mb)[2]
  names(mb)[2] <- "Grupo"
  confere(nrow(mb) == 13 * n_itens, sprintf("%d linhas município × item (13 × %d)", nrow(mb), n_itens))
  STATUS <- c("DOCUMENTADO", "REGIONAL", "INFERIDO", "NÃO AVALIADO", "CONTRAINDICADO")
  confere(all(mb$Status %in% STATUS), "status dentro dos cinco rótulos")
  junta_base <- function(status, base, ref, crit) {
    b <- primeira_maiuscula(trimws(base))
    ref <- trimws(ref); crit <- trimws(crit)
    out <- b
    tem_ref <- !is.na(ref) & ref != ""
    out[tem_ref] <- ifelse(status[tem_ref] == "INFERIDO",
                           paste0(out[tem_ref], " — município de referência: ", ref[tem_ref]),
                           paste0(out[tem_ref], " — ", ref[tem_ref]))
    tem_crit <- !is.na(crit) & crit != ""
    out[tem_crit] <- paste0(out[tem_crit], " — critério: ", crit[tem_crit])
    ifelse(is.na(out), "", out)
  }
  fofa_mun <- mb |>
    transmute(id_municipio = id_de(`Município`),
              municipio = nome_de(id_municipio),
              item_id = `Cód.`,
              dimensao = norm_dim(`Dimensão`),
              proposicao = primeira_maiuscula(trimws(Item)),
              status = Status,
              base = junta_base(Status, Base, `Referência`, `Critério`))
  confere(all(fofa_mun$item_id %in% itens$item_id), "itens do Anexo B existem na matriz")
  confere(nrow(distinct(fofa_mun, id_municipio, item_id)) == 13 * n_itens, "combinações município × item únicas")
  chk_dim <- fofa_mun |> distinct(item_id, dimensao) |> left_join(itens |> select(item_id, d2 = dimensao), by = "item_id")
  confere(all(chk_dim$dimensao == chk_dim$d2), "dimensão do Anexo B = dimensão da matriz")
  fofa_mun <- fofa_mun |>
    mutate(dimensao_f = factor(dimensao, ORDEM_DIM)) |>
    arrange(municipio, dimensao_f, item_id) |> select(-dimensao_f)
  gravar(fofa_mun, "fofa_municipal.csv")
  print(table(fofa_mun$status))

  tipologia <- mb |> distinct(`Município`, Grupo, `Posição no bioma`) |>
    transmute(id_municipio = id_de(`Município`), municipio = nome_de(id_municipio),
              grupo = Grupo, posicao_bioma = `Posição no bioma`) |>
    left_join(mun |> select(id_municipio, slug), by = "id_municipio") |> arrange(municipio)
  confere(nrow(tipologia) == 13, "tipologia com 13 municípios")
  gravar(tipologia, "tipologia_municipal.csv")

  # 6. Matriz complementar (sem IDs EVC)
  md <- read_excel(file.path(ANX_C, "Anexo_D_Matriz_Complementar.xlsx"), 1)
  if (econ) confere(nrow(md) == 69, "69 itens na matriz complementar")
  comp <- md |>
    transmute(codigo = `Cód.`, rodada = as.integer(Rodada), dimensao = norm_dim(`Dimensão`),
              proposicao = `Proposição verificável`,
              corresponde_a = ifelse(`Corresponde a` == "novo", "Item novo", `Corresponde a`),
              lacuna_tematica = ifelse(is.na(`Lacuna temática`) | `Lacuna temática` %in% c("—", "-"), "", `Lacuna temática`),
              fontes_obs_independentes = as.integer(`Fontes (obs. indep.)`),
              documentos = as.integer(`Docs.`),
              abrangencia = unname(ABRANG[`Abrangência`]),
              confianca = norm_conf(`Confiança`),
              convergencia = norm_conv(`Convergência`),
              auditoria = primeira_maiuscula(txt(Auditoria)),
              municipios_nomeados = txt(`Municípios nomeados`),
              # a cláusula pode remeter a evidências por ID (EVC-XXXX); a base de evidências
              # não é publicada, e o ID é retirado sem alterar o restante do texto
              clausula_insuficiencia = txt(`Cláusula de insuficiência`) |>
                gsub(pattern = ",\\s*EVC-[0-9]+", replacement = "") |>
                gsub(pattern = "\\s*\\(?EVC-[0-9]+\\)?", replacement = ""))
  confere(!anyNA(comp$abrangencia) && !anyNA(comp$confianca), "rótulos da complementar normalizados")
  confere(!any(grepl("EVC-", unlist(comp))), "matriz complementar sem identificadores EVC")
  gravar(comp, "matriz_complementar.csv")
  como_usar <- read_excel(file.path(ANX_C, "Anexo_D_Matriz_Complementar.xlsx"), "Como usar", col_names = FALSE, .name_repair = "minimal") |> unlist() |> txt()
  nao_cobre <- como_usar[grepl("^O QUE ESTA MATRIZ NÃO COBRE", como_usar)]
  nao_cobre <- sub("^O QUE ESTA MATRIZ NÃO COBRE[.]\\s*", "", nao_cobre)

  # 7. PRISMA
  pr <- read_excel(file.path(ANX_S, "Anexo_B_Triagem_e_PRISMA.xlsx"), "PRISMA", col_names = FALSE, .name_repair = "minimal")
  names(pr)[1:2] <- c("rotulo", "valor")
  pr <- pr |> mutate(rotulo = txt(rotulo), valor = txt(valor))
  etapa <- NA_character_; etapa_rot <- ""; linhas <- list()
  datas_prisma <- pr$rotulo[grepl("^Buscas:", pr$rotulo)][1]
  for (i in seq_len(nrow(pr))) {
    e <- if (!nzchar(pr$valor[i]) && nzchar(pr$rotulo[i])) ETAPA_PRISMA(pr$rotulo[i]) else NA_character_
    if (!is.na(e)) { etapa <- e; etapa_rot <- pr$rotulo[i]; next }
    if (is.na(etapa)) next
    ch <- NA_character_
    for (p in names(CHAVES_PRISMA)) if (grepl(p, pr$rotulo[i])) { ch <- CHAVES_PRISMA[[p]]; break }
    # linha sem valor só entra se for o total bruto (fórmula sem valor gravado na planilha)
    if (is.na(ch) || (!nzchar(pr$valor[i]) && ch != "total")) next
    linhas[[length(linhas) + 1]] <- tibble(etapa = etapa, etapa_rotulo = etapa_rot, chave = ch, rotulo = pr$rotulo[i], valor_texto = pr$valor[i])
  }
  prisma <- bind_rows(linhas) |> mutate(ordem = row_number(), valor = suppressWarnings(as.integer(valor_texto)))
  pv <- function(k) { x <- prisma$valor[prisma$chave == k]; if (length(x) == 1) x else 0L }
  # o total bruto vem como fórmula da planilha (=SUM(...)): soma das três bases
  prisma$valor[prisma$chave == "total"] <- pv("capes") + pv("scielo") + pv("scopus")
  prisma$valor_texto[prisma$chave == "total"] <- as.character(pv("total"))
  prisma <- bind_rows(prisma, tibble(etapa = "Resultado", etapa_rotulo = "RESULTADO FINAL", chave = "estudos", rotulo = "Estudos distintos no corpus (entradas do BibTeX)",
                                     valor_texto = as.character(nrow(bib)), ordem = nrow(prisma) + 1L, valor = nrow(bib)))
  if (econ) {
    confere(pv("total") == 1378, "1.378 registros brutos")
    confere(pv("triados") == 839, "839 registros triados")
    confere(pv("corpus") == 236, "236 registros no corpus")
  }
  confere(pv("unicos") - pv("fora_periodo") - pv("dup_removidas") == pv("triados"), "únicos − fora do período − duplicatas removidas = triados")
  confere(pv("corpus") + pv("exc_titulo") + pv("exc_resumo") + pv("exc_ident") + pv("exc_dup") + pv("sem_veredito") == pv("triados"),
          "incluídos + excluídos + sem veredito = triados")
  confere(pv("corpus") >= nrow(bib), "registros no corpus ≥ estudos distintos")
  gravar(prisma |> select(ordem, etapa, etapa_rotulo, chave, rotulo, valor, valor_texto), "prisma.csv")

  # 8. RSE em PDF
  rse <- list.files(file.path(PAC, "01_Relatorios/PDF"), pattern = "^RSE_.*[.]pdf$", full.names = TRUE)
  confere(length(rse) == 1, "um PDF do RSE no pacote")
  file.copy(rse, file.path(REL, basename(rse)), overwrite = TRUE)

  # 9. Metadados
  data_val <- regmatches(metodo_txt, regexpr("[0-9]{2}/[0-9]{2}/[0-9]{4}", metodo_txt))
  n_evid <- sub(".*mapeando as ([0-9.]+) evidências.*", "\\1", metodo_txt)
  n_arq <- length(readLines(file.path(PAC, "CHECKSUMS_SHA256.csv"), warn = FALSE)) - 1L
  M <- c(area = A$nome, de_area = A$de_area, data_validacao_matriz = data_val, n_itens = n_itens,
         sistematica_evidencias = n_evid, obras_citadas = nrow(refs), rse_pdf = basename(rse),
         bib_pub = bib_pub, arquivos_pacote = n_arq, data_publicacao_drive = A$data_drive,
         rotulo_grupo = rotulo_grupo, datas_prisma = txt(datas_prisma), complementar_nao_cobre = if (length(nao_cobre)) nao_cobre[1] else "")
  if (econ) M <- c(M,
    data_deliberacao = "21/09/2026", data_regeracao = "23/09/2026", data_reverificacao = "24/09/2026",
    resumo_complementar = "87 documentos do acervo curado pela coordenação triados e 86 lidos, com 1.837 evidências extraídas",
    advertencia_complementar = paste(
      "A matriz complementar não é um mapa da economia do Pantanal mato-grossense.",
      "É o mapa do que um acervo determinado registra sobre ela, e esse acervo tem dono, data e recorte:",
      "sete dos treze municípios concentram quase toda a base, e são exatamente os visitados pelo programa",
      "Pró-Pantanal do SEBRAE-MT entre 2019 e 2022. Usada como diagnóstico regional, converteria silêncio",
      "documental em ausência de economia nos outros seis."))
  else {
    leia <- readLines(file.path(PAC, "LEIA-ME_ENTREGA.md"), encoding = "UTF-8", warn = FALSE)
    lc <- leia[grepl("^- a \\*\\*revisão documental complementar\\*\\*", leia)]
    confere(length(lc) == 1, "linha da revisão complementar no LEIA-ME do pacote")
    M <- c(M, resumo_complementar = sub("[.;]\\s*$", "", sub("^.*— ", "", lc)))
  }
  gravar(tibble(chave = names(M), valor = unname(M)), "metadados.csv")

  # 10. Cópias para download
  for (f in c("itens_fofa.csv", "citacoes_fofa.csv", "estudos.csv", "fofa_municipal.csv", "matriz_complementar.csv"))
    file.copy(file.path(OUT, f), file.path(ARQ, f), overwrite = TRUE)

  tibble(ordem = match(A$slug, AREAS$slug), slug = A$slug, area = A$nome, dimensao_fase_i = A$dim_fase_i,
         brutos = pv("total"), triados = pv("triados"), registros = pv("corpus"), estudos = nrow(bib),
         itens = n_itens, citacoes = nrow(cit), complementar = nrow(comp), data_validacao = data_val,
         data_drive = A$data_drive, arquivos = n_arq,
         documentado = sum(fofa_mun$status == "DOCUMENTADO"), regional = sum(fofa_mun$status == "REGIONAL"),
         inferido = sum(fofa_mun$status == "INFERIDO"), contraindicado = sum(fofa_mun$status == "CONTRAINDICADO"),
         nao_avaliado = sum(fofa_mun$status == "NÃO AVALIADO"))
}

resumo <- bind_rows(lapply(seq_len(nrow(AREAS)), function(i) preparar_area(AREAS[i, ])))

# ---- Situação das áreas -------------------------------------------------------
# Transcrito dos registros de execução (pasta 06_Pesquisa/02_Revisao_FOFA): LEIA-ME de cada pacote,
# REGISTRO_EXECUCAO_INFRAESTRUTURA_20260924.md (decisão 31) e REGISTRO_DECISOES_CONJUNTAS_20260930.md (C6 a C8).
OBS <- c(
  economica = "Pacote deliberado pela coordenação em 21/09/2026, regerado (classe B da errata) em 23/09/2026 e reverificado em 24/09/2026.",
  infraestrutura = "Quadros FOFA validados em 30/09/2026; pacote deliberado pela coordenação em 01/10/2026 (decisão 31); documentos revisados e PDFs exportados pela coordenação.",
  `setor-publico` = "Executada em conjunto com as áreas sociodemográfica e ambiental (Fase B). Quadros FOFA validados em 04/10/2026; pacote deliberado em 07/10/2026 (decisão C8).",
  sociodemografica = "Executada em conjunto com as áreas do setor público e ambiental (Fase B). Quadros FOFA validados em 04/10/2026; pacote deliberado em 07/10/2026 (decisão C8).",
  ambiental = "Executada em conjunto com as áreas do setor público e sociodemográfica (Fase B). Quadros FOFA validados em 04/10/2026; pacote deliberado em 07/10/2026 (decisão C8). Dos 897 estudos, 214 lidos pelo texto completo, numa amostra estratificada, e os demais pelo resumo.")
situacao <- resumo |> mutate(situacao = "Concluída", observacao = unname(OBS[slug]))
data.table::fwrite(situacao, file.path(OUT_RAIZ, "situacao_areas.csv"), na = "")
print(as.data.frame(resumo[, c("area", "brutos", "triados", "registros", "estudos", "itens", "complementar", "arquivos")]))

# ---- Conferência final de conteúdo proibido ------------------------------
msg("\nVarredura de conteúdo não publicável")
arqs <- c(list.files(OUT_RAIZ, recursive = TRUE, full.names = TRUE), list.files(ARQ_RAIZ, recursive = TRUE, full.names = TRUE))
for (f in arqs) {
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  if (any(grepl("^\\s*abstract\\s*=", x, ignore.case = TRUE))) stop("campo abstract em ", f)
  if (any(grepl("[A-Za-z0-9._-]+@[A-Za-z0-9-]+\\.[A-Za-z.]+", x) & !grepl("doi|https?://", x))) stop("e-mail em ", f)
  if (any(grepl("\\(?\\b[0-9]{2}\\)?\\s?9?[0-9]{4}-[0-9]{4}\\b", x) & !grepl("doi|10\\.[0-9]{4}", x))) msg("  atenção: padrão de telefone em %s", f)
  if (any(grepl("EVC-[0-9]", x))) stop("identificador EVC em ", f)
  if (any(grepl("drive\\.google|1tPjVgXbHVDYmY48I0H9NzWrWTa", x))) stop("identificador do Google Drive em ", f)
}
msg("Concluído: %d arquivos em %s e %s", length(arqs), OUT_RAIZ, ARQ_RAIZ)
