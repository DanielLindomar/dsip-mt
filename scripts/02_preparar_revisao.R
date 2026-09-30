# =============================================================================
# 02_preparar_revisao.R — Observatório DSIP-MT · seção "Revisão de literatura"
# Lê o pacote da área econômica no Google Drive (somente leitura) e grava em
# dados_site/revisao/ e arquivos/revisao/ apenas o que é publicável e o que as
# páginas de revisao/ (e a página de município) usam na renderização.
#
# Rodar a partir da raiz do repositório (PowerShell):
#   & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/02_preparar_revisao.R
# (Não rodar pelo Bash do Git: o R falha com o caminho do Google Drive.)
#
# Não é publicado (direitos de terceiros e privacidade): PDFs e textos integrais
# do acervo, os campos abstract e keywords do .bib, a base de evidências
# (840 da revisão sistemática e 1.837 da complementar), os identificadores EVC
# da matriz complementar e qualquer identificador do Google Drive.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readxl); library(jsonlite); library(data.table)
})

DRIVE <- "C:/Users/lindo/Meu Drive (lindomar.pegorini@unemat.br)/_RESEARCH/(26-27) PROJETO SUDECO"
FOFA  <- file.path(DRIVE, "06_Pesquisa/02_Revisao_FOFA")
ECON  <- file.path(FOFA, "04_Entrega_Area_Economica")
ANX_S <- file.path(ECON, "05_Anexos/Revisao_Sistematica")
ANX_C <- file.path(ECON, "05_Anexos/Revisao_Complementar")
stopifnot(dir.exists(ECON))

OUT  <- "dados_site/revisao"
ARQ  <- "arquivos/revisao"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
dir.create(ARQ, recursive = TRUE, showWarnings = FALSE)

msg    <- function(...) cat(sprintf(...), "\n")
gravar <- function(x, f) data.table::fwrite(x, file.path(OUT, f), na = "")
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

# Ancoragem geográfica: um único conjunto de rótulos
ANCORAGEM <- c("Mato Grosso", "Mato Grosso (parcial)", "Pantanal Sul (MS) e outros")
norm_anc <- function(x) {
  k <- tolower(sem_acento(trimws(x)))
  out <- ifelse(grepl("parcial", k), ANCORAGEM[2],
         ifelse(grepl("ms|sul|outros", k), ANCORAGEM[3],
         ifelse(grepl("mato grosso|ancorado em mt", k), ANCORAGEM[1], NA)))
  if (anyNA(out)) stop("Ancoragem não reconhecida: ", paste(unique(x[is.na(out)]), collapse = ", "))
  out
}

# Município: chave sem acento, caixa, apóstrofo; "de Leverger" = "do Leverger"
chave_mun <- function(x) {
  k <- tolower(sem_acento(trimws(x)))
  k <- gsub("[^a-z0-9]+", " ", k)
  k <- gsub("\\bde leverger\\b", "do leverger", k)
  trimws(gsub("\\s+", " ", k))
}

primeira_maiuscula <- function(x) ifelse(is.na(x) | x == "", x, paste0(toupper(substr(x, 1, 1)), substring(x, 2)))

# ---- 1. Municípios do recorte -----------------------------------------------
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
# lista "caceres; pocone" -> nomes oficiais "Cáceres; Poconé"
nomes_oficiais <- function(x) vapply(x, function(s) {
  if (is.na(s) || trimws(s) == "") return("")
  p <- trimws(strsplit(s, ";")[[1]]); p <- p[p != ""]
  paste(nome_de(id_de(p)), collapse = "; ")
}, character(1), USE.NAMES = FALSE)

# ---- 2. BibTeX: parsing, versão sem resumos ----------------------------------
msg("BibTeX")
bib_f <- file.path(ECON, "04_Corpus_Bibliografico/Corpus_Economico_DSIP-MT.bib")
L <- readLines(bib_f, encoding = "UTF-8", warn = FALSE)
re_ini   <- "^@([A-Za-z]+)\\{([^,]+),\\s*$"
re_campo <- "^\\s+([A-Za-z]+)\\s*=\\s*\\{(.*)\\},?\\s*$"
estranhas <- which(!(grepl(re_ini, L) | grepl(re_campo, L) | L == "}" | L == "" | grepl("^%", L)))
confere(length(estranhas) == 0, "todas as linhas do .bib seguem o formato um campo por linha")

# Versão sem abstract e keywords (os campos ocupam uma linha cada)
tira <- grepl("^\\s+(abstract|keywords)\\s*=", L, ignore.case = TRUE)
L_pub <- L[!tira]
cab <- c("% Versão publicada no Observatório DSIP-MT sem os campos de resumo e de palavras-chave",
         "% (direitos de terceiros). Demais campos idênticos ao arquivo do pacote da área econômica.")
L_pub <- c(L_pub[grepl("^%", L_pub)][1:6], cab, L_pub[-(1:6)])
writeLines(enc2utf8(L_pub), file.path(ARQ, "Corpus_Economico_DSIP-MT_sem_resumos.bib"), useBytes = TRUE)
confere(!any(grepl("^\\s+(abstract|keywords)\\s*=", L_pub, ignore.case = TRUE)), "arquivo .bib publicado sem abstract/keywords")

desescapa <- function(x) {
  x <- gsub("\\\\([_&%$#])", "\\1", x)
  gsub("[{}]", "", x)
}
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
  ano       = as.integer(vapply(ents, campo, "", "year")),
  journal   = vapply(ents, campo, "", "journal"),
  booktitle = vapply(ents, campo, "", "booktitle"),
  howpub    = vapply(ents, campo, "", "howpublished"),
  volume    = vapply(ents, campo, "", "volume"),
  numero    = vapply(ents, campo, "", "number"),
  paginas   = vapply(ents, campo, "", "pages"),
  doi       = vapply(ents, campo, "", "doi"),
  url_bib   = vapply(ents, campo, "", "url"),
  idioma    = vapply(ents, campo, "", "language"),
  note      = vapply(ents, campo, "", "note"))
confere(nrow(bib) == 225, "225 entradas no .bib")
confere(!anyDuplicated(bib$chave), "chaves BibTeX únicas")
msg("  entradas com DOI: %d", sum(!is.na(bib$doi)))

# Autoria: "Sobrenome, Nome and ..." -> "Sobrenome, N. N.; ..." e forma curta autor-ano
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
  s <- sobrenomes(a)
  if (length(s) == 1) s else if (length(s) == 2) paste(s[1], "e", s[2]) else paste(s[1], "et al.")
}
bib <- bib |>
  mutate(autores   = vapply(author, function(a) paste(autores_lista(a), collapse = "; "), ""),
         autor_ano = paste0(vapply(author, autor_curto, ""), " (", ano, ")"),
         periodico = coalesce(journal, booktitle, howpub),
         url       = coalesce(ifelse(is.na(doi), NA, paste0("https://doi.org/", doi)), url_bib, ""),
         chave_interna = trimws(sub(";.*$", "", sub("^.*chave interna:\\s*", "", note))),
         redundantes   = ifelse(grepl("registros redundantes", note),
                                trimws(gsub("\\s*\\(G[0-9]+\\)", "", sub("^.*registros redundantes do mesmo estudo:\\s*", "", note))),
                                ""))

# ---- 3. Ancoragem por registro (analise_geografica.json) --------------------
msg("Ancoragem geográfica por registro")
geo <- jsonlite::fromJSON(file.path(FOFA, "03_Extracao_FOFA/analise_geografica.json"), simplifyVector = FALSE)
confere(geo$n_registros == 236 && geo$n_estudos_distintos == 225, "analise_geografica.json: 236 registros, 225 estudos distintos")
reg <- tibble(registro = names(geo$detalhe_corpus),
              cat      = vapply(geo$detalhe_corpus, function(d) d$cat, ""),
              mun13    = lapply(geo$detalhe_corpus, function(d) unlist(d$mun13)))
# registro -> entrada do .bib (chave interna + registros redundantes)
mapa <- bind_rows(
  bib |> transmute(registro = chave_interna, chave),
  bib |> filter(redundantes != "") |>
    transmute(chave, registro = strsplit(redundantes, "\\s*[;,]\\s*")) |> tidyr::unnest(registro)
)
confere(nrow(mapa) == 236 && !anyDuplicated(mapa$registro), "236 registros mapeados em 225 entradas do .bib (11 vínculos)")
confere(all(reg$registro %in% mapa$registro), "todos os registros da análise geográfica têm entrada no .bib")
reg <- left_join(reg, mapa, by = "registro")

est_mun <- reg |> select(chave, mun13) |> tidyr::unnest(mun13) |>
  filter(!is.na(mun13), mun13 != "") |>
  mutate(id_municipio = id_de(mun13)) |> distinct(chave, id_municipio)
# categoria geográfica por estudo: a do registro canônico
cat_est <- reg |> inner_join(bib |> select(chave, chave_interna), by = "chave") |>
  filter(registro == chave_interna) |> select(chave, cat)
ROT_CAT <- c(MT_municipio_do_recorte = "Cita município do recorte (MT)",
             MT_pantanal_norte = "Pantanal Norte (MT), sem município do recorte",
             MT_e_MS = "Pantanal de MT e MS",
             MS_pantanal_sul = "Pantanal Sul (MS)",
             Pantanal_generico_ou_indefinido = "Pantanal genérico ou indefinido",
             fora_do_Brasil = "Fora do Brasil")
mun_por_est <- est_mun |> mutate(nome = nome_de(id_municipio)) |>
  arrange(nome) |> group_by(chave) |> summarise(municipios = paste(nome, collapse = "; "), .groups = "drop")

estudos <- bib |>
  left_join(cat_est, by = "chave") |> left_join(mun_por_est, by = "chave") |>
  transmute(chave, autores, autor_ano, ano, titulo, periodico, volume, numero, paginas,
            doi, url, tipo, idioma,
            recorte_geografico = unname(ROT_CAT[cat]),
            municipios = coalesce(municipios, ""),
            chave_interna, registros_redundantes = redundantes) |>
  arrange(ano, autor_ano)
confere(nrow(estudos) == 225, "estudos.csv com 225 estudos")
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

# ---- 4. Matriz FOFA integrada (40 itens) ------------------------------------
msg("Matriz FOFA integrada")
mi <- read_excel(file.path(ECON, "02_Matrizes_FOFA/02_Matriz_Formato_Integrado.xlsx"), "Matriz_Integrada")
confere(nrow(mi) == 40 && ncol(mi) == 15, "Matriz_Integrada com 40 itens × 15 colunas")
anc_c <- data.table::fread(file.path(ANX_S, "Anexo_C_Ancoragem_geografica_dos_itens.csv"), encoding = "UTF-8", sep = ";") |>
  as_tibble() |>
  transmute(dimensao = norm_dim(Dimensao),
            item_id = sprintf("%s-%02d", PREFIXO[dimensao], as.integer(Item)),
            anc_c = norm_anc(`Ancoragem geografica`),
            conv_c = norm_conv(Convergencia),
            fontes_exibidas_mt = `Fontes MT / total`,
            mun_c = nomes_oficiais(`Municipios do recorte citados`))
itens <- mi |>
  transmute(item_id = ID, dimensao = norm_dim(`Dimensão`),
            ordem = as.integer(sub("^.*-", "", ID)),
            proposicao = `Item (proposição verificável)`,
            expressao_original = `Expressão original (matriz validada)`,
            citacao_principal = `Evidência (citação literal)`,
            fonte_principal = Fonte, local_principal = Local,
            lastro_estudos = as.integer(`Nº de fontes`),
            ancoragem = norm_anc(`Abrangência`),
            municipios_citados = nomes_oficiais(`Municípios do recorte citados`),
            confianca = norm_conf(`Confiança`),
            convergencia = norm_conv(`Convergência`),
            verificavel_campo = ifelse(tolower(sem_acento(`Verificável em campo`)) == "sim", "Sim", "Não"),
            ressalva = Ressalva, como_verificar = `Como verificar`) |>
  left_join(anc_c, by = c("item_id", "dimensao"))
confere(!anyNA(itens$anc_c), "Anexo C casa com os 40 itens da matriz")
confere(all(itens$ancoragem == itens$anc_c), "ancoragem da matriz = ancoragem do Anexo C, item a item")
confere(all(itens$convergencia == itens$conv_c), "convergência da matriz = convergência do Anexo C, item a item")
confere(all(itens$municipios_citados == itens$mun_c), "municípios citados da matriz = Anexo C, item a item")
itens <- itens |> select(-anc_c, -conv_c, -mun_c) |>
  mutate(dimensao = factor(dimensao, ORDEM_DIM)) |> arrange(dimensao, ordem) |>
  mutate(dimensao = as.character(dimensao))
confere(all(table(itens$dimensao) == 10), "10 itens por dimensão")

# ---- 5. Citações exibidas (Anexo A, 196) -------------------------------------
msg("Citações exibidas (Anexo A)")
qa <- read_excel(file.path(ANX_S, "Anexo_A_Quadros_FOFA_planilha.xlsx"), "Quadros_FOFA")
refs <- read_excel(file.path(ANX_S, "Anexo_A_Quadros_FOFA_planilha.xlsx"), "Referencias")
confere(nrow(qa) == 196, "196 citações exibidas no Anexo A")
confere(nrow(refs) == 114, "114 obras citadas (aba Referencias)")
qa <- qa |> tidyr::fill(`Dimensão`, `Nº`, `Palavra-chave / expressão`, `Convergência`, .direction = "down")
cit <- qa |>
  transmute(dimensao = norm_dim(`Dimensão`),
            item_id = sprintf("%s-%02d", PREFIXO[dimensao], as.integer(`Nº`)),
            expressao = `Palavra-chave / expressão`,
            autor_ano = `Fonte (obra utilizada)`,
            citacao = `Citação (trecho de onde saiu o resultado)`,
            local = Local, chave_fonte = `Chave da fonte`,
            conferencia = `Conferência`,
            vinculo_estudo = coalesce(`Vínculo de estudo`, "")) |>
  group_by(item_id) |> mutate(ordem_citacao = row_number()) |> ungroup()
chk <- cit |> distinct(item_id, expressao) |> left_join(itens |> select(item_id, expressao_original), by = "item_id")
confere(nrow(chk) == 40 && all(chk$expressao == chk$expressao_original), "Anexo A e matriz: mesma expressão original nos 40 itens")
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
confere(n_distinct(cit$item_id) == 40, "citações cobrem os 40 itens")
gravar(cit, "citacoes_fofa.csv")
msg("  citações com link: %d de %d", sum(cit$url != ""), nrow(cit))

itens <- itens |> left_join(cit |> count(item_id, name = "n_citacoes"), by = "item_id")
gravar(itens, "itens_fofa.csv")

# ---- 6. FOFA municipal (Anexo B da complementar) -----------------------------
msg("FOFA municipal")
mb <- read_excel(file.path(ANX_C, "Anexo_B_Matriz_Cruzada_Municipal.xlsx"), 1)
confere(nrow(mb) == 520, "520 linhas município × item")
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
confere(nrow(distinct(fofa_mun, id_municipio, item_id)) == 520, "13 × 40 combinações únicas")
chk_dim <- fofa_mun |> distinct(item_id, dimensao) |> left_join(itens |> select(item_id, d2 = dimensao), by = "item_id")
confere(all(chk_dim$dimensao == chk_dim$d2), "dimensão do Anexo B = dimensão da matriz")
fofa_mun <- fofa_mun |>
  mutate(dimensao_f = factor(dimensao, ORDEM_DIM)) |>
  arrange(municipio, dimensao_f, item_id) |> select(-dimensao_f)
gravar(fofa_mun, "fofa_municipal.csv")
print(table(fofa_mun$status))

# Tipologia (grupo e posição no bioma), da mesma planilha
tipologia <- mb |> distinct(`Município`, `Grupo econômico`, `Posição no bioma`) |>
  transmute(id_municipio = id_de(`Município`), municipio = nome_de(id_municipio),
            grupo_economico = `Grupo econômico`, posicao_bioma = `Posição no bioma`) |>
  left_join(mun |> select(id_municipio, slug), by = "id_municipio") |> arrange(municipio)
confere(nrow(tipologia) == 13, "tipologia com 13 municípios")
gravar(tipologia, "tipologia_municipal.csv")

# Diagnóstico (só informa; os dados não são alterados): a regra 1 da tipologia exige o
# mesmo grupo econômico E a mesma posição no bioma entre o município inferido e o de referência
pos <- setNames(tipologia$posicao_bioma, tipologia$id_municipio)
inf <- mb |> filter(Status == "INFERIDO") |>
  transmute(alvo = id_de(`Município`), refs = strsplit(trimws(`Referência`), "\\s*;\\s*"), crit = `Critério`)
inf$n_ref_mesma_pos <- mapply(function(a, r) sum(pos[as.character(id_de(r))] == pos[as.character(a)]), inf$alvo, inf$refs)
inf$n_ref_outra_pos <- mapply(function(a, r) sum(pos[as.character(id_de(r))] != pos[as.character(a)]), inf$alvo, inf$refs)
msg("  INFERIDO: %d linhas; critério nomeia só o grupo: %d; com ao menos um município de referência de outra posição no bioma: %d; sem nenhum de mesma posição: %d",
    nrow(inf), sum(!grepl("posição", inf$crit)), sum(inf$n_ref_outra_pos > 0), sum(inf$n_ref_mesma_pos == 0))

# ---- 7. Matriz complementar (69 itens, sem IDs EVC) --------------------------
msg("Matriz complementar")
md <- read_excel(file.path(ANX_C, "Anexo_D_Matriz_Complementar.xlsx"), 1)
confere(nrow(md) == 69, "69 itens na matriz complementar")
comp <- md |>
  transmute(codigo = `Cód.`, rodada = as.integer(Rodada), dimensao = norm_dim(`Dimensão`),
            proposicao = `Proposição verificável`,
            corresponde_a = ifelse(`Corresponde a` == "novo", "Item novo", `Corresponde a`),
            lacuna_tematica = ifelse(`Lacuna temática` %in% c("—", "-"), "", `Lacuna temática`),
            fontes_obs_independentes = as.integer(`Fontes (obs. indep.)`),
            documentos = as.integer(`Docs.`),
            abrangencia = c(municipal = "Municipal", intermunicipal = "Intermunicipal", `pantanal-mt` = "Pantanal-MT")[Abrangência] |> unname(),
            confianca = norm_conf(`Confiança`),
            convergencia = norm_conv(`Convergência`),
            auditoria = primeira_maiuscula(Auditoria),
            municipios_nomeados = coalesce(`Municípios nomeados`, ""),
            # a cláusula remete a evidências por ID (EVC-XXXX) em dois itens; a base de
            # evidências não é publicada, e o ID é retirado sem alterar o restante do texto
            clausula_insuficiencia = coalesce(`Cláusula de insuficiência`, "") |>
              gsub(pattern = ",\\s*EVC-[0-9]+", replacement = "") |>
              gsub(pattern = "\\s*\\(?EVC-[0-9]+\\)?", replacement = ""))
confere(!anyNA(comp$abrangencia) && !anyNA(comp$confianca), "rótulos da complementar normalizados")
confere(!any(grepl("EVC-", unlist(comp))), "matriz complementar sem identificadores EVC")
gravar(comp, "matriz_complementar.csv")

# ---- 8. PRISMA ---------------------------------------------------------------
msg("PRISMA")
pr <- read_excel(file.path(ANX_S, "Anexo_B_Triagem_e_PRISMA.xlsx"), "PRISMA", col_names = FALSE)
names(pr) <- c("rotulo", "valor")
v <- function(padrao) { x <- pr$valor[grepl(padrao, pr$rotulo)]; stopifnot(length(x) == 1); x }
num <- function(padrao) as.integer(v(padrao))
par <- function(padrao) as.integer(trimws(strsplit(v(padrao), "/")[[1]]))
capes <- num("^Brutos CAPES"); scielo <- num("^Brutos SciELO"); scopus <- num("^Brutos Scopus")
tit <- par("^Incluir / Excluir / Incerto"); r1 <- par("^R1"); r2 <- par("^R2"); r3 <- par("^R3")
prisma <- tibble::tribble(
  ~ordem, ~etapa, ~rotulo, ~valor,
  1, "Identificação", "Registros brutos — Periódicos CAPES", capes,
  2, "Identificação", "Registros brutos — SciELO", scielo,
  3, "Identificação", "Registros brutos — Scopus (termos em inglês)", scopus,
  4, "Identificação", "Total de registros brutos", capes + scielo + scopus,
  5, "Identificação", "Registros únicos após deduplicação", num("^Únicos após"),
  6, "Identificação", "Excluídos por estar fora do período 2011–2026", num("^Excluídos fora de"),
  7, "Triagem", "Registros submetidos à triagem", num("^Submetidos à triagem"),
  8, "Triagem", "Triagem por título — incluir", tit[1],
  9, "Triagem", "Triagem por título — excluir", tit[2],
  10, "Triagem", "Triagem por título — incerto", tit[3],
  11, "Triagem", "Triagem por resumo — incluídos (3 rodadas)", r1[1] + r2[1] + r3[1],
  12, "Triagem", "Triagem por resumo — excluídos (3 rodadas)", r1[2] + r2[2] + r3[2],
  13, "Inclusão", "Registros incluídos no corpus", num("^CORPUS INCLU"),
  14, "Inclusão", "Estudos distintos no corpus", nrow(bib),
  15, "Exclusão", "Excluídos por título", num("^Excluídos por título"),
  16, "Exclusão", "Excluídos por resumo", num("^Excluídos por resumo"),
  17, "Exclusão", "Excluídos por identificação da fonte", num("^Excluídos por identificação"),
  18, "Exclusão", "Excluídos por duplicata do mesmo estudo", num("^Excluídos por duplicata"))
g <- function(i) prisma$valor[prisma$ordem == i]
confere(g(4) == 1378, "1.378 registros brutos")
confere(g(7) == 839, "839 registros triados")
confere(g(5) - g(6) == g(7), "únicos − fora do período = triados")
confere(g(13) == 236 && g(14) == 225, "236 registros, 225 estudos distintos")
confere(g(13) + g(15) + g(16) + g(17) + g(18) == g(7), "incluídos + excluídos = triados")
confere(g(11) == g(13), "incluídos por resumo = corpus")
gravar(prisma, "prisma.csv")

# ---- 9. Situação das áreas e metadados do pacote -----------------------------
# Transcrito dos registros de execução (pasta 06_Pesquisa/02_Revisao_FOFA):
# LEIA-ME_ENTREGA.md (econômica), REGISTRO_EXECUCAO_INFRAESTRUTURA_20260924.md
# (decisões 19, 25 e 26; diário de 30/09/2026) e REGISTRO_DECISOES_CONJUNTAS_20260930.md (C1, C2).
fase_b_obs <- paste(
  "Execução conjunta com as outras duas áreas da Fase B, em modo completo (Etapas 1 a 9).",
  "Preparação aprovada pela coordenação em 30/09/2026 (decisão C1) e coleta nas bases CAPES, SciELO e Scopus",
  "feita em 30/09/2026 (decisão C2). Calendário previsto na preparação: G-lista em 09/10/2026,",
  "G-quadros em 27/10/2026, G-pacote em 01/12/2026 e publicação até 04/12/2026.")
situacao <- tibble::tribble(
  ~ordem, ~area, ~dimensao_fase_i, ~situacao, ~marco, ~data, ~observacao,
  1, "Econômica", "D1 · PESM", "Concluída",
  "Matriz FOFA de 40 itens validada pela coordenação", "24/08/2026",
  "Pacote deliberado pela coordenação em 21/09/2026 e regerado (classe B da errata) em 23/09/2026, com reverificação em 24/09/2026. Revisão sistemática (236 registros, 225 estudos distintos) e revisão documental complementar (69 itens) mantidas separadas e cruzadas.",
  2, "Infraestrutura", "D5 · PIM", "Em execução",
  "Quadros FOFA aprovados no gate G-quadros (1º marco)", "28/09/2026",
  "Validação condicional aplicada e reapresentação aprovada em 30/09/2026 (quadro com 37 itens: forças 8, oportunidades 9, fraquezas 10, ameaças 10). Busca documental dirigida na internet com o lote 1 autorizado em 30/09/2026. Falta o 2º marco (G-pacote): revisão complementar, tipologia, FOFA municipal e pacote publicado.",
  3, "Setor Público", "D6 · PGPM", "Em execução (fase inicial)",
  "Documento de preparação aprovado (decisão C1)", "30/09/2026", fase_b_obs,
  4, "Sociodemográfica", "D3 · PSDM", "Em execução (fase inicial)",
  "Documento de preparação aprovado (decisão C1)", "30/09/2026", fase_b_obs,
  5, "Ambiental", "D4 · PAM", "Em execução (fase inicial)",
  "Documento de preparação aprovado (decisão C1)", "30/09/2026", fase_b_obs)
gravar(situacao, "situacao_areas.csv")

rse <- list.files("arquivos/relatorios", pattern = "^RSE_.*\\.pdf$")
confere(length(rse) == 1, "um PDF do RSE em arquivos/relatorios")
metadados <- tibble::tribble(
  ~chave, ~valor,
  "data_validacao_matriz", "24/08/2026",
  "data_deliberacao", "21/09/2026",
  "data_regeracao", "23/09/2026",
  "data_reverificacao", "24/09/2026",
  "arquivos_pacote", "397",
  "rse_pdf", rse,
  "complementar_docs_triados", "87",
  "complementar_docs_lidos", "86",
  "complementar_evidencias", "1837",
  "sistematica_evidencias", "840",
  "obras_citadas", as.character(nrow(refs)),
  "municipios_visitados_ate_21_09_2026", "0",
  "advertencia_complementar",
  paste("A matriz complementar não é um mapa da economia do Pantanal mato-grossense.",
        "É o mapa do que um acervo determinado registra sobre ela, e esse acervo tem dono, data e recorte:",
        "sete dos treze municípios concentram quase toda a base, e são exatamente os visitados pelo programa",
        "Pró-Pantanal do SEBRAE-MT entre 2019 e 2022. Usada como diagnóstico regional, converteria silêncio",
        "documental em ausência de economia nos outros seis."))
gravar(metadados, "metadados_pacote.csv")

# ---- 10. Cópias para download no site (arquivos/revisao) ---------------------
for (f in c("itens_fofa.csv", "citacoes_fofa.csv", "estudos.csv", "fofa_municipal.csv", "matriz_complementar.csv"))
  file.copy(file.path(OUT, f), file.path(ARQ, f), overwrite = TRUE)

# ---- 11. Conferência final de conteúdo proibido ------------------------------
msg("Varredura de conteúdo não publicável")
arqs <- c(list.files(OUT, full.names = TRUE), list.files(ARQ, full.names = TRUE))
for (f in arqs) {
  x <- readLines(f, encoding = "UTF-8", warn = FALSE)
  if (any(grepl("abstract", x, ignore.case = TRUE))) stop("'abstract' em ", f)
  if (any(grepl("@(gmail|hotmail|yahoo|outlook)\\.", x, ignore.case = TRUE))) stop("e-mail em ", f)
  if (any(grepl("\\(?\\b[0-9]{2}\\)?\\s?9?[0-9]{4}-[0-9]{4}\\b", x) & !grepl("doi|10\\.[0-9]{4}", x))) msg("  atenção: padrão de telefone em %s", f)
  if (any(grepl("EVC-[0-9]", x))) stop("identificador EVC em ", f)
}
msg("Concluído: %d arquivos em %s e %s", length(arqs), OUT, ARQ)
