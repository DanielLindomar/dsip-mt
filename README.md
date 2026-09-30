# Observatório DSIP-MT

Site de resultados do Projeto Desenvolvimento Sustentável e Inclusivo do Pantanal Norte Mato-grossense (DSIP-MT) — Convênio 984799/2025 SUDECO–UNEMAT.

Publicado em: https://daniellindomar.github.io/dsip-mt/

## Como atualizar

1. **Preparar os dados** (lê as bases congeladas no Google Drive e grava `dados_site/` e `arquivos/`). Rodar pelo PowerShell, na raiz do repositório:

   ```powershell
   & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/01_preparar_dados.R
   & "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" scripts/02_preparar_revisao.R
   python scripts/03_extrair_figuras.py
   ```

2. **Renderizar e conferir localmente:** `quarto preview`

3. **Publicar:** `quarto publish gh-pages` (renderiza e envia o HTML para o ramo `gh-pages`).

4. Registrar a mudança em "Novidades" (`index.qmd`) e fazer commit no ramo `main`.

## Estrutura

| Pasta/arquivo | Conteúdo |
|---|---|
| `scripts/` | Preparação de dados (Drive → repositório) e extração das figuras dos relatórios |
| `dados_site/` | Dados usados na renderização (`indicadores.csv` define os indicadores de cada página de município) |
| `arquivos/` | Downloads publicados: PDFs, bases CSV, dicionários, malha GeoJSON |
| `R/funcoes.R` | Funções compartilhadas (formatação pt-BR, cartões, tabelas, gráficos, mapa) |
| `municipios/_perfil.qmd` | Modelo das 13 páginas de município (as páginas `<slug>.qmd` são geradas pelo script 01) |
| `dimensoes/`, `revisao/`, `dados/`, `biblioteca/` | Demais seções |

## Regras de conteúdo

Não publicar: textos integrais de artigos de terceiros (PDFs do corpus), campos `abstract`/`keywords` do BibTeX, microdados com informação pessoal, IDs do Google Drive, e a base do IFDM (redistribuição depende da FIRJAN). Mapas com dados de rodovias levam "© OpenStreetMap contributors".

## Observação técnica

`readr` não é usado: a política de Controle de Aplicativos do Windows bloqueia a DLL do pacote `tzdb`. A leitura de CSV usa `data.table::fread`.
