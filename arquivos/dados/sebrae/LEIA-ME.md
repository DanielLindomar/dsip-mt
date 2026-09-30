# Dados complementares — Observatório Setorial Territorial do Sebrae (OST)

Coleta em 30/09/2026 para os 141 municípios de Mato Grosso (Projeto DSIP-MT).

## Origem

- Plataforma: Observatório Setorial Territorial do Sebrae — https://observatorio.sebrae.com.br/ (desenvolvido pela Datawheel).
- Acesso: API pública Tesseract OLAP do OST (`https://apiv2-observatorio.sebrae.com.br/tesseract/`), filtro `State:51` (Mato Grosso).
- Script: `coletar_sebrae.py` (Python 3.12, só biblioteca padrão). Cada consulta, com a URL completa, está em `consultas.csv`.
- A chave `id_municipio` é o código IBGE de 7 dígitos, igual à das bases da Fase I. Os indicadores per capita usam a população de `d3_demografico.csv` da Fase I (mesmo ano; na falta, ano vizinho).

## Termos de uso

O OST declara "Todos os direitos reservados" e não publica licença de reuso; dúvidas: apoiodados@sebrae.com.br. Os dados abaixo vêm, em sua maioria, de fontes oficiais públicas (ANATEL, MTur, Receita Federal, BCB, MJSP/Sinesp, IBGE), republicadas pelo OST. Regra adotada no projeto:

- **Fontes oficiais públicas:** podem ser usadas e redistribuídas com citação da fonte original e da forma de acesso ("via Observatório Sebrae").
- **ISDEL** (índice calculado pelo próprio Sebrae): uso interno e exibição com citação "© Sebrae"; **não redistribuir** o arquivo sem autorização.

## Arquivos

`01_brutos/` — respostas da API em CSV (uma por consulta). `02_tratados/` — bases no formato id_municipio × ano.

| Base tratada | Fonte original | Anos | Cobertura MT / PN-13 | Observação |
|---|---|---|---|---|
| sebrae_conectividade.csv | ANATEL — Índice Brasileiro de Conectividade (IBC) e componentes | 2021–2024 | 141 / 13 | IBC de 0 a 100 |
| sebrae_turismo_cadastur.csv | MTur — CadasTur (prestadores de serviços turísticos) | 2017–2026 | 133 / 11 | Empresas (contagem distinta), leitos, UH; 2026 parcial |
| sebrae_turismo_mapa.csv | MTur — Mapa do Turismo Brasileiro (economia do turismo) | 2016–2019 | 141 / 13 | Empregos e estabelecimentos do turismo, visitas estimadas, arrecadação; região turística e cluster (A–E) |
| sebrae_turismo_categorizacao.csv | MTur — categorização dos municípios turísticos | retrato | 59 / 7 | Categoria e 10 índices |
| sebrae_empresas_ativas.csv | Receita Federal — CNPJ (situação "Ativa") | retrato em 30/09/2026 | 141 / 13 | Por porte (MEI, ME, EPP, demais) e grande setor |
| sebrae_aberturas_empresas.csv | Receita Federal — CNPJ (data de início de atividade) | 2000–2026 | 141 / 13 | Todas as situações cadastrais atuais; 2026 parcial (até set.) |
| sebrae_compras_publicas.csv | PNCP — compras públicas homologadas (comprador no município) | 2023–2026 | 27 / 4 | **Cobertura insuficiente**; não usar para comparação |
| sebrae_agencias_bancarias.csv | Banco Central — agências e postos | 2021–2026 | 141 / 13 | Último mês de cada ano (2026: jun.) |
| sebrae_homicidios.csv | MJSP — Sinesp (homicídios) | 2018–2022 | 141 / 13 | Taxa por 100 mil hab. calculada com a população da Fase I |
| sebrae_censo_ocupacao.csv | IBGE — Censo 2022 (condição de ocupação) | 2022 | 141 / 13 | Taxas de desocupação e de participação |
| sebrae_isdel.csv | Sebrae — ISDEL (© Sebrae) | 2015–2021 | 141 / 13 | Índice geral e 5 dimensões; não redistribuir |

**Não coletado:** agricultura familiar (MDA — CAF): os cubos `MDA_CAF` e `MDA_CAF_Renda` retornaram erro HTTP 500 no servidor do OST em 30/09/2026; repetir a coleta depois. ENEM (`INEP_enem`) não foi usado: a média municipal muda de escala a partir de 2020 (≈500 → ≈330), o que indica mudança de tratamento na fonte. A complexidade econômica (`ECI New`) não tem recorte municipal.

## Uso no projeto

Os indicadores selecionados entram no Observatório DSIP-MT (https://daniellindomar.github.io/dsip-mt/) nas dimensões D1 (empresas, ISDEL), D2 (turismo), D3 (homicídios, ocupação) e D5 (conectividade, agências), identificados como "OST" e com a fonte original na nota.
