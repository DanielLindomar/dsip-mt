#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
03_extrair_figuras.py
Extrai as figuras dos seis Relatorios Parciais I (DOCX) e grava:
  dimensoes/figuras/D1/fig-01.png ... dimensoes/figuras/D6/fig-09.png
  dimensoes/figuras/figuras.csv  (dim, arquivo, ordem, numero, legenda, fonte)

Metodo: le word/document.xml de cada DOCX (zipfile + xml.etree, sem dependencias
externas), percorre os paragrafos na ordem do documento, localiza cada imagem
(a:blip r:embed -> word/_rels/document.xml.rels -> word/media/*) e associa:
  - legenda: paragrafo "Figura X.Y: ..." (estilo ImageCaption) imediatamente
    depois da imagem (padrao dos relatorios) ou, na falta, imediatamente antes;
  - fonte: paragrafo "Fonte: ..." logo depois da legenda, quando houver.
    (Nos DOCX atuais as figuras nao trazem linha de fonte propria; as linhas
    "Fonte:" existentes pertencem as tabelas e NAO sao atribuidas a figuras.)

Somente leitura sobre o Drive. Uso:
  python scripts/03_extrair_figuras.py [pasta_docx]
"""
import csv
import re
import sys
import zipfile
import xml.etree.ElementTree as ET
from pathlib import Path

NS = {
    "w": "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
    "a": "http://schemas.openxmlformats.org/drawingml/2006/main",
    "r": "http://schemas.openxmlformats.org/officeDocument/2006/relationships",
    "rel": "http://schemas.openxmlformats.org/package/2006/relationships",
}
W = "{%s}" % NS["w"]
R_EMBED = "{%s}embed" % NS["r"]

PASTA_DOCX_PADRAO = Path(
    r"C:\Users\lindo\Meu Drive (lindomar.pegorini@unemat.br)\_RESEARCH"
    r"\(26-27) PROJETO SUDECO\07_Entrega_Fase_I\03_Relatorios\DOCX"
)
RAIZ = Path(__file__).resolve().parent.parent
SAIDA = RAIZ / "dimensoes" / "figuras"

ESPERADO = {"D1": 25, "D2": 6, "D3": 9, "D4": 8, "D5": 6, "D6": 9}
RE_LEGENDA = re.compile(r"^\s*Figura\s+(\d+(?:\.\d+)?)\s*[:.\-\u2013\u2014]\s*(.*\S)\s*$")
RE_FONTE = re.compile(r"^\s*Fonte\s*:", re.IGNORECASE)


def texto(p):
    return "".join(t.text or "" for t in p.iter(W + "t")).strip()


def imagens_do_paragrafo(p):
    return [b.get(R_EMBED) for b in p.iter("{%s}blip" % NS["a"]) if b.get(R_EMBED)]


def proximo_nao_vazio(paras, i, passo, limite=3):
    """Devolve o indice do proximo paragrafo com texto ou imagem (ate `limite` saltos)."""
    j = i
    for _ in range(limite):
        j += passo
        if j < 0 or j >= len(paras):
            return None
        if texto(paras[j]) or imagens_do_paragrafo(paras[j]):
            return j
    return None


def extrair_docx(caminho):
    with zipfile.ZipFile(caminho) as z:
        rels = ET.fromstring(z.read("word/_rels/document.xml.rels"))
        alvo = {
            r.get("Id"): r.get("Target")
            for r in rels.findall("rel:Relationship", NS)
            if r.get("Type", "").endswith("/image")
        }
        doc = ET.fromstring(z.read("word/document.xml"))
        paras = list(doc.find(W + "body").iter(W + "p"))
        itens = []
        for i, p in enumerate(paras):
            for rid in imagens_do_paragrafo(p):
                destino = alvo.get(rid)
                if not destino:
                    continue
                destino = destino.lstrip("/")
                if not destino.startswith("word/"):
                    destino = "word/" + destino
                legenda = numero = fonte = None
                # legenda depois (padrao) ou antes
                for passo in (1, -1):
                    j = proximo_nao_vazio(paras, i, passo)
                    if j is None or imagens_do_paragrafo(paras[j]):
                        continue
                    m = RE_LEGENDA.match(texto(paras[j]))
                    if m:
                        numero, legenda = m.group(1), m.group(2)
                        if passo == 1:
                            k = proximo_nao_vazio(paras, j, 1, limite=2)
                            if k is not None and RE_FONTE.match(texto(paras[k])):
                                fonte = texto(paras[k])
                        break
                itens.append(
                    {
                        "destino": destino,
                        "bytes": z.read(destino),
                        "numero": numero or "",
                        "legenda": legenda or "",
                        "fonte": fonte or "",
                    }
                )
        return itens


def main():
    pasta = Path(sys.argv[1]) if len(sys.argv) > 1 else PASTA_DOCX_PADRAO
    arquivos = sorted(pasta.glob("D[1-6]_*.docx"))
    if not arquivos:
        sys.exit(f"Nenhum DOCX D1..D6 em {pasta}")
    linhas, problemas = [], []
    for f in arquivos:
        dim = f.name.split("_")[0]
        itens = extrair_docx(f)
        pasta_dim = SAIDA / dim
        pasta_dim.mkdir(parents=True, exist_ok=True)
        for antigo in pasta_dim.glob("fig-*.png"):
            antigo.unlink()
        for ordem, it in enumerate(itens, start=1):
            ext = Path(it["destino"]).suffix.lower() or ".png"
            nome = f"fig-{ordem:02d}{ext}"
            (pasta_dim / nome).write_bytes(it["bytes"])
            if not it["numero"]:
                problemas.append(f"{dim} ordem {ordem}: sem legenda associada")
            linhas.append(
                {
                    "dim": dim,
                    "arquivo": f"{dim}/{nome}",
                    "ordem": ordem,
                    "numero": it["numero"],
                    "legenda": it["legenda"],
                    "fonte": it["fonte"],
                }
            )
        esp = ESPERADO.get(dim)
        status = "ok" if esp == len(itens) else f"DIVERGE (esperado {esp})"
        print(f"{dim}: {len(itens)} figuras [{status}]  <- {f.name}")
    SAIDA.mkdir(parents=True, exist_ok=True)
    with open(SAIDA / "figuras.csv", "w", newline="", encoding="utf-8") as fh:
        w = csv.DictWriter(
            fh, fieldnames=["dim", "arquivo", "ordem", "numero", "legenda", "fonte"]
        )
        w.writeheader()
        w.writerows(linhas)
    print(f"Total: {len(linhas)} figuras; manifesto em {SAIDA / 'figuras.csv'}")
    for p in problemas:
        print("AVISO:", p)


if __name__ == "__main__":
    main()
