#!/usr/bin/env python3
"""Build replication.R from index.qmd.

Writes every executable R chunk of the manuscript, in order, to
replication.R, with a comment giving the chunk label and the section it
belongs to.  Figure chunks are wrapped so that each figure is written to
replication-figures/<label>.pdf at the size used in the manuscript.  The
quantities that the manuscript computes inline (`r ...`) are printed at the
end of the script.  Run with:  python3 make-replication.py
"""
import re

SRC, OUT = "index.qmd", "replication.R"
DEFAULT_W, DEFAULT_H = 4.9, 3.675   # jss-pdf defaults

src = open(SRC, encoding="utf-8").read()
title = re.search(r'^title-plain:\s*"(.*)"', src, re.M).group(1)
body = src.split("\n---\n", 1)[1] if src.startswith("---\n") else src

out = [f"""## Replication script for the manuscript
##   "{title}"
##   submitted to the Journal of Statistical Software
##
## This script is generated from the Quarto source of the manuscript
## (index.qmd) and contains every code chunk of the paper in order.  The
## comment before each chunk gives the chunk label and the manuscript section
## it belongs to.  Figures are written to replication-figures/ as PDF files
## named after the figure labels used in the manuscript.  The quantities that
## the text reports inline are printed at the end of the script.
##
## Requirements: R (>= 4.1.0) and the packages posc (>= 0.2.0), ggplot2,
## patchwork, geomtextpath, knitr and MASS, all available from CRAN.
## Running time: about two minutes, most of it in the bootstrap fits.
##
## Usage:  Rscript replication.R

dir.create("replication-figures", showWarnings = FALSE)
"""]

section = ""
inline_items = []   # (section, expression)
lines = body.split("\n")
i = 0
while i < len(lines):
    line = lines[i]
    m = re.match(r"^(##+)\s+(.*?)\s*(\{.*\})?\s*$", line)
    if m:
        level = len(m.group(1)) - 1
        section = ("Section " if level == 1 else "Subsection ") + m.group(2)
        i += 1
        continue
    if re.match(r"^```\{r[^}]*\}\s*$", line):
        opts, code = {}, []
        i += 1
        while i < len(lines) and not lines[i].startswith("```"):
            om = re.match(r"^#\|\s*([\w-]+):\s*(.*)$", lines[i])
            if om:
                opts[om.group(1)] = om.group(2).strip()
            else:
                code.append(lines[i])
            i += 1
        i += 1  # closing fence
        if opts.get("eval", "true").lower() == "false":
            continue
        while code and not code[0].strip():
            code.pop(0)
        while code and not code[-1].strip():
            code.pop()
        label = opts.get("label", "unlabelled")
        header = f"## ---- {label}"
        if section:
            header += f"  ({section})"
        if label == "setup":
            header += "\n## Options and helper functions used throughout the manuscript."
        out.append("\n" + header + "\n")
        if label.startswith("fig-"):
            w = float(opts.get("fig-width", DEFAULT_W))
            h = float(opts.get("fig-height", DEFAULT_H))
            out.append(f'cairo_pdf("replication-figures/{label}.pdf", '
                       f"width = {w:g}, height = {h:g})\n")
            out.append("\n".join(code) + "\n")
            out.append("invisible(dev.off())\n")
        else:
            out.append("\n".join(code) + "\n")
        continue
    # inline R code in prose
    for im in re.finditer(r"`r\s+(.+?)`", line):
        inline_items.append((section, im.group(1)))
    i += 1

if inline_items:
    out.append("\n## ---- Quantities reported inline in the text\n"
               "## In the order in which they appear in the manuscript.\n")
    last = None
    for sec, expr in inline_items:
        if sec != last:
            out.append(f"\n## {sec}\n")
            last = sec
        out.append(f"print({expr})\n")

open(OUT, "w", encoding="utf-8").write("".join(out))
print(f"wrote {OUT}: {sum(1 for l in open(OUT))} lines, "
      f"{len(inline_items)} inline quantities")
