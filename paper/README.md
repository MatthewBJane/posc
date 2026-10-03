# posc manuscript (Journal of Statistical Software)

Quarto manuscript for the `posc` package in the JSS house style, using the
[quarto-journals/jss](https://github.com/quarto-journals/jss) extension. This
folder is listed in the package's `.Rbuildignore`, so it never becomes part of
the CRAN package.

## One-time setup

In a terminal, **inside this folder** (`cd ~/posc/paper` first; Quarto installs
extensions into the current folder):

```sh
quarto add quarto-journals/jss     # JSS formats; commit the _extensions folder
quarto install tinytex              # LaTeX, if you do not already have one
```

In R:

```r
devtools::install("~/posc")                      # the package itself
install.packages(c("ggplot2", "patchwork", "geomtextpath", "knitr"))
```

## Render

```sh
quarto render          # PDF + HTML into _manuscript/
quarto preview         # live preview in the browser
```

`_manuscript/index.pdf` is the JSS-style PDF for submission (and for Zenodo);
`_manuscript/index.html` is the web page, with the PDF linked in its sidebar.
Render from the terminal (or open this folder as its own RStudio project):
RStudio's Render button on `index.qmd` renders the file alone and skips the
manuscript project settings.

Results are cached in `_freeze/`, so a text-only edit re-renders in seconds;
delete `_freeze/` to recompute the figures (the two bootstrap fits take about
a minute). Re-install the package after changing it so the paper uses the
current code.

## Publish to the package website (GitHub Pages)

The package website is a pkgdown site in `docs/`, served by GitHub Pages
from the `main` branch. The manuscript is part of it, at `/paper/` (HTML
with a link to the PDF). From the package root:

```sh
bash dev/build-site.sh     # pkgdown + quarto render + copy into docs/paper/
git add docs && git commit -m "Update website" && git push
```

## JSS submission checklist

JSS asks for three things: the manuscript PDF, the software, and replication
materials that reproduce every result. Everything is assembled by one script:

```sh
cd ~/posc/paper
quarto render                  # PDF and LaTeX source into _manuscript/
Rscript replication.R          # must run cleanly; figures go to replication-figures/
bash make-submission.sh        # -> submission/posc-jss-submission.zip
```

The zip contains `posc.pdf`, `source/` (LaTeX source, bibliography, figures
and the JSS class files), `posc_<version>.tar.gz` (the package as built for
CRAN) and `replication.R`. Upload it, with the PDF, through the submission
form at <https://www.jstatsoft.org/> (Make a Submission).

`replication.R` is generated from `index.qmd` by `python3 make-replication.py`,
which writes every code chunk in order with the section it belongs to, saves
each figure to a PDF at the size used in the manuscript, and prints the
quantities the text reports inline. Regenerate it whenever a code chunk in
`index.qmd` changes.

House-style points already handled: `[R]{.proglang}`, `[posc]{.pkg}` and
`[posc]{.class}` markup, including in the title and keywords; inline code
typeset with `\code{}` in the PDF (`jss-code.lua`); sentence-case section
headings; captions in sentence style ending with a period; code shown with the
`R> ` prompt, spaces around operators and no comments inside code chunks;
`library("posc")` with quotes; title-case entries in `references.bib`; a
Computational details section with the versions used.
