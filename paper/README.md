# posc full manuscript

Quarto manuscript for the `posc` package: the full technical write-up of
the probability of outcome superiority curve and its implementation. It is
rendered to HTML and PDF and published on the package website at
<https://matthewbjane.com/posc/paper/>. It is not a journal submission; the
short paper for the Journal of Open Source Software lives in `joss/` at the
package root. This folder is listed in the package's `.Rbuildignore`, so it
never becomes part of the CRAN package.

## One-time setup

Install [Quarto](https://quarto.org/) and, for the PDF, a LaTeX
distribution:

```sh
quarto install tinytex              # LaTeX, if you do not already have one
```

In R:

```r
devtools::install("~/posc")                      # the package itself
install.packages(c("ggplot2", "patchwork", "geomtextpath", "knitr", "MASS"))
```

## Render

In a terminal, **inside this folder** (`cd ~/posc/paper` first):

```sh
quarto render          # HTML + PDF into _manuscript/
quarto preview         # live preview in the browser
```

`_manuscript/index.html` is the web page, with the PDF linked in its
sidebar; `_manuscript/index.pdf` is the PDF. Render from the terminal (or
open this folder as its own RStudio project): RStudio's Render button on
`index.qmd` renders the file alone and skips the manuscript project
settings.

Results are cached in `_freeze/`, so a text-only edit re-renders in seconds;
delete `_freeze/` to recompute the figures (the bootstrap fits take about
two minutes). Re-install the package after changing it so the manuscript
uses the current code.

## Publish to the package website (GitHub Pages)

The package website is a pkgdown site in `docs/`, served by GitHub Pages
from the `main` branch. The manuscript is part of it, at `/paper/` (HTML
with a link to the PDF). From the package root:

```sh
bash dev/build-site.sh     # pkgdown + quarto render + copy into docs/paper/
git add docs && git commit -m "Update website" && git push
```

## Replication script

`replication.R` is a standalone script that reproduces every result and
figure in the manuscript. It is generated from `index.qmd`:

```sh
cd ~/posc/paper
python3 make-replication.py    # regenerate replication.R from index.qmd
Rscript replication.R          # run it; figures go to replication-figures/
```

`make-replication.py` writes every code chunk in order with the section it
belongs to, saves each figure to a PDF at the size used in the manuscript,
and prints the quantities the text reports inline. Regenerate the script
whenever a code chunk in `index.qmd` changes.
