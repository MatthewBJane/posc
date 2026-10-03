#!/usr/bin/env bash
# Build the package website into docs/ (served by GitHub Pages from main:/docs).
#
#   bash dev/build-site.sh        # from the package root
#
# 1. pkgdown builds the reference pages, README home page and NEWS into docs/.
# 2. Quarto renders the manuscript (HTML with a PDF link, and the PDF) into
#    paper/_manuscript/, which is copied to docs/paper/ without the LaTeX
#    sources.
# Then commit docs/ and push.
set -euo pipefail
cd "$(dirname "$0")/.."

Rscript -e 'pkgdown::build_site(preview = FALSE)'
( cd paper && quarto render )
mkdir -p docs/paper
rsync -a --delete --exclude _tex --exclude '*.ipynb' --exclude '*.qmd' \
      --exclude index-preview.html paper/_manuscript/ docs/paper/
touch docs/.nojekyll
echo "Site built in docs/. Open docs/index.html to check it, then commit docs/."
