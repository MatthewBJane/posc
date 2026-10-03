#!/usr/bin/env bash
# Assemble the JSS submission from the rendered manuscript and the package.
#
#   cd ~/posc/paper
#   quarto render                 # PDF (+ LaTeX source) into _manuscript/
#   Rscript replication.R         # check that the replication script runs
#   bash make-submission.sh       # -> submission/posc-jss-submission.zip
#
# The zip contains what the JSS submission form asks for:
#   posc.pdf           the manuscript
#   source/            LaTeX source, bibliography and figures (keep-tex output)
#   posc_<ver>.tar.gz  the package source, as built for CRAN
#   replication.R      standalone script reproducing every result and figure
set -euo pipefail
cd "$(dirname "$0")"

pdf=_manuscript/index.pdf
tex=_manuscript/_tex
[ -f "$pdf" ] || { echo "No $pdf; run 'quarto render' first." >&2; exit 1; }
[ -f "$tex/index.tex" ] || { echo "No $tex/index.tex; keep-tex output missing." >&2; exit 1; }

rm -rf submission
mkdir -p submission/source
cp "$pdf" submission/posc.pdf
cp "$tex/index.tex" submission/source/posc.tex
cp "$tex/references.bib" "$tex/jss.cls" "$tex/jss.bst" "$tex/jsslogo.jpg" submission/source/
cp -R "$tex/index_files" submission/source/
cp replication.R submission/

# Package source tarball, built from the package root (the parent folder).
( cd .. && R CMD build --no-manual . >/dev/null )
tarball=$(ls -t ../posc_*.tar.gz | head -n 1)
mv "$tarball" submission/

( cd submission && zip -qr posc-jss-submission.zip posc.pdf source posc_*.tar.gz replication.R )
echo "Submission assembled in paper/submission/:"
ls -l submission
