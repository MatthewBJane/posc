# Release checklist: GitHub, website, CRAN, JSS

Run in this order. Steps 1 and 2 must finish before CRAN (step 3), because
CRAN checks the website URL in DESCRIPTION.

## 1. Final checks and push to GitHub

```r
# R, working directory ~/posc
devtools::document()
devtools::check(cran = TRUE)        # expect 0 errors, 0 warnings, 1 note (new submission)
devtools::install()                 # the paper and website use the installed package
```

```sh
cd ~/posc
git add -A
git commit -m "posc 0.2.0: CRAN release, JSS manuscript and website"
git push origin main
```

## 2. Build and publish the website

```sh
cd ~/posc
Rscript -e 'install.packages("pkgdown")'    # once
bash dev/build-site.sh                       # pkgdown + quarto render -> docs/
open docs/index.html                         # look at home, Reference, Paper
git add -A && git commit -m "Build website" && git push
```

On GitHub: repository **Settings > Pages > Build and deployment**: Source
"Deploy from a branch", branch `main`, folder `/docs`, Save. After a minute
check <https://matthewbjane.github.io/posc/> and the Paper menu (HTML and PDF).

## 3. CRAN

```r
# R, working directory ~/posc
spelling::spell_check_package()     # install.packages("spelling") if needed
urlchecker::url_check()             # install.packages("urlchecker") if needed
devtools::check_win_devel()         # results by e-mail, 15-30 min
devtools::check_mac_release()       # results by e-mail
```

When the e-mails come back clean, record them in `cran-comments.md` (it
already lists these environments; adjust if anything differs), commit, then:

```r
devtools::submit_cran()
```

This builds the tarball, asks a few yes/no questions and uploads it. CRAN
sends a confirmation e-mail to matthewbjane@gmail.com; the submission is not
in the queue until you click the link in it. Expect an automated reply within
an hour and a human decision within a few days. If CRAN asks for changes,
edit, bump nothing (same version is fine for a resubmission), add a note at
the top of `cran-comments.md` describing what changed, and submit again.

After acceptance:

```r
usethis::use_github_release()       # tags v0.2.0 using NEWS.md
```

## 4. JSS

```sh
cd ~/posc/paper
quarto render                       # fresh PDF after the final text edits
Rscript replication.R               # must run end to end; check replication-figures/
bash make-submission.sh             # -> submission/posc-jss-submission.zip
```

Read `submission/posc.pdf` once more (title page, address, a caption with
code, the reference list).

Submit at <https://www.jstatsoft.org/> > **Make a Submission** (register or
log in; ORCID optional). The form asks for:

1. Section: *Article*. Title and abstract: paste from the PDF.
2. Manuscript file: `submission/posc.pdf`.
3. Supplementary files: `submission/posc-jss-submission.zip` (PDF, LaTeX
   source and figures, `posc_0.2.0.tar.gz`, `replication.R`).
4. Comments to the editor: say that the package is on CRAN (or under
   review), that the LaTeX source was produced with Quarto from the included
   `index.qmd`-based workflow and compiles with the JSS class, and that
   `replication.R` reproduces every figure and number in about two minutes.
5. Confirm the author agreement (GPL-compatible license: MIT is fine).

JSS acknowledges by e-mail; first editorial screening usually takes a few
weeks, review several months. Keep the GitHub repository and the website in
place, since reviewers follow the links in the Computational details section.

## 5. Zenodo (optional, for a citable DOI of the paper)

Upload `submission/posc.pdf` at <https://zenodo.org/> (Upload > New upload,
type Preprint), add the GitHub URL as a related identifier, publish, and put
the DOI in the package `README.md` and `inst/CITATION`.
