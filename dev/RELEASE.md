# Release checklist: GitHub, website, CRAN, JOSS

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
git commit -m "posc 0.2.0: CRAN release, manuscript and website"
git push origin main
```

## 2. Build and publish the website

```sh
cd ~/posc
Rscript -e 'install.packages("pkgdown")'    # once
bash dev/build-site.sh                       # pkgdown + quarto render -> docs/
open docs/index.html                         # look at home, Reference, Manuscript
git add -A && git commit -m "Build website" && git push
```

Also on GitHub: **Settings > General > Social preview > Edit**, upload
`man/figures/social-preview.png` (1280 x 640). This is the card shown when
the repository link is shared.

On GitHub: repository **Settings > Pages > Build and deployment**: Source
"Deploy from a branch", branch `main`, folder `/docs`, Save. After a minute
check <https://matthewbjane.com/posc/> and the Manuscript menu (HTML and PDF).

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

## 4. JOSS (Journal of Open Source Software)

The JOSS paper lives in `joss/` (`paper.md`, `paper.bib`, `empirical.png`).
Every push that touches `joss/` builds the PDF with the Draft PDF workflow
(Actions tab -> the run -> Artifacts -> paper). JOSS asks for 750-1750 words
with the sections Summary, Statement of need, State of the field, Software
design, Research impact statement, AI usage disclosure, Acknowledgements and
References.

1. Check the PDF from the workflow, then push.
2. Submit at <https://joss.theoj.org/papers/new> with the repository URL
   (https://github.com/MatthewBJane/posc), branch `main`, paper path
   `joss/paper.md`, software version 0.2.0, and the "R" language tag. The
   form asks for an ORCID and a short description.
3. An editor opens a pre-review issue in openjournals/joss-reviews, then a
   review issue with two reviewers. Reviewers open issues on this repository;
   respond there and push fixes.
4. At acceptance: tag a release (v0.2.x), archive it on Zenodo (link the
   GitHub repository to Zenodo first so releases are archived automatically),
   and post the version and the Zenodo DOI in the review issue. Make the
   archive title and author list match the paper.
5. Add the JOSS DOI to `inst/CITATION`, the README badges and the website.

## Done so far

posc 0.2.0 was published on CRAN on 2026-10-21 (DOI 10.32614/CRAN.package.posc)
from commit f794ddf, tagged v0.2.0.

JSS submission 6925 was withdrawn in favour of JOSS.

## 5. Zenodo (optional, for a citable DOI of the full manuscript)

Upload `paper/_manuscript/index.pdf` at <https://zenodo.org/> (Upload > New upload,
type Preprint), add the GitHub URL as a related identifier, publish, and put
the DOI in the package `README.md` and `inst/CITATION`.
