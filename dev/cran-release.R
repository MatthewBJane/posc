## Preparing and submitting posc to CRAN.  Run from the package root, one
## step at a time, in an interactive R session.  This folder (dev/) and
## paper/ are listed in .Rbuildignore and are not part of the package.

## 1. Rebuild the documentation and run the local checks ----------------------
devtools::document()
devtools::check(remote = TRUE, manual = TRUE, cran = TRUE)  # like R CMD check --as-cran
spelling::spell_check_package()     # install.packages("spelling") if needed
urlchecker::url_check()             # install.packages("urlchecker") if needed

## The same from a terminal:
##   cd ~/posc && R CMD build . && R CMD check --as-cran posc_0.2.0.tar.gz

## 2. Check on the platforms CRAN uses -------------------------------------------
## Results arrive by e-mail (matthewbjane@gmail.com, from DESCRIPTION).
devtools::check_win_devel()         # Windows, R-devel
devtools::check_win_release()       # Windows, R-release
devtools::check_mac_release()       # macOS, R-release
## Optional, for Linux/other platforms: rhub::rhub_setup(); rhub::rhub_check()

## 3. Before submitting --------------------------------------------------------
## * Record the check results and test environments in cran-comments.md.
## * Make sure NEWS.md has an entry for the version in DESCRIPTION.
## * Commit everything and push to GitHub.

## 4. Submit ---------------------------------------------------------------------
## Builds the tarball, asks the confirmation questions, uploads it to CRAN and
## records the submission in CRAN-SUBMISSION (ignored by git and build).
devtools::submit_cran()
## Or by hand: upload posc_0.2.0.tar.gz at https://cran.r-project.org/submit.html
## and confirm the e-mail CRAN sends.

## 5. After acceptance -----------------------------------------------------------
## usethis::use_github_release()   # tag v0.2.0 on GitHub from NEWS.md
## usethis::use_dev_version()      # bump DESCRIPTION to 0.2.0.9000
