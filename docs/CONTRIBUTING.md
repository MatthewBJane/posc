# Contributing to posc

Thank you for your interest in posc. Contributions of all kinds are
welcome: bug reports, questions, documentation fixes and new features.

## Reporting a problem

Open an issue at <https://github.com/MatthewBJane/posc/issues>. Please
include a minimal reproducible example (the smallest piece of code and
data that shows the problem), the output you got, the output you
expected, and the result of
[`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html). If the
problem is with a plot, attach the image.

## Asking for help

Questions about using the package, or about probability of outcome
superiority curves themselves, are also welcome as issues. Label them as
a question. The documentation website at
<https://matthewbjane.com/posc/> has the function reference and a longer
manuscript that derives the curves.

## Suggesting a change

For a small fix (a typo, a clearer error message, a documentation
example), open a pull request directly. For anything larger, open an
issue first so that we can agree on the approach before you spend time
on it.

To work on the package:

``` r
# install.packages(c("devtools", "testthat"))
devtools::load_all()      # load the development version
devtools::test()          # run the tests
devtools::document()      # rebuild the help files after changing roxygen comments
devtools::check()         # full R CMD check before opening a pull request
```

Please add a test for any new behaviour, keep the existing tests
passing, and follow the style of the surrounding code (two-space
indentation, `snake_case` names, roxygen2 documentation for every
exported function). Add a line to `NEWS.md` under the development
version describing the change.

## Code of conduct

Be respectful and constructive. Reports of unacceptable behaviour can be
sent to the maintainer at <matthewbjane@gmail.com>.
