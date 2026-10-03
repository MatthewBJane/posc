# Plot a comparison of POSCs

Plot a comparison of POSCs

## Usage

``` r
# S3 method for class 'posc_comparison'
autoplot(
  object,
  full = FALSE,
  colors = NULL,
  ci = TRUE,
  percent = FALSE,
  grid = TRUE,
  mark = NULL,
  mark_ci = FALSE,
  title = NULL,
  subtitle = NULL,
  ...
)

# S3 method for class 'posc_comparison'
plot(x, ...)
```

## Arguments

- object, x:

  A `"posc_comparison"` object from
  [`posc_comp()`](https://matthewbjane.github.io/posc/reference/posc_comp.md).

- full:

  Plot the full sigmoid over negative and positive differences?

- colors:

  Colours for the curves, one per curve. Defaults to a colour-blind-safe
  palette.

- ci:

  Draw confidence bands (where available)?

- percent:

  Label the probability axis as percentages instead of probabilities?

- grid:

  Draw light gridlines?

- mark, mark_ci:

  Differences in the predictor to highlight on every curve, and whether
  to add confidence intervals to their labels; see
  [`autoplot.posc()`](https://matthewbjane.github.io/posc/reference/autoplot.posc.md).

- title, subtitle:

  Plot title and subtitle. None by default; supply a string, or `TRUE`
  for an automatic one (the subtitle then reports the test of equal
  correlations).

- ...:

  Passed from [`plot()`](https://rdrr.io/r/graphics/plot.default.html)
  to
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html).

## Value

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
returns a `ggplot` object;
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) returns it
invisibly.

## Examples

``` r
cmp <- posc_comp(a = posc_uni(0.5, n = 100), b = posc_uni(0.3, n = 100))
plot(cmp)

plot(cmp, mark = 1, title = TRUE, subtitle = TRUE)
```
