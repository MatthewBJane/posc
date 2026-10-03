# Plot a probability of outcome superiority curve

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
returns a
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html)
object that can be modified further;
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws it.

## Usage

``` r
# S3 method for class 'posc'
autoplot(
  object,
  full = FALSE,
  color = "#1F5A96",
  ci = TRUE,
  reference = FALSE,
  bins = FALSE,
  percent = FALSE,
  grid = TRUE,
  mark = NULL,
  mark_ci = FALSE,
  title = NULL,
  subtitle = NULL,
  ...
)

# S3 method for class 'posc'
plot(x, ...)
```

## Arguments

- object, x:

  A `"posc"` object.

- full:

  If `FALSE` (default), plot differences \\\Delta \ge 0\\, where the
  curve runs from 0.5 upwards (or downwards for a negative
  relationship). If `TRUE`, plot the full sigmoid over negative and
  positive differences. The two halves are mirror images, since
  \\P(-\Delta) = 1 - P(\Delta)\\.

- color:

  Colour of the curve and its confidence band.

- ci:

  Draw the confidence band (if one is available)?

- reference:

  For empirical curves from
  [`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md):
  also draw the bivariate-normal POSC with the same Pearson correlation,
  as a dashed line? Off by default.

- bins:

  For empirical curves fitted with bins: draw the binned observed
  proportions as points, with whiskers showing simultaneous confidence
  intervals (see
  [`posc_bins()`](https://matthewbjane.com/posc/reference/posc_bins.md))?
  Off by default.

- percent:

  Label the probability axis as percentages (50, 60, ...) instead of
  probabilities (0.5, 0.6, ...)?

- grid:

  Draw light gridlines?

- mark:

  Optional numeric vector of differences in the predictor to highlight.
  For each one, a guide line runs up from the x-axis to the curve and
  the probability is labelled at the point. Negative values are only
  shown when `full = TRUE`.

- mark_ci:

  Add the confidence interval to the labels of marked points?

- title, subtitle:

  Plot title and subtitle. None by default; supply a string, or `TRUE`
  for an automatic one (the subtitle then describes the method,
  correlation, sample size and band).

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
fit <- posc_uni(r = 0.5, n = 120, sd_x = 15,
                predictor_name = "IQ", outcome_name = "job performance")
plot(fit)

plot(fit, full = TRUE)

plot(fit, mark = c(10, 20, 30))

plot(fit, mark = 15, mark_ci = TRUE, percent = TRUE)

plot(fit, title = TRUE, subtitle = TRUE)
```
