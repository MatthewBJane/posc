# posc: Probability of Outcome Superiority Curves

A correlation tells you how strongly a predictor and an outcome are
related, but not what that means for a decision. A **probability of
outcome superiority curve (POSC)** answers a more concrete question:

> If person A scores Δ points higher than person B on the predictor,
> what is the probability that A also has the higher outcome?

\\P(\Delta) = P(Y_A \> Y_B \mid X_A - X_B = \Delta)\\

For example: if one applicant scores 15 points higher than another on a
cognitive ability test, how likely is it that they will also perform
better on the job?

## Installation

``` r
# From CRAN
install.packages("posc")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("MatthewBJane/posc")
```

## Functions

| Function | Starts from | Assumes |
|----|----|----|
| [`posc_uni()`](https://matthewbjane.com/posc/reference/posc_uni.md) | a correlation *r* and *n* | bivariate normality |
| [`posc_multi()`](https://matthewbjane.com/posc/reference/posc_multi.md) | a correlation matrix | multivariate normality |
| [`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md) | a fitted [`lm()`](https://rdrr.io/r/stats/lm.html) model | normality of fitted values |
| [`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md) | raw `x` and `y` data | nothing beyond exchangeable pairs |
| [`posc_comp()`](https://matthewbjane.com/posc/reference/posc_comp.md) | two or more curves | independent samples (for the test) |

[`posc_uni()`](https://matthewbjane.com/posc/reference/posc_uni.md),
[`posc_multi()`](https://matthewbjane.com/posc/reference/posc_multi.md),
[`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md) and
[`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md)
return a `"posc"` object with
[`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
[`predict()`](https://rdrr.io/r/stats/predict.html) and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods.

## Examples

``` r
library(posc)

# From a correlation; differences in raw IQ points (SD = 15)
iq <- posc_uni(r = 0.5, n = 200, sd_x = 15,
               predictor_name = "IQ", outcome_name = "job performance")
iq
plot(iq)

# Probability at specific differences, with confidence intervals
predict(iq, delta = c(5, 15, 30))

# Directly from data, without assuming normality
set.seed(1)
x <- rnorm(300)
y <- exp(0.8 * x + rnorm(300, sd = 0.6))   # skewed outcome
emp <- posc_emp(x, y)                       # 1000 bootstrap resamples (~30 s)
plot(emp)                                  # curve and 95% band
plot(emp, bins = TRUE, reference = TRUE)   # + binned observed proportions with
                                           #   family-wise CIs, and the
                                           #   normal-theory curve (dashed)
posc_bins(emp)     # the binned proportions as a table
plot(posc_emp(x, y, bins = 15), bins = TRUE)  # finer bins

# Compare curves
posc_comp(`r = .5` = posc_uni(0.5, n = 100),
          `r = .3` = posc_uni(0.3, n = 100)) |> plot()
```

## The full sigmoid and the half curve

Because the two people are interchangeable, every POSC satisfies
\\P(-\Delta) = 1 - P(\Delta)\\. The curve passes through 0.5 at \\\Delta
= 0\\ and is point-symmetric about that point, which makes \\\Delta =
0\\ its inflection point. The half for \\\Delta \ge 0\\ contains all the
information, so it is what
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) shows by
default. `plot(x, full = TRUE)` shows the whole sigmoid.

## Empirical curves

[`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md)
compares every pair of people (or a random subset of pairs for large
samples) and fits a logistic regression of “the person higher on *X* is
also higher on *Y*” on a natural spline of the difference in *X*. The
spline is set up so the fitted curve always satisfies \\P(0) = 0.5\\ and
\\P(-\Delta) = 1 - P(\Delta)\\. Pairs that share a person are not
independent, so the confidence band comes from a bootstrap that
resamples people, not pairs.

## Development

The core of this package was written by the author. Claude, a large
language model developed by Anthropic, was then used under the author’s
direction to assist with extending, documenting and testing the code.
The author specified the methods and reviewed and verified all code and
results.

## Citation

``` r
citation("posc")
```
