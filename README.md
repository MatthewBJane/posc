# posc: Probability of Outcome Superiority Curves <img src="man/figures/logo.png" align="right" height="139" alt="posc hex logo" />

<!-- badges: start -->
<!-- badges: end -->

A correlation tells you how strongly a predictor and an outcome are related,
but not what that means for a decision. A **probability of outcome
superiority curve (POSC)** answers a more concrete question:

> If person A scores Δ points higher than person B on the predictor, what is
> the probability that A also has the higher outcome?

$$P(\Delta) = P(Y_A > Y_B \mid X_A - X_B = \Delta)$$

For example: if one applicant scores 15 points higher than another on a
cognitive ability test, how likely is it that they will also perform better on
the job?

## Installation

```r
# From CRAN
install.packages("posc")

# Development version from GitHub
# install.packages("remotes")
remotes::install_github("MatthewBJane/posc")
```

## Functions

| Function       | Starts from                    | Assumes                     |
|----------------|--------------------------------|-----------------------------|
| `posc_uni()`   | a correlation *r* and *n*      | bivariate normality         |
| `posc_multi()` | a correlation matrix           | multivariate normality      |
| `posc_lm()`    | a fitted `lm()` model          | normality of fitted values  |
| `posc_emp()`   | raw `x` and `y` data           | nothing beyond exchangeable pairs |
| `posc_comp()`  | two or more curves             | independent samples (for the test) |

`posc_uni()`, `posc_multi()`, `posc_lm()` and `posc_emp()` return a `"posc"`
object with `print()`, `summary()`,
`plot()`, `autoplot()`, `predict()` and `as.data.frame()` methods.

## Examples

```r
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
$P(-\Delta) = 1 - P(\Delta)$. The curve passes through 0.5 at $\Delta = 0$
and is point-symmetric about that point, which makes $\Delta = 0$ its
inflection point. The half for $\Delta \ge 0$ contains all the information,
so it is what `plot()` shows by default. `plot(x, full = TRUE)` shows the
whole sigmoid.

## Empirical curves

`posc_emp()` compares every pair of people (or a random subset of pairs for
large samples) and fits a logistic regression of "the person higher on *X*
is also higher on *Y*" on a natural spline of the difference in *X*. The
spline is set up so the fitted curve always satisfies $P(0) = 0.5$ and
$P(-\Delta) = 1 - P(\Delta)$. Pairs that share a person are not independent,
so the confidence band comes from a bootstrap that resamples people, not
pairs.

## Development

The core of this package was written by the author. Claude, a large language
model developed by Anthropic, was then used under the author's direction to
assist with extending, documenting and testing the code. The author specified
the methods and reviewed and verified all code and results.

## Citation

```r
citation("posc")
```
