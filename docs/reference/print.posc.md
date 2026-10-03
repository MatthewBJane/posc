# Print and summarise a POSC

[`print()`](https://rdrr.io/r/base/print.html) gives a compact
description of the curve and its probabilities at a few differences in
the predictor. [`summary()`](https://rdrr.io/r/base/summary.html)
returns those probabilities as a data frame, at differences of your
choosing.

## Usage

``` r
# S3 method for class 'posc'
print(x, digits = 3, ...)

# S3 method for class 'posc'
summary(object, delta = NULL, ...)
```

## Arguments

- x, object:

  A `"posc"` object.

- digits:

  Number of decimal places to print.

- ...:

  Unused.

- delta:

  Differences in the predictor at which to report the curve. Defaults to
  0.5, 1 and 2 standard deviations of the predictor.

## Value

[`print()`](https://rdrr.io/r/base/print.html) returns `x` invisibly.
[`summary()`](https://rdrr.io/r/base/summary.html) returns a data frame
as described in
[`predict.posc()`](https://matthewbjane.github.io/posc/reference/predict.posc.md).

## Examples

``` r
fit <- posc_uni(r = 0.4, n = 250)
fit
#> Probability of Outcome Superiority Curve
#>   Method:    bivariate normal
#>   Predictor: X    Outcome: Y
#>   r = 0.400, 95% CI [0.290, 0.499], n = 250
#> 
#>   P(higher Y | X higher by delta):
#>  delta     p lower upper
#>  0.500 0.561 0.543 0.581
#>  1.000 0.621 0.585 0.658
#>  2.000 0.731 0.666 0.792
summary(fit, delta = c(0.25, 0.5, 1))
#>   delta         p     lower     upper
#> 1  0.25 0.5307486 0.5213875 0.5405695
#> 2  0.50 0.5613147 0.5427135 0.5807206
#> 3  1.00 0.6211896 0.5849388 0.6581703
```
