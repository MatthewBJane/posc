# Predicted probabilities from a POSC

Evaluates a probability of outcome superiority curve, with its
confidence interval, at any differences in the predictor.

## Usage

``` r
# S3 method for class 'posc'
predict(object, delta, level = object$level, ...)
```

## Arguments

- object:

  A `"posc"` object.

- delta:

  Numeric vector of signed differences in the predictor (person 1 minus
  person 2), in the units of the predictor. Negative values give \\1 -
  P(\|\Delta\|)\\.

- level:

  Confidence level for the interval. Defaults to the level used when the
  curve was created.

- ...:

  Unused.

## Value

A data frame with columns `delta`, `p` (probability that person 1 has
the higher outcome), `lower` and `upper` (confidence limits; `NA` when
no interval is available).

## Examples

``` r
fit <- posc_uni(r = 0.5, n = 100, sd_x = 15)
predict(fit, delta = c(5, 10, 15))
#>   delta         p     lower     upper
#> 1     5 0.5541221 0.5335775 0.5766406
#> 2    10 0.6072526 0.5669176 0.6504787
#> 3    15 0.6584543 0.5997878 0.7190156
```
