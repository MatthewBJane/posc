# Binned observed proportions for an empirical POSC

Returns the observed proportion of concordant pairs (the person higher
on the predictor is also higher on the outcome) in bins of the absolute
difference in the predictor, with simultaneous (family-wise) bootstrap
confidence intervals. These are the points and whiskers drawn by
`plot(fit, bins = TRUE)` for curves from
[`posc_emp()`](https://matthewbjane.github.io/posc/reference/posc_emp.md);
see its Details for the method.

## Usage

``` r
posc_bins(object, level = object$level)
```

## Arguments

- object:

  A `"posc"` object from
  [`posc_emp()`](https://matthewbjane.github.io/posc/reference/posc_emp.md)
  created with bins.

- level:

  Confidence level of the simultaneous intervals. Defaults to the level
  used when the curve was created.

## Value

A data frame with one row per bin: `delta_low` and `delta_high` (bin
limits), `delta` (mean absolute difference of the pairs in the bin),
`n_pairs`, `p` (observed proportion), and `lower` and `upper`
(simultaneous confidence limits; `NA` if the curve was fitted with
`n_boot = 0`). Bins that contain no pairs have `NA` estimates.

## Examples

``` r
set.seed(1)
x <- rnorm(100)
y <- x + rnorm(100)
fit <- posc_emp(x, y, n_boot = 200, bins = 6)
posc_bins(fit)
#>   delta_low delta_high     delta n_pairs         p     lower     upper
#> 1 0.0000000  0.2568641 0.1289515     784 0.5650510 0.4816081 0.6448096
#> 2 0.2568641  0.5227262 0.3913171     784 0.6262755 0.5332455 0.7105363
#> 3 0.5227262  0.8048609 0.6613284     783 0.6756066 0.5770506 0.7603478
#> 4 0.8048609  1.1490468 0.9684919     784 0.7576531 0.6343326 0.8488087
#> 5 1.1490468  1.6023812 1.3532661     783 0.8301405 0.7032188 0.9092579
#> 6 1.6023812  2.5177959 1.9665980     784 0.9221939 0.7946907 0.9727928
```
