# Coerce a POSC to a data frame

Coerce a POSC to a data frame

## Usage

``` r
# S3 method for class 'posc'
as.data.frame(x, row.names = NULL, optional = FALSE, full = TRUE, ...)
```

## Arguments

- x:

  A `"posc"` object.

- row.names, optional:

  Unused; included for compatibility with the generic.

- full:

  If `TRUE` (default), return the curve over negative and positive
  differences; if `FALSE`, only differences \\\ge 0\\.

- ...:

  Unused.

## Value

A data frame with columns `delta`, `p`, `lower` and `upper`.

## Examples

``` r
head(as.data.frame(posc_uni(r = 0.5, n = 100), full = FALSE))
#>   delta         p     lower     upper
#> 1 0.000 0.5000000 0.5000000 0.5000000
#> 2 0.015 0.5024430 0.5015128 0.5034703
#> 3 0.030 0.5048859 0.5030255 0.5069403
#> 4 0.045 0.5073286 0.5045382 0.5104098
#> 5 0.060 0.5097711 0.5060509 0.5138785
#> 6 0.075 0.5122132 0.5075634 0.5173461
```
