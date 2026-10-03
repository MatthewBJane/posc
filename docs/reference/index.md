# Package index

## Creating a curve

Each constructor returns a `"posc"` object.

- [`posc_uni()`](https://matthewbjane.com/posc/reference/posc_uni.md) :
  POSC from a single correlation (bivariate normal)
- [`posc_multi()`](https://matthewbjane.com/posc/reference/posc_multi.md)
  : POSC for a composite of several predictors (bivariate normal)
- [`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md) :
  POSC for the fitted values of a linear model
- [`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md) :
  Empirical POSC from raw data

## Working with a curve

- [`print(`*`<posc>`*`)`](https://matthewbjane.com/posc/reference/print.posc.md)
  [`summary(`*`<posc>`*`)`](https://matthewbjane.com/posc/reference/print.posc.md)
  : Print and summarise a POSC
- [`predict(`*`<posc>`*`)`](https://matthewbjane.com/posc/reference/predict.posc.md)
  : Predicted probabilities from a POSC
- [`as.data.frame(`*`<posc>`*`)`](https://matthewbjane.com/posc/reference/as.data.frame.posc.md)
  : Coerce a POSC to a data frame
- [`autoplot(`*`<posc>`*`)`](https://matthewbjane.com/posc/reference/autoplot.posc.md)
  [`plot(`*`<posc>`*`)`](https://matthewbjane.com/posc/reference/autoplot.posc.md)
  : Plot a probability of outcome superiority curve
- [`posc_bins()`](https://matthewbjane.com/posc/reference/posc_bins.md)
  : Binned observed proportions for an empirical POSC

## Comparing curves

- [`posc_comp()`](https://matthewbjane.com/posc/reference/posc_comp.md)
  : Compare probability of outcome superiority curves
- [`autoplot(`*`<posc_comparison>`*`)`](https://matthewbjane.com/posc/reference/autoplot.posc_comparison.md)
  [`plot(`*`<posc_comparison>`*`)`](https://matthewbjane.com/posc/reference/autoplot.posc_comparison.md)
  : Plot a comparison of POSCs

## Package

- [`posc`](https://matthewbjane.com/posc/reference/posc-package.md)
  [`posc-package`](https://matthewbjane.com/posc/reference/posc-package.md)
  : posc: Probability of Outcome Superiority Curves
- [`reexports`](https://matthewbjane.com/posc/reference/reexports.md)
  [`autoplot`](https://matthewbjane.com/posc/reference/reexports.md) :
  Objects exported from other packages
