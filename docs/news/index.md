# Changelog

## posc 0.2.0

### New features

- [`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md)
  estimates a POSC directly from raw data, with no normality assumption:
  a spline logistic regression on all pairwise differences (or a random
  subset of `max_pairs` of them for large samples), set up so that P(0)
  = 0.5 and P(-delta) = 1 - P(delta), with a person-level bootstrap
  confidence band.
- [`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md)
  also computes observed proportions in bins of the difference (`bins` =
  number of equal-count bins or a vector of breakpoints).
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) draws them as
  points with simultaneous (family-wise) confidence intervals, computed
  by a max-t bootstrap on the empirical-logit scale so they are
  asymmetric and stay within \[0, 1\].
  [`posc_bins()`](https://matthewbjane.com/posc/reference/posc_bins.md)
  returns them as a table.
- `plot(x, mark = ...)` highlights chosen differences: a guide line runs
  from the x-axis up to the curve, with the probability labelled (add
  `mark_ci = TRUE` for its confidence interval). Works for single curves
  and comparisons.
- [`posc_uni()`](https://matthewbjane.com/posc/reference/posc_uni.md),
  [`posc_multi()`](https://matthewbjane.com/posc/reference/posc_multi.md),
  [`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md) and
  [`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md)
  return a `"posc"` object with
  [`print()`](https://rdrr.io/r/base/print.html),
  [`summary()`](https://rdrr.io/r/base/summary.html),
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html),
  [`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
  [`predict()`](https://rdrr.io/r/stats/predict.html) and
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  methods. [`predict()`](https://rdrr.io/r/stats/predict.html) gives
  probabilities and confidence intervals at any difference.
- `plot(x, full = TRUE)` draws the full sigmoid over negative and
  positive differences.
- [`posc_comp()`](https://matthewbjane.com/posc/reference/posc_comp.md)
  accepts any number of curves (named or with `labels`) and uses a
  homogeneity test of the correlations when there are more than two.
- [`posc_multi()`](https://matthewbjane.com/posc/reference/posc_multi.md)
  and [`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md)
  gain `adjust`, which uses the adjusted multiple correlation by default
  to correct the optimism of in-sample R.
- Redesigned plots: by default just the curve and its confidence band,
  with axis lines, very light gridlines and a probability axis in steps
  of 0.1. Options add percentages (`percent = TRUE`), remove gridlines
  (`grid = FALSE`), an automatic title and subtitle (`title = TRUE`,
  `subtitle = TRUE`), and for empirical curves the binned proportions
  (`bins = TRUE`) and normal-theory curve (`reference = TRUE`).
  Comparisons use a colour-blind-safe palette.

### Development

- The core of the package was written by the author; Claude (Anthropic)
  assisted with extending, documenting and testing the code in this
  version, under the author’s direction and review.

### Bug fixes

- The x-axis of
  [`posc_uni()`](https://matthewbjane.com/posc/reference/posc_uni.md),
  [`posc_multi()`](https://matthewbjane.com/posc/reference/posc_multi.md)
  and [`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md)
  plots was stretched by a factor of sqrt(2) relative to the plotted
  probabilities. Differences are now on the predictor’s own scale.
- [`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md)
  referred to an object `mdl` that did not exist and assumed the
  response was named `y`; it now works with any
  [`lm()`](https://rdrr.io/r/stats/lm.html) model.
- Functions no longer change the global `warn` option.

### Breaking changes

- Curves are computed for signed differences, and always as the
  probability of a *higher* outcome. With a negative relationship the
  curve falls below 0.5, rather than being relabelled as the probability
  of a lower outcome.
- `ci.lvl` is now `level`; colours are set in
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) rather than
  when the curve is created; `posc_multi(outcome_idx =)` is now
  `outcome =` (the old name still works, with a warning).
- The returned object is a `"posc"` list with a `curve` data frame
  (`delta`, `p`, `lower`, `upper`), replacing `predict_matrix` and
  `posc_plot_object`.
- Plots are no longer drawn automatically; call
  [`plot()`](https://rdrr.io/r/graphics/plot.default.html) on the
  result.
- The unused ggExtra dependency was removed.

## posc 0.1.0

- Initial GitHub release.
