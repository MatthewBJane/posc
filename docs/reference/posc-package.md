# posc: Probability of Outcome Superiority Curves

A probability of outcome superiority curve (POSC) shows, for two people
who differ by \\\Delta\\ on a predictor, the probability that the person
with the higher predictor score also has the higher outcome:
\$\$P(\Delta) = P(Y_1 \> Y_2 \mid X_1 - X_2 = \Delta).\$\$

## Details

Because the two people are exchangeable, every POSC satisfies
\\P(-\Delta) = 1 - P(\Delta)\\: it passes through 0.5 at \\\Delta = 0\\
and is point-symmetric about that point. The curve for \\\Delta \ge 0\\
therefore carries all of the information, and is what is plotted by
default (use `plot(x, full = TRUE)` to see the full sigmoid).

## Functions

- [`posc_uni()`](https://matthewbjane.com/posc/reference/posc_uni.md):
  POSC from a correlation, assuming bivariate normality.

- [`posc_multi()`](https://matthewbjane.com/posc/reference/posc_multi.md):
  POSC for an optimally weighted composite of several predictors, from a
  correlation matrix.

- [`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md):
  POSC for the fitted values of a linear model.

- [`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md):
  empirical POSC estimated from raw data without assuming normality,
  with bootstrap confidence bands.

- [`posc_comp()`](https://matthewbjane.com/posc/reference/posc_comp.md):
  overlay and compare several POSCs.

- [`posc_bins()`](https://matthewbjane.com/posc/reference/posc_bins.md):
  binned observed proportions for an empirical POSC.

[`posc_uni()`](https://matthewbjane.com/posc/reference/posc_uni.md),
[`posc_multi()`](https://matthewbjane.com/posc/reference/posc_multi.md),
[`posc_lm()`](https://matthewbjane.com/posc/reference/posc_lm.md) and
[`posc_emp()`](https://matthewbjane.com/posc/reference/posc_emp.md)
return a `"posc"` object with
[print()](https://matthewbjane.com/posc/reference/print.posc.md),
[summary()](https://matthewbjane.com/posc/reference/print.posc.md),
[plot()](https://matthewbjane.com/posc/reference/autoplot.posc.md),
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html),
[predict()](https://matthewbjane.com/posc/reference/predict.posc.md) and
[as.data.frame()](https://matthewbjane.com/posc/reference/as.data.frame.posc.md)
methods.

## See also

Useful links:

- <https://matthewbjane.com/posc/>

- <https://github.com/MatthewBJane/posc>

- Report bugs at <https://github.com/MatthewBJane/posc/issues>

## Author

**Maintainer**: Matthew B. Jané <matthewbjane@gmail.com>

Authors:

- Matthew B. Jané <matthewbjane@gmail.com>
