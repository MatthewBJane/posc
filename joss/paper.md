---
title: 'posc: Probability of Outcome Superiority Curves in R'
tags:
  - R
  - effect size
  - common language effect size
  - probability of superiority
  - correlation
  - prognosis
  - personnel selection
authors:
  - name: Matthew B. Jané
    # orcid: 0000-0000-0000-0000
    affiliation: 1
affiliations:
  - name: Department of Psychological Sciences, University of Connecticut, United States
    index: 1
date: 26 October 2026
bibliography: paper.bib
---

# Summary

A correlation coefficient says how strongly a predictor is related to an
outcome. It does not say what the relationship means for a choice between two
people. Suppose that a rehabilitation program has one place and two eligible
patients. Patient A scores 34 on a prognostic score and patient B scores 22.
In a validation study of 100 earlier patients, the score correlated 0.52
with recovery. The standard deviation of the score was 10. The clinician wants
the probability that patient A, the higher scorer, will recover better. A
probability of outcome superiority curve (POSC) gives that probability as a
function of the difference between the two people on the predictor. The
curve equals 0.5 when the two people have the same score and moves away from
0.5 as the difference grows. For the two patients, who differ by 12 points,
it gives patient A a probability of about 0.70 of the better recovery.

`posc` is an R package [@rcore] that computes, plots and compares these
curves. A curve can be computed from a published correlation and sample size,
from a correlation matrix for a weighted composite of several predictors,
from a fitted linear model, or directly from raw data without assuming a
distribution. Each curve comes with a confidence band and with print,
summary, predict and plot methods.

# Statement of need

Common language effect sizes express an effect as a probability so that a
reader without statistical training can interpret it. The original version
is the probability of superiority for two groups [@mcgraw1992; @grissom1994;
@vargha2000], the probability that a randomly chosen member of one group
scores higher than a randomly chosen member of the other. @dunlap1994
extended the idea to a correlation between two continuous variables. His
statistic is the probability that, of two randomly chosen people, the one
with the higher predictor score also has the higher outcome. Under bivariate
normality it equals $1/2 + \arcsin(\rho)/\pi$, so a correlation of 0.50
corresponds to a probability of 0.67.

A single number is not enough for a decision between two specific people,
because the probability depends on how far apart they are. The clinician in
the example knows both scores, as does a hiring committee that compares two
applicants. When the two scores are close, the probability that the higher
scorer has the better outcome is close to 0.5. When they are far apart, it
can be far from 0.5. Under bivariate normality with correlation $\rho$ and
predictor standard deviation $\sigma_X$, the curve is
$$
\theta(\Delta) = \Phi\!\left(\frac{\rho\,\Delta}{\sigma_X\sqrt{2(1-\rho^2)}}\right),
$$
where $\Delta$ is the difference between the two people on the predictor and
$\Phi$ is the standard normal distribution function. Dunlap's statistic is
the average of this curve over the distribution of $\Delta$. For
$\rho = 0.50$ the curve gives 0.66 at a difference of one standard deviation
and 0.79 at two. Two populations can share the same average of 0.67 and have
different curves.

`posc` serves applied researchers who report or interpret correlations,
validity coefficients and prognostic scores. It also serves methodologists
who want an effect size tied to a decision. A published correlation and its sample size
are enough to draw the normal-theory curve with a confidence band, so the
curve can be drawn for any reported correlation. When raw data are
available, the package estimates the curve without a distributional
assumption. The empirical curve can recover shapes that a correlation cannot
describe, such as the curve for a dose-response relationship with an
optimum, which can rise and then level off.

# State of the field

Several R packages compute the probability of superiority for group
differences, among them `effectsize` [@benshachar2020], which also reports
related indices such as Cohen's $U_3$. For correlations, @li2019 proposed the
probability of bivariate superiority, a single-number estimate related to
Dunlap's statistic. In clinical research the concordance index
[@harrell1982], available in `Hmisc` [@hmisc] and `survival` [@survival],
summarizes how often a prognostic model orders two patients correctly. For a continuous outcome it
is Dunlap's statistic with the model prediction as the predictor, again as a
single number. None of these tools computes the probability as a function of
the difference between the two people.

A dedicated package suits the curve better than an addition to a general
effect-size package. The curve is a function, not a number, so it needs its
own object, a confidence band, a prediction method and a plot. The empirical
estimator adds a spline model, a bootstrap and binned diagnostics.

# Software design

Four constructors build a curve. `posc_uni()` takes a correlation and a
sample size and computes the normal-theory curve with a confidence band from
the Fisher transformation. `posc_multi()` takes a correlation matrix and
computes the curve for the optimally weighted composite of several
predictors. `posc_lm()` does the same for the fitted values of a model
estimated with `lm()`. Both use the adjusted multiple correlation by
default. `posc_emp()` takes raw predictor and outcome scores. It forms pairs of
observations, records for each pair whether the person higher on the
predictor is also higher on the outcome, and fits a spline logistic
regression of that indicator on the difference. Each constructor returns an
object of class `"posc"` that holds the curve on a symmetric grid of
differences, the quantities it was computed from, and a prediction function
that evaluates the curve and its interval at any difference and any
confidence level. The constructors differ only in how they build the
prediction function, so one set of methods serves all four types of curve.
`print()`, `summary()` and `predict()` report probabilities with confidence
limits at default or chosen differences, `as.data.frame()` returns the grid,
and `plot()` and `autoplot()` draw the curve with `ggplot2` [@ggplot2] as an
object that the user can modify. `posc_comp()` overlays two or more curves
and tests whether their correlations differ. `posc_bins()` returns the
binned proportions of an empirical curve as a table.

Three design choices in `posc_emp()` follow from the properties of the curve.
First, because the two people are exchangeable, every POSC satisfies
$\theta(0) = 0.5$ and $\theta(-\Delta) = 1 - \theta(\Delta)$. The estimator
models the logit of the curve as $\operatorname{sign}(\Delta)\,g(|\Delta|)$,
where $g$ is a natural cubic spline with no intercept whose basis is zero at
a difference of zero, with `df = 3` by default. Both properties then hold for
every fit. A spline fitted without the constraint could cross 0.5 away from
zero, or give two different answers for the same pair depending on which
person is listed first.

Second, pairs that share a person are not independent, because each person
appears in $n - 1$ pairs. Standard errors from the pairwise logistic
regression treat the pairs as independent and would be far too small. The
band therefore comes from a bootstrap [@efron1979] that resamples people,
rebuilds the pairs, refits the spline with the knots held fixed, and takes
pointwise percentiles over the resamples, `n_boot = 1000` by default. The
number of pairs grows with the square of the sample size, so each fit uses a
random subset of at most `max_pairs = 20000` pairs when the sample has more.

Third, the observed proportions in bins of the difference, `bins = 10` by
default, are a check on the spline and are read as a set. Pointwise intervals
would miss at least one true proportion more often than the stated level
implies. The intervals are therefore simultaneous, computed from the same
bootstrap with a sup-$t$ critical value [@montielolea2019] on the
empirical-logit scale, which keeps them inside $[0, 1]$.

The package imports only `ggplot2` beyond the base packages `splines`,
`stats` and `utils`. Its unit tests check `posc_uni()` against the
closed-form curve, the point symmetry of normal-theory and empirical curves,
and `posc_emp()` against the normal-theory curve on bivariate normal data and
against the exact curves for U-shaped relationships.

![The left panel shows Karnofsky performance status and survival time in
days for the patients in the Veterans Administration lung cancer trial who
died during follow-up. The right panel shows the empirical POSC from
`posc_emp()` with its 95% bootstrap band, the observed proportions in bins
of the difference in Karnofsky score with 95% simultaneous intervals, and
the normal-theory curve for the same Pearson correlation (dashed). In this
example the outcome is skewed and the normal-theory curve lies below the
empirical curve across the whole range.](empirical.png)

# Research impact statement

`posc` 0.2.0 reached CRAN on 21 October 2026 [@posc], with a documentation
website at <https://matthewbjane.com/posc/> and its source at
<https://github.com/MatthewBJane/posc>. A longer manuscript on the website
(<https://matthewbjane.com/posc/paper/>) derives the curve, shows that
Dunlap's statistic is its average, and documents the empirical estimator. A browser-based POSC simulator at
<https://matthewbjane.com/posc-simulator> uses the same formulas without
an R installation. The package is new, so this section describes the
questions it answers. Personnel selection research reports validity
coefficients routinely but rarely translates them into the probability that
the higher-scoring applicant performs better. In clinical prognosis, a
concordance index of 0.7 may come from a model that orders patients who are
far apart on its score well and patients who are close together poorly. The
curve shows which is the case. The curve also answers a question that
correlation benchmarks cannot, namely how far apart two people must be
before a predictor identifies the better outcome with a chosen probability.
For $\rho = 0.50$ and a probability of 0.75, the answer is 1.65 standard
deviations.

# AI usage disclosure

The author wrote the original package code and the original manuscript on
which this paper is based. The author then used Claude, a large language
model developed by Anthropic, to help extend, document and test the code, to
revise text and figures, and to draft this paper from the author's
manuscript. The author specified every method, reviewed all generated code
and text, and verified the results. The author checked the averaging result
numerically and the empirical estimator on simulated data with known curves.

# Acknowledgements

This work received no specific funding.

# References
