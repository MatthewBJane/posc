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

A correlation coefficient describes how strongly a predictor and an outcome
are related. It does not say what the relationship means for a decision
between two people. A probability of outcome superiority curve (POSC) answers
that question. For two people who differ by a given amount on the predictor,
the curve gives the probability that the person with the higher predictor
score also has the higher outcome. The curve equals 0.5 when the two people
have the same score and increases as the difference between them grows.
`posc` is an R package [@rcore] that computes, plots and compares these
curves. A curve can be computed from a published correlation and sample size,
from a correlation matrix for a weighted composite of several predictors,
from a fitted linear model, or directly from raw data without assuming a
distribution. Every curve comes with a confidence band, a common set of
print, summary, predict and plot methods, and a function for comparing
curves.

# Statement of need

Common language effect sizes express an effect as a probability so that a
reader can interpret it without training in statistics. The original version
is the probability of superiority for two groups [@mcgraw1992; @grissom1994;
@vargha2000], the probability that a randomly chosen member of one group
scores higher than a randomly chosen member of the other. @dunlap1994
extended the idea to a correlation between two continuous variables. His
statistic is the probability that, of two randomly chosen people, the one
with the higher predictor score also has the higher outcome. Under bivariate
normality it equals $1/2 + \arcsin(\rho)/\pi$, so a correlation of 0.50
corresponds to a probability of 0.67.

Dunlap's statistic is a single number because it averages over how far apart
the two people are. In practice the two people are a known distance apart. A
clinician deciding which of two patients should receive a scarce treatment
knows both prognostic scores. A hiring committee knows both test scores. The
probability that the higher scorer has the better outcome is close to 0.5
when the two scores are close together and can be far from it when they are
far apart. The POSC makes this dependence explicit. Under bivariate normality
with correlation $\rho$ and predictor standard deviation $\sigma_X$, the curve
is $\theta(\Delta) = \Phi\{\rho\Delta / (\sigma_X\sqrt{2(1-\rho^2)})\}$,
where $\Delta$ is the difference between the two people on the predictor.
Dunlap's statistic is the average of this curve over the distribution of
$\Delta$.

`posc` is written for applied researchers who report or interpret
correlations, validity coefficients and prognostic scores, and for
methodologists who want an effect size that is tied to a decision. The
package gives a curve, with its uncertainty, from the quantities that are
usually available. When only a correlation and a sample size are reported,
`posc_uni()` computes the normal-theory curve with a confidence band from the
Fisher transformation. When several predictors are combined, `posc_multi()`
computes the curve for the optimally weighted composite from a correlation
matrix, using the adjusted multiple correlation by default. `posc_lm()` does
the same for the fitted values of a model estimated with `lm()`. When raw
data are available, `posc_emp()` estimates the curve without a distributional
assumption. It forms all pairs of observations, records for each pair whether
the person higher on the predictor is also higher on the outcome, and fits a
natural cubic spline on the logit scale that is constrained to pass through
0.5 at a difference of zero and to be point-symmetric about that point. A
bootstrap that resamples people, not pairs, gives the confidence band. The
empirical curve recovers relationships that a correlation cannot describe,
such as a dose-response relationship with an optimum, where the curve
increases and then becomes flat.

# State of the field

Several R packages compute the probability of superiority for group
differences, among them `effectsize` [@benshachar2020], which also reports
related indices such as Cohen's $U_3$. For correlations, @li2019 proposed the
probability of bivariate superiority, a single-number estimate related to
Dunlap's statistic. In clinical
research the concordance index [@harrell1982], available in `Hmisc` and
`survival`, summarizes how often a prognostic model orders two patients
correctly. It is Dunlap's statistic with the model prediction as the
predictor, again as a single number. No package computes the probability as a
function of the difference between the two people, which is the quantity that
a comparison of two specific people requires. The curve, its confidence
bands, the empirical estimator and the comparison methods form a coherent
unit with its own object class, so they are better served by a dedicated
package than by additions to a general effect-size package.

# Software design

Each constructor returns an object of class `"posc"` that holds the curve on
a symmetric grid of differences, the inputs it was computed from, and a
prediction function that evaluates the curve and its confidence interval at
any difference and any confidence level. The constructors differ only in how
they build the prediction function, so one set of methods serves all four
types of curve. `print()`, `summary()` and `predict()` report probabilities
at chosen differences, `as.data.frame()` returns the grid, and `plot()` and
`autoplot()` draw the curve with `ggplot2` [@ggplot2], returning an object
that the user can modify. `posc_comp()` overlays any number of curves and
tests whether their correlations differ.

Three design choices follow from the properties of the curve. Because two
people are exchangeable, every POSC satisfies $\theta(-\Delta) = 1 -
\theta(\Delta)$. The empirical estimator builds this symmetry into the spline
basis instead of imposing it after the fact, which also guarantees
$\theta(0) = 0.5$. Because pairs that share a person are not independent,
model-based standard errors from the pairwise regression would be far too
small, so the band comes from a person-level bootstrap [@efron1979]. Because
the binned observed proportions that `posc_emp()` reports are read as a set,
their intervals are simultaneous rather than pointwise, computed with a
sup-$t$ critical value [@montielolea2019] on the empirical-logit scale so
that they stay inside $[0, 1]$. The pairwise construction grows with the
square of the sample size, so large samples use a random subset of pairs by
default. The package imports only `ggplot2` beyond base and recommended R,
and its unit tests check the closed-form curve, the symmetry property, and
the empirical estimator against the normal-theory curve and against the
exact curve for a U-shaped relationship.

![Left: Karnofsky performance status and survival time for the patients in
the Veterans Administration lung cancer trial who died during follow-up.
Right: the empirical POSC from `posc_emp()` with its 95% bootstrap band, the
observed proportions in eight bins with simultaneous intervals, and the
normal-theory curve for the same Pearson correlation (dashed). The skewed
outcome makes the normal-theory curve too low across the whole
range.](empirical.png)

# Research impact statement

`posc` 0.2.0 was released on CRAN on 21 October 2026 and is accompanied by a
documentation website, a longer manuscript that derives the curve and
documents the empirical estimator, and a browser-based POSC simulator on the
author's website that uses the same formulas. The immediate uses are in
personnel selection, where validity coefficients are routinely reported but
rarely translated into the probability that a higher-scoring applicant
performs better, and in clinical prognosis, where a concordance index
summarizes a model with one number but says nothing about pairs of patients
who are close together on the score. The curve also gives a direct answer to
a question that correlation benchmarks cannot, namely how far apart two
people must be before a predictor identifies the better outcome with a
chosen probability.

# AI usage disclosure

The author wrote the original package code and the original manuscript on
which this paper is based. Claude, a large language model developed by
Anthropic, was then used under the author's direction to help extend,
document and test the code, to revise text and figures, and to draft this
paper from the author's manuscript. The author specified every method,
reviewed all generated code and text, and verified the results. The averaging
result was checked numerically, and the empirical estimator was checked on
simulated data with known curves.

# Acknowledgements

This work received no specific funding.

# References
