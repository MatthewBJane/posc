#' posc: Probability of Outcome Superiority Curves
#'
#' A probability of outcome superiority curve (POSC) shows, for two people
#' who differ by \eqn{\Delta} on a predictor, the probability that the person
#' with the higher predictor score also has the higher outcome:
#' \deqn{P(\Delta) = P(Y_1 > Y_2 \mid X_1 - X_2 = \Delta).}
#'
#' Because the two people are exchangeable, every POSC satisfies
#' \eqn{P(-\Delta) = 1 - P(\Delta)}: it passes through 0.5 at
#' \eqn{\Delta = 0} and is point-symmetric about that point. The curve for
#' \eqn{\Delta \ge 0} therefore carries all of the information, and is what
#' is plotted by default (use `plot(x, full = TRUE)` to see the full
#' sigmoid).
#'
#' @section Functions:
#' * [posc_uni()]: POSC from a correlation, assuming bivariate normality.
#' * [posc_multi()]: POSC for an optimally weighted composite of several
#'   predictors, from a correlation matrix.
#' * [posc_lm()]: POSC for the fitted values of a linear model.
#' * [posc_emp()]: empirical POSC estimated from raw data without assuming
#'   normality, with bootstrap confidence bands.
#' * [posc_comp()]: overlay and compare several POSCs.
#'
#' * [posc_bins()]: binned observed proportions for an empirical POSC.
#'
#' [posc_uni()], [posc_multi()], [posc_lm()] and [posc_emp()] return a
#' `"posc"` object with [print()][print.posc],
#' [summary()][summary.posc], [plot()][plot.posc], [ggplot2::autoplot()],
#' [predict()][predict.posc] and [as.data.frame()][as.data.frame.posc]
#' methods.
#'
#' @keywords internal
"_PACKAGE"

#' @importFrom stats pnorm qnorm qlogis plogis sd cor complete.cases glm.fit
#'   quasibinomial quantile model.frame model.weights nobs predict
#' @importFrom ggplot2 autoplot ggplot aes geom_line geom_ribbon geom_point
#'   geom_linerange geom_segment geom_text labs
#'   theme_classic theme element_text element_line element_blank
#'   scale_y_continuous scale_colour_manual scale_fill_manual coord_cartesian
#' @importFrom utils globalVariables
NULL

# Column names used inside aes() calls.
utils::globalVariables(c("delta", "p", "lower", "upper", "model", "lab",
                         "hj", "vj"))
