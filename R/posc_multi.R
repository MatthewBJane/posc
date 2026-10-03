#' POSC for a composite of several predictors (bivariate normal)
#'
#' Computes the probability of outcome superiority curve for the optimally
#' weighted (regression) composite of several predictors, from a correlation
#' matrix.
#'
#' @details
#' The composite that best predicts the outcome correlates with it at the
#' multiple correlation
#' \deqn{R = \sqrt{\mathbf{r}_{xy}^\top \mathbf{R}_{xx}^{-1} \mathbf{r}_{xy}},}
#' where \eqn{\mathbf{R}_{xx}} is the predictor intercorrelation matrix and
#' \eqn{\mathbf{r}_{xy}} the predictor-outcome correlations. The POSC is then
#' computed as in [posc_uni()], with differences in the composite expressed in
#' its standard-deviation units.
#'
#' A sample multiple correlation is biased upwards as an estimate of the
#' population multiple correlation, because the weights are fitted to the same
#' sample. With `adjust = TRUE` (the default) \eqn{R} is replaced by the
#' square root of the adjusted \eqn{R^2},
#' \eqn{1 - (1 - R^2)(n - 1)/(n - k - 1)} for \eqn{k} predictors, which
#' corrects most of this bias. (How well weights estimated in this sample
#' would predict in a new sample is lower still.)
#'
#' The confidence band uses a Fisher-\eqn{z} interval for \eqn{R} with
#' standard error \eqn{1/\sqrt{n - k - 2}}, truncated at zero, and should be
#' treated as approximate.
#'
#' @param r_mat Correlation matrix of the predictors and the outcome.
#' @param n Sample size used to estimate `r_mat`.
#' @param outcome Row/column of `r_mat` holding the outcome, as an index or a
#'   name. Defaults to the last one.
#' @param adjust Use the adjusted multiple correlation? See Details.
#' @param level Confidence level of the band.
#' @param max_delta Largest difference in the composite (in SD units) to
#'   evaluate.
#' @param predictor_name,outcome_name Labels used in printed output and plots.
#'   `outcome_name` defaults to the outcome's name in `r_mat`, if it has one.
#' @param outcome_idx Deprecated; use `outcome`.
#'
#' @inherit posc_uni return
#' @seealso [posc_lm()] to start from a fitted linear model instead.
#' @examples
#' r_mat <- matrix(c(1.0, 0.4, 0.3, 0.2,
#'                   0.4, 1.0, -0.3, 0.2,
#'                   0.3, -0.3, 1.0, 0.3,
#'                   0.2, 0.2, 0.3, 1.0), 4, 4,
#'                 dimnames = list(c("X1", "X2", "X3", "Y"),
#'                                 c("X1", "X2", "X3", "Y")))
#' fit <- posc_multi(r_mat, n = 100, outcome = "Y")
#' fit
#' plot(fit)
#' @export
posc_multi <- function(r_mat, n, outcome = ncol(r_mat), adjust = TRUE,
                       level = 0.95, max_delta = 3,
                       predictor_name = "Predicted Y", outcome_name = NULL,
                       outcome_idx = NULL) {
  if (!is.null(outcome_idx)) {
    warning("`outcome_idx` is deprecated; use `outcome` instead.",
            call. = FALSE)
    outcome <- outcome_idx
  }
  if (!is.matrix(r_mat) || !is.numeric(r_mat) || nrow(r_mat) != ncol(r_mat) ||
      nrow(r_mat) < 2L) {
    stop("`r_mat` must be a square numeric correlation matrix.", call. = FALSE)
  }
  if (anyNA(r_mat) || !isSymmetric(unname(r_mat), tol = 1e-8) ||
      any(abs(diag(r_mat) - 1) > 1e-8) || any(abs(r_mat) > 1 + 1e-8)) {
    stop("`r_mat` must be symmetric, with ones on the diagonal and all ",
         "entries in [-1, 1].", call. = FALSE)
  }
  check_n(n, allow_null = FALSE)
  check_level(level)
  check_scalar(max_delta, "max_delta", 0, Inf, lower_open = TRUE)
  check_string(predictor_name, "predictor_name")
  if (!is.logical(adjust) || length(adjust) != 1L || is.na(adjust)) {
    stop("`adjust` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.character(outcome)) {
    idx <- match(outcome, colnames(r_mat))
    if (length(outcome) != 1L || is.na(idx)) {
      stop("`outcome` is not a column name of `r_mat`.", call. = FALSE)
    }
    outcome <- idx
  }
  check_scalar(outcome, "outcome", 1, ncol(r_mat))
  if (outcome != round(outcome)) {
    stop("`outcome` must be a whole-number index.", call. = FALSE)
  }
  if (is.null(outcome_name)) {
    outcome_name <- colnames(r_mat)[outcome]
    if (is.null(outcome_name) || is.na(outcome_name) || outcome_name == "") {
      outcome_name <- "Y"
    }
  }
  check_string(outcome_name, "outcome_name")

  r_xx <- r_mat[-outcome, -outcome, drop = FALSE]
  r_xy <- r_mat[-outcome, outcome]
  k <- length(r_xy)
  if (min(eigen(r_mat, symmetric = TRUE, only.values = TRUE)$values) <= 0) {
    stop("`r_mat` is not positive definite, so the multiple correlation is ",
         "not defined.", call. = FALSE)
  }
  r2 <- as.numeric(crossprod(r_xy, solve(r_xx, r_xy)))
  r_raw <- sqrt(r2)

  if (n - k - 2 <= 0) {
    stop("`n` must exceed the number of predictors + 2.", call. = FALSE)
  }
  if (adjust) {
    r2_adj <- max(0, 1 - (1 - r2) * (n - 1) / (n - k - 1))
    r <- sqrt(r2_adj)
  } else {
    r <- r_raw
  }

  new_posc(
    predict_fun = normal_predictor(r, n, 1, k = k, nonneg = TRUE),
    method = "multiple", r = r, n = n, level = level,
    max_delta = max_delta, sd_x = 1,
    predictor_name = predictor_name, outcome_name = outcome_name,
    sd_units = TRUE,
    details = list(adjusted = adjust, r_unadjusted = r_raw, k = k,
                   weights = stats::setNames(as.numeric(solve(r_xx, r_xy)),
                                             rownames(r_xx))),
    call = match.call()
  )
}
