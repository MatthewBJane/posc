#' POSC for the fitted values of a linear model
#'
#' Computes the probability of outcome superiority curve for a linear model's
#' predictions: for two people whose predicted outcomes differ by
#' \eqn{\Delta}, the probability that the one predicted higher actually scores
#' higher.
#'
#' @details
#' The fitted values are treated as a single predictor whose correlation with
#' the observed outcome is the multiple correlation \eqn{R} of the model, and
#' the curve is computed as in [posc_uni()] under bivariate normality.
#' Differences are expressed in the units of the outcome (that is, of the
#' fitted values). With `adjust = TRUE` (the default) \eqn{R} is replaced by
#' the square root of the adjusted \eqn{R^2}; see [posc_multi()] for why.
#'
#' @param model A fitted [stats::lm()] model (not `glm` or multivariate
#'   `mlm`).
#' @param adjust Use the adjusted multiple correlation?
#' @param level Confidence level of the band.
#' @param max_delta Largest difference in the predicted outcome to evaluate.
#'   Defaults to 3 standard deviations of the fitted values.
#' @param predictor_name,outcome_name Labels used in printed output and plots.
#'   `outcome_name` defaults to the name of the response variable.
#'
#' @inherit posc_uni return
#' @examples
#' set.seed(1)
#' d <- data.frame(x1 = rnorm(150), x2 = rnorm(150))
#' d$y <- d$x1 + 0.5 * d$x2 + rnorm(150)
#' fit <- posc_lm(lm(y ~ x1 + x2, data = d))
#' fit
#' plot(fit)
#' @export
posc_lm <- function(model, adjust = TRUE, level = 0.95, max_delta = NULL,
                    predictor_name = NULL, outcome_name = NULL) {
  if (!inherits(model, "lm") || inherits(model, c("glm", "mlm"))) {
    stop("`model` must be a model fitted with lm().", call. = FALSE)
  }
  if (!is.logical(adjust) || length(adjust) != 1L || is.na(adjust)) {
    stop("`adjust` must be TRUE or FALSE.", call. = FALSE)
  }
  check_level(level)
  mf <- model.frame(model)
  if (!is.null(model.weights(mf))) {
    stop("Weighted lm() models are not supported.", call. = FALSE)
  }
  # Use the stored (unpadded) values so na.exclude models work too.
  y_hat <- unname(model$fitted.values)
  y <- y_hat + unname(model$residuals)
  n <- nobs(model)
  k <- model$rank - as.integer(attr(model$terms, "intercept") > 0)
  if (k < 1L) stop("`model` has no predictors.", call. = FALSE)
  if (n - k - 2 <= 0) stop("Too few observations for this model.",
                           call. = FALSE)
  sd_x <- sd(y_hat)
  if (!is.finite(sd_x) || sd_x == 0) {
    stop("The model's fitted values are constant.", call. = FALSE)
  }

  r_raw <- cor(y_hat, y)
  r_raw <- max(min(r_raw, 1 - 1e-12), 0)
  if (adjust) {
    r2_adj <- max(0, 1 - (1 - r_raw^2) * (n - 1) / (n - k - 1))
    r <- sqrt(r2_adj)
  } else {
    r <- r_raw
  }

  if (is.null(outcome_name)) {
    outcome_name <- paste(deparse(stats::formula(model)[[2L]]),
                          collapse = "")
  }
  if (is.null(predictor_name)) {
    predictor_name <- paste("predicted", outcome_name)
  }
  check_string(predictor_name, "predictor_name")
  check_string(outcome_name, "outcome_name")
  if (is.null(max_delta)) max_delta <- 3 * sd_x
  check_scalar(max_delta, "max_delta", 0, Inf, lower_open = TRUE)

  new_posc(
    predict_fun = normal_predictor(r, n, sd_x, k = k, nonneg = TRUE),
    method = "lm", r = r, n = n, level = level,
    max_delta = max_delta, sd_x = sd_x,
    predictor_name = predictor_name, outcome_name = outcome_name,
    sd_units = FALSE,
    details = list(adjusted = adjust, r_unadjusted = r_raw, k = k),
    call = match.call()
  )
}
