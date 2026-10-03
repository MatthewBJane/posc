# Internal helpers ------------------------------------------------------------

# POSC under bivariate normality.
#   r       : correlation between predictor and outcome (scalar)
#   delta_z : signed difference in the predictor, in SD units of the predictor
# If (X, Y) are bivariate normal with correlation r, then
#   Y1 - Y2 | X1 - X2 = d  ~  N(r d sd_y / sd_x, 2 (1 - r^2) sd_y^2),
# so P(Y1 > Y2 | X1 - X2 = d) = pnorm(r z / sqrt(2 (1 - r^2))), z = d / sd_x.
posc_normal_p <- function(r, delta_z) {
  if (abs(r) >= 1) {
    out <- as.numeric(sign(r) * delta_z > 0)
    out[delta_z == 0] <- 0.5
    return(out)
  }
  pnorm(r * delta_z / sqrt(2 * (1 - r^2)))
}

# Fisher-z confidence interval for a correlation. For a multiple correlation
# with k predictors the standard error 1 / sqrt(n - k - 2) is used (this is
# 1 / sqrt(n - 3) when k = 1), and the interval is truncated at 0 because a
# multiple correlation cannot be negative.
fisher_ci <- function(r, n, level, k = 1L, nonneg = FALSE) {
  z <- qnorm(1 - (1 - level) / 2)
  se <- 1 / sqrt(n - k - 2)
  ci <- tanh(atanh(r) + c(-1, 1) * z * se)
  if (nonneg) ci <- pmax(ci, 0)
  ci
}

# Builds the prediction function for any normal-theory POSC.
# The POSC is monotone in r for every fixed delta, so a confidence interval
# for r maps directly onto a pointwise confidence band for the curve.
normal_predictor <- function(r, n, sd_x, k = 1L, nonneg = FALSE) {
  force(r)
  force(n)
  force(sd_x)
  force(k)
  force(nonneg)
  function(delta, level) {
    dz <- delta / sd_x
    p <- posc_normal_p(r, dz)
    if (is.null(n)) {
      lower <- upper <- rep(NA_real_, length(delta))
    } else {
      ci <- fisher_ci(r, n, level, k = k, nonneg = nonneg)
      p_lo <- posc_normal_p(ci[1], dz)
      p_hi <- posc_normal_p(ci[2], dz)
      lower <- pmin(p_lo, p_hi)
      upper <- pmax(p_lo, p_hi)
    }
    data.frame(delta = delta, p = p, lower = lower, upper = upper)
  }
}

# Symmetric grid of signed differences, always containing 0.
delta_grid <- function(max_delta, n_points = 201L) {
  h <- seq(0, max_delta, length.out = n_points)
  c(-rev(h[-1L]), h)
}

# Constructor for "posc" objects.
new_posc <- function(predict_fun, method, r, n, level, max_delta, sd_x,
                     predictor_name, outcome_name, sd_units = FALSE,
                     details = list(), call = NULL) {
  grid <- delta_grid(max_delta)
  structure(
    list(
      curve = predict_fun(grid, level),
      method = method,
      r = r,
      n = n,
      level = level,
      sd_x = sd_x,
      max_delta = max_delta,
      labels = list(predictor = predictor_name, outcome = outcome_name,
                    sd_units = sd_units),
      details = details,
      predict_fun = predict_fun,
      call = call
    ),
    class = "posc"
  )
}

method_label <- function(method) {
  switch(method,
    normal = "bivariate normal",
    multiple = "bivariate normal, optimally weighted composite",
    lm = "bivariate normal, linear model fitted values",
    empirical = "empirical (spline logistic, bootstrap CI)",
    method
  )
}

# Argument checks -------------------------------------------------------------

check_scalar <- function(x, name, lower = -Inf, upper = Inf,
                         lower_open = FALSE, upper_open = FALSE) {
  ok <- is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x)
  if (ok) {
    ok <- if (lower_open) x > lower else x >= lower
    ok <- ok && (if (upper_open) x < upper else x <= upper)
  }
  if (!ok) {
    lb <- if (lower_open) "(" else "["
    ub <- if (upper_open) ")" else "]"
    stop(sprintf("`%s` must be a single number in %s%s, %s%s.",
                 name, lb, format(lower), format(upper), ub), call. = FALSE)
  }
  invisible(x)
}

check_level <- function(level) {
  check_scalar(level, "level", 0, 1, lower_open = TRUE, upper_open = TRUE)
}

check_n <- function(n, allow_null = TRUE) {
  if (is.null(n) && allow_null) return(invisible(NULL))
  check_scalar(n, "n", 3, Inf, lower_open = TRUE)
  invisible(n)
}

check_string <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop(sprintf("`%s` must be a single character string.", name),
         call. = FALSE)
  }
  invisible(x)
}

warn_dots <- function(...) {
  if (...length() > 0L) {
    nms <- names(list(...))
    nms <- if (is.null(nms)) "" else paste(nms[nms != ""], collapse = ", ")
    warning("Ignoring unused argument(s)",
            if (nzchar(nms)) paste0(": ", nms) else "", ".", call. = FALSE)
  }
  invisible(NULL)
}

# Formatting ------------------------------------------------------------------

fmt <- function(x, digits = 3) {
  formatC(x, digits = digits, format = "f")
}

# Default differences at which to summarise a curve: 0.5, 1 and 2 SDs of the
# predictor, kept within the plotted range.
default_summary_deltas <- function(object) {
  d <- c(0.5, 1, 2) * object$sd_x
  d <- d[d <= object$max_delta + 1e-12]
  if (length(d) == 0L) d <- object$max_delta * c(0.25, 0.5, 1)
  d
}
