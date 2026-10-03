#' Empirical POSC from raw data
#'
#' Estimates the probability of outcome superiority curve directly from
#' predictor and outcome scores, without assuming bivariate normality, with a
#' bootstrap confidence band.
#'
#' @details
#' **Pairwise data.** Every pair of people \eqn{(i, j)} with
#' \eqn{x_i \ne x_j} contributes the difference \eqn{\Delta_{ij} = x_i - x_j}
#' and an indicator of whether the person with the higher predictor score
#' also has the higher outcome (ties in the outcome count as 1/2). For large
#' samples a random subset of `max_pairs` pairs is used.
#'
#' **Model.** The curve is modelled as
#' \deqn{\mathrm{logit}\, P(\Delta) = \mathrm{sign}(\Delta)\, g(|\Delta|),}
#' where \eqn{g} is a natural cubic spline with `df` degrees of freedom and no
#' intercept, whose basis is zero at \eqn{|\Delta| = 0}. This builds in the two
#' properties every POSC must have, \eqn{P(0) = 0.5} and
#' \eqn{P(-\Delta) = 1 - P(\Delta)}, while leaving the shape free. The 0/1
#' responses are shrunk by 0.0005 towards 0.5 so that the fit exists even for
#' near-perfect relationships; this changes the curve by a negligible amount. Increase
#' `df` for a more flexible curve; `df = 1` makes the logit of the curve
#' linear in \eqn{\Delta}. Because a natural spline has zero curvature at its
#' boundary, the curve is smooth through \eqn{\Delta = 0}. It is not forced to
#' be monotone.
#'
#' **Confidence band.** Pairs that share a person are not independent, so
#' model-based standard errors from the pairwise regression would be far too
#' small. Instead, people are resampled with replacement `n_boot` times, the
#' curve is refitted to each resample (with the knots held fixed), and the
#' band is formed from pointwise percentiles of the bootstrap curves. Pairs
#' formed by a person and their own duplicate are excluded. Use
#' [set.seed()] beforehand for reproducible results.
#'
#' **Binned observed proportions.** To check the fitted curve against the
#' data, the pairs with \eqn{|\Delta| \le} `max_delta` are grouped into bins
#' of \eqn{|\Delta|} and the observed proportion of pairs in which the person
#' higher on the predictor is also higher on the outcome is computed in each
#' bin. `plot(fit, bins = TRUE)` shows these as points with whiskers, and
#' [posc_bins()]
#' returns them as a table. The whiskers are *simultaneous* (family-wise)
#' confidence intervals: with probability `level` they cover all of the bin
#' proportions at once. They are computed from the same person-level
#' bootstrap as the curve, on the empirical-logit scale
#' \eqn{\ell_k = \mathrm{logit}\{(n_k \hat p_k + 0.5)/(n_k + 1)\}} for a bin
#' with \eqn{n_k} pairs: the interval is
#' \eqn{\mathrm{logit}^{-1}(\ell_k \pm c\,\widehat{se}_k)}, where
#' \eqn{\widehat{se}_k} is the bootstrap standard error of \eqn{\ell_k} and
#' \eqn{c} is the `level` quantile of
#' \eqn{\max_k |\ell^*_k - \ell_k| / \widehat{se}_k} over the bootstrap
#' resamples (the max-\eqn{t} or sup-\eqn{t} method). The intervals are
#' therefore asymmetric and lie between 0 and 1.
#'
#' **Non-monotone relationships.** A POSC averages over where in the
#' predictor's distribution a pair of people sits. For a U-shaped
#' relationship, the person higher on the predictor is more likely to have
#' the higher outcome on one side of the minimum and less likely on the
#' other, so the curve reflects the balance of the two. For a symmetric U
#' centred on the middle of the predictor's distribution the true POSC is flat
#' at 0.5, even though the predictor is strongly related to the outcome. The
#' empirical curve is not constrained to be monotone and recovers such shapes;
#' the dashed normal-theory reference, which depends only on the linear
#' correlation, is not meaningful in that case.
#'
#' @param x,y Numeric vectors of predictor and outcome scores. Pairs with a
#'   missing value in either are dropped.
#' @param df Degrees of freedom of the spline (1 to 10).
#' @param level Confidence level of the band.
#' @param n_boot Number of bootstrap resamples. At least 1000 is recommended
#'   for a 95 percent band; use 0 for no band.
#' @param max_pairs Maximum number of pairs used in each fit.
#' @param max_delta Largest difference in the predictor to evaluate. Defaults
#'   to the 95th percentile of the absolute pairwise differences.
#' @param bins Bins for the observed proportions: either the number of bins
#'   (default 10), which are formed to hold roughly equal numbers of pairs, or
#'   a vector of breakpoints for \eqn{|\Delta|} (in the units of the
#'   predictor, after standardizing if `standardize = TRUE`). Use 0 or `NULL`
#'   for no bins.
#' @param standardize If `TRUE`, the predictor is standardized first so that
#'   differences are in standard-deviation units.
#' @param predictor_name,outcome_name Labels used in printed output and plots.
#'
#' @inherit posc_uni return
#' @seealso [posc_uni()] for the curve implied by bivariate normality, which
#'   `plot(fit, reference = TRUE)` overlays as a dashed line.
#' @examples
#' set.seed(1)
#' x <- rnorm(120)
#' y <- exp(0.8 * x + rnorm(120, sd = 0.6))   # skewed, non-normal outcome
#'
#' fit <- posc_emp(x, y, n_boot = 200)   # use n_boot >= 1000 in practice
#' fit
#' plot(fit)
#' posc_bins(fit)
#' plot(fit, bins = TRUE, reference = TRUE)   # add data and normal curve
#' @export
posc_emp <- function(x, y, df = 3, level = 0.95, n_boot = 1000,
                     max_pairs = 20000, max_delta = NULL, bins = 10,
                     standardize = FALSE,
                     predictor_name = "X", outcome_name = "Y") {
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("`x` and `y` must be numeric vectors.", call. = FALSE)
  }
  if (length(x) != length(y)) {
    stop("`x` and `y` must have the same length.", call. = FALSE)
  }
  check_scalar(df, "df", 1, 10)
  if (df != round(df)) stop("`df` must be a whole number.", call. = FALSE)
  check_level(level)
  check_scalar(n_boot, "n_boot", 0, Inf)
  if (n_boot != round(n_boot)) {
    stop("`n_boot` must be a whole number.", call. = FALSE)
  }
  check_scalar(max_pairs, "max_pairs", 100, Inf)
  if (!is.logical(standardize) || length(standardize) != 1L ||
      is.na(standardize)) {
    stop("`standardize` must be TRUE or FALSE.", call. = FALSE)
  }
  check_string(predictor_name, "predictor_name")
  check_string(outcome_name, "outcome_name")

  keep <- complete.cases(x, y) & is.finite(x) & is.finite(y)
  x <- as.numeric(x[keep])
  y <- as.numeric(y[keep])
  n <- length(x)
  if (n < 10L) {
    stop("At least 10 complete observations are needed.", call. = FALSE)
  }
  if (length(unique(x)) < 3L) {
    stop("`x` needs at least 3 distinct values.", call. = FALSE)
  }
  if (sd(y) == 0) stop("`y` is constant.", call. = FALSE)
  if (standardize) x <- (x - mean(x)) / sd(x)
  if (n_boot > 0 && n_boot < 200) {
    warning("`n_boot` = ", n_boot, " is small; percentile confidence bands ",
            "are unstable with fewer than ~1000 resamples.", call. = FALSE)
  }

  # Point estimate ------------------------------------------------------------
  pairs <- emp_pairs(x, y, seq_len(n), max_pairs)
  if (is.null(max_delta)) {
    max_delta <- unname(quantile(pairs$d, 0.95))
  }
  check_scalar(max_delta, "max_delta", 0, Inf, lower_open = TRUE)
  n_inner <- df - 1L
  knots <- if (n_inner > 0) {
    unname(quantile(pairs$d, probs = seq_len(n_inner) / (n_inner + 1)))
  } else {
    numeric(0)
  }
  bknots <- c(0, max(pairs$d))
  # With discrete predictors several quantiles can coincide; keep distinct
  # interior knots only (this can lower the effective df).
  knots <- unique(knots[knots > bknots[1] & knots < bknots[2]])
  fit <- emp_fit(pairs, knots, bknots)
  if (is.null(fit)) {
    stop("The spline model could not be fitted to these data.",
         call. = FALSE)
  }
  if (!isTRUE(attr(fit, "converged"))) {
    warning("The spline model did not converge; the curve may be ",
            "unreliable. Try a smaller `df`.", call. = FALSE)
  }
  fit <- as.numeric(fit)

  breaks <- emp_breaks(pairs$d, bins, max_delta)
  bin_est <- if (is.null(breaks)) NULL else emp_bin_means(pairs, breaks)

  # Bootstrap -----------------------------------------------------------------
  boot_coef <- NULL
  boot_bins <- boot_bin_n <- NULL
  if (n_boot > 0) {
    boot_coef <- matrix(NA_real_, n_boot, length(fit))
    if (!is.null(breaks)) {
      boot_bins <- matrix(NA_real_, n_boot, length(breaks) - 1L)
      boot_bin_n <- boot_bins
    }
    for (b in seq_len(n_boot)) {
      idx <- sample.int(n, n, replace = TRUE)
      pb <- emp_pairs(x[idx], y[idx], idx, max_pairs)
      if (length(pb$d) >= 10L) {
        cb <- emp_fit(pb, knots, bknots, start = fit)
        if (!is.null(cb)) boot_coef[b, ] <- cb
        if (!is.null(breaks)) {
          bb <- emp_bin_means(pb, breaks)
          boot_bins[b, ] <- bb$p
          boot_bin_n[b, ] <- bb$n_pairs
        }
      }
    }
    boot_coef <- boot_coef[stats::complete.cases(boot_coef), , drop = FALSE]
    if (nrow(boot_coef) < 0.9 * n_boot) {
      warning(n_boot - nrow(boot_coef), " of ", n_boot, " bootstrap fits ",
              "failed; the confidence band may be unreliable.", call. = FALSE)
    }
    if (nrow(boot_coef) == 0L) boot_coef <- NULL
  }

  new_posc(
    predict_fun = emp_predictor(fit, boot_coef, knots, bknots),
    method = "empirical", r = cor(x, y), n = n, level = level,
    max_delta = max_delta, sd_x = sd(x),
    predictor_name = predictor_name, outcome_name = outcome_name,
    sd_units = standardize,
    details = list(
      df = length(knots) + 1L, knots = knots, boundary_knots = bknots,
      coefficients = fit, n_pairs = length(pairs$d),
      all_pairs = n * (n - 1) / 2 <= max_pairs,
      n_boot = if (is.null(boot_coef)) 0L else nrow(boot_coef),
      max_observed_delta = max(pairs$d),
      bins = bin_est, boot_bins = boot_bins, boot_bin_n = boot_bin_n
    ),
    call = match.call()
  )
}

# Pairwise differences. `id` identifies the original person, so that pairs of
# a person with their own bootstrap duplicate can be dropped.
emp_pairs <- function(x, y, id, max_pairs) {
  m <- length(x)
  if (m * (m - 1) / 2 <= max_pairs) {
    ij <- which(upper.tri(matrix(FALSE, m, m)), arr.ind = TRUE)
    i <- ij[, 1L]
    j <- ij[, 2L]
  } else {
    i <- sample.int(m, max_pairs, replace = TRUE)
    j <- sample.int(m - 1L, max_pairs, replace = TRUE)
    j <- j + (j >= i)
  }
  dx <- x[i] - x[j]
  ok <- dx != 0 & id[i] != id[j]
  s <- sign(dx[ok]) * (y[i][ok] - y[j][ok])
  list(d = abs(dx[ok]), out = (s > 0) + 0.5 * (s == 0))
}

emp_basis <- function(d, knots, bknots) {
  splines::ns(d, knots = knots, Boundary.knots = bknots)
}

# Logistic regression of the outcome indicator on the spline basis, with no
# intercept (the basis is zero at d = 0, so P(0) = 0.5). Returns the
# coefficients (with a "converged" attribute), or NULL if the fit fails.
# The 0/1 responses are shrunk by 0.0005 towards 0.5 so that a finite
# solution always exists, even when large differences perfectly order the
# outcome (separation); the effect on the fitted curve is negligible.
emp_fit <- function(pairs, knots, bknots, start = NULL) {
  B <- unclass(emp_basis(pairs$d, knots, bknots))
  y <- pairs$out * (1 - 1e-3) + 5e-4
  fit <- tryCatch(
    suppressWarnings(glm.fit(B, y, family = quasibinomial(),
                             start = start, intercept = FALSE,
                             control = list(maxit = 100))),
    error = function(e) NULL
  )
  if (is.null(fit) || anyNA(fit$coefficients)) return(NULL)
  structure(as.numeric(fit$coefficients), converged = fit$converged)
}

emp_predictor <- function(coef, boot_coef, knots, bknots) {
  force(coef)
  force(boot_coef)
  force(knots)
  force(bknots)
  function(delta, level) {
    B <- emp_basis(abs(delta), knots, bknots)
    s <- sign(delta)
    p <- plogis(s * drop(B %*% coef))
    if (is.null(boot_coef)) {
      lower <- upper <- rep(NA_real_, length(delta))
    } else {
      P <- plogis(s * (B %*% t(boot_coef)))
      a <- (1 - level) / 2
      lower <- apply(P, 1L, quantile, probs = a, names = FALSE)
      upper <- apply(P, 1L, quantile, probs = 1 - a, names = FALSE)
    }
    data.frame(delta = delta, p = p, lower = lower, upper = upper)
  }
}

# Breakpoints for the binned proportions (NULL for no bins).
emp_breaks <- function(d, bins, max_delta) {
  if (is.null(bins) || (length(bins) == 1L && isTRUE(bins == 0))) {
    return(NULL)
  }
  if (!is.numeric(bins) || anyNA(bins) || any(!is.finite(bins))) {
    stop("`bins` must be a number of bins or a numeric vector of breakpoints.",
         call. = FALSE)
  }
  if (length(bins) == 1L) {
    if (bins != round(bins) || bins < 1 || bins > 50) {
      stop("`bins` must be a whole number between 1 and 50.", call. = FALSE)
    }
    dd <- d[d <= max_delta]
    breaks <- unname(quantile(dd, probs = seq(0, 1, length.out = bins + 1)))
    breaks[1L] <- 0
    breaks[length(breaks)] <- max_delta
    breaks <- unique(breaks)
  } else {
    breaks <- sort(unique(bins))
    if (any(breaks < 0)) {
      stop("Breakpoints in `bins` must be non-negative.", call. = FALSE)
    }
  }
  if (length(breaks) < 2L) {
    stop("`bins` gives fewer than one bin.", call. = FALSE)
  }
  breaks
}

# Observed proportion of concordant pairs in each bin of |delta|; bins are
# (b[k-1], b[k]].
emp_bin_means <- function(pairs, breaks) {
  k <- findInterval(pairs$d, breaks, left.open = TRUE)
  K <- length(breaks) - 1L
  ok <- k >= 1L & k <= K
  k <- factor(k[ok], levels = seq_len(K))
  cnt <- tabulate(k, nbins = K)
  p <- as.numeric(tapply(pairs$out[ok], k, mean))
  mid <- as.numeric(tapply(pairs$d[ok], k, mean))
  data.frame(delta = mid, p = p, n_pairs = cnt,
             delta_low = breaks[-length(breaks)], delta_high = breaks[-1L])
}

#' Binned observed proportions for an empirical POSC
#'
#' Returns the observed proportion of concordant pairs (the person higher on
#' the predictor is also higher on the outcome) in bins of the absolute
#' difference in the predictor, with simultaneous (family-wise) bootstrap
#' confidence intervals. These are the points and whiskers drawn by
#' `plot(fit, bins = TRUE)` for curves from [posc_emp()]; see its Details for
#' the method.
#'
#' @param object A `"posc"` object from [posc_emp()] created with bins.
#' @param level Confidence level of the simultaneous intervals. Defaults to
#'   the level used when the curve was created.
#'
#' @return A data frame with one row per bin: `delta_low` and `delta_high`
#'   (bin limits), `delta` (mean absolute difference of the pairs in the
#'   bin), `n_pairs`, `p` (observed proportion), and `lower` and `upper`
#'   (simultaneous confidence limits; `NA` if the curve was fitted with
#'   `n_boot = 0`). Bins that contain no pairs have `NA` estimates.
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' y <- x + rnorm(100)
#' fit <- posc_emp(x, y, n_boot = 200, bins = 6)
#' posc_bins(fit)
#' @export
posc_bins <- function(object, level = object$level) {
  if (!inherits(object, "posc") || !identical(object$method, "empirical")) {
    stop("`object` must be a curve from posc_emp().", call. = FALSE)
  }
  bins <- object$details$bins
  if (is.null(bins)) {
    stop("This curve was fitted without bins; refit with `bins` > 0.",
         call. = FALSE)
  }
  check_level(level)
  boot <- object$details$boot_bins
  boot_n <- object$details$boot_bin_n
  est <- bins$p
  lower <- upper <- rep(NA_real_, length(est))
  if (!is.null(boot) && nrow(boot) > 1L) {
    # Work on the empirical-logit scale, so the intervals are asymmetric,
    # stay inside (0, 1) and behave well for proportions near 0 or 1.
    elogit <- function(p, n) stats::qlogis((p * n + 0.5) / (n + 1))
    L <- elogit(est, bins$n_pairs)
    Lb <- elogit(boot, boot_n)
    se <- apply(Lb, 2L, sd, na.rm = TRUE)
    dev <- abs(sweep(Lb, 2L, L)) / rep(se, each = nrow(Lb))
    dev[, !is.finite(se) | se == 0] <- 0
    t_max <- apply(dev, 1L, function(v) {
      if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE)
    })
    crit <- quantile(t_max, level, names = FALSE, na.rm = TRUE)
    lower <- pmin(plogis(L - crit * se), est)
    upper <- pmax(plogis(L + crit * se), est)
  }
  data.frame(delta_low = bins$delta_low, delta_high = bins$delta_high,
             delta = bins$delta, n_pairs = bins$n_pairs, p = est,
             lower = lower, upper = upper)
}
