test_that("posc_emp passes through 0.5 and is point-symmetric", {
  set.seed(1)
  x <- rnorm(150)
  y <- x + rnorm(150)
  fit <- posc_emp(x, y, n_boot = 0)
  expect_s3_class(fit, "posc")
  expect_equal(predict(fit, 0)$p, 0.5)
  d <- c(0.2, 0.8, 1.5)
  expect_equal(predict(fit, -d)$p, 1 - predict(fit, d)$p)
})

test_that("posc_emp recovers the bivariate-normal curve from normal data", {
  set.seed(2)
  n <- 600
  r <- 0.5
  x <- rnorm(n)
  y <- r * x + sqrt(1 - r^2) * rnorm(n)
  fit <- posc_emp(x, y, n_boot = 0, max_pairs = 1e5)
  d <- c(0.5, 1, 2)
  expect_equal(predict(fit, d)$p, posc_normal_p(r, d), tolerance = 0.06)
})

test_that("bootstrap bands contain the estimate and are ordered", {
  set.seed(3)
  x <- rnorm(80)
  y <- 0.6 * x + rnorm(80)
  fit <- suppressWarnings(posc_emp(x, y, n_boot = 60))
  expect_equal(fit$details$n_boot, 60)
  pr <- predict(fit, c(0.25, 1, 2))
  expect_true(all(pr$lower <= pr$upper))
  expect_true(all(pr$lower <= pr$p + 1e-8 & pr$p - 1e-8 <= pr$upper))
  # wider band at a higher level
  w95 <- with(predict(fit, 1, level = 0.95), upper - lower)
  w50 <- with(predict(fit, 1, level = 0.50), upper - lower)
  expect_gt(w95, w50)
})

test_that("small n_boot triggers a warning; n_boot = 0 gives no band", {
  set.seed(4)
  x <- rnorm(40)
  y <- x + rnorm(40)
  expect_warning(posc_emp(x, y, n_boot = 20), "small")
  fit <- posc_emp(x, y, n_boot = 0)
  expect_true(all(is.na(fit$curve$lower)))
})

test_that("posc_emp handles negative relationships, ties and missing data", {
  set.seed(5)
  x <- round(rnorm(120) * 2)            # many ties in x
  y <- round(-x + rnorm(120))           # ties in y
  x[c(3, 7)] <- NA
  fit <- posc_emp(x, y, n_boot = 0)
  expect_equal(fit$n, 118)
  expect_true(all(predict(fit, c(1, 2))$p < 0.5))
  expect_lte(fit$details$df, 3)
})

test_that("posc_emp uses a subset of pairs for large samples", {
  set.seed(6)
  x <- rnorm(1000)
  y <- x + rnorm(1000)
  fit <- posc_emp(x, y, n_boot = 0, max_pairs = 5000)
  expect_false(fit$details$all_pairs)
  expect_lte(fit$details$n_pairs, 5000)
})

test_that("standardize puts the predictor in SD units", {
  set.seed(7)
  x <- rnorm(100, 50, 10)
  y <- x + rnorm(100, sd = 10)
  fit <- posc_emp(x, y, n_boot = 0, standardize = TRUE)
  expect_equal(fit$sd_x, 1)
  expect_true(fit$labels$sd_units)
})

test_that("predict warns when extrapolating", {
  set.seed(8)
  x <- rnorm(50)
  y <- x + rnorm(50)
  fit <- posc_emp(x, y, n_boot = 0)
  expect_warning(predict(fit, 100), "extrapolated")
})

test_that("posc_emp validates its inputs", {
  expect_error(posc_emp(1:10, 1:9), "same length")
  expect_error(posc_emp(letters, 1:26), "numeric")
  expect_error(posc_emp(1:5, 1:5), "10 complete")
  expect_error(posc_emp(rep(1:2, 10), rnorm(20)), "3 distinct")
  expect_error(posc_emp(rnorm(20), rep(1, 20)), "constant")
  expect_error(posc_emp(rnorm(20), rnorm(20), df = 2.5), "whole")
  expect_error(posc_emp(rnorm(20), rnorm(20), df = 0), "`df`")
})

test_that("posc_bins returns binned proportions with simultaneous CIs", {
  set.seed(11)
  x <- rnorm(120)
  y <- 0.7 * x + rnorm(120)
  fit <- posc_emp(x, y, n_boot = 200, bins = 6)
  b <- posc_bins(fit)
  expect_equal(nrow(b), 6)
  expect_named(b, c("delta_low", "delta_high", "delta", "n_pairs", "p",
                    "lower", "upper"))
  expect_equal(b$delta_low[1], 0)
  expect_equal(b$delta_high[6], fit$max_delta)
  expect_true(all(diff(b$delta) > 0))
  expect_true(all(b$p >= 0 & b$p <= 1))
  expect_true(all(b$lower <= b$p & b$p <= b$upper))
  # equal-count bins hold similar numbers of pairs
  expect_lt(max(b$n_pairs) / min(b$n_pairs), 1.2)
  # simultaneous intervals widen with the level
  b50 <- posc_bins(fit, level = 0.5)
  expect_true(all(b$upper - b$lower >= b50$upper - b50$lower))
})

test_that("bins accept custom breakpoints and can be turned off", {
  set.seed(12)
  x <- rnorm(80)
  y <- x + rnorm(80)
  fit <- posc_emp(x, y, n_boot = 0, bins = c(0, 0.5, 1, 2))
  b <- posc_bins(fit)
  expect_equal(b$delta_low, c(0, 0.5, 1))
  expect_equal(b$delta_high, c(0.5, 1, 2))
  expect_true(all(is.na(b$lower)))

  none <- posc_emp(x, y, n_boot = 0, bins = 0)
  expect_null(none$details$bins)
  expect_error(posc_bins(none), "without bins")
  expect_error(posc_bins(posc_uni(r = 0.5, n = 50)), "posc_emp")
  expect_error(posc_emp(x, y, n_boot = 0, bins = 2.5), "whole number")
  expect_error(posc_emp(x, y, n_boot = 0, bins = c(-1, 1)), "non-negative")
})

test_that("plots draw bins for half and full curves", {
  set.seed(13)
  x <- rnorm(70)
  y <- x + rnorm(70)
  fit <- suppressWarnings(posc_emp(x, y, n_boot = 50, bins = 5))
  expect_true(inherits(autoplot(fit), "ggplot"))
  expect_true(inherits(autoplot(fit, full = TRUE, bins = TRUE,
                                reference = TRUE), "ggplot"))
  expect_true(inherits(autoplot(fit, bins = TRUE, ci = FALSE),
                       "ggplot"))
  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_true(inherits(plot(fit, full = TRUE, bins = TRUE), "ggplot"))
  # default plot is the curve and band only
  g <- autoplot(fit)
  expect_equal(length(g$layers), 2)
  expect_null(g$labels$title)
  g2 <- autoplot(fit, title = TRUE, subtitle = TRUE)
  expect_match(g2$labels$subtitle, "Empirical")
})

test_that("bin intervals are asymmetric, within (0, 1), and handle p = 1", {
  set.seed(14)
  x <- rnorm(150)
  y <- 2 * x + rnorm(150, sd = 0.3)       # very strong: far bins near 1
  fit <- posc_emp(x, y, n_boot = 200, bins = 8)
  b <- posc_bins(fit)
  expect_true(all(b$lower >= 0 & b$upper <= 1))
  expect_true(all(b$lower <= b$p & b$p <= b$upper))
  far <- nrow(b)
  # near 1 the interval extends further below the estimate than above
  expect_gt(b$p[far] - b$lower[far], b$upper[far] - b$p[far])
})

test_that("a symmetric U-shaped relationship gives a flat curve at 0.5", {
  set.seed(15)
  z <- rnorm(250)
  x <- c(z, -z)                       # exactly symmetric about the minimum
  y <- x^2 + rnorm(500, sd = 0.3)
  fit <- posc_emp(x, y, n_boot = 0, bins = 8)
  expect_equal(predict(fit, c(0.5, 1, 2))$p, c(0.5, 0.5, 0.5),
               tolerance = 0.06)
  expect_equal(posc_bins(fit)$p, rep(0.5, 8), tolerance = 0.1)
})

test_that("an off-centre U-shape matches its exact POSC", {
  # For y = (x - c)^2 the person higher on x has the higher y exactly when
  # the pair's midpoint exceeds c. With normal x the midpoint is independent
  # of the difference, so for large differences P = pnorm(-c * sqrt(2)).
  set.seed(16)
  c0 <- 0.5
  x <- rnorm(800)
  y <- (x - c0)^2 + rnorm(800, sd = 0.3)
  fit <- posc_emp(x, y, n_boot = 0, df = 4, bins = 10)
  target <- pnorm(-c0 * sqrt(2))
  expect_equal(predict(fit, c(1.5, 2))$p, rep(target, 2), tolerance = 0.2)
  expect_true(all(predict(fit, c(0.5, 1, 2))$p < 0.5))
  # the binned proportions agree with the fitted curve
  b <- posc_bins(fit)
  expect_lt(max(abs(predict(fit, b$delta)$p - b$p)), 0.08)
})
