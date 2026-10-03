test_that("posc_uni matches the closed-form curve", {
  fit <- posc_uni(r = 0.5, n = 100)
  expect_s3_class(fit, "posc")
  d <- c(0, 0.5, 1, 2)
  pr <- predict(fit, d)
  expect_equal(pr$p, pnorm(0.5 * d / sqrt(2 * (1 - 0.25))))
  expect_equal(pr$p[1], 0.5)
})

test_that("sd_x puts differences on the raw scale", {
  raw <- posc_uni(r = 0.4, n = 80, sd_x = 15)
  std <- posc_uni(r = 0.4, n = 80)
  expect_equal(predict(raw, 15)$p, predict(std, 1)$p)
  expect_equal(predict(raw, c(-30, 30))$upper,
               predict(std, c(-2, 2))$upper)
})

test_that("curves are point-symmetric about (0, 0.5)", {
  fit <- posc_uni(r = 0.35, n = 60)
  d <- c(0.3, 1, 2.5)
  expect_equal(predict(fit, -d)$p, 1 - predict(fit, d)$p)
  expect_equal(predict(fit, -d)$lower, 1 - predict(fit, d)$upper)
})

test_that("confidence bands contain the estimate and widen with level", {
  fit <- posc_uni(r = 0.3, n = 50)
  pr <- predict(fit, c(-2, -0.5, 0.5, 2))
  expect_true(all(pr$lower <= pr$p & pr$p <= pr$upper))
  pr99 <- predict(fit, 1, level = 0.99)
  pr80 <- predict(fit, 1, level = 0.80)
  expect_gt(pr99$upper - pr99$lower, pr80$upper - pr80$lower)
})

test_that("negative correlations give curves below 0.5 for positive delta", {
  fit <- posc_uni(r = -0.4, n = 100)
  pr <- predict(fit, c(0.5, 1, 2))
  expect_true(all(pr$p < 0.5))
  expect_true(all(diff(pr$p) < 0))
})

test_that("n = NULL gives a curve without a band", {
  fit <- posc_uni(r = 0.5)
  expect_true(all(is.na(fit$curve$lower)))
  expect_false(anyNA(fit$curve$p))
})

test_that("posc_uni validates its inputs", {
  expect_error(posc_uni(r = 1.2, n = 100), "`r`")
  expect_error(posc_uni(r = 1, n = 100), "`r`")
  expect_error(posc_uni(r = 0.5, n = 3), "`n`")
  expect_error(posc_uni(r = 0.5, n = 100, level = 1), "`level`")
  expect_error(posc_uni(r = 0.5, n = 100, sd_x = -1), "`sd_x`")
  expect_error(posc_uni(r = c(0.2, 0.3), n = 100), "`r`")
  expect_error(posc_uni(r = "0.5", n = 100), "`r`")
})

test_that("posc_multi with one predictor equals posc_uni", {
  r_mat <- matrix(c(1, 0.45, 0.45, 1), 2, 2)
  m <- posc_multi(r_mat, n = 120, adjust = FALSE)
  u <- posc_uni(r = 0.45, n = 120)
  expect_equal(m$r, 0.45)
  expect_equal(m$curve, u$curve)
})

test_that("posc_multi computes the multiple correlation", {
  r_mat <- matrix(c(1.0, 0.3, 0.5,
                    0.3, 1.0, 0.4,
                    0.5, 0.4, 1.0), 3, 3,
                  dimnames = list(c("a", "b", "y"), c("a", "b", "y")))
  m <- posc_multi(r_mat, n = 200, outcome = "y", adjust = FALSE)
  rxy <- c(0.5, 0.4)
  rxx <- matrix(c(1, 0.3, 0.3, 1), 2)
  expect_equal(m$r, sqrt(drop(t(rxy) %*% solve(rxx) %*% rxy)))
  expect_equal(m$labels$outcome, "y")

  adj <- posc_multi(r_mat, n = 200, outcome = 3)
  expect_lt(adj$r, m$r)
  expect_equal(adj$r^2, 1 - (1 - m$r^2) * 199 / 197)
})

test_that("posc_multi accepts the deprecated outcome_idx with a warning", {
  r_mat <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
  expect_warning(posc_multi(r_mat, n = 50, outcome_idx = 2), "deprecated")
})

test_that("posc_multi validates its inputs", {
  expect_error(posc_multi(matrix(1:4, 2), n = 50), "symmetric")
  expect_error(posc_multi(diag(2)[, 1, drop = FALSE], n = 50), "square")
  bad <- matrix(c(1, 0.9, 0.9, 0.9, 1, -0.9, 0.9, -0.9, 1), 3)
  expect_error(posc_multi(bad, n = 50), "positive definite")
  ok <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
  expect_error(posc_multi(ok, n = 50, outcome = "nope"), "column name")
  expect_error(posc_multi(ok, n = 50, outcome = 3), "`outcome`")
})

test_that("posc_lm uses the fitted values and the model R", {
  set.seed(42)
  d <- data.frame(x1 = rnorm(200), x2 = rnorm(200))
  d$y <- 2 * d$x1 + d$x2 + rnorm(200, sd = 2)
  mod <- lm(y ~ x1 + x2, data = d)
  fit <- posc_lm(mod, adjust = FALSE)
  expect_equal(fit$r, cor(fitted(mod), d$y))
  expect_equal(fit$sd_x, sd(fitted(mod)))
  expect_equal(fit$n, 200)
  expect_equal(fit$labels$outcome, "y")
  ref <- posc_uni(r = fit$r, n = 200, sd_x = sd(fitted(mod)))
  expect_equal(predict(fit, 1)$p, predict(ref, 1)$p)
  expect_equal(posc_lm(mod)$details$k, 2)
  expect_lt(posc_lm(mod)$r, fit$r)
})

test_that("posc_lm rejects unsupported models", {
  d <- data.frame(x = rnorm(30), y = rbinom(30, 1, 0.5))
  expect_error(posc_lm(glm(y ~ x, family = binomial, data = d)), "lm\\(\\)")
  expect_error(posc_lm("not a model"), "lm\\(\\)")
  expect_error(posc_lm(lm(y ~ 1, data = d)), "no predictors")
})

test_that("posc_lm works with na.exclude models", {
  set.seed(10)
  d <- data.frame(x = rnorm(60))
  d$y <- d$x + rnorm(60)
  d$y[c(2, 9)] <- NA
  mod <- lm(y ~ x, data = d, na.action = na.exclude)
  fit <- posc_lm(mod, adjust = FALSE)
  expect_equal(fit$n, 58)
  expect_equal(fit$r, cor(d$x, d$y, use = "complete.obs"))
})

test_that("multiple-correlation bands use n - k - 2 and stay non-negative", {
  r_mat <- matrix(c(1, 0.1, 0.05, 0.1, 1, 0.08, 0.05, 0.08, 1), 3)
  fit <- posc_multi(r_mat, n = 40, adjust = FALSE)
  pr <- predict(fit, c(0.5, 1, 2))
  expect_true(all(pr$lower >= 0.5))
  ci <- fisher_ci(0.3, 50, 0.95, k = 4)
  expect_equal(ci, tanh(atanh(0.3) + c(-1, 1) * qnorm(0.975) / sqrt(44)))
  expect_error(posc_multi(diag(4), n = 5), "predictors \\+ 2")
})
