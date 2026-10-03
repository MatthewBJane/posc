test_that("print and summary work for every constructor", {
  set.seed(1)
  x <- rnorm(60)
  y <- x + rnorm(60)
  r_mat <- matrix(c(1, 0.3, 0.4, 0.3, 1, 0.2, 0.4, 0.2, 1), 3)
  fits <- list(
    posc_uni(r = 0.4, n = 100),
    posc_uni(r = 0.4),
    posc_multi(r_mat, n = 100),
    posc_lm(lm(y ~ x)),
    posc_emp(x, y, n_boot = 0)
  )
  for (f in fits) {
    expect_output(print(f), "Probability of Outcome Superiority Curve")
    s <- summary(f)
    expect_s3_class(s, "data.frame")
    expect_named(s, c("delta", "p", "lower", "upper"))
  }
})

test_that("summary accepts custom differences", {
  fit <- posc_uni(r = 0.5, n = 100)
  s <- summary(fit, delta = c(0.1, 0.2))
  expect_equal(s$delta, c(0.1, 0.2))
})

test_that("as.data.frame returns the full or half curve", {
  fit <- posc_uni(r = 0.5, n = 100)
  full <- as.data.frame(fit)
  half <- as.data.frame(fit, full = FALSE)
  expect_true(min(full$delta) < 0)
  expect_equal(min(half$delta), 0)
  expect_equal(max(half$delta), 3)
})

test_that("predict validates delta", {
  fit <- posc_uni(r = 0.5, n = 100)
  expect_error(predict(fit, "a"), "numeric")
  expect_error(predict(fit, NA_real_), "numeric")
  expect_error(predict(fit, 1, level = 2), "`level`")
})

test_that("autoplot and plot return ggplot objects", {
  set.seed(2)
  x <- rnorm(60)
  y <- x + rnorm(60)
  u <- posc_uni(r = 0.4, n = 100)
  e <- suppressWarnings(posc_emp(x, y, n_boot = 30))
  expect_true(inherits(ggplot2::autoplot(u), "ggplot"))
  expect_true(inherits(ggplot2::autoplot(u, full = TRUE), "ggplot"))
  expect_true(inherits(ggplot2::autoplot(e, reference = FALSE), "ggplot"))
  expect_true(inherits(ggplot2::autoplot(posc_uni(r = 0.4)), "ggplot"))

  pdf(NULL)
  on.exit(dev.off(), add = TRUE)
  expect_true(inherits(plot(u), "ggplot"))
  expect_true(inherits(plot(e), "ggplot"))
})

test_that("posc_comp tests two independent correlations", {
  a <- posc_uni(r = 0.5, n = 100)
  b <- posc_uni(r = 0.3, n = 120)
  cmp <- posc_comp(A = a, B = b)
  expect_s3_class(cmp, "posc_comparison")
  z <- (atanh(0.5) - atanh(0.3)) / sqrt(1 / 97 + 1 / 117)
  expect_equal(cmp$test$statistic, z)
  expect_equal(cmp$test$p.value, 2 * pnorm(-abs(z)))
  expect_equal(cmp$table$curve, c("A", "B"))
  expect_output(print(cmp), "z = ")
  expect_true(inherits(ggplot2::autoplot(cmp), "ggplot"))
})

test_that("posc_comp uses a homogeneity test for three or more curves", {
  fits <- list(posc_uni(r = 0.5, n = 100), posc_uni(r = 0.3, n = 100),
               posc_uni(r = 0.4, n = 100))
  cmp <- posc_comp(fits)
  z <- atanh(c(0.5, 0.3, 0.4))
  q <- sum(97 * (z - mean(z))^2)
  expect_equal(cmp$test$statistic, q)
  expect_equal(cmp$test$df, 2)
  expect_equal(cmp$table$curve, paste("POSC", 1:3))
})

test_that("posc_comp handles missing sample sizes and bad input", {
  cmp <- posc_comp(posc_uni(r = 0.5), posc_uni(r = 0.3, n = 50))
  expect_null(cmp$test)
  expect_output(print(cmp), "No test")
  expect_error(posc_comp(posc_uni(r = 0.5)), "two or more")
  expect_error(posc_comp(1, 2), "two or more")
  expect_error(posc_comp(posc_uni(r = 0.5), posc_uni(r = 0.3),
                         labels = c("a", "a")), "distinct")
  expect_warning(posc_comp(posc_uni(r = 0.5),
                           posc_uni(r = 0.3, sd_x = 15)), "scales")
})

test_that("the curve grid is symmetric and contains exactly zero", {
  for (m in c(3, 3.3, 7, 0.123456)) {
    g <- delta_grid(m)
    expect_true(any(g == 0))
    expect_equal(g, -rev(g))
  }
  set.seed(9)
  x <- rnorm(57)
  y <- x + rnorm(57)
  half <- as.data.frame(posc_emp(x, y, n_boot = 0), full = FALSE)
  expect_equal(half$delta[1], 0)
  expect_equal(half$p[1], 0.5)
})

test_that("unused plotting arguments are reported", {
  fit <- posc_uni(r = 0.5, n = 100)
  expect_warning(ggplot2::autoplot(fit, colour = "red"), "colour")
})

test_that("autoplot is re-exported", {
  expect_true(inherits(autoplot(posc_uni(r = 0.3, n = 50)), "ggplot"))
})

test_that("marked points are placed on the curve and labelled", {
  fit <- posc_uni(r = 0.5, n = 100, sd_x = 15)
  g <- autoplot(fit, mark = c(15, 30))
  expect_true(inherits(g, "ggplot"))
  m <- make_marks(list(POSC = fit), c(15, 30), full = FALSE)
  expect_equal(m$p, predict(fit, c(15, 30))$p)
  expect_equal(prob_text(0.6915, FALSE), "0.69")
  expect_equal(prob_text(0.6915, TRUE), "69%")
  expect_warning(autoplot(fit, mark = c(-10, 10)), "full = TRUE")
  expect_true(inherits(autoplot(fit, mark = c(-10, 10), full = TRUE,
                                mark_ci = TRUE), "ggplot"))
  expect_error(autoplot(fit, mark = "a"), "`mark`")
  cmp <- posc_comp(a = posc_uni(0.5, n = 80), b = posc_uni(0.2, n = 80))
  expect_true(inherits(autoplot(cmp, mark = 1), "ggplot"))
  expect_true(inherits(autoplot(cmp, title = TRUE, subtitle = TRUE),
                       "ggplot"))
  expect_equal(nrow(make_marks(cmp$posc, c(1, 2), FALSE)), 4)
})

test_that("percent labels are optional", {
  expect_equal(prob_labels(FALSE)(c(0.5, 0.6)), c("0.5", "0.6"))
  expect_equal(prob_labels(TRUE)(c(0.5, 0.6)), c("50%", "60%"))
})

test_that("gridlines can be turned off", {
  fit <- posc_uni(r = 0.5, n = 100)
  on <- autoplot(fit)
  off <- autoplot(fit, grid = FALSE)
  expect_true(inherits(off, "ggplot"))
  expect_false(identical(on$theme$panel.grid.major,
                         off$theme$panel.grid.major))
  cmp <- posc_comp(a = fit, b = posc_uni(r = 0.3, n = 100))
  expect_true(inherits(autoplot(cmp, grid = FALSE), "ggplot"))
})
