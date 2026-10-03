#' Compare probability of outcome superiority curves
#'
#' Overlays several POSCs and tests whether their underlying correlations
#' differ.
#'
#' @details
#' Under bivariate normality a POSC is determined entirely by its correlation,
#' so curves from independent samples can be compared by testing whether the
#' correlations are equal. For two curves this is the usual Fisher-\eqn{z}
#' test,
#' \deqn{z = \frac{\tanh^{-1} r_1 - \tanh^{-1} r_2}
#'              {\sqrt{1/(n_1 - 3) + 1/(n_2 - 3)}};}
#' for more than two it is the homogeneity test
#' \eqn{Q = \sum_i (n_i - 3)(z_i - \bar z)^2}, referred to a \eqn{\chi^2}
#' distribution with one fewer degrees of freedom than curves, where
#' \eqn{\bar z} is the weighted mean of the \eqn{z_i}.
#'
#' Both tests assume the curves come from independent samples. For curves from
#' [posc_emp()] the test compares the Pearson correlations, not the shapes of
#' the curves. No test is reported if any curve lacks a sample size.
#'
#' @param ... Two or more `"posc"` objects, optionally named (the names are
#'   used as labels). A single list of `"posc"` objects is also accepted.
#' @param labels Optional character vector of labels, one per curve.
#'
#' @return An object of class `"posc_comparison"`, a list with elements
#'   `posc` (the curves), `table` (a data frame of each curve's method,
#'   correlation and sample size) and `test` (a list with the statistic,
#'   degrees of freedom and p-value, or `NULL`). It has `print()`, `plot()`
#'   and [ggplot2::autoplot()] methods.
#' @examples
#' a <- posc_uni(r = 0.5, n = 100)
#' b <- posc_uni(r = 0.3, n = 120)
#' cmp <- posc_comp(`Structured interview` = a, `Unstructured interview` = b)
#' cmp
#' plot(cmp)
#' @export
posc_comp <- function(..., labels = NULL) {
  objs <- list(...)
  if (length(objs) == 1L && is.list(objs[[1L]]) &&
      !inherits(objs[[1L]], "posc")) {
    objs <- objs[[1L]]
  }
  if (length(objs) < 2L || !all(vapply(objs, inherits, logical(1), "posc"))) {
    stop("Supply two or more \"posc\" objects.", call. = FALSE)
  }
  k <- length(objs)
  if (is.null(labels)) {
    labels <- names(objs)
    if (is.null(labels)) labels <- rep("", k)
    blank <- is.na(labels) | labels == ""
    labels[blank] <- paste("POSC", seq_len(k))[blank]
  }
  if (!is.character(labels) || length(labels) != k || anyDuplicated(labels)) {
    stop("`labels` must be ", k, " distinct strings.", call. = FALSE)
  }
  names(objs) <- labels

  scales <- unique(vapply(objs, function(o) {
    paste(o$labels$predictor, o$sd_x, o$labels$sd_units)
  }, character(1)))
  if (length(scales) > 1L) {
    warning("The curves use different predictors or predictor scales; ",
            "check that overlaying them is meaningful.", call. = FALSE)
  }

  r <- unname(vapply(objs, function(o) o$r, numeric(1)))
  n <- unname(vapply(objs, function(o) if (is.null(o$n)) NA_real_ else o$n,
                     numeric(1)))
  method <- unname(vapply(objs, function(o) o$method, character(1)))
  table <- data.frame(curve = labels, method = method, r = r, n = n,
                      row.names = NULL, stringsAsFactors = FALSE)

  test <- NULL
  if (!anyNA(n)) {
    z <- atanh(r)
    w <- n - 3
    if (k == 2L) {
      stat <- (z[1] - z[2]) / sqrt(1 / w[1] + 1 / w[2])
      test <- list(type = "z", statistic = stat, df = NA_real_,
                   p.value = 2 * stats::pnorm(-abs(stat)))
    } else {
      z_bar <- sum(w * z) / sum(w)
      stat <- sum(w * (z - z_bar)^2)
      test <- list(type = "Q", statistic = stat, df = k - 1,
                   p.value = stats::pchisq(stat, df = k - 1,
                                           lower.tail = FALSE))
    }
  }

  structure(list(posc = objs, table = table, test = test),
            class = "posc_comparison")
}

#' @export
print.posc_comparison <- function(x, digits = 3, ...) {
  cat("Comparison of", length(x$posc), "probability of outcome superiority",
      "curves\n\n")
  tab <- x$table
  tab$r <- fmt(tab$r, digits)
  tab$n <- ifelse(is.na(tab$n), "", format(tab$n))
  print(tab, row.names = FALSE, right = FALSE)
  if (is.null(x$test)) {
    cat("\nNo test: every curve needs a sample size.\n")
  } else if (x$test$type == "z") {
    cat("\nTest of equal correlations (independent samples):\n")
    cat("  z = ", fmt(x$test$statistic, 2), ", p = ",
        format.pval(x$test$p.value, digits = 3), "\n", sep = "")
  } else {
    cat("\nTest of equal correlations (independent samples):\n")
    cat("  Q = ", fmt(x$test$statistic, 2), ", df = ", x$test$df, ", p = ",
        format.pval(x$test$p.value, digits = 3), "\n", sep = "")
  }
  if (any(x$table$method == "empirical")) {
    cat("  Note: for empirical curves the test compares Pearson r, not the",
        "curve shapes.\n")
  }
  invisible(x)
}

#' Plot a comparison of POSCs
#'
#' @param object,x A `"posc_comparison"` object from [posc_comp()].
#' @param full Plot the full sigmoid over negative and positive differences?
#' @param colors Colours for the curves, one per curve. Defaults to a
#'   colour-blind-safe palette.
#' @param ci Draw confidence bands (where available)?
#' @param percent Label the probability axis as percentages instead of
#'   probabilities?
#' @param grid Draw light gridlines?
#' @param mark,mark_ci Differences in the predictor to highlight on every
#'   curve, and whether to add confidence intervals to their labels; see
#'   [autoplot.posc()].
#' @param title,subtitle Plot title and subtitle. None by default; supply a
#'   string, or `TRUE` for an automatic one (the subtitle then reports the
#'   test of equal correlations).
#' @param ... Passed from `plot()` to `autoplot()`.
#' @return `autoplot()` returns a `ggplot` object; `plot()` returns it
#'   invisibly.
#' @examples
#' cmp <- posc_comp(a = posc_uni(0.5, n = 100), b = posc_uni(0.3, n = 100))
#' plot(cmp)
#' plot(cmp, mark = 1, title = TRUE, subtitle = TRUE)
#' @export
autoplot.posc_comparison <- function(object, full = FALSE, colors = NULL,
                                     ci = TRUE, percent = FALSE, grid = TRUE,
                                     mark = NULL, mark_ci = FALSE,
                                     title = NULL, subtitle = NULL, ...) {
  warn_dots(...)
  if (isTRUE(title)) title <- "Probability of Outcome Superiority Curves"
  if (isTRUE(subtitle)) subtitle <- comparison_subtitle(object)
  okabe_ito <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                 "#56B4E9", "#000000", "#F0E442")
  k <- length(object$posc)
  if (is.null(colors)) {
    if (k > length(okabe_ito)) {
      stop("Supply `colors` for more than 8 curves.", call. = FALSE)
    }
    colors <- okabe_ito[seq_len(k)]
  }
  curves <- do.call(rbind, lapply(names(object$posc), function(nm) {
    d <- as.data.frame.posc(object$posc[[nm]], full = full)
    d$model <- nm
    d
  }))
  first <- object$posc[[1L]]$labels
  draw_posc(curves, ref = NULL, full = full, colors = colors, ci = ci,
            percent = percent, grid = grid,
            marks = make_marks(object$posc, mark, full),
            mark_ci = mark_ci,
            title = title, subtitle = subtitle, x_lab = x_axis_label(first),
            y_lab = paste("Probability of higher", first$outcome),
            legend = TRUE)
}

#' @rdname autoplot.posc_comparison
#' @export
plot.posc_comparison <- function(x, ...) {
  p <- autoplot.posc_comparison(x, ...)
  print(p)
  invisible(p)
}

comparison_subtitle <- function(object) {
  t <- object$test
  if (is.null(t)) return(NULL)
  stat <- if (t$type == "z") {
    paste0("z = ", fmt(t$statistic, 2))
  } else {
    paste0("Q(", t$df, ") = ", fmt(t$statistic, 2))
  }
  paste0("Test of equal correlations: ", stat, ", p = ",
         format.pval(t$p.value, digits = 2, eps = 0.001))
}
