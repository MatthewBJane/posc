#' Predicted probabilities from a POSC
#'
#' Evaluates a probability of outcome superiority curve, with its confidence
#' interval, at any differences in the predictor.
#'
#' @param object A `"posc"` object.
#' @param delta Numeric vector of signed differences in the predictor
#'   (person 1 minus person 2), in the units of the predictor. Negative values
#'   give \eqn{1 - P(|\Delta|)}.
#' @param level Confidence level for the interval. Defaults to the level used
#'   when the curve was created.
#' @param ... Unused.
#'
#' @return A data frame with columns `delta`, `p` (probability that person 1
#'   has the higher outcome), `lower` and `upper` (confidence limits; `NA`
#'   when no interval is available).
#' @examples
#' fit <- posc_uni(r = 0.5, n = 100, sd_x = 15)
#' predict(fit, delta = c(5, 10, 15))
#' @export
predict.posc <- function(object, delta, level = object$level, ...) {
  if (!is.numeric(delta) || anyNA(delta) || any(!is.finite(delta))) {
    stop("`delta` must be a numeric vector of finite values.", call. = FALSE)
  }
  check_level(level)
  if (identical(object$method, "empirical")) {
    lim <- object$details$max_observed_delta
    if (any(abs(delta) > lim)) {
      warning("Some `delta` values exceed the largest observed difference (",
              fmt(lim), "); the curve is extrapolated there.", call. = FALSE)
    }
  }
  object$predict_fun(delta, level)
}

#' Coerce a POSC to a data frame
#'
#' @param x A `"posc"` object.
#' @param row.names,optional Unused; included for compatibility with the
#'   generic.
#' @param full If `TRUE` (default), return the curve over negative and
#'   positive differences; if `FALSE`, only differences \eqn{\ge 0}.
#' @param ... Unused.
#' @return A data frame with columns `delta`, `p`, `lower` and `upper`.
#' @examples
#' head(as.data.frame(posc_uni(r = 0.5, n = 100), full = FALSE))
#' @export
as.data.frame.posc <- function(x, row.names = NULL, optional = FALSE,
                               full = TRUE, ...) {
  out <- x$curve
  if (!full) out <- out[out$delta >= 0, , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Print and summarise a POSC
#'
#' `print()` gives a compact description of the curve and its probabilities
#' at a few differences in the predictor. `summary()` returns those
#' probabilities as a data frame, at differences of your choosing.
#'
#' @param x,object A `"posc"` object.
#' @param delta Differences in the predictor at which to report the curve.
#'   Defaults to 0.5, 1 and 2 standard deviations of the predictor.
#' @param digits Number of decimal places to print.
#' @param ... Unused.
#' @return `print()` returns `x` invisibly. `summary()` returns a data frame
#'   as described in [predict.posc()].
#' @examples
#' fit <- posc_uni(r = 0.4, n = 250)
#' fit
#' summary(fit, delta = c(0.25, 0.5, 1))
#' @export
print.posc <- function(x, digits = 3, ...) {
  lab <- x$labels
  cat("Probability of Outcome Superiority Curve\n")
  cat("  Method:    ", method_label(x$method), "\n", sep = "")
  cat("  Predictor: ", lab$predictor, "    Outcome: ", lab$outcome, "\n",
      sep = "")
  r_line <- paste0("  r = ", fmt(x$r, digits))
  if (x$method == "empirical") r_line <- paste0(r_line, " (Pearson)")
  if (!is.null(x$n)) {
    if (x$method != "empirical") {
      k <- if (is.null(x$details$k)) 1L else x$details$k
      ci <- fisher_ci(x$r, x$n, x$level, k = k,
                      nonneg = x$method %in% c("multiple", "lm"))
      r_line <- paste0(r_line, ", ", round(100 * x$level), "% CI [",
                       fmt(ci[1], digits), ", ", fmt(ci[2], digits), "]")
    }
    r_line <- paste0(r_line, ", n = ", x$n)
  }
  cat(r_line, "\n", sep = "")
  if (isTRUE(x$details$adjusted)) {
    cat("  (r adjusted for the number of predictors; unadjusted r = ",
        fmt(x$details$r_unadjusted, digits), ")\n", sep = "")
  }
  if (x$method == "empirical") {
    cat("  Spline df = ", x$details$df, ", pairs used = ", x$details$n_pairs,
        ", bootstrap resamples = ", x$details$n_boot, "\n", sep = "")
  }
  tab <- summary.posc(x)
  cat("\n  P(higher ", lab$outcome, " | ", lab$predictor,
      " higher by delta):\n", sep = "")
  tab[] <- lapply(tab, function(v) fmt(v, digits))
  tab[tab == "NA"] <- ""
  print(tab, row.names = FALSE, right = TRUE)
  invisible(x)
}

#' @rdname print.posc
#' @export
summary.posc <- function(object, delta = NULL, ...) {
  if (is.null(delta)) delta <- default_summary_deltas(object)
  suppressWarnings(predict.posc(object, delta))
}

#' Plot a probability of outcome superiority curve
#'
#' `autoplot()` returns a [ggplot2::ggplot()] object that can be modified
#' further; `plot()` draws it.
#'
#' @param object,x A `"posc"` object.
#' @param full If `FALSE` (default), plot differences \eqn{\Delta \ge 0},
#'   where the curve runs from 0.5 upwards (or downwards for a negative
#'   relationship). If `TRUE`, plot the full sigmoid over negative and positive
#'   differences. The two halves are mirror images, since
#'   \eqn{P(-\Delta) = 1 - P(\Delta)}.
#' @param color Colour of the curve and its confidence band.
#' @param ci Draw the confidence band (if one is available)?
#' @param reference For empirical curves from [posc_emp()]: also draw the
#'   bivariate-normal POSC with the same Pearson correlation, as a dashed
#'   line? Off by default.
#' @param bins For empirical curves fitted with bins: draw the binned observed
#'   proportions as points, with whiskers showing simultaneous confidence
#'   intervals (see [posc_bins()])? Off by default.
#' @param percent Label the probability axis as percentages (50, 60, ...)
#'   instead of probabilities (0.5, 0.6, ...)?
#' @param grid Draw light gridlines?
#' @param mark Optional numeric vector of differences in the predictor to
#'   highlight. For each one, a guide line runs up from the x-axis to the
#'   curve and the probability is labelled at the point. Negative values are
#'   only shown when `full = TRUE`.
#' @param mark_ci Add the confidence interval to the labels of marked points?
#' @param title,subtitle Plot title and subtitle. None by default; supply a
#'   string, or `TRUE` for an automatic one (the subtitle then describes the
#'   method, correlation, sample size and band).
#' @param ... Passed from `plot()` to `autoplot()`.
#' @return `autoplot()` returns a `ggplot` object; `plot()` returns it
#'   invisibly.
#' @examples
#' fit <- posc_uni(r = 0.5, n = 120, sd_x = 15,
#'                 predictor_name = "IQ", outcome_name = "job performance")
#' plot(fit)
#' plot(fit, full = TRUE)
#' plot(fit, mark = c(10, 20, 30))
#' plot(fit, mark = 15, mark_ci = TRUE, percent = TRUE)
#' plot(fit, title = TRUE, subtitle = TRUE)
#' @export
autoplot.posc <- function(object, full = FALSE, color = "#1F5A96", ci = TRUE,
                          reference = FALSE, bins = FALSE, percent = FALSE,
                          grid = TRUE,
                          mark = NULL, mark_ci = FALSE,
                          title = NULL, subtitle = NULL, ...) {
  warn_dots(...)
  if (isTRUE(title)) title <- "Probability of Outcome Superiority Curve"
  if (isTRUE(subtitle)) subtitle <- default_subtitle(object)
  curves <- as.data.frame.posc(object, full = full)
  curves$model <- "POSC"
  ref <- NULL
  if (identical(object$method, "empirical") && isTRUE(reference)) {
    ref <- data.frame(delta = curves$delta,
                      p = posc_normal_p(object$r, curves$delta / object$sd_x))
  }
  pts <- NULL
  if (identical(object$method, "empirical") && isTRUE(bins) &&
      !is.null(object$details$bins)) {
    pts <- posc_bins(object)[, c("delta", "p", "lower", "upper")]
    pts <- pts[!is.na(pts$p), , drop = FALSE]
    if (full) {
      pts <- rbind(pts, data.frame(delta = -pts$delta, p = 1 - pts$p,
                                   lower = 1 - pts$upper,
                                   upper = 1 - pts$lower))
    }
    if (!isTRUE(ci)) pts$lower <- pts$upper <- NA_real_
  }
  marks <- make_marks(list(POSC = object), mark, full)
  draw_posc(curves, ref = ref, pts = pts, full = full, colors = color,
            ci = ci, level = object$level, percent = percent, grid = grid,
            marks = marks, mark_ci = mark_ci,
            title = title, subtitle = subtitle,
            x_lab = x_axis_label(object$labels),
            y_lab = paste("Probability of higher", object$labels$outcome),
            legend = FALSE)
}

#' @rdname autoplot.posc
#' @export
plot.posc <- function(x, ...) {
  p <- autoplot.posc(x, ...)
  print(p)
  invisible(p)
}

# Plot internals --------------------------------------------------------------

x_axis_label <- function(labels) {
  paste0("Difference in ", labels$predictor,
         if (isTRUE(labels$sd_units)) " (SD units)" else "")
}

default_subtitle <- function(object) {
  if (identical(object$method, "empirical")) {
    out <- paste0("Empirical (spline df = ", object$details$df, "): n = ",
                  object$n)
    if (object$details$n_boot > 0) {
      out <- paste0(out, "; band = ", round(100 * object$level),
                    "% percentile bootstrap CI (", object$details$n_boot,
                    " resamples)")
    }
    return(out)
  }
  parts <- method_label(object$method)
  parts <- paste0(toupper(substr(parts, 1, 1)), substring(parts, 2))
  parts <- paste0(parts, ": r = ", fmt(object$r, 2))
  if (!is.null(object$n)) parts <- paste0(parts, ", n = ", object$n)
  has_ci <- !all(is.na(object$curve$lower))
  if (has_ci) {
    parts <- paste0(parts, "; band = ", round(100 * object$level), "% CI")
  }
  parts
}

# Y-axis labels: probabilities (0.5, 0.6, ...) or, if requested, percentages.
prob_labels <- function(percent) {
  if (percent) {
    function(b) paste0(round(100 * b), "%")
  } else {
    function(b) formatC(b, digits = 1, format = "f")
  }
}

draw_posc <- function(curves, ref = NULL, pts = NULL, full, colors, ci,
                      title, subtitle, x_lab, y_lab, legend, level = 0.95,
                      percent = FALSE, marks = NULL, mark_ci = FALSE,
                      grid = TRUE) {
  has_ci <- isTRUE(ci) && !all(is.na(curves$lower))
  y_rng <- range(c(0.5, curves$p, if (has_ci) c(curves$lower, curves$upper),
                   if (!is.null(ref)) ref$p,
                   if (!is.null(pts)) c(pts$p, pts$lower, pts$upper),
                   if (!is.null(marks)) marks$p),
                 na.rm = TRUE)
  if (diff(y_rng) < 0.1) {
    y_rng <- mean(y_rng) + c(-0.05, 0.05)
  }
  # Snap the visible range to multiples of 0.1 so the 0.1 gridlines frame it.
  y_rng <- c(max(0, floor(y_rng[1] * 10 + 1e-9) / 10),
             min(1, ceiling(y_rng[2] * 10 - 1e-9) / 10))
  levels_m <- unique(curves$model)
  curves$model <- factor(curves$model, levels = levels_m)
  if (length(colors) < length(levels_m)) {
    stop("Supply at least one colour per curve.", call. = FALSE)
  }
  colors <- stats::setNames(colors[seq_along(levels_m)], levels_m)

  g <- ggplot(curves, aes(x = delta, y = p))
  if (has_ci) {
    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper, fill = model),
                         alpha = 0.18, colour = NA, na.rm = TRUE)
  }
  if (!is.null(pts)) {
    if (!all(is.na(pts$lower))) {
      g <- g + geom_linerange(data = pts,
                              aes(x = delta, ymin = lower, ymax = upper),
                              inherit.aes = FALSE, colour = colors[[1L]],
                              alpha = 0.35, linewidth = 0.6, na.rm = TRUE)
    }
    g <- g + geom_point(data = pts, aes(x = delta, y = p),
                        inherit.aes = FALSE, colour = colors[[1L]],
                        fill = "white", shape = 21, size = 2.2, stroke = 0.8,
                        alpha = 0.6)
  }
  if (!is.null(ref)) {
    g <- g + geom_line(data = ref, aes(x = delta, y = p), inherit.aes = FALSE,
                       colour = "grey35", linewidth = 0.7, linetype = "22")
  }
  if (!is.null(marks)) {
    marks$model <- factor(marks$model, levels = levels_m)
    marks$lab <- prob_text(marks$p, percent)
    if (isTRUE(mark_ci) && !all(is.na(marks$lower))) {
      has <- !is.na(marks$lower)
      marks$lab[has] <- paste0(marks$lab[has], " [",
                               prob_text(marks$lower[has], percent), ", ",
                               prob_text(marks$upper[has], percent), "]")
    }
    # Place labels away from the curve: below-right where it is rising,
    # above-right where it is falling.
    rising <- sign(marks$delta) * sign(marks$p - 0.5) >= 0
    marks$vj <- ifelse(rising, 1.6, -0.8)
    marks$hj <- -0.15
    g <- g +
      geom_segment(data = marks, aes(x = delta, xend = delta, y = -Inf,
                                     yend = p),
                   inherit.aes = FALSE, colour = "grey45", linewidth = 0.4,
                   linetype = "dashed")
  }
  g <- g +
    geom_line(aes(colour = model), linewidth = 1.1)
  if (!is.null(marks)) {
    g <- g +
      geom_point(data = marks, aes(x = delta, y = p, colour = model),
                 inherit.aes = FALSE, size = 2.6) +
      geom_text(data = marks, aes(x = delta, y = p, label = lab, hjust = hj,
                                  vjust = vj),
                inherit.aes = FALSE, size = 3.5, colour = "grey15")
  }
  g <- g +
    scale_colour_manual(values = colors, name = NULL) +
    scale_fill_manual(values = colors, name = NULL, guide = "none") +
    scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                       labels = prob_labels(percent)) +
    coord_cartesian(ylim = y_rng) +
    labs(x = x_lab, y = y_lab, title = title, subtitle = subtitle,
         caption = plot_caption(ref, pts, level)) +
    theme_classic(base_size = 12) +
    theme(
      axis.line = element_line(colour = "grey25", linewidth = 0.5),
      axis.ticks = element_line(colour = "grey25", linewidth = 0.5),
      axis.text = element_text(colour = "grey20"),
      panel.grid.major = if (isTRUE(grid)) {
        element_line(colour = "grey94", linewidth = 0.35)
      } else {
        element_blank()
      },
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold", size = 13),
      plot.title.position = "plot",
      plot.subtitle = element_text(colour = "grey30", size = 10),
      plot.caption = element_text(colour = "grey40", size = 8.5),
      axis.title = element_text(size = 11),
      legend.position = if (legend) "top" else "none",
      legend.justification = "left"
    )
  g
}

plot_caption <- function(ref, pts, level) {
  parts <- character(0)
  if (!is.null(pts)) {
    parts <- c(parts, if (all(is.na(pts$lower))) {
      "Points: observed proportions in bins of the difference"
    } else {
      paste0("Points: observed proportions in bins of the difference, with ",
             round(100 * level), "% simultaneous bootstrap CIs")
    })
  }
  if (!is.null(ref)) {
    parts <- c(parts,
               "Dashed grey line: bivariate-normal POSC with the same Pearson r")
  }
  if (length(parts) == 0L) NULL else paste(parts, collapse = "\n")
}

prob_text <- function(p, percent) {
  if (percent) paste0(round(100 * p), "%") else formatC(p, digits = 2,
                                                        format = "f")
}

# Points to highlight on one or more curves (a named list of "posc" objects).
make_marks <- function(objs, mark, full) {
  if (is.null(mark)) return(NULL)
  if (!is.numeric(mark) || anyNA(mark) || any(!is.finite(mark))) {
    stop("`mark` must be a numeric vector of finite differences.",
         call. = FALSE)
  }
  if (!full && any(mark < 0)) {
    warning("Negative `mark` values are only shown with `full = TRUE`.",
            call. = FALSE)
    mark <- mark[mark >= 0]
  }
  if (length(mark) == 0L) return(NULL)
  out <- do.call(rbind, lapply(names(objs), function(nm) {
    d <- suppressWarnings(predict.posc(objs[[nm]], mark))
    d$model <- nm
    d
  }))
  rownames(out) <- NULL
  out
}
