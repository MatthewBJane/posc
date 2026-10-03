## Replication script for the manuscript
##   "posc: Probability of Outcome Superiority Curves in R"
##   submitted to the Journal of Statistical Software
##
## This script is generated from the Quarto source of the manuscript
## (index.qmd) and contains every code chunk of the paper in order.  The
## comment before each chunk gives the chunk label and the manuscript section
## it belongs to.  Figures are written to replication-figures/ as PDF files
## named after the figure labels used in the manuscript.  The quantities that
## the text reports inline are printed at the end of the script.
##
## Requirements: R (>= 4.1.0) and the packages posc (>= 0.2.0), ggplot2,
## patchwork, geomtextpath, knitr and MASS, all available from CRAN.
## Running time: about two minutes, most of it in the bootstrap fits.
##
## Usage:  Rscript replication.R

dir.create("replication-figures", showWarnings = FALSE)

## ---- setup
## Options and helper functions used throughout the manuscript.
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE,
        digits = 4)
knitr::opts_chunk$set(prompt = TRUE, comment = NA, fig.align = "center",
                      dev = if (knitr::is_latex_output()) "cairo_pdf" else "png")
library("posc")
library("ggplot2")
library("patchwork")
library("geomtextpath")
set.seed(2026)
fmt <- function(x, d = 2) formatC(x, digits = d, format = "f")
prob_theme <- function() {
  list(scale_y_continuous(breaks = seq(0, 1, by = 0.1)),
       theme_classic(base_size = 12),
       theme(axis.line = element_line(colour = "grey25", linewidth = 0.5),
             panel.grid.major = element_line(colour = "grey94",
                                             linewidth = 0.35)))
}
label_paths <- function(cmp, hjust = NULL, labels = NULL, ci = TRUE,
                        colors = NULL, parse = FALSE, size = 3.6,
                        ylab = NULL) {
  nm <- names(cmp$posc)
  curves <- do.call(rbind, lapply(nm, function(m) {
    d <- as.data.frame(cmp$posc[[m]], full = FALSE)
    d$model <- m
    d
  }))
  curves$model <- factor(curves$model, levels = nm)
  lab <- if (is.null(labels)) nm else unname(labels[nm])
  curves$label <- lab[as.integer(curves$model)]
  if (is.null(hjust)) hjust <- seq(0.35, 0.8, length.out = length(nm))
  curves$hjust <- hjust[as.integer(curves$model)]
  if (is.null(colors)) {
    colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7", "#E69F00",
                "#56B4E9", "#000000")[seq_along(nm)]
  }
  has_ci <- isTRUE(ci) && !all(is.na(curves$lower))
  y_rng <- range(c(0.5, curves$p, if (has_ci) c(curves$lower, curves$upper)),
                 na.rm = TRUE)
  y_rng <- c(max(0, floor(y_rng[1] * 10) / 10), min(1, ceiling(y_rng[2] * 10) / 10))
  first <- cmp$posc[[1]]$labels
  g <- ggplot(curves, aes(delta, p, colour = model, group = model))
  if (has_ci) {
    g <- g + geom_ribbon(aes(ymin = lower, ymax = upper, fill = model),
                         alpha = 0.18, colour = NA, na.rm = TRUE)
  }
  g +
    geom_textpath(aes(label = label, hjust = hjust), linewidth = 1.1,
                  size = size, parse = parse, text_smoothing = 30) +
    scale_colour_manual(values = colors, guide = "none") +
    scale_fill_manual(values = colors, guide = "none") +
    coord_cartesian(ylim = y_rng) +
    labs(x = paste0("Difference in ", first$predictor,
                    if (isTRUE(first$sd_units)) " (SD units)" else ""),
         y = if (is.null(ylab)) paste("Probability of higher", first$outcome)
             else ylab) +
    prob_theme()
}

## ---- fig-conditionals  (Subsection Bivariate normality)
cairo_pdf("replication-figures/fig-conditionals.pdf", width = 9, height = 10.6)
rho <- 0.5
cols <- c("1" = "#B2182B", "2" = "#1F5A96")
ellipse <- function(level) {
  th <- seq(0, 2 * pi, length.out = 200)
  rad <- sqrt(qchisq(level, df = 2))
  L <- t(chol(matrix(c(1, rho, rho, 1), 2)))
  pts <- t(L %*% rbind(rad * cos(th), rad * sin(th)))
  data.frame(x = pts[, 1], y = pts[, 2], level = level)
}
contours <- do.call(rbind, lapply(c(0.2, 0.5, 0.8, 0.95), ellipse))
theta_fun <- function(d) pnorm(rho * d / sqrt(2 * (1 - rho^2)))
panel_theme <- theme_classic(base_size = 11) +
  theme(axis.line = element_line(colour = "grey35", linewidth = 0.4),
        axis.ticks = element_line(colour = "grey35", linewidth = 0.4),
        plot.margin = margin(4, 8, 4, 4),
        aspect.ratio = 1)

panel_joint <- function(x1, x2) {
  pts <- data.frame(x = c(x2, x1), y = rho * c(x2, x1), who = c("2", "1"))
  ggplot(contours, aes(x, y, group = level)) +
    geom_path(colour = "grey75", linewidth = 0.45) +
    geom_segment(data = pts, aes(x = x, xend = x, y = -3.6, yend = 2.6,
                                 colour = who),
                 inherit.aes = FALSE, linetype = "22", linewidth = 0.5) +
    geom_point(data = pts, aes(x, y, colour = who), inherit.aes = FALSE,
               size = 2.6) +
    annotate("segment", x = x2, xend = x1, y = 2.9, yend = 2.9,
             linewidth = 0.4, colour = "grey20",
             arrow = arrow(ends = "both", length = unit(0.1, "cm"),
                           type = "closed")) +
    annotate("text", x = (x1 + x2) / 2, y = 3.3,
             label = sprintf("Delta == %s * sigma[X]", x1 - x2),
             parse = TRUE, size = 3.4) +
    annotate("text", x = c(x2, x1), y = -3.3, label = c("X[2]", "X[1]"),
             parse = TRUE, hjust = c(1.2, -0.2),
             colour = unname(cols[c("2", "1")]), size = 3.6) +
    scale_colour_manual(values = cols, guide = "none") +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL) +
    coord_equal(xlim = c(-3.6, 3.6), ylim = c(-3.6, 3.6), expand = FALSE) +
    labs(x = "Predictor (X)", y = "Outcome (Y)") +
    panel_theme
}

panel_cond <- function(x1, x2) {
  y <- seq(-3.3, 3.3, length.out = 300)
  d <- rbind(
    data.frame(y = y, f = dnorm(y, rho * x2, sqrt(1 - rho^2)), who = "2"),
    data.frame(y = y, f = dnorm(y, rho * x1, sqrt(1 - rho^2)), who = "1"))
  d$f <- d$f / max(d$f)
  top <- 1
  ggplot(d, aes(y, f, colour = who)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = rho * c(x2, x1), colour = unname(cols[c("2", "1")]),
               linetype = "22", linewidth = 0.4) +
    annotate("text", x = c(-2.1, 2.1), y = top * 0.62,
             label = c("Y ~ '|' ~ X[2]", "Y ~ '|' ~ X[1]"), parse = TRUE,
             colour = unname(cols[c("2", "1")]), size = 3.4) +
    scale_colour_manual(values = cols, guide = "none") +
    scale_x_continuous(breaks = c(-2, 0, 2)) +
    scale_y_continuous(breaks = NULL, limits = c(0, 1.3), expand = c(0, 0)) +
    labs(x = "Outcome (Y)", y = "Density") +
    panel_theme
}

panel_diff <- function(x1, x2) {
  dl <- x1 - x2
  m <- rho * dl
  sdv <- sqrt(2 * (1 - rho^2))
  v <- seq(-3.2, 4.4, length.out = 400)
  d <- data.frame(v = v, f = dnorm(v, m, sdv))
  d$f <- d$f / max(d$f)
  ggplot(d, aes(v, f)) +
    geom_area(data = d[d$v >= 0, ], fill = "#1F5A96", alpha = 0.22) +
    geom_line(linewidth = 0.9, colour = "grey25") +
    geom_vline(xintercept = 0, linetype = "22", linewidth = 0.4,
               colour = "grey35") +
    annotate("text", x = 4.2, y = 1.17, hjust = 1,
             label = sprintf("theta(Delta) == %s", fmt(theta_fun(dl))),
             parse = TRUE, size = 3.6) +
    scale_x_continuous(breaks = c(-2, 0, 2, 4)) +
    scale_y_continuous(breaks = NULL, limits = c(0, 1.3), expand = c(0, 0)) +
    labs(x = expression(Y[1] - Y[2]), y = "Density") +
    panel_theme
}

d_small <- 0.5
d_large <- 2
p_g <- autoplot(posc_uni(r = rho, predictor_name = "predictor",
                         outcome_name = "outcome"),
                mark = c(d_small, d_large)) +
  theme(plot.margin = margin(10, 8, 4, 4))
wrap_plots(
  panel_joint(d_small / 2, -d_small / 2), panel_cond(d_small / 2, -d_small / 2),
  panel_diff(d_small / 2, -d_small / 2),
  panel_joint(d_large / 2, -d_large / 2), panel_cond(d_large / 2, -d_large / 2),
  panel_diff(d_large / 2, -d_large / 2),
  p_g,
  design = "ABC\nDEF\nGGG", heights = c(1, 1, 1.2)) +
  plot_annotation(tag_levels = "A")
invisible(dev.off())

## ---- fig-family  (Subsection Bivariate normality)
cairo_pdf("replication-figures/fig-family.pdf", width = 10, height = 4.2)
rhos <- c(0.1, 0.3, 0.5, 0.7, 0.9)
fits <- lapply(rhos, function(r) {
  posc_uni(r = r, predictor_name = "predictor", outcome_name = "outcome")
})
names(fits) <- paste0("\u03c1 = ", fmt(rhos, 1))
blues <- c("#9ECAE1", "#6BAED6", "#3182BD", "#08519C", "#08306B")
fam <- posc_comp(fits)
p_left <- label_paths(fam, hjust = c(0.8, 0.7, 0.6, 0.5, 0.3),
                      colors = blues)
p_right <- autoplot(posc_uni(r = 0.5, n = 100, predictor_name = "predictor",
                             outcome_name = "outcome"),
                    full = TRUE, mark = c(-1, 1))
p_left | p_right
invisible(dev.off())

## ---- fig-composite  (Subsection Composites of several predictors)
cairo_pdf("replication-figures/fig-composite.pdf", width = 10, height = 4.4)
r_prog <- matrix(c(1.00, 0.30, 0.00, 0.51,
                   0.30, 1.00, 0.10, 0.38,
                   0.00, 0.10, 1.00, 0.31,
                   0.51, 0.38, 0.31, 1.00), 4, 4,
  dimnames = rep(list(c("Function", "Rating", "Adherence", "Recovery")), 2))
pn <- "predictor"
on <- "recovery"
yl <- "Probability of better recovery"
singles <- posc_comp(
  "BF (0.51)" = posc_uni(0.51, predictor_name = pn, outcome_name = on),
  "CR (0.38)" = posc_uni(0.38, predictor_name = pn, outcome_name = on),
  "AD (0.31)" = posc_uni(0.31, predictor_name = pn, outcome_name = on),
  "Composite (0.63)" = posc_multi(r_prog, n = 1e6, outcome = "Recovery",
                                  predictor_name = pn, outcome_name = on))
p_left <- label_paths(singles, hjust = c(0.55, 0.7, 0.8, 0.4), ci = FALSE,
                      ylab = yl)
small <- posc_comp(
  "Composite" = posc_multi(r_prog, n = 100, outcome = "Recovery",
                           predictor_name = pn, outcome_name = on),
  "BF alone" = posc_uni(0.51, n = 100, predictor_name = pn,
                        outcome_name = on))
p_right <- label_paths(small, hjust = c(0.4, 0.7), ylab = yl)
p_left | p_right
invisible(dev.off())

## ---- fig-pairs  (Subsection Confidence bands and binned checks)
cairo_pdf("replication-figures/fig-pairs.pdf", width = 10, height = 4.4)
x_sk <- rnorm(150)
y_sk <- exp(0.8 * x_sk + rnorm(150, sd = 0.6))
emp_sk <- posc_emp(x_sk, y_sk, bins = 8, predictor_name = "x",
                   outcome_name = "y")
sk <- data.frame(x = x_sk, y = y_sk)
ord <- order(x_sk)
pick <- rbind(c(ord[20], ord[110]), c(ord[60], ord[140]), c(ord[75], ord[95]))
seg <- data.frame(x = x_sk[pick[, 1]], y = y_sk[pick[, 1]],
                  xend = x_sk[pick[, 2]], yend = y_sk[pick[, 2]])
seg$type <- ifelse((seg$xend - seg$x) * (seg$yend - seg$y) > 0,
                   "concordant pair", "discordant pair")
p_left <- ggplot(sk, aes(x, y)) +
  geom_point(alpha = 0.35, colour = "grey40") +
  geom_segment(data = seg, aes(x = x, y = y, xend = xend, yend = yend,
                               colour = type), linewidth = 1) +
  geom_point(data = seg, aes(x, y, colour = type), size = 2.5) +
  geom_point(data = seg, aes(xend, yend, colour = type), size = 2.5) +
  scale_colour_manual(values = c("#1F5A96", "#B2182B"), guide = "none") +
  labs(x = "Predictor (x)", y = "Outcome (y)") +
  theme_classic(base_size = 12)
p_right <- autoplot(emp_sk, bins = TRUE, reference = TRUE)
p_left | p_right
invisible(dev.off())

## ---- empirical-u  (Subsection Nonmonotone relationships)
dose <- rnorm(300)
benefit <- -(dose - 0.5)^2 + rnorm(300, sd = 0.5)
emp_u <- posc_emp(dose, benefit, df = 4, bins = 8, standardize = TRUE,
  predictor_name = "dose", outcome_name = "benefit")
predict(emp_u, delta = c(0.5, 1, 2))

## ---- fig-invu  (Subsection Nonmonotone relationships)
cairo_pdf("replication-figures/fig-invu.pdf", width = 10, height = 4.4)
p_left <- ggplot(data.frame(dose, benefit), aes(dose, benefit)) +
  geom_point(alpha = 0.55, colour = "#1F5A96") +
  labs(x = "Dose (SD units)", y = "Clinical benefit") +
  theme_classic(base_size = 12)
p_right <- autoplot(emp_u, bins = TRUE, reference = TRUE) +
  labs(y = "Probability of greater benefit")
p_left | p_right
invisible(dev.off())

## ---- fig-plots  (Subsection Design)
cairo_pdf("replication-figures/fig-plots.pdf", width = 10, height = 8)
demo <- posc_uni(r = 0.5, n = 100, sd_x = 15,
                 predictor_name = "prognostic score", outcome_name = "recovery")
yl <- "Probability of better recovery"
p_a <- autoplot(demo) + labs(y = yl)
p_b <- autoplot(demo, full = TRUE) + labs(y = yl)
p_c <- autoplot(demo, mark = c(10, 20), mark_ci = TRUE, percent = TRUE) +
  labs(y = yl)
two <- posc_comp(
  "r = 0.5" = demo,
  "r = 0.3" = posc_uni(r = 0.3, n = 100, sd_x = 15,
                       predictor_name = "prognostic score",
                       outcome_name = "recovery"))
p_d <- label_paths(two, hjust = c(0.45, 0.7), ylab = yl)
(p_a | p_b) / (p_c | p_d)
invisible(dev.off())

## ---- basic  (Subsection Basic use)
library("posc")
fit <- posc_uni(r = 0.5, n = 100)
fit

## ---- basic-predict  (Subsection Basic use)
predict(fit, delta = c(0.5, 1, 2))
head(as.data.frame(fit, full = FALSE), 3)

## ---- selection  (Subsection Prioritizing patients with a prognostic score)
prog <- posc_uni(r = 0.52, n = 100, sd_x = 10,
  predictor_name = "prognostic score", outcome_name = "recovery")
predict(prog, delta = 12)

## ---- fig-selection  (Subsection Prioritizing patients with a prognostic score)
cairo_pdf("replication-figures/fig-selection.pdf", width = 6, height = 4.2)
autoplot(prog, mark = 12, mark_ci = TRUE) +
  labs(y = "Probability of better recovery")
invisible(dev.off())

## ---- selection-summary  (Subsection Prioritizing patients with a prognostic score)
summary(prog, delta = c(5, 10, 12, 20, 30))

## ---- tbl-selection  (Subsection Prioritizing patients with a prognostic score)
sel_tab <- summary(prog, delta = c(5, 10, 12, 20, 30))
sel_tab$ci <- paste0("[", fmt(sel_tab$lower, 3), ", ", fmt(sel_tab$upper, 3),
                     "]")
knitr::kable(sel_tab[, c("delta", "p", "ci")], digits = 3,
             format = if (knitr::is_latex_output()) "latex" else "html",
             booktabs = TRUE, longtable = FALSE,
             col.names = c("Difference in prognostic score", "Probability",
                           "95% CI"),
             align = c("c", "c", "c"))

## ---- methods  (Subsection Comparing prognostic measures)
methods <- posc_comp(
  "BF (0.51)" = posc_uni(0.51, predictor_name = "predictor",
                         outcome_name = "recovery"),
  "CR (0.38)" = posc_uni(0.38, predictor_name = "predictor",
                         outcome_name = "recovery"),
  "PSR (0.26)" = posc_uni(0.26, predictor_name = "predictor",
                          outcome_name = "recovery"))

## ---- comp-test  (Subsection Comparing prognostic measures)
posc_comp(posc_uni(0.51, n = 100), posc_uni(0.38, n = 120))

## ---- composite  (Subsection Composites and linear models)
r_prog <- matrix(c(1.00, 0.30, 0.00, 0.51,
                   0.30, 1.00, 0.10, 0.38,
                   0.00, 0.10, 1.00, 0.31,
                   0.51, 0.38, 0.31, 1.00), 4, 4,
  dimnames = rep(list(c("Function", "Rating", "Adherence", "Recovery")), 2))
comp <- posc_multi(r_prog, n = 500, outcome = "Recovery",
  predictor_name = "predictor", outcome_name = "recovery")
comp

## ---- lm  (Subsection Composites and linear models)
sim <- data.frame(baseline = rnorm(200), adherence = rnorm(200))
sim$recovery <- sim$baseline + 0.5 * sim$adherence + rnorm(200)
mod <- posc_lm(lm(recovery ~ baseline + adherence, data = sim))
predict(mod, delta = c(0.5, 1, 2))

## ---- fig-methods  (Subsection Composites and linear models)
cairo_pdf("replication-figures/fig-methods.pdf", width = 10, height = 4.4)
p_left <- label_paths(methods, hjust = c(0.45, 0.6, 0.75),
                      ylab = "Probability of better recovery")
p_right <- autoplot(mod) + labs(y = "Probability of better recovery")
p_left | p_right
invisible(dev.off())

## ---- empirical-va  (Subsection An empirical POSC: performance status and survival)
va <- MASS::VA[MASS::VA$status == 1, ]
emp_va <- posc_emp(va$Karn, va$stime, bins = 8,
  predictor_name = "Karnofsky score", outcome_name = "survival time")
emp_va

## ---- empirical-va-ref  (Subsection An empirical POSC: performance status and survival)
ref_va <- posc_uni(cor(va$Karn, va$stime), n = nrow(va), sd_x = sd(va$Karn))

## ---- empirical-bins  (Subsection An empirical POSC: performance status and survival)
posc_bins(emp_va)

## ---- fig-empirical  (Subsection An empirical POSC: performance status and survival)
cairo_pdf("replication-figures/fig-empirical.pdf", width = 10, height = 4.4)
p_left <- ggplot(va, aes(Karn, stime)) +
  geom_point(alpha = 0.55, colour = "#1F5A96") +
  labs(x = "Karnofsky performance status", y = "Survival time (days)") +
  theme_classic(base_size = 12)
p_right <- autoplot(emp_va, bins = TRUE, reference = TRUE) +
  labs(y = "Probability of longer survival")
p_left | p_right
invisible(dev.off())

## ---- versions  (Section Computational details)
r_ver <- paste(R.version$major, R.version$minor, sep = ".")
pkg_ver <- function(p) as.character(packageVersion(p))

## ---- Quantities reported inline in the text
## In the order in which they appear in the manuscript.

## Subsection Bivariate normality
print(fmt(100 * pnorm(0.3 / sqrt(2 * (1 - 0.09))), 0))
print(fmt(sqrt(2 * 0.75) / 0.5 * qnorm(0.75), 2))

## Subsection Nonmonotone relationships
print(fmt(100 * pnorm(0.5 * sqrt(2)), 0))
print(fmt(cor(dose, benefit), 2))
print(fmt(pnorm(0.5 * sqrt(2)), 2))

## Subsection Prioritizing patients with a prognostic score
print(fmt(predict(prog, 12)$lower, 3))
print(fmt(predict(prog, 12)$upper, 3))
print(fmt(predict(prog, 30)$p, 2))
print(fmt(10 * sqrt(2 * (1 - 0.52^2)) / 0.52 * qnorm(0.75), 1))

## Subsection Comparing prognostic measures
print(fmt(100 * pnorm(0.51 / sqrt(2 * (1 - 0.51^2))), 0))
print(fmt(100 * pnorm(0.38 / sqrt(2 * (1 - 0.38^2))), 0))

## Subsection Composites and linear models
print(fmt(comp$details$r_unadjusted, 3))

## Subsection An empirical POSC: performance status and survival
print(fmt(cor(va$Karn, va$stime), 2))
print(fmt(cor(va$Karn, va$stime, method = "spearman"), 2))
print(fmt(predict(emp_va, 20)$p, 2))
print(fmt(predict(emp_va, 20)$lower, 2))
print(fmt(predict(emp_va, 20)$upper, 2))
print(fmt(predict(ref_va, 20)$p, 2))

## Section Computational details
print(r_ver)
print(pkg_ver("posc"))
print(pkg_ver("ggplot2"))
print(pkg_ver("patchwork"))
print(pkg_ver("geomtextpath"))
print(pkg_ver("MASS"))
