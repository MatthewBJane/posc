#' POSC from a single correlation (bivariate normal)
#'
#' Computes the probability of outcome superiority curve implied by a
#' correlation between a predictor and an outcome, assuming the two are
#' bivariate normal.
#'
#' @details
#' If \eqn{X} and \eqn{Y} are bivariate normal with correlation \eqn{r}, then
#' for two randomly chosen people whose predictor scores differ by \eqn{\Delta}
#' \deqn{P(Y_1 > Y_2 \mid X_1 - X_2 = \Delta) =
#'   \Phi\left(\frac{r\,\Delta / \sigma_X}{\sqrt{2(1 - r^2)}}\right),}
#' where \eqn{\Phi} is the standard normal distribution function and
#' \eqn{\sigma_X} the standard deviation of the predictor. Set `sd_x` to put
#' \eqn{\Delta} on the raw scale of the predictor; the default `sd_x = 1`
#' gives differences in standard-deviation units.
#'
#' The curve is monotone in \eqn{r}, so the pointwise confidence band is
#' obtained by evaluating it at the limits of the Fisher-\eqn{z} confidence
#' interval for \eqn{r}.
#'
#' @param r Correlation between predictor and outcome, in \eqn{(-1, 1)}.
#' @param n Sample size used to estimate `r`. Needed for the confidence band;
#'   use `NULL` to draw the curve without one.
#' @param sd_x Standard deviation of the predictor. The curve is expressed in
#'   the predictor's units; the default `1` gives standard-deviation units.
#' @param level Confidence level of the band.
#' @param max_delta Largest difference in the predictor to evaluate. Defaults
#'   to 3 standard deviations of the predictor.
#' @param predictor_name,outcome_name Labels used in printed output and plots.
#'
#' @return An object of class `"posc"`: a list with elements
#'   \describe{
#'     \item{curve}{data frame with columns `delta`, `p`, `lower`, `upper`,
#'       over a symmetric grid of differences.}
#'     \item{method, r, n, level, sd_x, max_delta, labels, details}{
#'       information about how the curve was computed.}
#'   }
#'   Use [plot()][plot.posc], [summary()][summary.posc] and
#'   [predict()][predict.posc] to work with it.
#'
#' @seealso [posc_emp()] for a curve estimated from raw data without assuming
#'   normality; [posc_comp()] to compare curves.
#' @examples
#' # Differences in SD units
#' fit <- posc_uni(r = 0.5, n = 100)
#' fit
#' plot(fit)
#'
#' # Raw units: an IQ test with SD = 15
#' iq <- posc_uni(r = 0.5, n = 100, sd_x = 15, predictor_name = "IQ",
#'                outcome_name = "job performance")
#' predict(iq, delta = c(5, 15, 30))
#' @export
posc_uni <- function(r, n = NULL, sd_x = 1, level = 0.95,
                     max_delta = 3 * sd_x,
                     predictor_name = "X", outcome_name = "Y") {
  check_scalar(r, "r", -1, 1, lower_open = TRUE, upper_open = TRUE)
  check_n(n)
  check_scalar(sd_x, "sd_x", 0, Inf, lower_open = TRUE)
  check_level(level)
  check_scalar(max_delta, "max_delta", 0, Inf, lower_open = TRUE)
  check_string(predictor_name, "predictor_name")
  check_string(outcome_name, "outcome_name")

  new_posc(
    predict_fun = normal_predictor(r, n, sd_x),
    method = "normal", r = r, n = n, level = level,
    max_delta = max_delta, sd_x = sd_x,
    predictor_name = predictor_name, outcome_name = outcome_name,
    sd_units = sd_x == 1, call = match.call()
  )
}
