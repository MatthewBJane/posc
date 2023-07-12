\name{posc_uni}
\alias{posc_uni}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{posc_uni: Probability of Outcome Superiority Curve (POSC) with a Single Predictor}
\description{This function generates a probability of outcome superiority curve from a given correlation coefficient and sample size.}
\usage{posc_uni(r, n, sd_x = 1, predictor_name = "X", outcome_name = "Y",
color = "black", ci.lvl = .95)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{ The correlation between predictor and outcome variable. }
  \item{n}{ The sample size. }
  \item{sd_x}{ The standard deviation of the predictor variable. The default is 1.}
  \item{predictor_name}{ String indicating the name of the predictor variable. The default is "X".}
  \item{outcome_name}{ String indicating the name of the outcome variable. The default is "Y".}
  \item{color}{ Color used to generate the overall color scheme. The default is "black".}
  \item{ci.lvl}{ Confidence interval proportion (0-1). }
}
\details{
Probability of Outcome Superiority Curves (POSCs) are calculated from the probability of superiority in Y given the difference in the predictor:

\deqn{ p(y_i>y_j | x_i > x_j) = \phi \left( \frac{r \Delta _{x}}{\sqrt{2(1-r^2)}} \right) }

}
\value{
\item{parameters}{a \code{data.frame()} object containing the following parameters: r = the correlation between x and y, n = sample size }
\item{predict_matrix}{a \code{data.frame()} object with the following predicted values: delta_x = the x-values (difference in predictor), py = probability of higher/lower outcome at each delta_x, pyUCI = Upper confidence bound of py, pyLCI = Lower confidence bound of py. }
\item{posc_plot_object}{a \code{ggplot()} object containing the POSC plot}
}

\author{
Matthew B. Jané
}



\seealso{
\code{posc_multi}, \code{posc_lm}, \code{posc_comp}
}
\examples{
r = .5
n = 100
sd_x = 30

posc_uni(r = r, n = n, sd_x = 30, ci.lvl = .90)

}
