\name{posc_lm}
\alias{posc_lm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{posc_lm: Probability of Outcome Superiority Curve (POSC) with a Single Predictor}
\description{This function generates a probability of outcome superiority curve from a given correlation coefficient and sample size.}
\usage{posc_lm(model, predictor_name = 'Predicted Y', outcome_name = 'Y', color = 'black', ci.lvl = .95)}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{A \code{lm()} or \code{glm()} object.}
  \item{predictor_name}{ String indicating the name of the predictor variable. The default is "Predicted Y".}
  \item{outcome_name}{ String indicating the name of the outcome variable. The default is "Y".}
  \item{color}{ Color used to generate the color scheme. The default is "black".}
  \item{ci.lvl}{ Confidence interval proportion (0-1). }
}
\details{
Probability of Outcome Superiority Curves (POSCs) are calculated from the probability of superiority given the difference in the predictor:

\deqn{ p(y_h>y_l | \hat{y}_h > \hat{y}_l) = \phi \left( \frac{r \Delta _{\hat{y}}}{\sqrt{2(1-r^2)}} \right) }
}

\value{
A list containing the following:
\item{parameters}{a \code{data.frame()} object containing the following parameters: r = the correlation between predicted y and actual y from the regression model, n = sample size }
\item{predict_matrix}{a \code{data.frame()} object with the following predicted values: delta_x = the x-values (difference in predictor), py = probability of higher/lower outcome at each delta_x, pyUCI = Upper confidence bound of py, pyLCI = Lower confidence bound of py. }
\item{posc_plot_object}{A \code{ggplot()} object containing the POSC plot}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Matthew B. Jané
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{posc_uni}, \code{posc_multi}, \code{posc_comp}
}

\examples{

x1 = rnorm(100,0,1)
x2 = rnorm(100,0,1)
y = x1 + x2 + rnorm(100,0,1)
mdl = lm(y ~ x1 + x2)

posc_lm(model = mdl,color = "darkgreen")

}
