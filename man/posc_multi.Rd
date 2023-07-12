\name{posc_multi}
\alias{posc_multi}

\title{posc_multi: Probability of Outcome Superiority Curve (POSC) with Multiple Predictors}
\description{This function plots a probability of outcome superiority curve from a given correlation coefficient and sample size.}
\usage{posc_multi(r_mat, n, outcome_idx,
predictor_name = "Predicted Y", outcome_name = "Y",
color = "black", ci.lvl = .95)}

\arguments{
  \item{r_mat}{ A correlation matrix between multiple predictor variables and a single outcome variable.}
  \item{n}{ The sample size. }
  \item{outcome_idx}{ An integer denoting the row/column of the outcome variable within the correlation matrix.}
  \item{predictor_name}{ String indicating the name of the predictor variable. The default is "Predicted Y".}
  \item{outcome_name}{ String indicating the name of the outcome variable. The default is "Y".}
  \item{color}{ Color used to generate the overall color scheme. The default is "black".}
  \item{ci.lvl}{ Confidence interval proportion (0-1). }
}
\details{
Probability of Outcome Superiority Curves with Multiple predictors (mPOSCs) are calculated from the probability of superiority given the difference in the optimal linear combination of predictors:

\deqn{ p(y_h>y_l | \hat{y}_h > \hat{y}_l) = \phi \left( \frac{r \Delta _{\hat{y}}}{\sqrt{2(1-r^2)}} \right) }

Where r is the composite correlation coefficient between the optimal linear combination of predictors (\eqn{\hat{y}}) and the outcome (\eqn{y}). The composite correlation coefficient is calculated by:

\deqn{\beta_i = R^{-1}_{xx} R_{xy} }

\deqn{w_i = \frac{ \beta_i }{\sum \beta_i} }

\deqn{r = \frac{w_i \sum R_{xy} }{w_i \sum R_{xx} w_i }}

}
\value{
\item{parameters}{a \code{data.frame()} object containing the following parameters: r = the correlation between predicted y and actual y from the regression model, n = sample size }
\item{predict_matrix}{a \code{data.frame()} object with the following predicted values: delta_x = the x-values (difference in predictor), py = probability of higher/lower outcome at each delta_x, pyUCI = Upper confidence bound of py, pyLCI = Lower confidence bound of py. }
\item{posc_plot_object}{a \code{ggplot()} object containing the POSC plot}
}

\author{Matthew B. Jané}

\seealso{ \code{posc_uni}, \code{posc_lm}, \code{posc_comp} }
\examples{

r_mat = rbind(X1 = c(1.0,  0.4,  0.3,  0.2),
              X2 = c(0.4,  1.0, -0.3,  0.2),
              X3 = c(0.3, -0.3,  1.0,  0.3),
              Y  = c(0.2,  0.2,  0.3,  1.0))
n = 100

posc_multi(r_mat = r_mat, n = n, outcome_idx = 4, ci.lvl = .90)

}
