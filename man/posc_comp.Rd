\name{posc_comp}
\alias{posc_comp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
posc_comp: Comparison Plot of Probability of Outcome Suporiority Curves
}
\description{
A visual comparison of two POSC curves.
}
\usage{
posc_comp(posc_1, posc_2, color1 = 'red', color2 = 'blue')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{posc_1}{POSC object 1 (\code{posc_uni()}, \code{posc_multi()}), \code{posc_lm()}).}
  \item{posc_2}{POSC object 2 (\code{posc_uni()}, \code{posc_multi()}), \code{posc_lm()}).}
  \item{color1}{Color of POSC 1.}
  \item{color2}{Color of POSC 2.}
}

\details{
Visual comparison and statistical test of the difference between two POSCs.
}
\value{
\item{statistics}{a \code{data.frame()} object containing the following statistics: r = the correlation between predicted x and y from POSC 1 and POSC 2, n = sample size from POSC 1 and POSC 2, z = z-statistic for the difference between the two correlation coefficients, p = p-value between}
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
\code{posc_uni}, \code{posc_multi}, \code{posc_lm}
}

\examples{
r1 = .5
n1 = 100

r2 = .3
n2 = 100

posc_1 = posc_uni(r = r1, n = n1)
posc_2 = posc_uni(r = r2, n = n2)

posc_comp(posc_1, posc_2, color1 = 'red', color2 = 'darkgreen')

{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
