\name{KSPA}
\alias{KSPA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A Test for Comparing the Predictive Accuracy of Two Sets of Forecasts
}
\description{
This function introduces a complement statistical test for distinguishing between
the predictive accuracy of two sets of forecasts. We propose a non-parametric test founded
upon the principles of the Kolmogorov-Smirnov (KS) test, referred to as the KS Predictive
Accuracy (KSPA) test. The KSPA test is able to serve two distinct purposes. Initially, the
test seeks to determine whether there exists a statistically significant difference between the
distribution of forecast errors, and secondly it exploits the principles of stochastic dominance
to determine whether the forecasts with the lower error also reports a stochastically smaller
error than forecasts from a competing model, and thereby enables distinguishing between
the predictive accuracy of forecasts.
}
\usage{
KSPA(Error1, Error2, method = c("abs", "sqe", "biqc"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Error1}{
the forecast errors from  model 1.
}
  \item{Error2}{
the forecast errors from  model 2.
}
  \item{method}{
character string specifying the used loss function (abs as absolute errors, sqe as square errors, or biqc as fourth power of errors).
}
}
\details{
Input the forecast errors from two models. Let Error1 show errors from the model with the lower error based on some loss function.
}
\value{
Draw histograms for the forecast errors from each model.

Plot the cdf of forecast errors from each model.

And a list.
  \item{ks.oneside }{One-sided KSPA test}
  \item{ks.twoside }{Two-sided KSPA test.}
}
\references{
Hassani, H., & Silva, E. S. (2015). A Kolmogorov-Smirnov based test for comparing the predictive accuracy of two sets of forecasts. Econometrics, 3(3), 590-609.
}
\author{
Hossein Hassani and Emmanuel Sirimal Silva and Leila Marvian Mashhad.
}

\seealso{
\code{\link{ks.test}}
}
\examples{
x <- rnorm(40); y <- runif(30)
KSPA(x, y, method = "sqe")
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
