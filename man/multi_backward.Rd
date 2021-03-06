% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/multi_backward.R
\name{multi_backward}
\alias{multi_backward}
\title{Multi-backward step}
\usage{
multi_backward(data, crit = mbic, ...)
}
\arguments{
\item{data}{an object of class \code{big}.}

\item{crit}{a function defining the model selection criterion. You can use
your own function or one of these: \code{bic}, \code{mbic}, \code{mbic2},
\code{aic}, \code{maic}, \code{maic2}.}

\item{...}{optional arguments to \code{crit}.}
}
\value{
An object of class \code{big}.
}
\description{
Remove the worst variables from a model as long as they reduce the given
criterion (backward elimination).
}
\details{
Type \code{browseVignettes("bigstep")} for more details.
}
\examples{
set.seed(1)
n <- 30
p <- 10
X <- matrix(rnorm(n * p), ncol = p)
y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
d <- prepare_data(y, X)
d \%>\%
  fast_forward(crit = aic) \%>\%
  multi_backward(crit = bic)

}
