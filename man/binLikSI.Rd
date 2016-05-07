\name{binLikSI}
\alias{binLikSI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Binomial support interval calculation
}
\description{
This function takes binomial data and using a root finding algorithm to calculate the support interval at the user's specified level.  If conf=T, the function will attempt to estimate the confidence level of the support interval using a modified parametric bootstrap approach.
}
\usage{
binLikSI(dat, n, level, tol = 0.001, conf = F, B = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
number of success in binomial experiment
}
  \item{n}{
number of trials in binomial experiment
}
  \item{level}{
desired support level
}
  \item{tol}{
tolerance passed to uniroot for finding SI endpoints
}
  \item{conf}{
Logical, whether or not bootstrap should be performed to estimate confidence level
}
  \item{B}{
Number of bootstraps to be performed
}
}

\value{
    \item{si}{The desired support interval}
    \item{conf.equiv}{The estimated confidence level of the SI}

}

\author{
Derek Smith
}


\examples{
set.seed(10)
binLikSI(dat=4, n=10, level=9)
binLikSI(dat=4, n=10, level=8, conf=TRUE)
}
\keyword{ Support Interval }
\keyword{ Calibrated Support Interval }
