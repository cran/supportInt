\name{poisLikSI}
\alias{poisLikSI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Poisson support interval calculation
}
\description{
This function takes Poisson data and using a root finding algorithm to calculate the support interval at the user's specified level.  If conf=T, the function will attempt to estimate the confidence level of the support interval using a parametric bootstrap approach.
}
\usage{
poisLikSI(dat, level, tol = 0.001, conf = F, B = 500)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
Numeric; outcome from a Poisson experiment
}
  \item{level}{
Desired support level
}
  \item{tol}{
tolerance argument passed to uniroot for finding SI endpoints
}
  \item{conf}{
Logical; Whether or not to simulate the confidence level of the SI
}
  \item{B}{
Number of simulations to determine coverage if conf=T
}
}

\value{

  \item{si }{1/level support interval}
  \item{conf.equiv }{estimated confidence level of interval returned}

}

\author{
Derek Smith
}

\examples{

poisLikSI(4, 8)
poisLikSI(4, 8, conf=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
