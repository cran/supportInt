\name{normLikSI}
\alias{normLikSI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normal support interval calculation.
}
\description{
This function calculates support intervals for the mean of normal data. Confidence levels are calculate from the formula if conf=T.
}
\usage{
normLikSI(dat, level, tol = 0.001, conf = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
a numeric vector
}
  \item{level}{
support interval relative likelihood
}
  \item{tol}{
tolerance passed to uniroot for finding likelihood endpoints
}
  \item{conf}{
Logical, whether or not confidence levels should be returned.
}
}

\value{

  \item{si }{1/level support interval}
  \item{conf.equiv }{Calculated confidence level}

}

\author{
Derek Smith
}

\examples{
set.seed(10)
normLikSI(rnorm(10), 9)
normLikSI(rnorm(10), 9, conf=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
