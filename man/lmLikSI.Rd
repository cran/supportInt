\name{lmLikSI}
\alias{lmLikSI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Profile support intervals for lm coefficients
}
\description{
Calculates profile likelihood intervals using data returned from the 'ProfileLikelihood' package for each coefficient of a lm.
}
\usage{
lmLikSI(lm.mod, level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lm.mod}{
A fit lm object.
}
  \item{level}{
Desired level of support interval (1/level).
}
}

\value{
  \item{ }{a matrix of support intervals for each coefficient}
}

\author{
Derek Smith
}

\examples{
set.seed(10)
x <- rnorm(50, 0 , 5)
y <- sapply(1:length(x), function(z) 3+.5*x[z]+rnorm(1, 0, 5))
lm.obj <- lm(y~x)
lmLikSI(lm.obj, 8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
