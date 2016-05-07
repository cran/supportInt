\name{glmLikSI}
\alias{glmLikSI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Profile support intervals for glm coefficients
}
\description{
Calculates profile likelihood intervals using data returned from the 'ProfileLikelihood' package for each coefficient of a glm.
}
\usage{
glmLikSI(glm.mod, level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{glm.mod}{
A fit glm object.
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
expit <- function(z) exp(z)/(1+exp(z))
p <- expit(.1+ .4*x)
y <- sapply(1:length(p), function(z) rbinom(1,1, p[z]))
glm.obj <- glm(y~x, family="binomial")
glmLikSI(glm.obj, 8)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
