\name{calibSI}
\alias{calibSI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Attempts to find a support interval with the desired confidence level for binomial, poisson or normal data.
}
\description{
This function performs a grid search to find the lowest support level that has the desired coverage probability.  It begins by searching a grid of relative likelihood values using a fixed number of parametric bootstraps at each point.  The likelihood value is then refined using a golden section based search.
}
\usage{
calibSI(dat, n = NULL, family, conf.level = 0.95, B = 2000, 
gridlo = 4, gridhi = 20, gridix = 2, tol = 0.03)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
This argument is the number of success if family='binomial', the number of occurences if family='poisson', or a vector of normal draws if family='normal'.
  
}
  \item{n}{
Number of trials if family='binomial'
}
  \item{family}{
Data type entered, accepts "binomial", "poisson", "pois", "gaussian", "normal"
}
  \item{conf.level}{
target confidence level
}
  \item{B}{
Number of parametric bootstraps to use to estimate coverage probability after the initial grid search.
}
  \item{gridlo}{
Lowest relative likelihood considered in the initial grid search
}
  \item{gridhi}{
Highest relative likelihood considered in the initial grid search
}
  \item{gridix}{
size of steps to be checked between gridlo and gridhi in initial search
}
  \item{tol}{
acceptable error in the likelihood support level returned
}
}

\value{

  \item{si }{The support interval estimated to have the desired confidence level}
  \item{support.level }{The relative likelihood defining the interval's endpoints}
  \item{init.grid }{Relative likelihoods checked as part of the initial gridsearch and their estimated coverage}
}

\author{
Derek Smith
}

\examples{
set.seed(10)
calibSI(rnorm(15), family="normal", conf.level=.95, B=1000)
}

