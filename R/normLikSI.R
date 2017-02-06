#' @export
#' @import stats
normLikSI <- function(dat, level, tol=.001, conf=F, B=500){
  pl <- function(z) sapply(1:length(z), function(w) sum(dnorm(dat, z[w], sqrt(sum((dat-z[w])^2)/length(dat)), log=T)))
  pl2 <- function(z) exp(pl(z)-pl(mean(dat)))-1/8
  llim <- uniroot(pl2, c(mean(dat)-2*sd(dat), mean(dat)), tol=tol)$root
  ulim <- uniroot(pl2, c(mean(dat), mean(dat)+2*sd(dat)), tol=tol)$root
  if(conf==F) {return(c(llim, ulim))
  } else{
    covs <- 0
    for(i in 1:B){
      bdat <- rnorm(length(dat), mean(dat), sd(dat))
      bsi <- normLikSI(bdat, level)
      if(bsi[1]<mean(dat) & bsi[2]> mean(dat)){covs <- covs+1}
    }
    out <- list(c(llim, ulim), covs/B)
    names(out) <- c("si", "conf.equiv")
    return(out)
  }
}
