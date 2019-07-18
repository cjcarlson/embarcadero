#' @title Better, nicer, friendlier partials
#'
#' @description
#'
#' I don't really like the partial visualizations in dbarts so I made some of my own, with a little more customization options.
#'
#' @param model A dbarts model object
#' @param xind A list of the variables for which you want to run the partials. Defaults to doing all of them.
#' @param equal Spacing x levels equally instead of using quantiles
#' @param smooth A multiplier for how much smoother you want the sampling of the levels to be.
#' @param posterior The way the posterior is visualized: defaults to nothing, but can do "CI" for the credible interval or "trace" for traceplots
#'
#'
#' @return Returns a nice plot
#'
#' @example
#'
#' f <- function(x) { return(0.5 * x[,1] + 2 * x[,2] * x[,3]) }
#'  sigma <- 0.2
#' n     <- 100
#' set.seed(27)
#' x <- matrix(2 * runif(n * 3) -1, ncol = 3)
#' colnames(x) <- c('rob', 'hugh', 'ed')
#' Ey <- f(x)
#' y  <- rnorm(n, Ey, sigma)
#' df <- data.frame(y, x)
#' set.seed(99)
#' bartFit <- bart(y ~ rob + hugh + ed, df,
#'                keepevery = 10, ntree = 100, keeptrees = TRUE)
#'
#'
#'
#' @export
#'
#'

partial <- function(model, x.vars=NULL, equal=FALSE, smooth=1,
                    posterior=NULL) {
  
# This is for something else ultimately: attr(bartFit$fit$data@x, "term.labels")
# This is where equal happens
  
if (is.null(x.vars)) { raw <- bartFit$fit$data@x} else ( raw <- bartFit$fit$data@x[,x.vars])


if(equal==TRUE) {
    minmax <- data.frame(mins = apply(raw, 2, min),
                         maxs = apply(raw, 2, max))
    lev <- lapply(c(1:nrow(minmax)), function(i) {seq(minmax$mins[i], minmax$maxs[i], (minmax$maxs[i]-minmax$mins[i])/(10*smooth))})
    pd <- pdbart(model, xind = x.vars, levs = lev, pl=TRUE)
} else {
  levq = c(0.05, seq(0.1, 0.9, 0.1/smooth), 0.95)
  pd <- pdbart(model, xind = x.vars, levquants = levq, pl=TRUE)
}
  
# This is the wrapper itself
  
#for (i )
# as.name
#}

# This is the wrapper itself
pd <- pdbart(model, xind = x.vars, levs = lev, pl=TRUE)
#for (i )
# as.name

}
