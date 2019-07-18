
# well that's the end of that. time to rewrite
# add levels to it option
# add an option for CI this time
# xind using as.name

partial <- function(model, x.vars=NULL, equal=FALSE, smooth=1,
                    posterior=NULL) {
  
# This is for something else ultimately: attr(bartFit$fit$data@x, "term.labels")
  
# This is where equal happens
if (is.null(x.vars)) { raw <- bartFit$fit$data@x} else ( raw <- bartFit$fit$data@x[,x.vars])

if(equal==TRUE) {
    minmax <- data.frame(mins = apply(raw, 2, min),
    maxs = apply(raw, 2, max))
    lev <- lapply(c(1:nrow(minmax)), function(i) {seq(minmax$mins[i], minmax$maxs[i], (minmax$maxs[i]-minmax$mins[i])/(10*smooth))})
} else {
    lev <- NULL
}
  
# This is the wrapper itself
pd <- pdbart(model, xind = x.vars, levs = lev, pl=TRUE)
#for (i )
# as.name

}
