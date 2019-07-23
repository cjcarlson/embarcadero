#'  @export

bigstack <- function(stack, by) {
  if(!(class(stack)=='RasterStack')) { stop("Needs a raster stack dude") }
  pb <- txtProgressBar(style=3, min=0, max=nlayers(stack))
  setTxtProgressBar(pb, 0)
  for(i in 1:nlayers(stack)) {
    vx <- velox(stack[[i]])
    vx$aggregate(factor=c(by,by), aggtype='mean')
    if (i == 1) {
      bigstack <- stack(vx$as.RasterLayer())
    } else {
      bigstack <- stack(bigstack,vx$as.RasterLayer())
    }
    setTxtProgressBar(pb, i)
  }  
  names(bigstack) <- names(stack)
  return(bigstack)
}
