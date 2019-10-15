#' @title Big stack
#'
#' @description
#'
#' Big stack is an incredibly goofy convenience tool (analogous to raster::aggregate) I use that runs a velox wrapper to aggregate up a rasterstack. This is useful if you're doing high-res mapping and you want to do some predictive checks. 
#' 
#' @param stack A raster stack. It can be a one-layer stack if you'd like, but then just use velox's $aggregate function.
#' @param by The factor by which you want to aggregate your rasterstack.
#' 
#' @export

bigstack <- function(stack, by) {
  crs0 <- stack@crs
  
  if(!(class(stack)=='RasterStack')) { stop("Needs a raster stack dude") }
  pb <- txtProgressBar(style=3, min=0, max=nlayers(stack))
  setTxtProgressBar(pb, 0)
  for(i in 1:nlayers(stack)) {
    vx <- velox(stack[[i]])
    vx$aggregate(factor=c(by,by), aggtype='mean')
    if (i == 1) {
      bigstack <- stack(vx$as.RasterLayer())
      bigstack@crs <- crs0
    } else {
      v <- vx$as.RasterLayer()
      v@crs <- crs0 
      bigstack <- stack(bigstack,v)
    }
    setTxtProgressBar(pb, i)
  }  
  names(bigstack) <- names(stack)
  return(bigstack)
}

