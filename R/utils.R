#' @export

theme_bluewhite <- function (base_size = 11, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightblue"),
      panel.border = element_rect(color = "black", fill = NA),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black")
    )
}

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
