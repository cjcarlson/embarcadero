
#' @title Spatial prediction using BART
#'
#' @description
#' I got really sick of trying to do prediction using BART for species distribution models so here's a so-far-one-function package that fixes this. Bababooey
#'
#' @param model Dude that's a model. It better be a freakin lBART model dude
#' @param inputstack That's your rasterStack, boss
#'
#'
#' @export
#'

predict.bart.raster <- function(model, inputstack) {

  input.matrix <- as.matrix(getValues(inputstack))
  output = predict.lbart(model,
                         input.matrix,
                         mc.cores=3)

  results <- inputstack
  output.m <- t(matrix(output$prob.test.mean,
                       nrow = ncol(results),
                       ncol = nrow(results)))
  output.r <- raster(output.m,
                     xmn=xmin(inputstack[[1]]), xmx=xmax(inputstack[[1]]),
                     ymn=ymin(inputstack[[1]]), ymx=ymax(inputstack[[1]]),
                     crs=inputstack[[1]]@crs) +
    inputstack[[1]]*0
  plot(output.r)
  return(output.r)

}
