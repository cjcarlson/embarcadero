#' @title Spatial prediction using dbarts
#'
#' @description
#'
#' dbarts analog code for prediction
#'
#' @param model model object from dbarts package
#' @param inputstack That's your rasterStack. check var names
#'
#' @export
#'

predict.dbart.raster <- function(model, inputstack, cores=3) {

  input.matrix <- as.matrix(getValues(inputstack))
  output = predict(model,
                   input.matrix)
  output = pnorm(output)
  results <- inputstack
  output.m <- t(matrix(colMeans(output),
                       nrow = ncol(results),
                       ncol = nrow(results)))
  output.r <- raster(output.m,
                     xmn=xmin(inputstack[[1]]), xmx=xmax(inputstack[[1]]),
                     ymn=ymin(inputstack[[1]]), ymx=ymax(inputstack[[1]]),
                     crs=inputstack[[1]]@crs) + inputstack[[1]]*0

  plot(output.r)
  return(output.r)

}
