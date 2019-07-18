#' @title Spatial prediction using dbarts
#'
#' @description
#'
#' Spatial prediction of a BART model
#'
#' @param model model object from dbarts package
#' @param inputstack A RasterStack of environmental predictors; make sure they have the same names as the predictors that went into the model object.
#'
#' @export
#'

predict.dbart.raster <- function(model, inputstack, ci=FALSE, plots=FALSE) {

  input.matrix <- as.matrix(getValues(inputstack))
  output = predict(model,
                   input.matrix)
  output = pnorm(output)
  output.m <- t(matrix(colMeans(output),
                       nrow = ncol(inputstack),
                       ncol = nrow(inputstack)))
  output.r <- raster(output.m,
                     xmn=xmin(inputstack[[1]]), xmx=xmax(inputstack[[1]]),
                     ymn=ymin(inputstack[[1]]), ymx=ymax(inputstack[[1]]),
                     crs=inputstack[[1]]@crs) + inputstack[[1]]*0

  if(ci==TRUE) {

    output.m.U <- t(matrix(apply(output, 2, function(x) quantile(x, probs=0.95)),
                         nrow = ncol(inputstack),
                         ncol = nrow(inputstack)))
    output.r.U <- raster(output.m.U,
                       xmn=xmin(inputstack[[1]]), xmx=xmax(inputstack[[1]]),
                       ymn=ymin(inputstack[[1]]), ymx=ymax(inputstack[[1]]),
                       crs=inputstack[[1]]@crs) + inputstack[[1]]*0


    output.m.L <- t(matrix(apply(output, 2, function(x) quantile(x, probs=0.05)),
                           nrow = ncol(inputstack),
                           ncol = nrow(inputstack)))
    output.r.L <- raster(output.m.L,
                         xmn=xmin(inputstack[[1]]), xmx=xmax(inputstack[[1]]),
                         ymn=ymin(inputstack[[1]]), ymx=ymax(inputstack[[1]]),
                         crs=inputstack[[1]]@crs) + inputstack[[1]]*0

    if(plots==TRUE){
    par(mfrow=c(2,2))
    plot(output.r,main="Mean")
    plot(output.r.U-output.r.L,main="95% CI range")
    plot(output.r.L,main='Lower 95% CI')
    plot(output.r.U,main='Upper 95% CI')
    }

    return(list(mean=output.r, upper.ci=output.r.U, lower.ci=output.r.L))

  } else { if(plots==TRUE){plot(output.r)}; return(output.r) }


}
