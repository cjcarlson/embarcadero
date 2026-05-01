#' @title Spatial partial dependence plots
#'
#' @description
#'
#' Project partial dependence surfaces in the real, geography-having world: see the spatial signal of any given variable's contribution to the model predictions.
#'
#' @param model A dbarts model object
#' @param envs The rasterStack of environmental variables, OR (this is new functionality and mostly untested, but important if you're doing tSDMs) a list of rasterstacks (e.g., climate layers by year). If it's a list, this will return a rasterstack of spartials ordered by that list, then by variable (e.g., x1-stack1, x1-stack2, x2-stack1, x2-stack2)
#' @param x.vars The particular x variables you want to map
#' @param equal Equal spacing on x breaks (versus quantiles).
#' @param smooth Smoothing factor for the x breaks (works like partials).
#' @param transform Backtransform the pnorm or not
#' @param save Turn this on if you want to save the outputs as a RasterStack, for nicer use/plotting elsewhere.
#'
#' @return Returns a nice plot
#'
#' @export

spartial <- function(model, envs, x.vars=NULL,
                     equal=FALSE, smooth=1,
                     transform=TRUE, save=TRUE) {

  if(smooth>10) {
    warning("You have chosen way, way too much smoothing... poorly")
  }


  if (is.null(x.vars)) { raw <- model$fit$data@x} else { raw <- model$fit$data@x[,x.vars]}

  if(equal==TRUE) {
    if(!is.null(x.vars) && length(x.vars)==1) {
      minmax <- data.frame(mins = min(raw),
                           maxs = max(raw)) } else {
                             minmax <- data.frame(mins = apply(raw, 2, min),
                                                  maxs = apply(raw, 2, max))
                           }
    lev <- lapply(c(1:nrow(minmax)), function(i) {seq(minmax$mins[i], minmax$maxs[i], (minmax$maxs[i]-minmax$mins[i])/(10*smooth))})

    for(i in 1:length(lev)){
      if(length(lev)==1) {
        if(length(unique(raw))==2) { lev[[i]] <- unique(raw) }
      } else {
        if(length(unique(raw[,i]))==2) { lev[[i]] <- unique(raw[,i])}
      }
    }

    pd <- pdbart(model, xind = x.vars, levs = lev, pl=FALSE)
  } else {
    levq = c(0.05, seq(0.1, 0.9, 0.1/smooth), 0.95)
    pd <- pdbart(model, xind = x.vars, levquants = levq, pl=FALSE)
  }


  # This is the wrapper itself

  plots <- list()

  for (i in 1:length(pd$fd)) {

    envi <- envs[[pd$xlbs[[i]]]]

    if(length(unique(pd$fit$data@x[,pd$xlbs[[i]]]))==2) {
      # THIS INTERIOR IF IS FOR BINARY VARIABLES

      print(paste("WARNING: "," is a binary variable; the plot will look bad/be uninformative", sep=pd$xlbs[[i]]))
      print()
      dfbin <- data.frame(pd$fd[[i]])
      colnames(dfbin) <- c(0,1)
      dfbin <- reshape2::melt(dfbin)

      if(transform==TRUE){
        dfbin$value <- pnorm(dfbin$value)
      }

      dfbin %>% group_by(variable) %>% summarize(value = median(value)) %>% data.frame() -> dfbin
      colnames(dfbin) <- c('is','becomes')
      dfbin$is <- as.numeric(as.character(dfbin$is))

      if(class(envs) %in% c('RasterStack','RasterBrick')) {
        lyrtmp <- envs[[pd$xlbs[[i]]]]
        lyrtr <- raster::reclassify(lyrtmp,as.matrix(dfbin))
      } else if (class(envs)=='list') {
        lyrtr <- lapply(envs, function(x) {
          lyrtmp <- x[[pd$xlbs[[i]]]]
          return(raster::reclassify(lyrtmp,as.matrix(dfbin)))
        })
      }

      if(save==TRUE) {if(exists("pdstack")) {pdstack <- c(pdstack, lyrtr)} else {pdstack <- c(lyrtr)} }

    } else {
      # FOR NON-BINARY VARIABLES

      q50 <- pnorm(apply(pd$fd[[i]],2,median))
      if(transform==TRUE) {q50 <- pnorm(q50)}
      df <- data.frame(x=pd$levs[[i]],med=q50)

      ### DEFINE RASTER RECLASS
      nmax <- length(df$x)
      xmeds <- (df$x[2:nmax] - df$x[1:(nmax-1)])/2 + df$x[1:(nmax-1)]

      if(class(envs) %in% c('RasterStack','RasterBrick')) {
        lyrtmp <- envs[[pd$xlbs[[i]]]]
        xmat <- data.frame(from=c(min(cellStats(lyrtmp, min),min(df$x)), xmeds),
                           to=c(xmeds, max(cellStats(lyrtmp, max),max(df$x))),
                           becomes=df$med)
        lyrtr <- raster::reclassify(lyrtmp,xmat,
                                    include.lowest=TRUE)

      } else if (class(envs)=='list') {
        lyrtr <- lapply(envs, function(x) {
          lyrtmp <- x[[pd$xlbs[[i]]]]
          xmat <- data.frame(from=c(min(cellStats(lyrtmp, min),min(df$x)), xmeds),
                             to=c(xmeds, max(cellStats(lyrtmp, max),max(df$x))),
                             becomes=df$med)
          return(raster::reclassify(lyrtmp,xmat,
                                    include.lowest=TRUE))
        })
      }

      if(save==TRUE) {if(exists("pdstack")) {pdstack <- c(pdstack, lyrtr)} else {pdstack <- c(lyrtr)} }
    }
  }

  if(exists("pdstack")) {return(stack(pdstack))}
}
