#' @title Spatial partial dependence plots
#'
#' @description
#'
#' Why don't ecologists project partial dependence surfaces in the real, geography-having world? Now they can. That's probably good right? Am I doing good? Father?
#'
#' @param model A dbarts model object
#' @param envs The rasterStack of environmental variables 
#' @param x.vars The particular x variables you want to map
#' @param equal Equal spacing on x breaks (versus quantiles). 
#' @param smooth Smoothing factor for the x breaks (works like partials).
#' @param transform Backtransform the pnorm or not
#' @param save Turn this on if you want to save the outputs as a RasterStack, for nicer use/plotting elsewhere.
#' 
#' @return Returns a nice plot
#'
#' @examples
#' library(embarcadero)
#' library(velox)
#' set.seed(12345)
#' 
#' data(ticks)
#' data(covs)
#' mod <- SpatialPointsDataFrame(ticks[,3:4],data.frame(ticks[,1]))
#' names(mod@data) <- 'Presence'
#' tmp=rasterize(mod, covs[[1]], field="Presence", fun="min")
#' pts.sp1=rasterToPoints(tmp, fun=function(x){x>0})
#' pres.cov <- raster::extract(covs, pts.sp1[,1:2])
#' absence <- randomPoints(covs,nrow(ticks)) 
#' abs.cov <- raster::extract(covs, absence)
#' pres.cov <- data.frame(pres.cov); pres.cov$tick <- 1
#' abs.cov <- data.frame(abs.cov); abs.cov$tick <- 0
#' all.cov <- rbind(pres.cov, abs.cov)
#' all.cov <- all.cov[complete.cases(all.cov),]
#' 
#' first.model <- bart(all.cov[,1:11], all.cov[,'tick'], keeptrees=TRUE)
#' 
#' s1 <- spartial(first.model, covs, x.vars=c('bio1'),
#'                equal = TRUE, smooth=5)
#' 
#' @export
#'
#'

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
      
      lyrtmp <- envs[[pd$xlbs[[i]]]]
      lyrtr <- raster::reclassify(lyrtmp,as.matrix(dfbin))
      plot(lyrtr)
      
      if(save==TRUE) {if(exists("pdstack")) {pdstack <- c(pdstack, lyrtr)} else {pdstack <- c(lyrtr)} }
      
    } else {
      # FOR NON-BINARY VARIABLES
      
      q50 <- apply(pd$fd[[i]],2,median)
      if(transform==TRUE) {q50 <- pnorm(q50)}
      df <- data.frame(x=pd$levs[[i]],med=q50)
      
      ### DEFINE RASTER RECLASS 
      nmax <- length(df$x)
      xmeds <- (df$x[2:nmax] - df$x[1:(nmax-1)])/2 + df$x[1:(nmax-1)]
      
      lyrtmp <- envs[[pd$xlbs[[i]]]]
      xmat <- data.frame(from=c(cellStats(lyrtmp, min), xmeds),
                         to=c(xmeds, cellStats(lyrtmp, max)),
                         becomes=df$med)
      
      lyrtr <- raster::reclassify(lyrtmp,xmat,
                                   include.lowest=TRUE)
      plot(lyrtr)
      
      if(save==TRUE) {if(exists("pdstack")) {pdstack <- c(pdstack, lyrtr)} else {pdstack <- c(lyrtr)} }
    }
  }
  
  if(exists("pdstack")) {return(stack(pdstack))}
}