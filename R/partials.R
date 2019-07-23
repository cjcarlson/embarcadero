#' @title Better, nicer, friendlier partials
#'
#' @description
#'
#' I don't really like the partial visualizations in dbarts so I made some of my own, with a little more customization options.
#'
#' @param model A dbarts model object
#' @param x.vars A list of the variables for which you want to run the partials. Defaults to doing all of them.
#' @param equal Spacing x levels equally instead of using quantiles
#' @param smooth A multiplier for how much smoother you want the sampling of the levels to be.
#' @param posterior The way the posterior is visualized: defaults to nothing, but can do "CI" for the credible interval or "trace" for traceplots
#'
#'
#' @return Returns a nice plot
#'
#' @examples
#' f <- function(x) { return(0.5 * x[,1] + 2 * x[,2] * x[,3]) - 5*x[,4] }
#' sigma <- 0.2
#' n <- 100
#' x <- matrix(2 * runif(n * 3) -1, ncol = 3)
#' x <- data.frame(x)
#' x[,4] <- rbinom(100, 1, 0.3)
#' colnames(x) <- c('rob', 'hugh', 'ed', 'phil')
#' Ey <- f(x)
#' y  <- rnorm(n, Ey, sigma)
#' df <- data.frame(y, x)
#' set.seed(99)
#' 
#' bartFit <- bart(y ~ rob + hugh + ed + phil, df,
#'                keepevery = 10, ntree = 100, keeptrees = TRUE)
#'
#' partial(bartFit, x.vars='hugh', trace=TRUE, ci=TRUE)
#' partial(bartFit, x.vars='hugh', equal=TRUE, trace=TRUE, ci=TRUE)
#' partial(bartFit, x.vars='hugh', equal=TRUE, smooth=10, trace=TRUE, ci=TRUE)
#' 
#' partial(bartFit, x.vars='rob', equal=TRUE, smooth=10, trace=FALSE, ci=TRUE)
#' partial(bartFit, x.vars='ed', equal=TRUE, smooth=10, trace=TRUE, ci=FALSE)
#' partial(bartFit, equal=TRUE, smooth=10, trace=FALSE, ci=TRUE, panel=TRUE)
#'
#' @export
#'
#'

partial <- function(model, x.vars=NULL, equal=TRUE, smooth=1,
                    ci=TRUE, trace=TRUE,
                    transform=TRUE, panels=FALSE) {
  
  # A couple errors in case I'm Idiot
  
  if(smooth>10) {
    warning("You have chosen way, way too much smoothing... poorly")
  }
  
  if(!is.null(x.vars) && length(x.vars)==1 && panels==TRUE) {
    stop("Hey bud, you can't do several panels on only one variable!")
  }
  
# This is for something else ultimately: attr(bartFit$fit$data@x, "term.labels")
# This is where equal happens
  
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
  
  if(length(unique(pd$fit$data@x[,pd$xlbs[[i]]]))==2) {
    
    dfbin <- data.frame(pd$fd[[i]])
    colnames(dfbin) <- c(0,1)
    dfbin <- reshape2::melt(dfbin)
    
    if(transform==TRUE){
      dfbin$value <- pnorm(dfbin$value)
    }
  
    if(ci==FALSE) {
    g <- ggplot(dfbin,aes(x=variable, y=value)) + geom_boxplot() + 
      labs(title=pd$xlbs[[i]], y='Response',x='') + theme_light(base_size = 20) + 
      theme(plot.title = element_text(hjust = 0.5),
            axis.title.y = element_text(vjust=1.7))
    } else {
      g <- ggplot(dfbin,aes(x=variable, y=value)) + geom_boxplot( fill='deepskyblue1') + 
        labs(title=pd$xlbs[[i]], y='Response',x='') + theme_light(base_size = 20) + 
        theme(plot.title = element_text(hjust = 0.5),
              axis.title.y = element_text(vjust=1.7)) 
    }
    
    
    plots[[i]] <- g
    if(panels==FALSE) {print(g)}
    
  } else {
    
  q50 <- apply(pd$fd[[i]],2,median)
  if(transform==TRUE) {q50 <- pnorm(q50)}
  
  df <- data.frame(x=pd$levs[[i]],med=q50)

  if(ci==TRUE) {
    q05 <- apply(pd$fd[[i]],2,quantile,probs=0.05)
    if(transform==TRUE) {q05 <- pnorm(q05)}
    q95 <- apply(pd$fd[[i]],2,quantile,probs=0.95)
    if(transform==TRUE) {q95 <- pnorm(q95)}
    df$q05 <- q05
    df$q95 <- q95
  }
  
  if(trace==TRUE) {
    f <- data.frame(t(pd$fd[[i]]))
    df <- cbind(df, f)
  }
      
  g <- ggplot(df,aes(x=x, y=med)) + 
    labs(title=pd$xlbs[[i]], y='Response',x='') + theme_light(base_size = 20) + 
    theme(plot.title = element_text(hjust = 0.5),
          axis.title.y = element_text(vjust=1.7))
  
  if(ci==TRUE) {alpha2 <- 0.05; k <- 4} else {alpha2 <- 0.05*(model$fit$control@n.trees/200); k <- 2}
  if(trace==TRUE) {
    if(transform==TRUE) {
      for(j in 1:nrow(pd$fd[[i]])) {
        g <- g + geom_line(aes_string(y=pnorm(df[,j+k])), alpha=alpha2)
      }
    } else {
      for(j in 1:nrow(pd$fd[[i]])) {
        g <- g + geom_line(aes_string(y=df[,j+k]), alpha=alpha2)
      }
    }
  }
  
  if(ci==TRUE) {
    g <- g + geom_ribbon(aes(ymin=q05, ymax=q95), fill='deepskyblue1', alpha=0.3)
  }
  
  g <- g + geom_line(size=1.25)
  plots[[i]] <- g
  if(panels==FALSE) {print(g)}

  }
}
  
if(panels==TRUE) {print(cowplot::plot_grid(plotlist=plots))}
  
}
