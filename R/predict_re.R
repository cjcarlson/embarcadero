
predict.re <- function (object, inputstack, splitby = 1, quantiles = c(), 
                         quiet = FALSE, time, timevar, ranef=FALSE) {
  
  xnames <- attr(object$fit[[1]]$data@x, "term.labels")
  if (all(xnames %in% c(names(inputstack),timevar))) {
    inputstack <- inputstack[[xnames[!(xnames==timevar)]]]
  } else {
    stop("Variable names of RasterStack don't match the requested names")
  }
  input.matrix <- as.matrix(getValues(inputstack))
  blankout <- data.frame(matrix(ncol = (1 + length(quantiles)), 
                                nrow = ncell(inputstack[[1]])))
  whichvals <- which(complete.cases(input.matrix))
  input.matrix <- input.matrix[complete.cases(input.matrix), 
                               ]
  
  input.matrix <- cbind(input.matrix,rep(time, nrow(input.matrix)))
  colnames(input.matrix)[ncol(input.matrix)] <- timevar
  
  if (splitby == 1) {
    
    if(ranef==FALSE) {
      pred <- dbarts:::predict.rbart(object, 
                                     input.matrix[,!(colnames(input.matrix)==timevar)], 
                                     group.by=input.matrix[,timevar],
                                     value='bart')
    } else {
      pred <- dbarts:::predict.rbart(object, 
                                     input.matrix[,!(colnames(input.matrix)==timevar)], 
                                     group.by=input.matrix[,timevar],
                                     value='ppd')
    }
    
    pred.summary <- dfextract(pred, quant = quantiles)
  } else {
    split <- floor(nrow(input.matrix)/splitby)
    input.df <- data.frame(input.matrix)
    input.str <- split(input.df, (as.numeric(1:nrow(input.df)) - 
                                    1)%/%split)
    for (i in 1:length(input.str)) {
      if (i == 1) {
        start_time <- Sys.time()
      }
      
      if(ranef==FALSE) {
        pred <- dbarts:::predict.rbart(object, 
                                       input.str[[i]][,!(colnames(input.str[[i]])==timevar)], 
                                       group.by=input.str[[i]][,timevar],
                                       value='bart')
      } else {
        pred <- dbarts:::predict.rbart(object, 
                                       input.str[[i]][,!(colnames(input.str[[i]])==timevar)], 
                                       group.by=input.str[[i]][,timevar],
                                       value='ppd')
      }
      
      pred.summary <- dfextract(pred, quant = quantiles)
      input.str[[i]] <- pred.summary
      if (i == 1) {
        end_time <- Sys.time()
        cat("Estimated time to total prediction (mins):\n")
        cat(length(input.str) * as.numeric(end_time - 
                                             start_time)/60)
        cat("\n")
        if (!quiet) {
          pb <- txtProgressBar(min = 0, max = length(input.str), 
                               style = 3)
        }
      }
      if (!quiet) {
        setTxtProgressBar(pb, i)
      }
    }
    if (length(quantiles) == 0) {
      pred.summary <- data.frame(means = unlist(input.str))
    } else {
      pred.summary <- rbindlist(input.str)
    }
  }
  pred.summary <- as.matrix(pred.summary)
  output = pnorm(pred.summary)
  blankout[whichvals, ] <- output
  output <- blankout
  outlist <- lapply(1:ncol(output), function(x) {
    output.m <- t(matrix(output[, x], nrow = ncol(inputstack), 
                         ncol = nrow(inputstack)))
    return(raster(output.m, xmn = xmin(inputstack[[1]]), 
                  xmx = xmax(inputstack[[1]]), ymn = ymin(inputstack[[1]]), 
                  ymx = ymax(inputstack[[1]]), crs = inputstack[[1]]@crs))
  })
  outlist <- stack(outlist)
  return(outlist)
}
