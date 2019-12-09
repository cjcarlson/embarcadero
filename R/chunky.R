
library(data.table)
library(matrixStats)

model <- sdm
inputstack <- bigstack
splitby=5
quantiles=c(0.05,0.95)
inputstack[1] <- NA

dfextract <- function(df, quant) {
  if(length(quant)==0) {return(colMeans(df))} else 
  return(cbind(data.frame(colMeans(df)),
               colQuantiles(df, probs=quant)))
}

############################

predict2 <- function(model, 
                     inputstack,
                     splitby=1,
                     quantiles=c()) {
  
  xnames <- attr(model$fit$data@x, "term.labels")
  if(all(xnames %in% names(inputstack))) {
    inputstack <- inputstack[[xnames]]
  } else {
    stop("Variable names of RasterStack don't match the requested names")
  }
  input.matrix <- as.matrix(getValues(inputstack))
  
  blankout <- data.frame(matrix(ncol=(1+length(quantiles)), nrow=ncell(inputstack[[1]])))
  whichvals <- which(complete.cases(input.matrix))
  input.matrix <- input.matrix[complete.cases(input.matrix),]
  
  if(splitby==1) {
    pred <- predict(model, input.matrix)
    pred.summary <- dfextract(pred, quant=quantiles)
  } else {
    split <- floor(nrow(input.matrix)/splitby)
    input.df <- data.frame(input.matrix)
    input.str <- split(input.df, (as.numeric(1:nrow(input.df))-1) %/% split)
    pb <- txtProgressBar(min = 0, max = length(input.str), style = 3)
    for(i in 1:length(input.str)){
        if(i==1) {start_time <- Sys.time()}
        pred <- predict(model, input.str[[i]])
        pred.summary <- dfextract(pred, quant=quantiles)
        input.str[[i]] <- pred.summary
        if(i==1) {end_time <- Sys.time()
                  print('Estimated time to total prediction (mins):') 
                  print(length(input.str)*as.numeric(end_time - start_time)/60)}
        setTxtProgressBar(pb, i)
    }
    if(length(quantiles)==0) {
        pred.summary <- data.frame(means=unlist(input.str)) } else {
        pred.summary <- rbindlist(input.str)
    }
  }
  
  pred.summary <- as.matrix(pred.summary)
  output = pnorm(pred.summary)
  
  blankout[whichvals,] <- output
  output <- blankout
  
  outlist <- lapply(1:ncol(output), function(x) {
      output.m <- t(matrix(output[,x],
                       nrow = ncol(inputstack),
                       ncol = nrow(inputstack)))
      return(raster(output.m,
                     xmn=xmin(inputstack[[1]]), xmx=xmax(inputstack[[1]]),
                     ymn=ymin(inputstack[[1]]), ymx=ymax(inputstack[[1]]),
                     crs=inputstack[[1]]@crs))
  })
  
  outlist <- stack(outlist)
  return(outlist)
}
