#' @title Wrapper for flexible riBART calls
#'
#' @description
#'
#' A little internal piece of code that allows you to easily drop random intercepts in. It's a convenience tool I don't suggest using - it's a shortcut for if you feel very comfortable with the pipeline.
#' 
#' @param x.data A data frame of covariates
#' @param y.data A vector of outcomes (1/0)
#' @param ri.data Random intercept data (a column in a data frame)
#' @param ri.name Identify the name of the random intercept column 
#' @param y.name Rename outcome variable from training data if you need to
#' 
#' @export 
#' 
#' 

bart.flex <- function(x.data, y.data, ri.data = NULL,
                      y.name = NULL, ri.name = NULL,
                      n.trees = 200) {
  
  if(is.null(ri.data)) {
    train <- cbind(y.data, x.data) 
    if(!is.null(y.name)) {colnames(train)[1] <- y.name}
    train <- na.omit(train)
    model <- bart(y.train = train[,1], 
                  x.train = train[,2:ncol(train)], 
                  ntree = n.trees, keeptrees=TRUE)
  } else { 
    train <- cbind(y.data, x.data, ri.data) 
    if(!is.null(y.name)) {colnames(train)[1] <- y.name}
    if(!is.null(ri.name)) {colnames(train)[ncol(train)] <- ri.name}
    f <- as.formula(paste(paste(colnames(train)[1],paste(colnames(train)[2:(ncol(train)-1)], 
                                   collapse=' + '), sep = ' ~ '), 
                    colnames(train)[ncol(train)], sep=' - '))
    
    train <- na.omit(train) 
    model <- rbart_vi(f, 
                       group.by=train[,ncol(train)],
                       data=train,
                       n.samples=1000,
                       n.burn=100,
                       n.chains=1,
                       n.threads=1,
                       n.trees = n.trees,
                       keepTrees = TRUE) 
  }
  return(model)
}

