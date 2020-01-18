#' @title Wrapper for flexible riBART calls
#'
#' @description
#'
#' A little internal piece of code that allows you to easily drop random intercepts in. Not really for anyone but me :)
#' 
#' @param x.data A data frame of covariates
#' @param y.data A vector of outcomes (1/0)
#' @param ri.data Random intercept data
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
                       n.chains=1L,
                       n.threads=1L,
                       n.trees = n.trees,
                       keepTrees = TRUE) 
  }
  return(model)
}