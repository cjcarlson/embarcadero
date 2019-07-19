#' @title Full-service variable selection
#'
#' @description
#'
#' A wrapper for a few core functions, including two diagnostic plots of variable importance and the automated stepwise variable set reduction algorithm. Returns an object with the model and the list of the best variables
#'
#' @param xdata A data frame of covariates
#' @param ydata A vector of outcomes (1/0)
#' @param iter.step How many BART models to run for each iteration of the stepwise reduction
#' @param tree.step How many trees to use in the variable set reduction
#' @param iter.plot How many iterations to use in the first diagnostic plot 
#' 
#' @return Returns a list of (1) Variables (the optimal variable set) and (2) Model.object, the best model using those
#' 
#' @export
#'
#'
#'

bart.var <- function(xdata, ydata,
                                iter.step=100, tree.step=10,
                                iter.plot=100) {
  varimp.plot(xdata, ydata, iter=iter.plot)
  vs <- variable.step(xdata, ydata, n.trees=tree.step, iter=iter.step)
  invisible(best.model <- bart(xdata[,vs], ydata, keeptrees=TRUE))
  varimp(best.model, plots=TRUE)
  
  
  pred.p <- colMeans(pnorm(best.model$yhat.train))[ydata==1]
  pred.a <- colMeans(pnorm(best.model$yhat.train))[ydata==0]
  e <- evaluate(p=pred.p,
                a=pred.a)
  plot(e, 'ROC')
  
  invisible(best.model)
}
