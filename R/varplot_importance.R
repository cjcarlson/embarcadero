#' @title Variable importance plot
#'
#' @description
#'
#' Variable importance, as measured in the proportion of total branches used for a given variable.
#'
#' @param model The dbarts model object
#' @param names The variable names (not conserved in dbarts necessarily)
#' @param plots Turn this on for a nice variable contribution plot
#'
#'
#' @export
#'
#'

varimp <- function(model, names,plots=FALSE) {

  varimps <- colMeans(model$varcount/rowSums(model$varcount))
  var.df <- data.frame(names, varimps)

  var.df$names <- factor(var.df$names)
  var.df <- transform(var.df, names = reorder(names,
                                              -varimps))

  if(plots==TRUE){
  g1 <- ggplot2::ggplot(var.df, aes(y=varimps, x=names)) +
    geom_bar(stat="identity", color="black") +
    ylab("Relative importance") + theme_bluewhite()
  print(g1)
  }

  return(var.df)

}

