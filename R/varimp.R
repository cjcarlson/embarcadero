#' @title Variable importance plot
#'
#' @description
#'
#' Variable importance, as measured in the proportion of total branches used for a given variable. Error bars show standard deviation across iterations.
#'
#' @param model The dbarts model object
#' @param plots Turn this on for a nice variable contribution plot
#'
#'
#' @export
#'
#'

varimp <- function(model, plots=FALSE) {

  if(class(model)=='rbart') {
    names <- attr(model$fit[[1]]$data@x, "term.labels")
    varimps <- rowMeans(model$varcount/colSums(model$varcount))
  }
  if(class(model)=='bart') {
    names <- attr(model$fit$data@x, "term.labels")
    varimps <- colMeans(model$varcount/rowSums(model$varcount))
  }
  
  var.df <- data.frame(names, varimps)

  var.df$names <- factor(var.df$names)
  var.df <- transform(var.df, names = reorder(names,
                                              -varimps))

  if(plots==TRUE){
  #g1 <- ggplot2::ggplot(var.df, aes(y=varimps, x=names)) +
  #  geom_bar(stat="identity", color="black") +
  #  theme(axis.text.x = element_text(angle = 45)) + 
  #  ylab("Relative importance") + theme_bluewhite()
  #print(g1)
  
  rel <- model$varcount/rowSums(model$varcount)
  colnames(rel) <- names
  
  rel %>% data.frame() %>% gather() %>%
    group_by(key) %>%
    summarise(mean = mean(value),
              sd = sd(value, na.rm = TRUE)) %>% 
    transform(Var = reorder(key, mean)) %>%
        ggplot(aes(x = Var, y = mean)) +
              geom_pointrange(aes(y = mean, x = Var, ymin = mean-sd, ymax = mean+sd),
                              color="#00AFDD") + 
              xlab(NULL) + ylab("Variable importance") + coord_flip() + 
              theme_bw() + theme(legend.position = "none",
                                 axis.title.x = element_text(size=rel(1.3), vjust = -0.8),
                                 axis.text.y = element_text(size=rel(1.4)),
                                 plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                                 panel.grid.minor = element_blank(),
                                 panel.grid.major.x = element_line(color='grey',
                                                                   linetype='dashed')) -> p
  
  print(p)
  
  }

  return(var.df)

}

