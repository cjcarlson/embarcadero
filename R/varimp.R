#' @title Variable importance plot
#'
#' @description
#'
#' Variable importance, as measured in the proportion of total branches used for a given variable. Error bars show standard deviation across iterations.
#' 
#' Variables that are explicitly dropped in the model are included as "0" in the variable importance table, but not plotted; variables that are included but used zero times are shown in both.
#' 
#' @param model The dbarts model object
#' @param plots Turn this on for a nice variable contribution plot
#'
#'
#' @export
#'
#'

varimp <- function(model, plots=FALSE) {

  if(!("fit" %in% names(model))) {
    stop("Please add \", keeptrees=TRUE\" to your dbarts model call")
  }
  if(class(model)=='rbart') {
    basenames <- unlist(attr(model$fit[[1]]$data@x,"drop"))
    names <- names(which(basenames==FALSE))
    varimps <- rowMeans(model$varcount/colSums(model$varcount))
    fitobj <- model$fit[[1]]
  }
  if(class(model)=='bart') {
    basenames <- unlist(attr(model$fit$data@x,"drop"))
    names <- names(which(basenames==FALSE))
    varimps <- colMeans(model$varcount/rowSums(model$varcount))
    fitobj <- model$fit
  }
  
  var.df <- data.frame(names, varimps)
  
  missing <- attr(fitobj$data@x,"term.labels")[!(attr(fitobj$data@x,"term.labels") %in% 
                                                    names(unlist(attr(fitobj$data@x,"drop"))))]
  
  if(length(missing)>0) {
  message("dbarts auto-dropped this variable(s). You will probably want to remove it")
    message(paste(missing,collapse = ' '), ' \n')
  }
  
  if(length(missing)>0) {
    missing.df <- data.frame(names = missing, varimps = 0)
    var.df <- rbind(var.df, missing.df)
  }

  var.df$names <- factor(var.df$names)
  var.df <- transform(var.df, names = reorder(names,
                                              -varimps))

  if(plots==TRUE){
  #g1 <- ggplot2::ggplot(var.df, aes(y=varimps, x=names)) +
  #  geom_bar(stat="identity", color="black") +
  #  theme(axis.text.x = element_text(angle = 45)) + 
  #  ylab("Relative importance") + theme_bluewhite()
  #print(g1)
  
  if(class(model)=='rbart') {
    rel <- t(model$varcount/colSums(model$varcount))
  } else {
    rel <- model$varcount/rowSums(model$varcount)
  }
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

