#' @title Diagnostic variable selection plot
#'
#' @description
#'
#' When BART is run with a small number of trees it tends to up-select variables that contribute the most meaningfully, and under-selects variables that have no relevance or contribute only marginally. As the number of trees, this pattern becomes less visible, as it overfits to less useful variables. Plotting this is a useful way of identifying which variables should be dropped - those that have the most visible increase relative to number of trees are performing the poorest.
#'
#' @param xdata A data frame of covariates
#' @param ydata A vector of outcomes (1/0)
#' @param iter How many BART models to run for each of (10, 20, 50, 100, 150, 200) tree models
#'
#' @export 
#' @aliases varimp.plot 
#'

varimp.diag <- function(x.data, y.data, iter=50, shhh=FALSE) {

  nvars <- ncol(x.data)
  varnums <- c(1:nvars)
  varlist <- colnames(x.data)

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }  # THANKS HADLEY 4 THIS CODE :) 


  for (n.trees in c(10, 20, 50, 100, 150, 200)) {
    
    cat(paste('\n', n.trees, 'tree models:', iter, 'iterations\n'))
    if(!shhh){pb <- txtProgressBar(min = 0, max = iter, style = 3)}
    
    for(index in 1:iter) {
      quiet(model.j <- bart(x.data[,varnums], y.data, ntree = n.trees, keeptrees=TRUE))

      vi.j <- varimp(model.j)
      if(index==1) {
        vi.j.df <- vi.j
      } else {
        vi.j.df[,index+1] <- vi.j[,2]
      }
      if(!shhh){setTxtProgressBar(pb, index)}
    }
    vi.j <- data.frame(vi.j.df[,1],
                       rowMeans(vi.j.df[,-1]))

    if(n.trees==10) { vi <- vi.j } else {  vi <- cbind(vi,vi.j[,2])  }
  }

  colnames(vi) <- c('variable','10','20','50','100','150','200')
  vi <- reshape::melt(vi, "variable")
  colnames(vi) <- c('variable','trees','imp')

  vi %>% group_by(variable) %>% summarise(max = max(imp)) -> vi.fac
  vi.fac <- vi.fac[order(-vi.fac$max),]

  vi$names <- factor(vi$variable, levels=vi.fac$variable)

  g1 <- ggplot2::ggplot(vi, aes(y=imp, x=names, group=trees)) +
    geom_line(aes(color=trees)) + geom_point(size=3) + theme_classic() +
    ylab("Relative contribution\n") + xlab("\nVariables dropped") +
    ggpubr::rotate_x_text(angle = 35) + 
    theme(axis.text = element_text(size=10),
          axis.title = element_text(size=14,face="bold")); print(g1)

  }
