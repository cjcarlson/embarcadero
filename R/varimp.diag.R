#' @title Diagnostic variable selection plot
#'
#' @description
#'
#' When BART is run with a small number of trees it tends to up-select variables that contribute the most meaningfully, and under-selects variables that have no relevance or contribute only marginally. As the number of trees, this pattern becomes less visible, as it overfits to less useful variables. Plotting this is a useful way of identifying which variables should be dropped - those that have the most visible increase relative to number of trees are performing the poorest.
#'
#' @param x.data A data frame of covariates
#' @param y.data A vector of outcomes (1/0)
#' @param iter How many BART models to run for each of (10, 20, 50, 100, 150, 200) tree models
#'
#' @export
#' @aliases varimp.plot
#'

varimp.diag <- function(x.data, y.data, ri.data=NULL, iter=50, quiet=FALSE) {

  # nvars <- ncol(x.data)
  # varnums <- c(1:nvars)
  # varlist <- colnames(x.data)
  # moved the above to after removal of dropped variables

  quietly <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }  # THANKS HADLEY 4 THIS CODE :)

  ###############

  # auto-drops

  quietly(model.0 <- bart.flex(x.data = x.data, y.data = y.data,
                               ri.data = ri.data,
                               n.trees = 200))

  if(class(model.0)=='rbart') {
    fitobj <- model.0$fit[[1]]
  }
  if(class(model.0)=='bart') {
    fitobj <- model.0$fit
  }
  # dropnames <- colnames(x.data)[!(colnames(x.data) %in% names(which(unlist(attr(fitobj$data@x,"drop"))==FALSE)))]
  # the above would also drop categorical variables; replaced with:
  dropnames <- colnames(x.data)[colnames(x.data) %in% names(which(unlist(attr(fitobj$data@x,"drop")) != 0))]

  if(length(dropnames)==0) {} else{
    message("Some of your variables have been automatically dropped by dbarts.")
    message("(This could be because they're homogenous, etc.)")
    message("It is strongly recommended that you remove these from the raw data:")
    message(paste(dropnames,collapse = ' '), ' \n')
  }

  x.data %>% dplyr::select(-any_of(dropnames)) -> x.data

  nvars <- ncol(x.data)
  varnums <- c(1:nvars)
  varlist <- colnames(x.data)

  ###############

  for (n.trees in c(10, 20, 50, 100, 150, 200)) {

    cat(paste('\n', n.trees, 'tree models:', iter, 'iterations\n'))
    if(!quiet){pb <- txtProgressBar(min = 0, max = iter, style = 3)}

    for(index in 1:iter) {
      quietly(model.j <- bart.flex(x.data = x.data[,varnums], y.data = y.data,
                                   ri.data = ri.data,
                                   n.trees = n.trees))

      vi.j <- suppressMessages(varimp(model.j))
      if(index==1) {
        vi.j.df <- vi.j
      } else {
        vi.j.df[,index+1] <- vi.j[,2]
      }
      if(!quiet){setTxtProgressBar(pb, index)}
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
