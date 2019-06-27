
#################################

varimp.d <- function(model, names,plots=FALSE) {

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

varimp.plot <- function(x.data, y.data, iter=50) {

  nvars <- ncol(x.data)
  varnums <- c(1:nvars)
  varlist <- colnames(x.data)

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }  # THANKS HADLEY


  for (n.trees in c(10, 20, 50, 100, 150, 200)) {
  for(index in 1:iter) {
    quiet(model.j <- bart(x.data[,varnums], y.data, ntree = n.trees, keeptrees=TRUE))

    vi.j <- varimp.d(model.j, varlist)
    if(index==1) {
      vi.j.df <- vi.j
    } else {
      vi.j.df[,index+1] <- vi.j[,2]
    }
  }
  vi.j <- data.frame(vi.j.df[,1],
                     rowMeans(vi.j.df[,-1]))

  if(n.trees==10) { vi <- vi.j } else {  vi <- cbind(vi,vi.j[,2])  }
  print(n.trees)
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
    theme(axis.text = element_text(size=10),
          axis.title = element_text(size=14,face="bold")); print(g1)


}
