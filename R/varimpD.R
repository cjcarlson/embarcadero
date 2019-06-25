
theme_bluewhite <- function (base_size = 11, base_family = "") {
  theme_classic() %+replace%
    theme(
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "lightblue"),
      panel.border = element_rect(color = "black", fill = NA),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black")
    )
}

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

  library(ggplot2); library(Metrics); library(tidyverse)

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

####### STEPWISE VS REDUCTION



















variable.step <- function(x.data, y.data, n.trees=10, iter=50) {

  library(ggplot2); library(Metrics)
  nvars <- ncol(x.data)
  varnums <- c(1:nvars)
  varlist.orig <- varlist <- colnames(x.data)

  comp <- complete.cases(x.data)
  x.data <- x.data[comp,]
  y.data <- y.data[comp]

  rmses <- data.frame(Variable.number=c(),RMSE=c())
  dropped.varlist <- c()

  for(var.j in c(nvars:3)) {

  print(noquote(paste("Number of variables included:",var.j)))
  print(noquote("Dropped:"))
  print(if(length(dropped.varlist)==0) {noquote("")} else {noquote(dropped.varlist)})

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }  # THANKS HADLEY

  rmse.list <- c()


  for(index in 1:iter) {
  quiet(model.j <- bart(x.data[,varnums], y.data, ntree = n.trees, keeptrees=TRUE))


    vi.j <- varimp.d(model.j, varlist)
  if(index==1) {
    vi.j.df <- vi.j
  } else {
    vi.j.df[,index+1] <- vi.j[,2]
  }

  pred.p <- colMeans(pnorm(model.j$yhat.train))[y.data==1]
  pred.a <- colMeans(pnorm(model.j$yhat.train))[y.data==0]
  #e <- evaluate(p=pred.p,
  #              a=pred.a)
  #aucs <- rbind(aucs,c(var.j,e@auc)); colnames(aucs) <- c('Vars','AUC')

  pred.c <- c(pred.p, pred.a)
  true.c <- c(rep(1,length(pred.p)), rep(0,length(pred.a)))
  rmsej.i <- Metrics::rmse(true.c,pred.c)
  rmse.list <- c(rmse.list,rmsej.i)
  }

  vi.j <- data.frame(vi.j.df[,1],
                     rowMeans(vi.j.df[,-1]))
  vi.j <- vi.j[order(vi.j[,2]),]

  drop.var <- vi.j[1,1]
  dropped.varlist <- c(dropped.varlist,as.character(drop.var))

  rmsej <- mean(rmse.list)

  rmses <- rbind(rmses,c(nvars-var.j,rmsej)); colnames(rmses) <- c('VarsDropped','RMSE')

  varnums <- varnums[!(varnums==which(varlist.orig==drop.var))]
  varlist <- varlist.orig[varnums]
  print(noquote("---------------------------------------"))
  }

  g1 <- ggplot2::ggplot(rmses, aes(y=RMSE, x=VarsDropped)) +
    geom_line(color="black") + geom_point(size=3) + theme_bluewhite() +
    ylab("RMSE of model\n") + xlab("\nVariables dropped") +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=14,face="bold")) +
    scale_x_discrete(limits=c(0:(nrow(rmses)))); print(g1)

  print(noquote("---------------------------------------"))
  print(noquote("Final recommended variable list"))
  varlist.final <- varlist.orig[!(varlist.orig %in% dropped.varlist[1:(which(rmses$RMSE==min(rmses$RMSE))-1)])]
  print(noquote(varlist.final))
  invisible(varlist.final)
}

########################













best.variable.model <- function(xdata, ydata,
                                iter.step=100, tree.step=10,
                                iter.plot=100) {
  varimp.plot(xdata, ydata, iter=iter.plot)
  vs <- variable.step(xdata, ydata, n.trees=tree.step, iter=iter.step)
  invisible(best.model <- bart(xdata[,vs], ydata, keeptrees=TRUE))
  varimp.d(best.model, names=vs, plots=TRUE)


  pred.p <- colMeans(pnorm(best.model$yhat.train))[ydata==1]
  pred.a <- colMeans(pnorm(best.model$yhat.train))[ydata==0]
  e <- evaluate(p=pred.p,
                a=pred.a)
  plot(e, 'ROC')

  invisible(list(Variables=vs,
              Model.object=best.model))
}
