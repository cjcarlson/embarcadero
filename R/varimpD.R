
varimp.d <- function(model, names,plots=FALSE) {

  varimps <- colMeans(model$varcount/rowSums(model$varcount))
  var.df <- data.frame(names, varimps)

  var.df$names <- factor(var.df$names)
  var.df <- transform(var.df, names = reorder(names,
                                              -varimps))

  if(plots==TRUE){
  g1 <- ggplot2::ggplot(var.df, aes(y=varimps, x=names)) +
    geom_bar(stat="identity", color="black") +
    ylab("Relative importance")
  print(g1)
  }

  return(var.df)

}

####### STEPWISE VS REDUCTION

varimp.loop <- function(x.data, y.data, n.trees=20, iter=20) {

  library(ggplot2); library(Metrics)
  nvars <- ncol(x.data)
  varnums <- c(1:nvars)
  varlist <- colnames(x.data)

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
  quiet(model.j <- bart(x.data[,varnums], y.data, ntree = n.trees, ndpost=2500, keeptrees=TRUE))

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

  drop.var <- vi.j[nrow(vi.j),1]
  dropped.varlist <- c(dropped.varlist,as.character(drop.var))

  print(noquote("Drop next:"))
  print(noquote(as.character(drop.var)))
  rmsej <- mean(rmse.list)

  rmses <- rbind(rmses,c(nvars-var.j,rmsej)); colnames(rmses) <- c('VarsDropped','RMSE')

  varnums <- varnums[-which(varlist==drop.var)]
  varlist <- varlist[varnums]
  for(i in 1:2) {print(noquote(""))}
  }

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

  g1 <- ggplot2::ggplot(rmses, aes(y=RMSE, x=VarsDropped)) +
    geom_line(color="black") + geom_point(size=3) + theme_bluewhite() +
    ylab("RMSE of model\n") + xlab("\nVariables dropped") +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=14,face="bold")) +
    scale_x_discrete(limits=c(0:(nrow(rmses)))); print(g1)
}
