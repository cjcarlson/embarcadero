#' @title Automatic variable selection
#'
#' @description
#'
#' The automated stepwise variable set reduction algorithm. It starts with the full variable set, runs a given number of models (iter) with a given number of trees (n.trees), and eliminates the variable with the worst contribution. It does this until there are only three left, and charts the RMSE of each model. Then it finally recommends the model with the lowest RMSE.
#' 
#' This is probably mostly useful as an internal part of bart.var, but if you *just* wanted to pull out which variables mattered and not the actual models, you could use this function to do so.
#'
#' @param xdata A data frame of covariates
#' @param ydata A vector of outcomes (1/0)
#' @param n.trees How many trees to use in the variable set reduction. Should be a SMALL number for optimal performance (10 or 20 trees)
#' @param iter How many BART models to run for each iteration of the stepwise reduction
#' 
#' @return Returns a list of the best variable set, and does some diagnostic plots along the way
#' 
#' @export
#'
#'

variable.step <- function(x.data, y.data, n.trees=10, iter=50) {
  
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
      
      
      vi.j <- varimp(model.j)
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