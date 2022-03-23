#' @title Automatic variable selection
#'
#' @description
#'
#' The automated stepwise variable set reduction algorithm. It starts with the full variable set, runs a given number of models (iter) with a given number of trees (n.trees), and eliminates the variable with the lowest importance. It does this until there are only three left, and charts the RMSE of each model. Then it finally recommends the model with the lowest RMSE.
#' 
#' This is probably mostly useful as an internal part of bart.var, but if you *just* wanted to pull out which variables mattered and not the actual models, you could use this function to do so.
#'
#' @param x.data A data frame of covariates
#' @param y.data A vector of outcomes (1/0)
#' @param n.trees How many trees to use in the variable set reduction. Should be a SMALL number (10 or 20 trees) in order to create the maximum disparity in variable importance between informative and uninformative predictors (recommendations taken from Chipman et al. 2010).
#' @param iter How many BART models to run for each iteration of the stepwise reduction
#' 
#' @return Returns a list of the best variable set, and does a diagnostic plot showing the RMSE for each model with a given number of variable drops.
#' 
#' @export
#'
#'

variable.step <- function(x.data, y.data, ri.data=NULL, n.trees=10, iter=50, quiet=FALSE) {
  
  quietly <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }  # THANKS HADLEY
  
  comp <- complete.cases(x.data)
  
  if(length(comp) < (nrow(x.data))) {
    message("Some rows with NA's have been automatically dropped. \n")
  }
  x.data <- x.data[comp,]
  y.data <- y.data[comp,]
  
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
  
  dropnames <- colnames(x.data)[!(colnames(x.data) %in% names(which(unlist(attr(fitobj$data@x,"drop"))==FALSE)))]
  
  if(length(dropnames) > 0) {
    message("Some of your variables have been automatically dropped by dbarts.")
    message("(This could be because they're characters, homogenous, etc.)")
    message("It is strongly recommended that you remove these from the raw data:")
    message(paste(dropnames,collapse = ' '), ' \n')
  }
  
  x.data %>% dplyr::select(-any_of(dropnames)) -> x.data  
  
  ###############
  
  nvars <- ncol(x.data)
  varnums <- c(1:nvars)
  varlist.orig <- varlist <- colnames(x.data)
  
  rmses <- data.frame(Variable.number=c(),RMSE=c())
  dropped.varlist <- c()
  
  for(var.j in c(nvars:3)) {
     
    print(noquote(paste("Number of variables included:",var.j)))
    print(noquote("Dropped:"))
    print(if(length(dropped.varlist)==0) {noquote("")} else {noquote(dropped.varlist)})
    
    rmse.list <- c()
    
    if(!quiet){pb <- txtProgressBar(min = 0, max = iter, style = 3)}
    for(index in 1:iter) {
      quietly(model.j <- bart.flex(x.data = x.data[,varnums], y.data = y.data, 
                                   ri.data = ri.data,
                                   n.trees = n.trees))
      
      quietly(vi.j <- varimp(model.j))
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
      if(!quiet){setTxtProgressBar(pb, index)}
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
    geom_line(color="black") + geom_point(size=3) + theme_bw() +
    ylab("RMSE of model\n") + xlab("\nVariables dropped") +
    theme(axis.text = element_text(size=12),
          axis.title = element_text(size=14,face="bold")) +
    scale_x_discrete(limits=c(0:(nrow(rmses)))); print(g1)
  
  print(noquote("---------------------------------------"))
  print(noquote("Final recommended variable list"))
  varlist.final <- varlist.orig[!(varlist.orig %in% dropped.varlist[0:(which(rmses$RMSE==min(rmses$RMSE))-1)])]
  print(noquote(varlist.final))
  invisible(varlist.final)
}
