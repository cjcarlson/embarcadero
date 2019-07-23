
#' @title AUC for BART object
#'
#' @description
#' Just for object from BART package
#'
#' @param model Dude that's a model. It better be a freakin lBART model dude
#'
#' @export
#'

bart.auc <- function(model) {
 
  true.vector <- model$fit$data@y
  
  pred <- prediction(colMeans(pnorm(model$yhat.train)), true.vector)
  
  print('AUC = ')
  print(performance(pred,"auc")@y.values[[1]])
  plot(performance(pred, "tpr", "fpr"),main='Receiver operator curve')
  abline(0,1,col='red')

  readline(prompt="Press [enter] to continue")

  perf.tss <- performance(pred,"sens","spec")
  tss.list <- (perf.tss@x.values[[1]] + perf.tss@y.values[[1]] - 1)
  tss.df <- data.frame(alpha=perf.tss@alpha.values[[1]],tss=tss.list)
  plot(tss.df,type='l')

  thresh <- tss.df$alpha[which(tss.df$tss==max(tss.df$tss))]
  print('TSS threshold')
  print(thresh)

  print('Type I error rate')
  print(1-perf.tss@y.values[[1]][which(perf.tss@alpha.values[[1]]==thresh)]) # Type I error rate
  print('Type II error rate')
  print(1-perf.tss@x.values[[1]][which(perf.tss@alpha.values[[1]]==thresh)]) # Type II error rate

}
