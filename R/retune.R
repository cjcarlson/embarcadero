#' @title Wrapper for xbart
#'
#' @description
#'
#' STILL IN DEVELOPMENT. New function to automate a little tuning of priors. Currently doesn't let you set the levels. Model goes in, model comes back out with slightly different priors.
#' 
#' @param object A classic BART model (not rbart) from the dbarts package
#' @param reps How many tries with each parameter combination
#' 
#' @export 
#' 
#' 

retune <- function(object, reps = 10) {
  
  # auto-drops 
  
  #quietly(model.0 <- bart.flex(x.data = x.data, y.data = y.data, 
  #                             ri.data = ri.data,
  #                             n.trees = 200))
  
  #dropnames <- colnames(x.data)[!(colnames(x.data) %in% names(which(unlist(attr(model.0$fit$data@x,"drop"))==FALSE)))]
  
  #  if(length(dropnames)==0) {} else{
  #    message("Some of your variables have been automatically dropped by dbarts.")
  #    message("(This could be because they're characters, homogenous, etc.)")
  #    message("It is strongly recommended that you remove these from the raw data:")
  #    message(paste(dropnames,collapse = ' '), ' \n')
  #   }
  
  # x.data %>% select(-dropnames) -> x.data  
  
  ####
  
  x <- xbart(object$fit$data@x,
             object$fit$data@y,
             n.samples = 100, n.reps = reps,
             k = c(1,2,3),
             power = c(1.5, 1.6, 1.7, 1.8, 1.9, 2),
             base = c(0.75, 0.8, 0.85, 0.9, 0.95))
  x2 <- apply(x, c(2,3,4), mean)
  
  priors <- which(x2==min(x2), arr.ind = TRUE)
  
  model <- bart(object$fit$data@x,
                object$fit$data@y,
                k = c(1,2,3)[priors[1]],
                power = c(1.5, 1.6, 1.7, 1.8, 1.9, 2)[priors[2]],
                base = c(0.75, 0.8, 0.85, 0.9, 0.95)[priors[3]],
                keeptrees = TRUE)
  return(model)
  
}
