#' @title Variable contributions
#'
#' @description
#' Crude but: something?
#'
#' @param model Dude that's a model. It better be a freakin lBART model dude
#'
#' @export
#'
#'
#'

var.imp <- function(model) {

  var.m <- model$varcount.mean
  nvar <- length(var.m)
  var.s <- sapply(c(1:nvar), x <- function(j) {sd(model$varcount[,j])})
  var.df <- data.frame(mean=var.m,
                       sd=var.s)

  var.df$names <- factor(rownames(var.df))

  var.df <- transform(var.df, names = reorder(names,
                                            -mean))

  g1 <- ggplot2::ggplot(var.df, aes(y=mean, x=names)) +
    geom_bar(stat="identity", color="black") +
    geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2,
                  position=position_dodge(.9)) +
    ylab("Mean number of branches per tree")

  var.p <- sapply(c(1:nvar), x <- function(j) {sum(model$varcount[,j] > 0)/length(model$varcount[,j])})

  var.df2 <- data.frame(proportion=var.p)
  var.df2$names <- factor(rownames(var.df))
  var.df2 <- transform(var.df2, names = reorder(names,
                                              -var.df$mean))
  min <- min(var.df2$proportion); max <- max(var.df2$proportion)
  g2 <- ggplot2::ggplot(var.df2, aes(y=proportion, x=names)) +
    geom_point(stat="identity", color="black") + ylim(min, max)+
    ylab("Proportion trees included")


  align_plots1 <- function (...) {
    pl <- list(...)
    stopifnot(do.call(all, lapply(pl, inherits, "gg")))
    gl <- lapply(pl, ggplotGrob)
    bind2 <- function(x, y) gtable:::rbind_gtable(x, y, "first")
    combined <- Reduce(bind2, gl[-1], gl[[1]])
    wl <- lapply(gl, "[[", "widths")
    combined$widths <- do.call(grid::unit.pmax, wl)
    grid::grid.newpage()
    grid::grid.draw(combined)
  }

align_plots1(g1,g2)
}
