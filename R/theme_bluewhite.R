#' @export

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