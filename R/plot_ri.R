#' @title A plot tool for riBART models
#'
#' @description
#'
#' Tiny code for tiny task. 
#'
#' @param model Object of class rbart
#' @param temporal Is this yearly data (line plot) or categorical
#'  
#' @return Returns a plot
#' 
#' @export
#'
#'
#'
plot.ri <- function(model, temporal=TRUE) {
  
  if(temporal == TRUE) {
    model$ranef %>% as_tibble() %>% gather() %>%
      group_by(key) %>%
      summarise(mean = mean(value),
                upper = quantile(value, 0.975, na.rm = TRUE),
                lower = quantile(value, 0.025, na.rm = TRUE)) %>%
      mutate(key = as.numeric(key)) -> df
    
    df %>% ggplot(aes(x = key, y = mean, ymin=lower, ymax=upper)) + 
      geom_ribbon(fill='grey95') + 
      geom_line(y=0, lty=2, col='grey20') +
      geom_line(color="#00AFDD", lwd=1.35) + 
      #geom_point(color="#00AFDD", cex=1.5) + 
      xlab(NULL) + ylab("Intercept") +
      theme_bw() + theme(legend.position = "none",
                         axis.text.x = element_text(angle=90, vjust=0.5),
                         axis.title.x = element_text(size=rel(1.3), vjust = -0.8),
                         axis.text.y = element_text(size=rel(1.4)),
                         plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                         panel.grid.minor = element_blank()) -> p
  } else {
    
    modelH.ri$ranef %>% data.frame() %>% gather() %>%
      group_by(key) %>%
      summarise(mean = mean(value),
                upper = quantile(value, 0.975, na.rm = TRUE),
                lower = quantile(value, 0.025, na.rm = TRUE)) %>% 
      #transform(key = reorder(key, mean)) %>% 
      ggplot(aes(x = key, y = mean)) +
      geom_pointrange(aes(y = mean, x = key, ymin = lower, ymax = upper),
                      color="#00AFDD") + 
      xlab(NULL) + ylab("Intercept") + coord_flip() + 
      theme_bw() + theme(legend.position = "none",
                         axis.text.x = element_text(angle=90),
                         axis.title.x = element_text(size=rel(1.3), vjust = -0.8),
                         axis.text.y = element_text(size=rel(1.4)),
                         plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
                         panel.grid.minor = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.major.y = element_line(color='grey',
                                                           linetype='dashed')) -> p
  }
  print(p)
}
