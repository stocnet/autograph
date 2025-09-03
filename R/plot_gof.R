#' Plotting functions for goodness-of-fit results
#' @param x A goodness of fit object using `RSiena::sienaGOF()` or 
#'   `MoNAn::monanGOF()`.
#' @param ... Additional arguments to be passed to the plotting function.
#' @examples
#' plot(res_siena_gof)
#' @export
plot.ag_gof <- function(x, ...){
  obs <- x[[1]]
  sims <- x[[2]]
  main <- x[[3]]
  p_value <- x[[4]]
  
  ggplot2::ggplot(sims, aes(x = name, y = value)) +
    ggplot2::geom_violin(scale = "width", trim = FALSE, color = ag_base(),
                         draw_quantiles = c(0.05,0.95)) +
    ggplot2::geom_point(data = obs, aes(x = name, y = value),
                        color = ag_highlight()) +
    ggplot2::geom_text(data = obs, aes(label = value), 
                       hjust = 0.05, nudge_x = 0.10,
                       color = ag_highlight()) +
    ggplot2::geom_line(data = obs, aes(x = name, y = value),
                       group = 1,
                       color = ag_highlight()) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Statistic", title = main, 
                  x = if(is.null(p_value)) "" else paste("p:", round(p_value, 3), collapse = " "))
}