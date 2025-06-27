#' plot.traces.monan
#' @name plot_monan_trace
#' @param x An object of class "traces.monan".
#' @param ... Additional plotting parameters, use not recommended.
#' @return The function `plot.traces.monan` shows a scatter plot of the
#'   statistics of simulated networks from phase three of the esimtation.
#' @examples
#' plot(res_monan_traces)
#' @export
plot.traces.monan <- function(x, ...) {
  nParams <- length(x[[1]])
  nSims <- length(x[[2]][, 1])
  dat <- x[[2]] %>% dplyr::as_tibble() %>% dplyr::mutate(sim = 1:dplyr::n()) %>% 
    tidyr::pivot_longer(-sim)
  dat <- dat %>% dplyr::mutate(name = gsub("_","\n",name, fixed = TRUE))
  dat <- dat %>% dplyr::mutate(name = gsub(" ","\n",name, fixed = TRUE))
  ggplot2::ggplot(dat, aes(x = sim, y = value), shape = 1, color = ag_base()) + 
    geom_point() + ggplot2::facet_grid(. ~ name) + ggplot2::theme_minimal() +
    ggplot2::theme(strip.text.x = element_text(size=6))
}

#' plot.gof.stats.monan
#' @name plot_monan_gof
#' @param x An object of class "gof.stats.monan".
#' @param lvls The values for which the gofFunction should be calculated/plotted.
#' @param ... Additional plotting parameters, use discouraged.
#' @return The function `plot.gof.stats.monan` returns violin plots of the 
#' gof tests with observed values superimposed in red.
#' @examples
#' plot(res_monan_gof, lvls = 1:15)
#' @export
plot.gof.stats.monan <- function(x, lvls, ...) {
  if (missing(lvls)) {
    lvls <- 1:length(x$observed)
  }
  simStats <- Reduce(rbind, x$simulated)
  sim <- simStats[, lvls] %>% dplyr::as_tibble() %>% 
    dplyr::mutate(sim = 1:dplyr::n()) %>% 
    tidyr::pivot_longer(-sim) %>% 
    dplyr::mutate(name = formatC(gsub("V","",name), width = 2))
  
  obs <- data.frame(name = sim$name[1:length(x$observed)], value = x$observed)
  
  ggplot2::ggplot(sim, aes(x = name, y = value)) +
    ggplot2::geom_violin(scale = "width", trim = FALSE, color = ag_base(),
                         draw_quantiles = c(0.05,0.95)) +
    ggplot2::geom_point(data = obs, 
                        aes(x = name, y = value),
                        color = ag_highlight()) +
    ggplot2::geom_line(data = obs, aes(x = name, y = value),
                       group = 1,
                       color = ag_highlight()) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Statistic", x = "")
  
}



