#' plot.traces.monan
#' @name plot_monan_trace
#' @family MoNAn
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

