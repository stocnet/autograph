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

  args <- list(...)
  if (is.null(args$main)) {
    main = "Goodness of fit"
  } else {
    main = args$main
  }
  
  obs <- dplyr::tibble(name = as.factor(1:length(x$observed)), 
                       value = x$observed)

  simsMat <- Reduce(rbind, x$simulated)
  colnames(simsMat) <- obs$name
  rownames(simsMat) <- 1:nrow(simsMat)
  sims <- dplyr::tibble(as.data.frame(as.table(simsMat)))
  names(sims) <- c("sim", "name", "value")
  
  p_value <- NULL
  
  
  out <- list(obs, sims, main, p_value)
  class(out) <- "ag_gof"
  plot.ag_gof(out)
  
}
