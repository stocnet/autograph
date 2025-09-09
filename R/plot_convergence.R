#' Plotting convergence diagnostics
#' @description
#'   These plotting methods are for diagnosing the convergence of
#'   simulation-based estimation procedures, such as those used in
#'   MoNAn and ergm.
#'   These plots are useful for identifying whether the estimation procedure
#'   has adequately explored the state space and converged to a stable
#'   distribution.
#' @name plot_convergence
#' @importFrom patchwork plot_layout
#' @param x An object of class "traces.monan".
#' @param ... Additional plotting parameters, currently unused.
#' @return The function shows a line plot tracing the statistics obtained at
#'   each simulation step, as well as a density plot showing the distribution
#'   of the statistics over the entire simulation.
NULL

#' @rdname plot_convergence
#' @export
plot.ag_conv <- function(x, ...){
  dat <- x
  trace_plot <- ggplot2::ggplot(dat, aes(x = sim, y = value), shape = 1, color = ag_base()) + 
    ggplot2::geom_line() + 
    ggplot2::facet_grid(name ~ ., scales = "free", switch = "y") + 
    ggplot2::geom_smooth(formula = y ~ x, method = "loess", se = FALSE, 
                         color = ag_highlight(), linewidth = 0.5) +
    ggplot2::theme_minimal(base_family = ag_font()) +
    ggplot2::theme(axis.text.y = element_blank(),
                   strip.text.y.left = element_text(angle = 0)) +
    ggplot2::labs(x = "Simulation step", y = "")
  density_plot <- ggplot2::ggplot(dat, aes(x = value)) +
    ggplot2::geom_density(fill = ag_base(), alpha = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(name ~ ., scales = "free_y", switch = "x") +
    ggplot2::theme_void() +
    ggplot2::theme(strip.text.y = element_blank())
  trace_plot + density_plot + patchwork::plot_layout(ncol = 2, widths = c(5, 1))
}

#' @rdname plot_convergence
#' @family MoNAn
#' @examples
#' plot(monan_conv)
#' @export
plot.traces.monan <- function(x, ...) {
  nParams <- length(x[[1]])
  nSims <- length(x[[2]][, 1])
  dat <- x[[2]] %>% dplyr::as_tibble() %>% dplyr::mutate(sim = 1:dplyr::n()) %>% 
    as.data.frame() %>% dplyr::select(sim, dplyr::everything())
  dat <- stats::reshape(data = dat, # tidyr::pivot_longer replacement
                 direction = "long",
                 varying = list(colnames(dat)[-1]),
                 v.names = "value",
                 timevar = "name",
                 times = colnames(dat)[-1],
                 idvar = "sim") %>% 
    dplyr::tibble() %>% dplyr::arrange(sim)
  # dat <- dat %>% dplyr::mutate(name = gsub("_","\n",name, fixed = TRUE))
  # dat <- dat %>% dplyr::mutate(name = gsub(" ","\n",name, fixed = TRUE))
  class(dat) <- c("ag_conv", class(dat))
  plot(dat)
}

#' @rdname plot_convergence
#' @family ergm
#' @examples
#' plot(ergm_res$sample)
#' @export
plot.mcmc.list <- function(x, ...) {
  dat <- x[[1]] %>% dplyr::as_tibble() %>% dplyr::mutate(sim = 1:dplyr::n()) %>% 
    as.data.frame()
  dat <- stats::reshape(data = dat, # tidyr::pivot_longer replacement
                        direction = "long",
                        varying = list(colnames(dat)[-ncol(dat)]),
                        v.names = "value",
                        timevar = "name",
                        times = colnames(dat)[-ncol(dat)],
                        idvar = "sim") %>% 
    dplyr::tibble() %>% dplyr::arrange(sim)
  class(dat) <- c("ag_conv", class(dat))
  plot(dat)
}

