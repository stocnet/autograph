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
#' @family MoNAn
#' @examples
#' plot(res_monan_traces)
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

