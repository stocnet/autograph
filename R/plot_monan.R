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
#' @param cumulative Logical, indicating whether the statistics should be
#'  plotted cumulatively (default FALSE).
#' @param ... Additional plotting parameters, use discouraged.
#' @return The function `plot.gof.stats.monan` returns violin plots of the 
#' gof tests with observed values superimposed in red.
#' @examples
#' plot(res_monan_gof, lvls = 1:15)
#' @export
plot.gof.stats.monan <- function(x, cumulative = FALSE, ...) {

  args <- list(...)
  if (is.null(args$main)) {
    main = "Goodness of fit"
  } else {
    main = args$main
  }
  
  obs <- dplyr::tibble(name = as.factor(1:length(x$observed)), 
                       value = x$observed)

  simsMat <- Reduce(rbind, x$simulated)
  sims.min <- apply(simsMat, 2, min)
  sims.max <- apply(simsMat, 2, max)
  no_vary <- sims.min == x$observed & sims.min == sims.max
  if (any(no_vary)) {
    statkeys <- as.character(which(no_vary))
    simsMat <- simsMat[,!no_vary]
    obs <- obs[!no_vary, ]
    cli::cli_alert_info("Note: statistic{?s} {statkeys} not plotted because their variance is 0.")
  }
  colnames(simsMat) <- obs$name
  rownames(simsMat) <- 1:nrow(simsMat)
  sims <- dplyr::tibble(as.data.frame(as.table(simsMat)))
  names(sims) <- c("sim", "name", "value")
  
  p_value <- NULL
  
  if(cumulative){
    sims <- sims %>% dplyr::group_by(sim) %>%
      dplyr::mutate(value = cumsum(.data$value)) %>%
      dplyr::ungroup()
    obs <- obs %>% 
      mutate(value = cumsum(.data$value))
  }
  
  out <- list(obs, sims, main, p_value)
  class(out) <- "ag_gof"
  plot.ag_gof(out)
  
}
