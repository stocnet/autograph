#' Plotting goodness-of-fit results
#' @description
#'   These plot methods plot goodness of fit objects created using `RSiena::sienaGOF()` or 
#'   `MoNAn::monanGOF()`.
#'   Internally, the data is translated into a common format
#'   and then plotted using `{ggplot2}` so that there is a consistent look and feel.
#'   Unlike the plot method included in the `{RSiena}` package,
#'   this function utilises `{ggplot2}` and not `{lattice}`,
#'   which makes the output more compatible and themeable.
#' @name plot_gof
#' @param x An object of class "sienaGOF" or "gof.stats.monan".
#' @param cumulative Logical, indicating whether the statistics should be
#'   plotted cumulatively (default FALSE).
#' @param ... Other parameters to be passed to the plotting function,
#'   for example `main = "Title"` for a different title than the default.
#' @returns A violin plot showing the distribution of statistics from the 
#'   simulations and a line highlighting the observed statistics.
#' @references
#' Hintze, J. L. and Nelson, R. D. 1998. 
#' "Violin plots: A box plot-density trace synergism". 
#' _The American Statistician_, 52:181–184.
#' \doi{10.1080/00031305.1998.10480559}
#' @export
plot.ag_gof <- function(x, ...){
  obs <- x[[1]]
  sims <- x[[2]]
  main <- x[[3]]
  p_value <- x[[4]]
  
  # Compute quantiles for each x
  bounds <- sims %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      q05 = stats::quantile(value, 0.05),
      q95 = stats::quantile(value, 0.95),
      .groups = "drop")
  
  ggplot2::ggplot(sims, aes(x = name, y = value)) +
    ggplot2::geom_violin(scale = "width", trim = FALSE, color = ag_base()) +
    ggplot2::geom_boxplot(width = 0.1, outlier.shape = 4, 
                          fill = ag_base()) +
    ggplot2::geom_line(data = bounds, aes(x = name, y = q05, group = 1), 
                       linetype = "dashed") +
    ggplot2::geom_line(data = bounds, aes(x = name, y = q95, group = 1), 
                       linetype = "dashed") +
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

#' @rdname plot_gof
#' @family RSiena
#' @examples
#' plot(res_siena_gof)
#' @export
plot.sienaGOF <- function(x, cumulative = FALSE, ...){
  
  args <- list(...)
  if (is.null(args$main)) {
    main = paste("Goodness of Fit of", attr(x, "auxiliaryStatisticName"))
    if (!attr(x, "joined")) {
      main = paste(main, "Period", period)
    }
  } else {
    main = args$main
  }
  if (attr(x, "joined")) {
    x <- x[[1]]
  } else {
    x <- x[[period]]
  }
  sims <- x$Simulations
  sims.min <- apply(sims, 2, min)
  sims.max <- apply(sims, 2, max)
  if(is.null(colnames(sims)) & !is.null(attr(x, "key"))){
    colnames(sims) <- attr(x, "key") # required for GOFs pre RSiena 1.3.20
  }
  obs <- x$Observations
  no_vary <- sims.min == obs & sims.min == sims.max
  if (any((diag(stats::var(rbind(sims, obs))) == 0))) {
    statkeys <- attr(x, "key")[which(diag(stats::var(rbind(sims, obs))) == 0)]
    cli::cli_alert_info("Note: statistic{?s} {statkeys} not plotted because their variance is 0.")
  }
  
  itns <- nrow(sims)
  n.obs <- nrow(obs)
  sims <- sims[,!no_vary]
  sims <- as.data.frame(sims) %>% 
    dplyr::mutate(sim = 1:nrow(sims)) %>% 
    tidyr::pivot_longer(!sim)
  obs <- obs[!no_vary]
  obs <- as.data.frame(obs) %>% 
    tidyr::pivot_longer(cols = dplyr::everything()) %>% 
    dplyr::mutate(name = as.character((1:length(obs))-1))
  
  if(!cumulative){
    sims <- sims %>% dplyr::group_by(sim) %>%
      dplyr::mutate(value = c(.data$value[1], diff(.data$value))) %>%
      dplyr::ungroup()
    obs <- obs %>% 
      mutate(value = c(.data$value[1], diff(.data$value)))
  }
  
  out <- list(obs, sims, main, x$p)
  class(out) <- "ag_gof"
  plot(out)
}

#' @rdname plot_gof
#' @family MoNAn
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
