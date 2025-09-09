#' Plotting goodness-of-fit results
#' @description
#'   These plot methods plot goodness of fit objects created using 
#'   `RSiena::sienaGOF()`, `MoNAn::monanGOF()`, or the 'ergm' package's gof() function.
#'   Internally, the GOF object is translated into a common class (`ag_gof`),
#'   which has its own plot method to ensure a consistent look and feel.
#'   It is not expected that users will create `ag_gof` class objects themselves.
#'   
#'   The plot shows a violin plot of the distribution of statistics from the
#'   simulations, with a boxplot inside the violin to show the interquartile range,
#'   and dashed lines connecting the 5th and 95th percentiles.
#'   The boxplot also shows outliers as crosses.
#'   The observed statistics are shown as points and connected by a line.
#'   The observed statistics are also labelled with their value.
#'   If a p-value is available (as in the case of `RSiena::sienaGOF()`),
#'   it is shown beneath the x-axis.
#' @details
#'   Since these plots methods are in `{autograph}`,
#'   the plots are automatically themed according to the current theme
#'   set using `stocnet_theme()`.
#'   The function uses the highlight colour defined in the current theme
#'   to highlight the observed statistics.
#'   The function also uses the base colour defined in the current theme
#'   to draw the violin and box plots.
#'   
#'   It is however completely customisable.
#'   While a title is automatically generated so that the graph is informative, 
#'   this can be customised by specifying the `main` argument in the plotting function,
#'   or added after the fact using `{ggplot2}` functions such as
#'   `ggtitle()` or `labs()`.
#'   
#'   The user can choose whether to plot the statistics cumulatively or not.
#'   This is typically handled within `RSiena::sienaGOF()`, 
#'   but for `MoNAn::monanGOF()` and the 'ergm' package's gof() function
#'   the cumulative option is handled here.
#'   The default is to plot the non-cumulative statistics.
#'   This is because the non-cumulative statistics are often more interpretable,
#'   and the cumulative statistics can be obtained by setting `cumulative = TRUE`.
#'   
#'   The function also checks whether any of the statistics have zero variance
#'   across the simulations, and if so, these statistics are not plotted,
#'   with a message to the user indicating which statistics were omitted.
#'   
#'   Note that these methods overwrite any plot methods for these classes
#'   that may be provided by the original packages.
#'   You may receive such a warning in the console when loading the package.
#'   Please load `{autograph}` after these other packages to ensure the plotting
#'   methods included in this package are used,
#'   or specify the package when calling the plotting method directly,
#'   e.g., `autograph:::plot.sienaGOF(res_siena_gof)`.
#' @name plot_gof
#' @param x An object of class "sienaGOF", "gof.stats.monan", or "gof.ergm".
#' @param cumulative Logical, indicating whether the statistics should be
#'   plotted cumulatively (default FALSE).
#'   This is typically treated in `sienaGOF()` for `{RSiena}`,
#'   but treated within the plotting function for `{MoNAn}` and 'ergm'.
#' @param ... Other parameters to be passed to the plotting function,
#'   for example `main = "Title"` for a different title than the default.
#' @returns A violin plot showing the distribution of statistics from the 
#'   simulations and a line joining points showing the observed statistics.
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
    ggplot2::theme_minimal(base_family = ag_font()) +
    ggplot2::labs(y = "Statistic", title = main, 
                  x = if(is.null(p_value)) "" else paste("p:", round(p_value, 3), collapse = " "))
}

#' @rdname plot_gof
#' @family MoNAn
#' @examples
#' plot(monan_gof)
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
    snet_info("Note: statistic{?s} {statkeys} not plotted because their variance is 0.")
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

#' @rdname plot_gof
#' @family RSiena
#' @examples
#' plot(siena_gof, cumulative = TRUE)
#' @export
plot.sienaGOF <- function(x, cumulative = FALSE, ...){
  
  args <- list(...)
  if (is.null(args$main)) {
    statName <- tolower(add_spaces(attr(x, "auxiliaryStatisticName")))
    main = paste("Goodness of fit of", statName)
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
    snet_info("Note: statistic{?s} {statkeys} not plotted because their variance is 0.")
  }
  
  itns <- nrow(sims)
  n.obs <- nrow(obs)
  sims <- sims[,!no_vary]
  sims <- as.data.frame(sims) %>% dplyr::mutate(sim = 1:nrow(sims))
  sims <- stats::reshape(sims, varying = list(colnames(sims)[-ncol(sims)]), 
                         v.names = "value", timevar = "name", times = colnames(sims)[-ncol(sims)],
                         idvar = "sim", direction = "long") %>% 
    dplyr::tibble() %>% dplyr::arrange(sim)
  obs <- obs[!no_vary]
  obs <- data.frame(name = as.character((1:length(obs))-1), value = obs)
  
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
#' @family ergm
#' @param statistic Character, indicating which statistic to plot.
#'   Since 'ergm' package GOFs include goodness of fit on multiple statistics,
#'   the user must specify which statistic to plot.
#'   Options are `"deg"` (degree distribution), `"espart"` (edgewise shared partners),
#'   and `"dist"` (geodesic distance).
#'   The default is `"deg"`.
#' @examples
#' plot(ergm_gof, statistic = "espart")
#' @export
plot.gof.ergm <- function(x, cumulative = FALSE, 
                          statistic = c("deg","espart","dist"), ...){
  statistic <- match.arg(statistic)
  args <- list(...)
  if (is.null(args$main)) {
    statdescription <- switch(statistic,
                              deg = "degree distribution",
                              espart = "edgewise shared partners",
                              dist = "geodesic distance")
    main = paste("Goodness of fit of", statdescription)
  } else {
    main = args$main
  }
  
  obs <- data.frame(name = as.factor(as.numeric(names(x[[paste0("obs.",statistic)]]))),
                    value = x[[paste0("obs.",statistic)]]) %>% 
    dplyr::tibble()
  simsMat <- x[[paste0("sim.",statistic)]]
  sims.min <- apply(simsMat, 2, min)
  sims.max <- apply(simsMat, 2, max)
  no_vary <- sims.min == obs$value & sims.min == sims.max
  if (any(no_vary)) {
    statkeys <- names(no_vary)[which(no_vary)]
    simsMat <- simsMat[,!no_vary]
    obs <- obs[!no_vary, ]
    snet_info("Note: statistic{?s} {statkeys} not plotted because their variance is 0.")
  }
  sims <- as.data.frame(simsMat)
  sims$sim <- 1:nrow(sims)
  sims <- stats::reshape(sims,
                 direction = "long",
                 varying = list(colnames(sims)[-ncol(sims)]),
                 v.names = "value",
                 timevar = "name",
                 times = colnames(sims)[-ncol(sims)],
                 idvar = "sim") %>% 
    dplyr::tibble() %>% 
    dplyr::mutate(name = as.factor(as.numeric(name))) %>% 
    dplyr::arrange(sim)
  
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